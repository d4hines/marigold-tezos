(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Legacy_logging.Make_semantic (struct
  let name = Protocol.name ^ ".baking.denunciation"
end)

open Protocol
open Alpha_context
open Protocol_client_context
open Client_baking_blocks
open Logging

module HLevel = Hashtbl.Make (struct
  type t = Chain_id.t * Raw_level.t

  let equal (c, l) (c', l') = Chain_id.equal c c' && Raw_level.equal l l'

  let hash (c, lvl) = Hashtbl.hash (c, lvl)
end)

module Baker_map = Map.Make (Baker_hash)

type state = {
  (* Endorsements seen so far *)
  endorsements_table : Kind.endorsement operation Baker_map.t HLevel.t;
  (* Blocks received so far *)
  blocks_table : Block_hash.t Baker_map.t HLevel.t;
  (* Maximum delta of level to register *)
  preserved_levels : int;
  (* Highest level seen in a block *)
  mutable highest_level_encountered : Raw_level.t;
}

let create_state ~preserved_levels =
  Lwt.return
    {
      endorsements_table = HLevel.create preserved_levels;
      blocks_table = HLevel.create preserved_levels;
      preserved_levels;
      highest_level_encountered = Raw_level.root (* 0l *);
    }

(* We choose a previous offset (5 blocks from head) to ensure that the
   injected operation is branched from a valid predecessor. *)
let get_block_offset level =
  match Raw_level.of_int32 5l with
  | Ok min_level ->
      Lwt.return (if Raw_level.(level < min_level) then `Head 0 else `Head 5)
  | Error errs ->
      let errs = Environment.wrap_tztrace errs in
      lwt_log_error
        Tag.DSL.(
          fun f ->
            f "Invalid level conversion : %a"
            -% t event "invalid_level_conversion"
            -% a errs_tag errs)
      >>= fun () -> Lwt.return (`Head 0)

let process_endorsements (cctxt : #Protocol_client_context.full) state
    (endorsements : Alpha_block_services.operation list) level =
  List.iter_es
    (fun {Alpha_block_services.shell; chain_id; receipt; hash; protocol_data; _}
         ->
      let chain = `Hash chain_id in
      match (protocol_data, receipt) with
      | ( Operation_data
            { contents =
                Single
                  (Endorsement_with_slot
                    { endorsement =
                        { protocol_data =
                            {contents = Single (Endorsement _); _} as
                            protocol_data;
                          _ };
                      _ });
              _ },
          Some
            Apply_results.(
              Operation_metadata
                { contents =
                    Single_result
                      (Endorsement_with_slot_result
                        (Endorsement_result {baker; slots = slot :: _; _}));
                  mapped_keys = _ }) ) -> (
          let new_endorsement : Kind.endorsement Alpha_context.operation =
            {shell; protocol_data}
          in
          let map =
            Option.value ~default:Baker_map.empty
            @@ HLevel.find state.endorsements_table (chain_id, level)
          in
          (* If a previous endorsement made by this pkh is found for
             the same level we inject a double_endorsement *)
          match Baker_map.find baker map with
          | None ->
              return
              @@ HLevel.add
                   state.endorsements_table
                   (chain_id, level)
                   (Baker_map.add baker new_endorsement map)
          | Some existing_endorsement
            when Block_hash.(
                   existing_endorsement.shell.branch
                   <> new_endorsement.shell.branch) ->
              get_block_offset level
              >>= fun block ->
              Alpha_block_services.hash cctxt ~chain ~block ()
              >>=? fun block_hash ->
              Alpha_services.Forge.double_endorsement_evidence
                cctxt
                (`Hash chain_id, block)
                ~branch:block_hash
                ~op1:existing_endorsement
                ~op2:new_endorsement
                ~slot
                ()
              >>=? fun bytes ->
              let bytes = Signature.concat bytes Signature.zero in
              lwt_log_notice
                Tag.DSL.(
                  fun f ->
                    f "Double endorsement detected"
                    -% t event "double_endorsement_detected"
                    -% t
                         conflicting_endorsements_tag
                         (existing_endorsement, new_endorsement))
              >>= fun () ->
              (* A denunciation may have already occurred *)
              Shell_services.Injection.operation cctxt ~chain bytes
              >>=? fun op_hash ->
              lwt_log_notice
                Tag.DSL.(
                  fun f ->
                    f "Double endorsement evidence injected %a"
                    -% t event "double_endorsement_denounced"
                    -% t signed_operation_tag bytes
                    -% a Operation_hash.Logging.tag op_hash)
              >>= fun () ->
              return
              @@ HLevel.replace
                   state.endorsements_table
                   (chain_id, level)
                   (Baker_map.add baker new_endorsement map)
          | Some _ ->
              (* This endorsement is already present in another
                   block but endorse the same predecessor *)
              return_unit )
      | _ ->
          lwt_log_error
            Tag.DSL.(
              fun f ->
                f "Inconsistent endorsement found %a"
                -% t event "inconsistent_endorsement"
                -% a Operation_hash.Logging.tag hash)
          >>= fun () -> return_unit)
    endorsements
  >>=? fun () -> return_unit

let process_block (cctxt : #Protocol_client_context.full) state
    (header : Alpha_block_services.block_info) =
  match header with
  | {hash; metadata = None; _} ->
      lwt_log_error
        Tag.DSL.(
          fun f ->
            f "Unexpected pruned block: %a"
            -% t event "unexpected_pruned_block"
            -% a Block_hash.Logging.tag hash)
      >>= fun () -> return_unit
  | { Alpha_block_services.chain_id;
      hash;
      metadata = Some {protocol_data = {baker; level = {level; _}; _}; _};
      _ } -> (
      let chain = `Hash chain_id in
      let map =
        match HLevel.find state.blocks_table (chain_id, level) with
        | None ->
            Baker_map.empty
        | Some x ->
            x
      in
      match Baker_map.find baker map with
      | None ->
          return
          @@ HLevel.add
               state.blocks_table
               (chain_id, level)
               (Baker_map.add baker hash map)
      | Some existing_hash when Block_hash.( = ) existing_hash hash ->
          (* This case should never happen *)
          lwt_debug
            Tag.DSL.(
              fun f ->
                f
                  "Double baking detected but block hashes are equivalent. \
                   Skipping..."
                -% t event "double_baking_but_not")
          >>= fun () ->
          return
          @@ HLevel.replace
               state.blocks_table
               (chain_id, level)
               (Baker_map.add baker hash map)
      | Some existing_hash ->
          (* If a previous endorsement made by this pkh is found for
           the same level we inject a double_endorsement *)
          Alpha_block_services.header
            cctxt
            ~chain
            ~block:(`Hash (existing_hash, 0))
            ()
          >>=? fun ({shell; protocol_data; _} :
                     Alpha_block_services.block_header) ->
          let bh1 = {Alpha_context.Block_header.shell; protocol_data} in
          Alpha_block_services.header cctxt ~chain ~block:(`Hash (hash, 0)) ()
          >>=? fun ({shell; protocol_data; _} :
                     Alpha_block_services.block_header) ->
          let bh2 = {Alpha_context.Block_header.shell; protocol_data} in
          (* If the blocks are on different chains then skip it *)
          get_block_offset level
          >>= fun block ->
          Alpha_block_services.hash cctxt ~chain ~block ()
          >>=? fun block_hash ->
          Alpha_services.Forge.double_baking_evidence
            cctxt
            (chain, block)
            ~branch:block_hash
            ~bh1
            ~bh2
            ()
          >>=? fun bytes ->
          let bytes = Signature.concat bytes Signature.zero in
          lwt_log_notice
            Tag.DSL.(
              fun f ->
                f "Double baking detected" -% t event "double_baking_detected")
          >>= fun () ->
          (* A denunciation may have already occurred *)
          Shell_services.Injection.operation cctxt ~chain bytes
          >>=? fun op_hash ->
          lwt_log_notice
            Tag.DSL.(
              fun f ->
                f "Double baking evidence injected %a"
                -% t event "double_baking_denounced"
                -% t signed_operation_tag bytes
                -% a Operation_hash.Logging.tag op_hash)
          >>= fun () ->
          return
          @@ HLevel.replace
               state.blocks_table
               (chain_id, level)
               (Baker_map.add baker hash map) )

(* Remove levels that are lower than the [highest_level_encountered] minus [preserved_levels] *)
let cleanup_old_operations state =
  let highest_level_encountered =
    Int32.to_int (Raw_level.to_int32 state.highest_level_encountered)
  in
  let diff = highest_level_encountered - state.preserved_levels in
  let threshold =
    if diff < 0 then Raw_level.root
    else
      Raw_level.of_int32 (Int32.of_int diff)
      |> function Ok threshold -> threshold | Error _ -> Raw_level.root
  in
  let filter hmap =
    HLevel.filter_map_inplace
      (fun (_, level) x ->
        if Raw_level.(level < threshold) then None else Some x)
      hmap
  in
  filter state.endorsements_table ;
  filter state.blocks_table ;
  ()

let endorsements_index = 0

(* Each new block is processed :
   - Checking that every endorser operated only once at this level
   - Checking that every baker injected only once at this level
*)
let process_new_block (cctxt : #Protocol_client_context.full) state
    {hash; chain_id; level; protocol; next_protocol; _} =
  if Protocol_hash.(protocol <> next_protocol) then
    lwt_log_error
      Tag.DSL.(
        fun f ->
          f "Protocol changing detected. Skipping the block."
          -% t event "protocol_change_detected"
        (* TODO which protocols -- in tag *))
    >>= fun () -> return_unit
  else
    lwt_debug
      Tag.DSL.(
        fun f ->
          f "Block level : %a"
          -% t event "accuser_saw_block"
          -% a level_tag level
          -% t Block_hash.Logging.tag hash)
    >>= fun () ->
    let chain = `Hash chain_id in
    let block = `Hash (hash, 0) in
    state.highest_level_encountered <-
      Raw_level.max level state.highest_level_encountered ;
    (* Processing blocks *)
    Alpha_block_services.info cctxt ~chain ~block ()
    >>= (function
          | Ok block_info ->
              process_block cctxt state block_info
          | Error errs ->
              lwt_log_error
                Tag.DSL.(
                  fun f ->
                    f "Error while fetching operations in block %a@\n%a"
                    -% t event "fetch_operations_error"
                    -% a Block_hash.Logging.tag hash
                    -% a errs_tag errs)
              >>= fun () -> return_unit)
    >>=? fun () ->
    (* Processing endorsements *)
    Alpha_block_services.Operations.operations cctxt ~chain ~block ()
    >>= (function
          | Ok operations -> (
            match List.nth operations endorsements_index with
            | Some endorsements ->
                process_endorsements cctxt state endorsements level
            | None ->
                return_unit )
          | Error errs ->
              lwt_log_error
                Tag.DSL.(
                  fun f ->
                    f "Error while fetching operations in block %a@\n%a"
                    -% t event "fetch_operations_error"
                    -% a Block_hash.Logging.tag hash
                    -% a errs_tag errs)
              >>= fun () -> return_unit)
    >>=? fun () ->
    cleanup_old_operations state ;
    return_unit

let create (cctxt : #Protocol_client_context.full) ~preserved_levels
    valid_blocks_stream =
  let process_block cctxt state bi =
    process_new_block cctxt state bi
    >>= function
    | Ok () ->
        lwt_log_notice
          Tag.DSL.(
            fun f ->
              f "Block %a registered"
              -% t event "accuser_processed_block"
              -% a Block_hash.Logging.tag bi.Client_baking_blocks.hash)
        >>= return
    | Error errs ->
        lwt_log_error
          Tag.DSL.(
            fun f ->
              f "Error while processing block %a@\n%a"
              -% t event "accuser_block_error"
              -% a Block_hash.Logging.tag bi.hash
              -% a errs_tag errs)
        >>= return
  in
  let state_maker _ = create_state ~preserved_levels >>= return in
  Client_baking_scheduling.main
    ~name:"accuser"
    ~cctxt
    ~stream:valid_blocks_stream
    ~state_maker
    ~pre_loop:(fun _ _ _ -> return_unit)
    ~compute_timeout:(fun _ -> Lwt_utils.never_ending ())
    ~timeout_k:(fun _ _ () -> return_unit)
    ~event_k:process_block
    ~finalizer:(fun _ -> Lwt.return_unit)
