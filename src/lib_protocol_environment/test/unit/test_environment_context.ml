(** Testing
    -------
    Component:    Tezos_protocol_environment.Context
    Invocation:   dune build @unittests src/lib_protocol_environment/test/unit
    Dependencies: --
    Subject:      Unit tests for Tezos_protocol_environment.Context 
*)

let c = function None -> None | Some s -> Some (Bytes.to_string s)

(* NOTE: Context is generic module whose functions are invoked by many consuming
   modules (context, Raw_context). Therefore, it's bound to get the most coverage
   while unit testing. For now, only the failure path of [copy] is tested. In future,
   we could add more robust tests *)

(** Tests for Context **)
module Dummy_context = struct
  module M = struct
    type t = unit

    type key = string list

    type value = Bytes.t

    let mem _ _ = assert false

    let dir_mem _ _ = assert false

    let get _ _ = Lwt.return @@ Some (Bytes.make 0 ' ')

    let set _ _ _ = Lwt.return ()

    let copy _ ~from:_ ~to_:_ = Lwt.return @@ None

    let remove_rec _ _ = assert false

    type key_or_dir = [`Key of key | `Dir of key]

    let fold _ _ ~init:_ ~f:_ = assert false

    let set_protocol _ _ = assert false

    let fork_test_chain _ ~protocol:_ ~expiration:_ = assert false
  end

  open Tezos_protocol_environment

  type _ Context.kind += Faked : unit Context.kind

  let ops = (module M : CONTEXT with type t = 'ctxt)

  let empty = Context.Context {ops; ctxt = (); kind = Faked}
end

module Test_context = struct
  let copy_dest_exists () =
    let mc = Dummy_context.empty in
    let foo_value_1 = Bytes.of_string "foovalue-1" in
    let foo_value_2 = Bytes.of_string "foovalue-2" in
    Context.set mc ["foo"; "source"] foo_value_1
    >>= fun mc ->
    Context.set mc ["foo"; "dest"] foo_value_2
    >>= fun mc ->
    Context.copy mc ~from:["foo"; "source"] ~to_:["foo"; "dest"]
    >>= (function
          | Some _ ->
              Assert.fail_msg
                "Dummy_context was design with copy operation that fails. It \
                 should not have succeeded"
          | None ->
              Lwt.return ())
    >>= fun () -> return_unit
end

let tests =
  [ Test.tztest
      "context.ml: copy operation on a Dummy context that always fails"
      `Quick
      Test_context.copy_dest_exists ]
