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
module M = struct
  type t = unit

  type key = string list

  type value = Bytes.t

  type tree = unit

  module Tree = struct
    let hash _ = assert false

    let empty _ = assert false

    let equal _ _ = assert false

    let is_empty _ = assert false

    let mem _ _ = assert false

    let kind _ = assert false

    let find _ _ = Lwt.return @@ Some (Bytes.make 0 ' ')

    let add _ _ _ = Lwt.return ()

    let remove _ _ = assert false

    let mem_tree _ _ = assert false

    let find_tree _ _ = Lwt.return_none

    let add_tree _ _ _ = Lwt.return_unit

    let clear ?depth:_ _ = assert false

    let list _ ?offset:_ ?length:_ _ = assert false

    let fold ?depth:_ _ _ ~init:_ ~f:_ = assert false
  end

  include Tree

  let set_protocol _ _ = assert false

  let get_protocol _ = assert false

  let fork_test_chain _ ~protocol:_ ~expiration:_ = assert false
end

include Environment_context.Register (M)

let empty =
  Context.Context
    {ops; ctxt = (); kind = Context; equality_witness; impl_name = "fake"}

let copy ctxt ~from ~to_ =
  M.find_tree ctxt from
  >>= function
  | None ->
      Lwt.return_none
  | Some sub_tree ->
      M.add_tree ctxt to_ sub_tree >>= Lwt.return_some

module Test_context = struct
  let copy_dest_exists () =
    let mc = empty in
    let foo_value_1 = Bytes.of_string "foovalue-1" in
    let foo_value_2 = Bytes.of_string "foovalue-2" in
    Context.add mc ["foo"; "source"] foo_value_1
    >>= fun mc ->
    Context.add mc ["foo"; "dest"] foo_value_2
    >>= fun mc ->
    copy mc ~from:["foo"; "source"] ~to_:["foo"; "dest"]
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
