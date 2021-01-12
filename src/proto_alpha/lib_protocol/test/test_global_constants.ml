open Protocol
open Alpha_context
open Util

(* This module tests that the global table of constants
can be written to and read from across blocks. *)
let get_next_context b =
  force_global_lwt
    ( Incremental.begin_construction b
    >>=? fun b -> return (Incremental.alpha_ctxt b) )

(* This test has a long wind-up, but is very simple:
it just asserts that values written to the global
table of constants persist across blocks. *)
let x () =
  let open Test_transfer in
  (* Set up two contracts, alice and bob to perform
  the operations. *)
  register_two_contracts ()
  >>=? fun (b, alice, bob) ->
  Incremental.begin_construction b
  >>=? fun b ->
  (* Add the operation that writes the constant *)
  let expr = Expr.from_string "Pair 3 7" in
  let ty_expr = Expr.from_string "pair int int" in
  Op.register_global
    (I b)
    alice
    "my constant"
    (Script_repr.lazy_expr ty_expr)
    (Script_repr.lazy_expr expr)
  >>=? fun op ->
  Incremental.add_operation b op
  (* Finalize the new block. *)
  >>=? fun b ->
  Incremental.finalize_block b
  (* Read the contents of the new block
    and assert they're unchanged. *)
  >>=? fun b ->
  let assert_unchanged b =
    let context = get_next_context b in
    let (_, expr_opt) =
      Global_constants.get_opt context "my constant" |> force_lwt
    in
    let (_, result_expr) = Option.get expr_opt in
    assert (expr = result_expr) ;
    Lwt.return_ok b
  in
  assert_unchanged b
  >>=? fun b ->
  (* Fill the block with a bunch of transactions. *)
  let do_many_transfers b =
    Incremental.begin_construction b
    >>=? fun b ->
    n_transactions 10 b alice bob (Util.of_mutez 1000)
    >>=? fun b ->
    Incremental.finalize_block b
    (* Assert the value hasn't changed. *)
    >>=? fun b -> assert_unchanged b
    (* >>= fun () -> Incremental.begin_construction b *)
  in
  (* Repeate 3x. *)
  do_many_transfers b >>=? do_many_transfers >>=? do_many_transfers
  >>= fun _ -> Lwt.return_ok ()

let tests = [Test.tztest "Multiple blocks happy path" `Quick x]
