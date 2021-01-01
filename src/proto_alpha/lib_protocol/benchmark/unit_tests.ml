[@@@warning "-27"]

[@@@warning "-26"]

[@@@warning "-21"]

[@@@warning "-33"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Pipeline
open Util
open Script_tagged_ir

let () = Printexc.record_backtrace true

let int_to_my_int n = My_int (Script_int.of_int n)

let i = int_to_my_int

let n n = My_nat (Obj.magic (Script_int.of_int n))

let s s = My_string s

let t = My_bool true

let f = My_bool false

let contract x =
  My_contract_item (x, Contract_type (Script_typed_ir.Unit_t None, "default"))

let address x = My_address_item (x, "default")

let (b, contracts) = Context.init 2 |> force_global_lwt

let (sender, recipient) =
  match contracts with a :: b :: _ -> (a, b) | _ -> assert false

let step_constants : Script_interpreter.step_constants =
  {
    source = sender;
    payer = sender;
    self = recipient;
    amount = Tez.zero;
    chain_id = Chain_id.zero;
  }

let counter = ref 0
let make_test instrs stack expected =
  let n = !counter in
incr counter;
  let _ =
    let ctxt = get_next_context b in
    let instr_arr : my_instr array = Array.of_list (instrs @ [ My_halt ]) in
    Printf.printf "test: %d\n%!" n;
    Lwt.bind
      (Script_interpreter.step_tagged None ctxt step_constants instr_arr stack)
      (fun x ->
        match x with
        | Ok (s, _) ->
            if expected = s then Error_monad.return ()
            else (
              print_endline "!!!!!!!!!! Unmatched expectation !!!!!!!!!!!!!" ;
              print_endline "Expected:" ;
              print_endline @@ show_my_item_list expected ;
              print_endline "Got" ;
              print_endline @@ show_my_item_list s ;
              assert false )
        | Error _ -> assert false)
  in
  ()

(* let make_test ?focus instrs stack expected =
  match focus with None -> () | Some _ -> make_test instrs stack expected *)

let () =
  (*(* Stack instructions *)*)
  (*| My_dup*)
  make_test [ My_dup ] [ i 3 ] [ i 3; i 3 ] ;
  (*| My_swap*)
  make_test [ My_swap ] [ i 3; i 7 ] [ i 7; i 3 ] ;
  (*| My_push of my_item*)
  make_test [ My_push (i 3) ] [ i 3 ] [ i 3; i 3 ] ;
  (*| My_dip*)
  make_test [ My_dip; My_dup ] [ i 3; i 7 ] [ i 7; i 7 ] ;
  (*| My_undip*)
  make_test [ My_dip; My_dup; My_undip ] [ i 3; i 7 ; i 10 ] [ i 3; i 7; i 7 ; i 10] ;
  (*| My_drop*)
  make_test [ My_drop ] [ i 3; i 7 ] [ i 7 ] ;
  (*| My_dropn of int*)
  make_test [ My_dropn 2 ] [ i 3; i 7 ] [] ;
  make_test [ My_dropn 2 ] [ i 3; i 7;  i 1 ] [ i 1 ] ;
  (*| My_dig of int*)
  make_test [ My_dig 2 ] [ i 1; i 2; i 3; i 4 ] [ i 3; i 1; i 2; i 4 ] ;
  (*| My_dug of int*)
  make_test [ My_dug 2 ] [ i 1; i 2; i 3; i 4 ] [ i 2; i 3; i 1; i 4 ] ;
  (*(* Loop *)*)
  (*| My_loop_if_not of int*)
  (*| My_jump of int (* Pai  r instructions *)*)
  make_test
    [
      My_loop_if_not 7;
      My_sub;
      My_dup;
      My_neq;
      My_push (i 1);
      My_dug 2;
      My_jump (-6);
      My_swap;
      My_drop;
    ]
    [ t; i 3; i 1 ]
    [ i 0 ] ;
  (*| My_car*)
  make_test [ My_car ] [ My_pair (i 1, i 2) ] [ i 1 ] ;
  (*| My_cons_pair*)
  make_test [ My_cons_pair ] [ i 1; i 2 ] [ My_pair (i 1, i 2) ] ;
  (*| My_nil*)
  make_test [ My_nil ] [] [ My_list [] ] ;
  (*(* Arithmetic instructions *)*)
  (*| My_compare*)
  make_test [ My_compare ] [ i 3; i 2 ] [ i 1 ] ;
  make_test [ My_compare ] [ i 2; i 3 ] [ i (-1) ] ;
  make_test [ My_compare ] [ i 2; i 2 ] [ i 0 ] ;
  (*| My_neq*)
  make_test [ My_neq ] [ i 1 ] [ t ] ;
  make_test [ My_neq ] [ i (-1) ] [ t ] ;
  make_test [ My_neq ] [ i 0 ] [ f ] ;
  (*| My_sub*)
  make_test [ My_sub ] [ i 3; i 2 ] [ i 1 ] ;
  make_test [ My_sub ] [ i 3; i 7 ] [ i (-4) ] ;
  (*| My_mul_int*)
  make_test [ My_mul_int ] [ i 3; i 7 ] [ i 21 ] ;
  (*| My_halt (No need to test)*)
  (*| My_add_int_int*)
  make_test [ My_add_int_int ] [ i 3; i 7 ] [ i 10 ] ;
  (*| My_add_nat_nat*)
  make_test [ My_add_nat_nat ] [ n 3; n 7 ] [ n 10 ] ;
  (*| My_abs*)
  make_test [ My_add_nat_nat ] [ n 3; n 7 ] [ n 10 ] ;
  (*(* Union *)*)
  (*| My_if_left of int*)
  make_test
    (* IF_LEFT { PUSH "foo"; } { PUSH "bar"; }; *)
    [ My_if_left 3; My_push (s "foo"); My_jump 2; My_push (s "bar") ]
    [ My_left (i 0) ]
    [ s "foo"; i 0 ] ;
  make_test
    [ My_if_left 3; My_push (s "foo"); My_jump 2; My_push (s "bar") ]
    [ My_right (i 0) ]
    [ s "bar"; i 0 ] ;
  make_test
    (* IF_None { PUSH "foo"; } { PUSH "bar"; }; *)
    [ My_IF_NONE 3; My_push (s "foo"); My_jump 2; My_push (s "bar") ]
    [ My_none ]
    [ s "foo" ] ;
  make_test
    (* IF_None { PUSH "foo"; } { PUSH "bar"; }; *)
    [ My_IF_NONE 3; My_push (s "foo"); My_jump 2; My_push (s "bar") ]
    [ My_some My_unit ]
    [ s "bar"; My_unit ] ;
  (*| My_IF of int*)
  (* pc + offset *)
  make_test
    [ My_IF 3; My_push (s "left"); My_jump 2; My_push (s "right") ]
    [ t ]
    [ s "left" ] ;
  make_test
    [ My_IF 3; My_push (s "left"); My_jump 2; My_push (s "right") ]
    [ f ]
    [ s "right" ] ;
  (* | My_address_instr *)
  make_test [ My_address_instr ] [ contract sender ] [ address sender ] ;
  (*| My_amount*)
  make_test [ My_amount ] [] [ My_mutez Tez.zero ] ;
  make_test [ My_cdr ] [ My_pair (i 1, i 2) ] [ i 2 ] ;
  (*| My_contract*)
  make_test
    [
      My_contract_instr (Contract_type (Script_typed_ir.Unit_t None, "default"));
    ]
    [ address recipient ]
    [ My_some (contract recipient ) ] ;
  make_test [ My_address_instr ] [ contract sender ] [ address sender ] ;
  (*| My_ediv*)
  make_test [ My_ediv ] [ i 13; i 3 ] [ My_some (My_pair (i 4, n 1)) ] ;
  make_test [ My_ediv ] [ i 13; i 0 ] [ My_none ] ;
  (* | My_EMPTY_MAP (Can't test with (=)*)
  (* make_test [ My_EMPTY_MAP ] [ ] [My_map Script_tagged_ir.my_empty_map] ; *)
  (*| My_EQ*)
  make_test [ My_EQ ] [ i 0 ] [ t ] ;
  make_test [ My_EQ ] [ i 3 ] [ f ] ;
  (*| My_FAILWITH Can't test this*)
  (*| My_GE *)
  make_test [ My_GE ] [ i 3 ] [ t ] ;
  make_test [ My_GE ] [ i 0 ] [ t ] ;
  make_test [ My_GE ] [ i (-1) ] [ f ] ;
  (*| My_big_map_get *)
  (* None case *)
  let big_map =
    { id = None; diff = my_empty_big_map; value_type = Ex_ty (Int_t None) }
  in
  make_test [ My_big_map_get ] [ s "some key"; My_big_map big_map ] [ My_none ] ;

  (* Some case *)
  let diff : my_big_map_diff =
    ( module struct
      include (val my_empty_big_map)

      let value = OPS.add (s "some key") (Some (s "some value")) value
    end )
  in
  let big_map = { big_map with diff } in
  make_test
    [ My_big_map_get ]
    [ s "some key"; My_big_map big_map ]
    [ My_some (s "some value") ] ;
  (*| My_map_get *)
  make_test [ My_map_get ] [ s "some key"; My_map my_empty_map ] [ My_none ] ;
  let map : my_map =
    ( module struct
      include (val my_empty_map)

      let value = OPS.add (s "some key") (s "some value") value
    end )
  in
  make_test
    [ My_map_get ]
    [ s "some key"; My_map map ]
    [ My_some (s "some value") ] ;
  (*| My_map_update *)
  make_test
    [ My_map_update; My_push (s "updated key"); My_map_get ]
    [ s "updated key"; My_some (s "updated value"); My_map my_empty_map ]
    [ My_some (s "updated value") ] ;
  (*| My_big_map_update *)
  make_test
    [ My_big_map_update; My_push (s "updated key"); My_big_map_get ]
    [ s "updated key"; My_some (s "updated value"); My_big_map big_map ]
    [ My_some (s "updated value") ] ;
  (*| My_GT*)
  make_test [ My_GT ] [ i 3 ] [ t ] ;
  make_test [ My_GT ] [ i 0 ] [ f ] ;
  make_test [ My_GT ] [ i (-1) ] [ f ] ;

  (*| My_IF_NONE of int*)
  (*| My_IMPLICIT_ACCOUNT*)
  (*| My_LT*)
  make_test [ My_LT ] [ i 3 ] [ f ] ;
  make_test [ My_LT ] [ i 0 ] [ f ] ;
  make_test [ My_LT ] [ i (-1) ] [ t ] ;
  (*| My_NOT*)
  make_test [ My_NOT ] [ t ] [ f ] ;
  make_test [ My_NOT ] [ f ] [ t ] ;
  (*| My_NOW can't test this one the normal way *)
  let ctxt = get_next_context b in
  make_test [ My_NOW ] [] [ My_timestamp (Script_timestamp.now ctxt) ] ;
  (*| My_OR*)
  make_test [ My_OR ] [ t; t ] [ t ] ;
  make_test [ My_OR ] [ t; f ] [ t ] ;
  make_test [ My_OR ] [ f; t ] [ t ] ;
  make_test [ My_OR ] [ f; f ] [ f ] ;
  (*| My_PAIR*)
  make_test [ My_PAIR ] [ i 1; i 2 ] [ My_pair (i 1, i 2) ] ;
  (* | My_PUSH *)
  make_test [ My_push t ] [] [ t ] ;
  (*| My_SELF*)
  make_test
    [ My_SELF (Contract_type (Script_typed_ir.Unit_t None, "default")) ]
    []
    [ contract recipient ] ;
  (*| My_SENDER*)
  make_test [ My_SENDER ] [] [ address sender ] ;
  (*| My_SET_DELEGATE can probably skip*)
  (*| My_cons_some*)
  make_test [ My_cons_some ] [ i 0 ] [ My_some (i 0) ] ;
  (* | My_TRANSFER_TOKENS *)
  (* let nonce = 0 in
     let operation = assert false in
     let
     make_test
       [ My_TRANSFER_TOKENS ]
       [
         My_unit;
         My_mutez (Tez.of_mutez 1_000L |> Option.get);
         My_contract_item
           (sender, Contract_type (Script_typed_ir.Unit_t None, "default"));
       ]
       [
         My_operation
           ( Internal_operation { source = step_constants.self; operation; nonce },
             lazy_storage_diff );
       ] ; *)
  (*| My_UNIT *)
  make_test [ My_UNIT ] [] [ My_unit ] ;
  (*| My_cons_list*)
  make_test [ My_cons_list ] [ i 0; My_list [ i 1 ] ] [ My_list [ i 0; i 1 ] ] ;
  (*| My_lambda_instr of int  *)
  (*| My_EXEC *)
  (*| My_apply *)
  (*| My_cdr*)
  (*
     LAMBDA {CDR;}
     EXEC;
     CDR;
  *)
  make_test
    [ My_lambda_instr 1; My_cdr; My_RET; My_swap; My_EXEC; My_cdr ]
    [ My_pair (i 1, My_pair (i 2, i 3)); ]
    [ i 3 ] ;
  (*
     LAMBDA { # 1 :: []
       # (1, (2, 3))
       CDR
     }
     APPLY # \x :: 1
     PUSH (Pair 2, 3) # \x
     EXEC # (2, 3) :: \x
     CDR
  *)
  make_test
    [
      My_lambda_instr 1;
      My_cdr;
      My_RET;
      My_push (i 1);
      My_apply;
      My_push (My_pair (i 2, i 3));
      My_EXEC;
      My_cdr;
    ]
    []
    [ i 3 ] ;
  ()

let () = print_endline "All tests pass :thumbs up:"
