[@@@warning "-27"]

[@@@warning "-26"]

[@@@warning "-21"]

[@@@warning "-33"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Pipeline
open Util
open Script_tagged_ir

let int_to_my_int n = My_int (Script_int.of_int n)

let i = int_to_my_int

let n n = My_nat (Obj.magic (Script_int.of_int n))

let s s = My_string s

let t = My_bool true

let f = My_bool false

let (b, contracts) = Context.init 2 |> force_global_lwt

let (sender, recipient) =
  match contracts with a :: b :: _ -> (a, b) | _ -> assert false

let make_test instrs stack expected =
  let _ =
    let step_constants : Script_interpreter.step_constants =
      {
        source = sender;
        payer = sender;
        self = recipient;
        amount = Tez.zero;
        chain_id = Chain_id.zero;
      }
    in
    let ctxt = get_next_context b in
    let instr_arr : my_instr array = Array.of_list (instrs @ [ My_halt ]) in
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
  make_test [ My_dip; My_dup; My_undip ] [ i 3; i 7 ] [ i 3; i 7; i 7 ] ;
  (*| My_drop*)
  make_test [ My_drop ] [ i 3; i 7 ] [ i 7 ] ;
  (*| My_dropn of int*)
  make_test [ My_dropn 2 ] [ i 3; i 7 ] [] ;
  (*| My_dig of int*)
  make_test [ My_dig 2 ] [ i 1; i 2; i 3 ] [ i 3; i 1; i 2 ] ;
  (*| My_dug of int*)
  make_test [ My_dug 2 ] [ i 1; i 2; i 3 ] [ i 2; i 3; i 1 ] ;
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
    [ My_if_left 1; My_push (s "foo"); My_jump 2; My_push (s "bar") ]
    [ My_left (i 0) ]
    [ s "foo"; i 0 ] ;
  make_test
    [ My_if_left 2; My_push (s "foo"); My_jump 2; My_push (s "bar") ]
    [ My_right (i 0) ]
    [ s "bar"; i 0 ] ;
  (* | My_address_instr *)
  make_test
    [ My_address_instr ]
    [ My_contract_item (sender, "default") ]
    [ My_address_item (sender, "default") ] ;
  (*| My_amount*)
  make_test [ My_amount ] [] [ My_mutez Tez.zero ] ;
  (*| My_lambda_instr of int FIXME: *)
  (*| My_EXEC FIXME:*)
  (*| My_apply FIXME:*)
  (* make_test [My_amount ] [] [My_mutez Tez.zero] ; *)
  (*| My_cdr*)
  (* make_test [ My_car ] [ My_pair (i 1, i 2) ] [ i 2 ] ; *)
  (*| My_contract*)
  make_test
    [
      My_contract_instr
        (Contract_type (Script_typed_ir.Unit_t None, "default"));
    ]
    [ My_address_item (recipient, "default") ]
    [ My_contract_item (recipient, "default") ] ;
  make_test [ My_address_instr ] [ My_contract_item sender] [My_address_item sender] ;
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
  (*| My_big_map_get FIXME: *)
  (*| My_map_get FIXME: *)
  (*| My_map_update FIXME:*)
  (*| My_GT*)
  make_test [ My_GT ] [ i 3 ] [ t ] ;
  make_test [ My_GT ] [ i 0 ] [ f ] ;
  make_test [ My_GT ] [ i (-1) ] [ f ] ;
  (*| My_IF of int*)
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
  make_test [ My_SELF "default" ] [] [ My_contract_item (recipient, "default") ] ;
  (*| My_SENDER*)
  make_test [ My_SENDER ] [] [ My_address_item (sender, "default") ] ;
  (*| My_SET_DELEGATE can probably skip*)
  (*| My_cons_some*)
  make_test [ My_cons_some ] [ i 0 ] [ My_some (i 0) ] ;
  (*| My_TRANSFER_TOKENS FIXME:*)
  (*| My_UNIT *)
  make_test [ My_UNIT ] [] [ My_unit ] ;
  (*| My_cons_list*)
  make_test [ My_cons_list ] [ i 0; My_list [ i 1 ] ] [ My_list [ i 0; i 1 ] ] ;
  ()

let () = print_endline "All tests pass :thumbs up:"
