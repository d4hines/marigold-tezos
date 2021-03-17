(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Gabriel Alfour <gabriel.alfour@gmail.com>              *)
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

let test msg f = Alcotest.test_case msg `Quick f

let check condition msg =
  if condition then () else failwith msg

open Tezos_new_storage

let noop = test "noop" @@ fun () ->  
  ()

let assert_fail f = try f() ; assert false with _ -> ()

let regular_store = test "regular-store" @@ fun () ->
  let open Patricia in
  let open Dev in
  let t = empty in
  assert (get t key_a = nul) ;
  assert (get t key_b = nul) ;
  assert (get t key_c = nul) ;

  let t = set t key_a value_x in
  assert (get t key_a = value_x) ; 
  assert (get t key_b = nul) ;
  assert (get t key_c = nul) ;

  let t = set t key_b value_y in
  assert (get t key_a = value_x) ; 
  assert (get t key_b = value_y) ;
  assert (get t key_c = nul) ;

  let t = unset t key_b in
  let t = set t key_c value_z in
  assert (get t key_a = value_x) ; 
  assert (get t key_b = nul) ;
  assert (get t key_c = value_z) ;
  
  ()

let dummy_patricia : Patricia.t =
  let open Patricia in
  let open Dev in
  let t = empty in
  let t = set t key_a value_x in
  let t = set t key_c value_z in
  t

let dummy_produce : Patricia_produce_stream.t =
  Patricia_produce_stream.of_patricia dummy_patricia

let produce_store = test "produce-store" @@ fun () ->
  let open Patricia_produce_stream in
  let open Dev in
  let s = empty_stream in
  let t = dummy_produce in

  let (value_a , (t , s)) = get (t , s) key_a in
  assert (value_a = value_x) ;
  let (value_b , (t , s)) = get (t , s) key_b in
  assert (value_b = nul) ;
  let (value_c , (_t , _s)) = get (t , s) key_c in
  assert (value_c = value_z) ;

  ()

let dummy_stream : stream =
  let open Patricia_produce_stream in
  let open Dev in
  let s = empty_stream in
  let t = dummy_produce in
  let (_value_a , (t , s)) = get (t , s) key_a in
  let (t , s) = set (t , s) key_b value_y in
  let (_value_c , (t , s)) = get (t , s) key_c in
  ignore t ;
  s

let consume_store = test "consume-store" @@ fun () ->
  let open Patricia_consume_stream in
  let open Dev in
  let s = List.rev dummy_stream in
  let h = Patricia_produce_stream.get_hash dummy_produce in
  let t = empty_hash h in

  (* Replay sequence of read/write in the same order, succeeds with stream entirely consumed *)
  let () =
    let (value_a , (t , s)) = get (t , s) key_a in
    assert (value_a = value_x) ;
    let (t , s) = set (t , s) key_b value_y in
    let (value_c , (t , s)) = get (t , s) key_c in
    assert (value_c = value_z) ;
    assert (s = []) ;
    ignore t ;
  in

  (* Replay sequence of read/write in different order, fails *)
  let () =
    assert_fail @@ fun () ->
    let (value_a , (t , s)) = get (t , s) key_a in
    assert (value_a = value_x) ;
    let (value_c , (t , s)) = get (t , s) key_c in
    assert (value_c = value_z) ;
    let (t , s) = set (t , s) key_b value_y in
    ignore (t , s) ;
  in

  (* Starts with different root hash, fails *)
  let () =
    let h' = Bytes.cat h (Bytes.of_string "lol") in
    let t = empty_hash h' in
    assert_fail @@ fun () ->
    let _ = get (t , s) key_a in
    ()
  in

  (* Replay the sequence of read with any part of the stream altered, fails *)
  let () =
    let alter n s =
      List.mapi
        (fun i x ->
           if i <> n
           then x
           else Bytes.cat x (Bytes.of_string "lol")
        )
        s
    in
    for i = 0 to (List.length s) - 1 do (
      assert_fail @@ fun () ->
      let s = alter i s in
      let (value_a , (t , s)) = get (t , s) key_a in
      assert (value_a = value_x) ;
      let (t , s) = set (t , s) key_b value_y in
      let (value_c , (t , s)) = get (t , s) key_c in
      assert (value_c = value_z) ;
      assert (s = []) ;
      ignore t ;
    ) done ;
  in

  ()

let tests = [
  noop ;
  regular_store ;
  produce_store ;
  consume_store ;
]

let () = Alcotest.run "tezos_new_storage" [ ("main" , tests) ]
