(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

open Environment

class ['block] proto_rpc_context_of_directory conv dir :
  ['block] RPC_context.simple =
  let lookup = new Tezos_rpc.RPC_context.of_directory dir in
  object
    method call_proto_service0
        : 'm 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            RPC_context.t,
            'q,
            'i,
            'o )
          RPC_service.t -> 'block -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun s block q i ->
        conv block >>= fun rpc_context -> lookup#call_service s rpc_context q i

    method call_proto_service1
        : 'm 'a 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            RPC_context.t * 'a,
            'q,
            'i,
            'o )
          RPC_service.t -> 'block -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t =
      fun s block a1 q i ->
        conv block
        >>= fun rpc_context -> lookup#call_service s (rpc_context, a1) q i

    method call_proto_service2
        : 'm 'a 'b 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            (RPC_context.t * 'a) * 'b,
            'q,
            'i,
            'o )
          RPC_service.t -> 'block -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t
        =
      fun s block a1 a2 q i ->
        conv block
        >>= fun rpc_context ->
        lookup#call_service s ((rpc_context, a1), a2) q i

    method call_proto_service3
        : 'm 'a 'b 'c 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            ((RPC_context.t * 'a) * 'b) * 'c,
            'q,
            'i,
            'o )
          RPC_service.t -> 'block -> 'a -> 'b -> 'c -> 'q -> 'i ->
          'o tzresult Lwt.t =
      fun s block a1 a2 a3 q i ->
        conv block
        >>= fun rpc_context ->
        lookup#call_service s (((rpc_context, a1), a2), a3) q i
  end
