[@@@warning "-21"]

[@@@warning "-20"]

[@@@warning "-27"]

[@@@warning "-26"]

open Tezos_raw_protocol_alpha
open Alpha_context
open Script
open Printf
open Show

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let bind m f = match m with None -> None | Some x -> f x
end

module Identity : Monad = struct
  type 'a t = Ident of 'a

  let return x = Ident x

  let bind m f =
    let (Ident x) = m in
    f x
end

(* open Identity *)

let x = bind (return 1) (fun x -> Ident (x + 1))

(* type 'a id_monad = IdMonad of 'a 

let id_unit x = IdMonad x

let id_bind : type a b. 'a id_monad -> ('a -> 'b id_monad) -> 'b id_monad = 
  fun value transform -> transform value *)

(* print_endline "hello" ; *)

(* let _ = Benchmark.main () *)
