(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2013 Simon Cruanes. All rights reserved.                    *)
(* Distributed under the BSD2 license, see terms at the file LICENSE         *)
(* in https://github.com/c-cube/ocaml-containers.                            *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Functional queues} *)

type 'a equal = 'a -> 'a -> bool

type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

(** Queue containing elements of type 'a *)
type +'a t

val empty : 'a t

val is_empty : 'a t -> bool

val singleton : 'a -> 'a t

val doubleton : 'a -> 'a -> 'a t

(** Push element at the front of the queue. *)
val cons : 'a -> 'a t -> 'a t

(** Push element at the end of the queue. *)
val snoc : 'a t -> 'a -> 'a t

(** Get and remove the first element. *)
val take_front : 'a t -> ('a * 'a t) option

val take_front_while : ('a -> bool) -> 'a t -> 'a list * 'a t

(** Take last element. *)
val take_back : 'a t -> ('a t * 'a) option

val take_back_while : ('a -> bool) -> 'a t -> 'a t * 'a list

(** {2 Individual extraction} *)

(** First element of the queue. *)
val first : 'a t -> 'a option

(** Last element of the queue. *)
val last : 'a t -> 'a option

(** Return the [i]-th element of the queue in logarithmic time. *)
val nth : int -> 'a t -> 'a option

(** Queue deprived of its first element. Does nothing on empty queues. *)
val tail : 'a t -> 'a t

(** Queue deprived of its last element. Does nothing on empty queues. *)
val init : 'a t -> 'a t

(** {2 Global Operations} *)

(** Append two queues. Elements from the second one come
    after elements of the first one.
    Linear in the size of the second queue. *)
val append : 'a t -> 'a t -> 'a t

(** Reverse the queue, [O(n)] complexity.
    @since 0.10 *)
val rev : 'a t -> 'a t

(** Map values. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Number of elements in the queue (constant time). *)
val size : 'a t -> int

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** {2 Conversions} *)

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

(** [a -- b] is the integer range from [a] to [b], both included.
    @since 0.10 *)
val ( -- ) : int -> int -> int t

(** [a -- b] is the integer range from [a] to [b], where [b] is excluded.
    @since 0.17 *)
val ( --^ ) : int -> int -> int t

(** @since 0.13 *)
val pp : 'a printer -> 'a t printer
