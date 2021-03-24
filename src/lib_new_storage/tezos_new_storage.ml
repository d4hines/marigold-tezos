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

module Path = struct
  type t = bool list
  let compare = compare
end

module Map = Map.Make(Path) (* TODO: Add hash table *)

type key = Path.t
type value = bytes

let nul : value = Bytes.empty


type height = int
let max_height = 8

type hash = bytes

let do_hash : bytes -> hash = fun x -> x
  
type stream = bytes list
let empty_stream : stream = []


module Patricia = struct

  [@@@ocaml.warning "-30"]
  
  type node_content = { left : t ; right : t ; hash : hash }
  and leaf_content = { content : bytes ; hash : hash }
  and t =
    | Node of node_content
    | Leaf of leaf_content

  let get_hash = function Leaf { hash ; _ } | Node { hash ; _ } -> hash
  
  let lr_hash l r = do_hash @@ Bytes.cat (get_hash l) (get_hash r)
  
  let rec empty : int -> t = fun k ->
    if k = 0 then Leaf { content = nul ; hash = do_hash nul }
    else (
      let e = empty (k - 1) in
      Node { left = e ; right = e ; hash = lr_hash e e }
    )

  let empty = empty max_height
  
  let rec get : t -> key -> value = fun t path ->
    match path , t with
    | [] , Leaf { content ; _ } -> content
    | [] , _ -> assert false
    | hd :: tl , Node { left ; right ; _ } -> (
        match hd with
        | false -> get left tl
        | true -> get right tl
      )
    | _ :: _ , _ -> assert false

  let get = fun t path ->
    assert (List.length path = max_height) ;
    get t path

  let rec set : t -> key -> value -> t = fun t path value ->
    match path , t with
    | [] , Leaf { hash = _ ; content = _ } -> Leaf { content = value ; hash = do_hash value }
    | [] , _ -> assert false
    | hd :: tl , Node { left ; right ; hash = _ } -> (
        match hd with
        | false -> Node (
            let l = set left tl value in
            { left = l ; right ; hash = lr_hash l right }
          )
        | true -> Node (
            let r = set right tl value in
            { left ; right = r ; hash = lr_hash left r }
          )
      )
    | _ :: _ , _ -> assert false

  let set = fun t path value ->
    assert (List.length path = max_height) ;
    set t path value

  let rec unset : t -> key -> t = fun t path ->
    match path , t with
    | [] , Leaf { content = _ ; hash = _ } -> Leaf { content = nul ; hash = do_hash nul }
    | [] , _ -> assert false
    | hd :: tl , Node { left ; right ; hash = _ } -> (
        match hd with
        | false -> Node (
            let l = unset left tl in
            { left = l ; right ; hash = lr_hash l right }
          )
        | true -> Node (
            let r = unset right tl in
            { left ; right = r ; hash = lr_hash left r }
          )
      )
    | _ :: _ , _ -> assert false

  let unset = fun t path ->
    assert (List.length path = max_height) ;
    unset t path

  
end

module Patricia_produce_stream = struct

  [@@@ocaml.warning "-30"]

  type node_content = { left : t ; right : t ; hash : hash ; visited : bool }
  and leaf_content = { content : bytes ; hash : hash ; visited : bool }
  and t =
    | Node of node_content
    | Leaf of leaf_content

  type tt = t * stream

  let produce : stream -> bytes -> stream = fun x y -> y :: x

  let get_hash : t -> hash = function
    | Leaf { hash ; _ } | Node { hash ; _ } -> hash

  let node_hash left right = do_hash (Bytes.cat (get_hash left) (get_hash right))
  
  let rec of_patricia : Patricia.t -> t = function
    | Leaf { content ; hash } -> Leaf { content ; hash ; visited = false }
    | Node { left ; right ; hash = _ } -> (
        let left = of_patricia left in
        let right = of_patricia right in
        let hash = node_hash left right in
        Node { left ; right ; visited = false ; hash }
      )

  let rec get : tt -> key -> value * tt = fun (tree , stream) path ->
    match path , tree with
    | [] , Leaf ({ visited ; hash ; content } as leaf) -> (
        let (tree , stream) =
          if visited then (tree , stream) else (
            let stream = produce stream hash in
            Leaf { leaf with visited = true } , stream
          )
        in
        (content , (tree , stream))
      )
    | [] , Node _ -> assert false
    | hd :: tl , Node ({ visited ; hash = _ ; left ; right } as node) -> (
        let (node , stream) =
          if visited then (node , stream) else (
            let stream = produce stream @@ get_hash left in
            let stream = produce stream @@ get_hash right in
            let node = { node with visited = true } in
            node , stream
          )
        in
        match hd with
        | false -> (
            let (result , (left , stream)) = get (left , stream) tl in
            result , (Node { node with left } , stream)
          )
        | true -> (
            let (result , (right , stream)) = get (right , stream) tl in
            result , (Node { node with right } , stream)
          )
      )
    | _ :: _ , Leaf _ -> assert false

  (* One could have even shorter proofs by only persisting
     the hash of the branch not being modified instead of both *)
  let rec set : tt -> key -> value -> tt = fun (tree , stream) path value ->
    match path , tree with
    | [] , Leaf ({ visited = _ ; hash = _ ; content = _ }) -> (
        let tree = Leaf { content = value ; hash = do_hash value ; visited = true } in
        (tree , stream)
      )
    | [] , Node _ -> assert false
    | hd :: tl , Node ({ visited ; hash = _ ; left ; right } as node) -> (
        let (node , stream) =
          if visited then (node , stream) else (
            let stream = produce stream @@ get_hash left in
            let stream = produce stream @@ get_hash right in
            let node = { node with visited = true } in
            node , stream            
          )
        in
        match hd with
        | false -> (
            let (left , stream) = set (node.left , stream) tl value in
            let node = { node with left ; hash = node_hash node.left right } in
            (Node node , stream)
          )
        | true -> (
            let (right , stream) = set (node.right , stream) tl value in
            let node = { node with right ; hash = node_hash node.left right } in
            (Node node , stream)
          )
      )
    | _ :: _ , Leaf _ -> assert false
  
end


module Patricia_consume_stream = struct

  [@@@ocaml.warning "-30"]

  type t =
    | Hash of hash
    | Full of tree

  and node_content = {
    left : t ;
    right : t ;
    hash : hash ;
  }

  and leaf_content = {
    content : bytes ;
    hash : bytes ;
  }

  and tree =
    | Leaf of leaf_content
    | Node of node_content

  let get_tree_hash : tree -> hash = function
    | Leaf { hash ; _ } | Node { hash ; _ } -> hash

  let get_hash : t -> hash = function
    | Hash x -> x
    | Full tr -> get_tree_hash tr

  type tt = t * stream

  let empty_hash : hash -> t = fun x -> Hash x

  let consume : stream -> bytes * stream = function
    | [] -> assert false
    | hd :: tl -> hd , tl


  let do_leaf ~no_check ~content ~hash =
    assert (no_check || (do_hash content = hash)) ;
    { content ; hash }

  let do_leaf_no_check = do_leaf ~no_check:true
  let do_leaf = do_leaf ~no_check:false

  let node_hash left right = do_hash (Bytes.cat (get_hash left) (get_hash right))

  let do_node ~no_check ~left ~right ~hash =
    assert (no_check || (node_hash left right = hash)) ;
    { left ; right ; hash }

  let do_node_no_check = do_node ~no_check:true
  let do_node = do_node ~no_check:false

  let rec get : tt -> key -> value * tt = fun (t , stream) path ->
    match path , t with
    | [] , Hash hash -> (
        let (content , stream) = consume stream in
        let t = Full (Leaf (do_leaf ~content ~hash)) in
        (content , (t , stream))
      )
    | [] , Full (Leaf { content ; hash = _ }) -> (content , (t , stream))
    | [] , Full (Node _) -> assert false
    | hd :: tl , t -> (
        let node , stream =
          match t with
          | Hash hash -> (
              let (left_hash , stream) = consume stream in
              let (right_hash , stream) = consume stream in
              do_node ~left:(Hash left_hash) ~right:(Hash right_hash) ~hash , stream
            )
          | Full (Node node) -> node , stream
          | Full (Leaf _) -> assert false
        in
        match hd with
        | false -> (
            let (result , (left , stream)) = get (node.left , stream) tl in
            (result , (Full (Node {node with left}) , stream))
          )
        | true -> (
            let (result , (right , stream)) = get (node.right , stream) tl in
            (result , (Full (Node {node with right}) , stream))
          )
      )

  let get : tt -> key -> value * tt = fun tt path ->
    assert (List.length path = max_height) ;
    get tt path

  let rec set : tt -> key -> value -> tt = fun (t , stream) path content ->
    match path , t with
    | [] , Full (Node _) -> assert false
    | [] , _ -> (
        let leaf = do_leaf ~content ~hash:(do_hash content) in
        (Full (Leaf leaf) , stream)
      )
    | hd :: tl , t -> (
        let node , stream =
          match t with
          | Hash hash -> (
              let (left_hash , stream) = consume stream in
              let (right_hash , stream) = consume stream in
              do_node ~left:(Hash left_hash) ~right:(Hash right_hash) ~hash , stream
            )
          | Full (Node node) -> node , stream
          | Full (Leaf _) -> assert false
        in
        match hd with
        | false -> (
            let (left , stream) = set (node.left , stream) tl content in
            let node = { node with left ; hash = node_hash left node.right } in
            (Full (Node node) , stream)
          )
        | true -> (
            let (right , stream) = set (node.right , stream) tl content in
            let node = { node with right ; hash = node_hash node.left right } in
            (Full (Node node) , stream)
          )
      )

  let set : tt -> key -> value -> tt = fun tt path ->
    assert (List.length path = max_height) ;
    set tt path

end

module Stub = struct
  module Path = Path
  type nonrec key = key
  type nonrec value = value
  type nonrec hash = hash
  type nonrec stream = stream

  let nul = nul
  let do_hash = do_hash
  let empty_stream = empty_stream
  let key_of_bits = fun x -> x
  let key_of_bytes =
    let bits_of_bytes = fun c ->
      let i = Char.code c in
      let rec aux k t =
        if t = 0
        then []
        else if k >= t
        then true :: (aux (k - t) (t / 2))
        else false :: aux k (t / 2)
      in
      aux i 128
    in
    fun x -> List.concat @@ List.map bits_of_bytes @@ List.of_seq @@ Bytes.to_seq x
    

  let get_hash : value Map.t -> hash =
    fun t ->
    do_hash @@ Bytes.concat Bytes.empty @@ List.map snd @@ List.of_seq @@ Map.to_seq t
  
  module Patricia = struct
    type t = value Map.t
    let empty : t = Map.empty
    let get : t -> key -> value = fun t k -> Map.find k t
    let set : t -> key -> value -> t = fun t k v -> Map.add k v t
    let unset : t -> key -> t = fun t k -> Map.remove k t
    let get_hash = get_hash
  end

  module Patricia_produce_stream = struct
    type t = Patricia.t
    type tt = t * stream

    let get_hash : t -> hash = get_hash

    let of_patricia : Patricia.t -> t = fun t -> t

    let empty : t = of_patricia Patricia.empty
    
    let get : tt -> key -> value * tt = fun (t , s) k ->
      let v = Patricia.get t k in
      (v , (t , v :: s))

    let set : tt -> key -> value -> tt = fun (t , s) k v ->
      let t = Patricia.set t k v in
      (t , s)
  end

  module Patricia_consume_stream = struct
    type t = Patricia.t
    type tt = t * stream

    let get_hash : t -> hash = fun t ->
      do_hash @@ Bytes.concat Bytes.empty @@ List.map snd @@ List.of_seq @@ Map.to_seq t
    
    let empty_hash : hash -> t = fun _ -> Patricia.empty

    let empty = empty_hash @@ Patricia_produce_stream.(get_hash empty)
    
    let consume : stream -> bytes * stream = function
      | [] -> assert false
      | hd :: tl -> hd , tl
    
    let get : tt -> key -> value * tt = fun (t , s) k ->
      let (v , s) = consume s in
      let t = Patricia.set t k v in
      (v , (t , s))

    let set : tt -> key -> value -> tt = fun (t , s) k v ->
      let t = Patricia.set t k v in
      (t , s)
      
  end
end

(* Used only in tests, dev, etc. *)
module Dev = struct

  let key_a : key = [ false ; false ; false ; false ; false ; false ; false ; false ]
  let key_b : key = [ false ; false ; false ; false ; true ; false ; false ; false ]
  let key_c : key = [ false ; false ; true ; false ; false ; false ; false ; false ]

  let value_x : value = Bytes.of_string "foo"
  let value_y : value = Bytes.of_string "bar"
  let value_z : value = Bytes.of_string "42"
  
end
