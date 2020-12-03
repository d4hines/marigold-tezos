(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

module type Type = sig
  module Tez : sig
    type t
  end

  module Script_int : sig
    type 't num

    type z

    type n
  end

  module Sapling : sig
    type transaction

    type state

    module Memo_size : sig
      type t
    end
  end

  module Big_map : sig
    module Id : sig
      type t
    end
  end

  module Signature : sig
    type t

    module Public_key : sig
      type t
    end

    module Public_key_hash : sig
      type t
    end
  end

  module Operation : sig
    type packed_internal_operation
  end

  module Script_timestamp : sig
    type t
  end

  module Script : sig
    type node

    type location
  end

  module Lazy_storage : sig
    type diffs
  end

  module Contract : sig
    type t
  end

  module Chain_id : sig
    type t
  end

  module Bls12_381 : sig
    module G2 : sig
      type t
    end

    module G1 : sig
      type t
    end

    module Fr : sig
      type t
    end
  end

  type ('elt_ty, 'elt) set

  type ('key_ty, 'key, 'value) map
end
