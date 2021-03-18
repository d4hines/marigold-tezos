type t = {
  consensus_key : Signature.Public_key.t;
  spending_key : Signature.Public_key.t;
}

type keychain = t

val pp : Format.formatter -> keychain -> unit

val encoding : keychain Data_encoding.t
