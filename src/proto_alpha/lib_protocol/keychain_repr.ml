type t = {
  consensus_key : Signature.Public_key.t;
  spending_key : Signature.Public_key.t;
}

type keychain = t

let pp ppf {consensus_key} =
  Signature.Public_key.pp ppf consensus_key

let encoding =
  let open Data_encoding in
  conv
    (fun {consensus_key; spending_key} -> (consensus_key, spending_key))
    (fun (consensus_key, spending_key) -> {consensus_key; spending_key})
    (obj2
      (req
         "consensus_key"
         ~description:
           "The masker key"
         Signature.Public_key.encoding)
      (req
         "spending_key"
         ~description:
           "The public key for transaction oply"
         Signature.Public_key.encoding))
