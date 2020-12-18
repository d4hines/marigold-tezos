eval `./src/bin_client/tezos-init-sandboxed-client.sh 1`
# tezos-activate-alpha

# ######### Generating Players ##############
# tezos-client transfer 42 from bootstrap1 to bootstrap2 &
# tezos-client bake for bootstrap1

# tezos-client transfer 42 from bootstrap1 to bootstrap3 &

# tezos-client get balance for bootstrap1

tezos-client gen keys alice
tezos-client gen keys bob
tezos-client transfer 1000 from bootstrap1 to alice --burn-cap 100 &
sleep 1
tezos-client bake for bootstrap1
tezos-client get balance for alice

alice_pkh=$(tezos-client list known addresses | grep alice | grep -P 'tz[a-zA-Z0-9]+' -o)
bob_pkh=$(tezos-client list known addresses | grep bob | grep -P 'tz[a-zA-Z0-9]+' -o)


init="Pair {Elt \"$alice_pkh\" (Pair 100000000000000 {})} 100000000000000"
contract=$(cat "../workspaces/fa1.2/morley/fa1.2.tz")
# # # ########### Originating Contract ##########
# --init $init 
tezos-client originate contract tezosGold transferring 0 from alice running $contract --fee 1

sleep 1

tezos-client bake for bootstrap1

# ########### Originate Dexter
# tezos-client originate contract tezosGoldExchange \ 
#              transferring 0 from alice \ 
#              running path/to/dexter.tz \
#               --init "Pair {} (Pair (Pair False (Pair False 0)) (Pair (Pair \"$alice_pkh\" \"$tezosGold_pkh\") (Pair 0 0)))" --burn-cap 20 --force

# tezosGold_pkh=$(cat ~/.tezos-client/public_key_hashs | grep tezosGold | grep -P 'tz[a-zA-Z0-9]+' -o)

# tezos-client transfer 0 \ 
#              from alice \
#              to tezosGold 
#              --entrypoint 'approve' \
#              --arg "Pair \"$tezosGold_pkh\" 200" \
#              --burn-cap 1

# tezos-client transfer 10 
#              from alice \
#              to tezosGoldExchange \
#              --entrypoint 'addLiquidity' \
#              --arg "Pair (Pair \"$alice_pkh\" 1) (Pair 10 \"2030-01-01T12:00:00Z\")" \
#              --burn-cap 1

# tezos-client transfer 5 \
#              from bob \
#              to tezosGoldExchange \
#              --entrypoint 'xtzToToken' \
#              --arg "Pair \"$alice_pkh\" (Pair 1 "2030-01-29T18:00:00Z")" \
#              --burn-cap 1

