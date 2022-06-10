(* ============================================================================
 * Generic Tests for FA3 Code
 * ============================================================================ *)

(* ============================================================================
 * SRC
 * ============================================================================ *)

#include "fa3_2.mligo"
let  main_fa3 = main
type storage_fa3 = storage 
type entrypoint_fa3 = entrypoint 
type result_fa3 = result

(* ============================================================================
 * Some Proxy Contracts
 * ============================================================================ *)

let get_bal (input, storage : (callback_data list) * nat) : (operation list) * nat = 
    ([] : operation list),
    (match input with 
        | x :: xs -> x.balance
        | [] -> 0n
    )

(* ============================================================================
 * Some Auxiliary Functions
 * ============================================================================ *)

// initiates an instance with alice, bob, and an operator
let init_contracts (alice_bal : nat) (bob_bal : nat) = 
    // generate some implicit addresses
    let reset_state_unit = Test.reset_state 4n ([] : tez list) in
    let (addr_alice, addr_bob, addr_operator, addr_governor) = 
        (Test.nth_bootstrap_account 0, Test.nth_bootstrap_account 1, 
         Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in 

    // initiate contract; both alice and bob have 1000n tokens
    let init_fa3_storage = {
        governance = addr_governor ; 
        ledger = ( Big_map.literal [ ( {token_owner = addr_alice ; token_id = 0n ;}, alice_bal) ; ({token_owner = addr_bob ; token_id = 0n ;}, bob_bal) ; ] );
        operators = (Big_map.empty : (operator, nat) big_map);
        exchange_rates = (Big_map.literal [ (0n, 1_000_000n) ; ] : (nat, nat) big_map) ;
        token_metadata = (Big_map.empty : (token_id, token_metadata) big_map) ;
        metadata = (Big_map.empty : (string, bytes) big_map);
    } in
    let (typed_addr_fa3, pgm_fa3, size_fa3) = 
        Test.originate main_fa3 init_fa3_storage 0tez in

    (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3)


// query balance 
let query_balance (typed_addr_fa3 : (entrypoint_fa3, storage_fa3) typed_address) (token_owner : address) (token_id : nat) : nat = 
    let (typed_addr_balanceOf, pgm_balanceOf, size_balanceOf) = 
        Test.originate get_bal 0n 0tez in
    let op_query_balance = 
        let get_balance_entrypoint = 
            ((Test.to_entrypoint "balance_of" typed_addr_fa3) : balance_of contract) in 
        let get_balance_data = 
            let get_balance_contract : callback_data list contract = Test.to_contract typed_addr_balanceOf in 
            let request = { token_owner = token_owner ; token_id = token_id ; } in 
            { requests = [ request ; ] ; callback = get_balance_contract ; } in 
        Test.transfer_to_contract get_balance_entrypoint get_balance_data 0tez in 
    Test.get_storage typed_addr_balanceOf


(* ============================================================================
 * Tests 
 * ============================================================================ *)

let test_transfer = 
    // contract setup 
    let alice_bal = 1000n in 
    let bob_bal   = 1000n in 
    let transfer_amt = 500n in 
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // transfer 500n from alice to bob 
    let alice_source = Test.set_source addr_alice in 
    let transfer_alice_to_bob = 
        let transfer_entrypoint = 
            ((Test.to_entrypoint "transfer" typed_addr_fa3) : transfer list contract) in 
        let txns : transfer list = 
            let txn : transfer = 
                let transfer_to_bob = { to_ = addr_bob ; token_id = 0n ; amount = transfer_amt ; } in 
                { from_ = addr_alice ; txs = [ transfer_to_bob ; ] ;  } in 
            [ txn ; ] in 
        Test.transfer_to_contract_exn transfer_entrypoint txns 0tez in 

    // assert that the balances are all right
    assert (
        (query_balance typed_addr_fa3 addr_alice 0n = abs(alice_bal - transfer_amt)) && 
        (query_balance typed_addr_fa3 addr_bob   0n = bob_bal + transfer_amt)
    )


let test_mint = 
    // contract setup 
    let alice_bal = 1000n in 
    let bob_bal   = 1000n in 
    let mint_amt = 500n in 
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // mint tokens 
    let governor_source = Test.set_source addr_governor in 
    let governor_mint_tokens = 
        let mint_entrypoint = 
            ((Test.to_entrypoint "mint" typed_addr_fa3) : mint contract) in 
        let mint_data = 
            [ { owner = addr_alice ; token_id = 0n ; qty = mint_amt ; } ; ] in 
        Test.transfer_to_contract_exn mint_entrypoint mint_data 0tez in 

    // check balances
    assert (query_balance typed_addr_fa3 addr_alice 0n = alice_bal + mint_amt)


let test_mint_2 = 
    // contract setup 
    let alice_bal = 1000n in 
    let bob_bal   = 1000n in 
    let mint_amt = 500n in 
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // add a token to the pool 
    let governor_source = Test.set_source addr_governor in 
    let governor_add_token_id = 
        let add_token_id_entrypoint = 
            ((Test.to_entrypoint "add_token_id" typed_addr_fa3) : add_token_id list contract) in 
        let new_tokens : add_token_id list = 
            let new_token = 
                let new_token_metadata = { token_id = 1n ; token_info = (Map.empty : (string, bytes) map) ; } in
                { exchange_rate = 1_000n ; token_metadata = new_token_metadata ; } in 
            [ new_token ; ] in 
        Test.transfer_to_contract_exn add_token_id_entrypoint new_tokens 0tez in 

    // mint tokens 
    let governor_source = Test.set_source addr_governor in 
    let governor_mint_tokens = 
        let mint_entrypoint = 
            ((Test.to_entrypoint "mint" typed_addr_fa3) : mint contract) in 
        let mint_data = 
            [ { owner = addr_alice ; token_id = 1n ; qty = mint_amt ; } ; ] in 
        Test.transfer_to_contract_exn mint_entrypoint mint_data 0tez in 

    // check balances
    assert (query_balance typed_addr_fa3 addr_alice 1n = mint_amt)


let test_burn = 
    // contract setup 
    let alice_bal = 1000n in 
    let bob_bal   = 1000n in 
    let burn_amt = 500n in 
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // burn tokens 
    let governor_source = Test.set_source addr_governor in 
    let governor_mint_tokens = 
        let mint_entrypoint = 
            ((Test.to_entrypoint "burn" typed_addr_fa3) : mint contract) in 
        let mint_data = 
            [ { owner = addr_alice ; token_id = 0n ; qty = burn_amt ; } ; ] in 
        Test.transfer_to_contract_exn mint_entrypoint mint_data 0tez in 

    // check balances
    assert (query_balance typed_addr_fa3 addr_alice 0n = abs(alice_bal - burn_amt))


let iter_test_trade_in_pool (trade_amt, new_token_rate, _ : nat * nat * nat) = 
    // contract setup 
    let alice_bal = 100_000_000n in 
    let bob_bal   = 100_000_000n in 
    let trade_amt = 50_000_000n in 
    let (token_in, token_out) = (0n, 1n) in
    let new_token_rate = 1_000_000_000n in 
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // add a token to the pool 
    let governor_source = Test.set_source addr_governor in 
    let governor_add_token_id = 
        let add_token_id_entrypoint = 
            ((Test.to_entrypoint "add_token_id" typed_addr_fa3) : add_token_id list contract) in 
        let new_tokens : add_token_id list = 
            let new_token = 
                let new_token_metadata = { token_id = 1n ; token_info = (Map.empty : (string, bytes) map) ; } in
                { exchange_rate = new_token_rate ; token_metadata = new_token_metadata ; } in 
            [ new_token ; ] in 
        Test.transfer_to_contract_exn add_token_id_entrypoint new_tokens 0tez in 

    // alice trades her tokens of id 0n for tokens of id 1n
    let alice_source = Test.set_source addr_alice in 
    let alice_swap_tokens = 
        let trade_in_pool_entrypoint = 
            ((Test.to_entrypoint "trade_in_pool" typed_addr_fa3) : trade_in_pool contract) in 
        let alice_trade : trade_in_pool = 
            { qty = trade_amt ; token_in = token_in ; token_out = token_out ; } in
        Test.transfer_to_contract trade_in_pool_entrypoint alice_trade 0tez in 

    // check alice's balance of token_id = 1n
    assert (
        (query_balance typed_addr_fa3 addr_alice 0n = 
            abs(alice_bal - trade_amt)) && 
        (query_balance typed_addr_fa3 addr_alice 1n = 
            trade_amt * new_token_rate / 1_000_000n)    
    )


let iter_test_trade_in_pool_reflexive (trade_amt, new_token_rate, _ : nat * nat * nat) = 
    // contract setup 
    let alice_bal = 100_000_000n in 
    let bob_bal   = 100_000_000n in 
    let (token_in, token_out) = (0n, 1n) in
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // add a token to the pool 
    let governor_source = Test.set_source addr_governor in 
    let governor_add_token_id = 
        let add_token_id_entrypoint = 
            ((Test.to_entrypoint "add_token_id" typed_addr_fa3) : add_token_id list contract) in 
        let new_tokens : add_token_id list = 
            let new_token = 
                let new_token_metadata = { token_id = 1n ; token_info = (Map.empty : (string, bytes) map) ; } in
                { exchange_rate = new_token_rate ; token_metadata = new_token_metadata ; } in 
            [ new_token ; ] in 
        Test.transfer_to_contract_exn add_token_id_entrypoint new_tokens 0tez in 

    // alice trades her tokens of id 0n for tokens of id 1n
    let alice_source = Test.set_source addr_alice in 
    let alice_swap_tokens = 
        let trade_in_pool_entrypoint = 
            ((Test.to_entrypoint "trade_in_pool" typed_addr_fa3) : trade_in_pool contract) in 
        let alice_trade : trade_in_pool = 
            { qty = trade_amt ; token_in = token_in ; token_out = token_out ; } in
        Test.transfer_to_contract trade_in_pool_entrypoint alice_trade 0tez in 
    let alice_out_bal = query_balance typed_addr_fa3 addr_alice 1n in 

    // alice trades her tokens of id 1n BACK for tokens of id 0n
    let alice_source = Test.set_source addr_alice in 
    let alice_swap_tokens = 
        let trade_in_pool_entrypoint = 
            ((Test.to_entrypoint "trade_in_pool" typed_addr_fa3) : trade_in_pool contract) in 
        let alice_trade : trade_in_pool = 
            { qty = alice_out_bal ; token_in = token_out ; token_out = token_in ; } in
        Test.transfer_to_contract trade_in_pool_entrypoint alice_trade 0tez in 

    // check alice's balance of token_id = 1n
    assert (
        (query_balance typed_addr_fa3 addr_alice 0n <= alice_bal) && 
        (query_balance typed_addr_fa3 addr_alice 1n <= 0n)    
    )


let iter_test_trade_in_pool_transitive (trade_amt, new_token_rate1, new_token_rate2 : nat * nat * nat) = 
    // contract setup 
    let alice_bal = 100_000_000n in 
    let bob_bal   = 100_000_000n in 
    let (token_in, token_out1, token_out2) = (0n, 1n, 2n) in
    let (addr_alice, addr_bob, addr_operator, addr_governor, typed_addr_fa3) = 
        init_contracts alice_bal bob_bal in

    // add two token types to the pool 
    let governor_source = Test.set_source addr_governor in 
    let governor_add_token_id = 
        let add_token_id_entrypoint = 
            ((Test.to_entrypoint "add_token_id" typed_addr_fa3) : add_token_id list contract) in 
        let new_tokens : add_token_id list = 
            let new_token_1 = 
                let new_token_metadata = { token_id = 1n ; token_info = (Map.empty : (string, bytes) map) ; } in
                { exchange_rate = new_token_rate1 ; token_metadata = new_token_metadata ; } in 
            let new_token_2 = 
                let new_token_metadata = { token_id = 2n ; token_info = (Map.empty : (string, bytes) map) ; } in
                { exchange_rate = new_token_rate2 ; token_metadata = new_token_metadata ; } in 
            [ new_token_1 ; new_token_2 ; ] in 
        Test.transfer_to_contract_exn add_token_id_entrypoint new_tokens 0tez in 

    // alice trades her tokens of id 0n for tokens of id 1n
    let alice_source = Test.set_source addr_alice in 
    let alice_swap_tokens = 
        let trade_in_pool_entrypoint = 
            ((Test.to_entrypoint "trade_in_pool" typed_addr_fa3) : trade_in_pool contract) in 
        let alice_trade : trade_in_pool = 
            { qty = trade_amt ; token_in = token_in ; token_out = token_out1 ; } in
        Test.transfer_to_contract trade_in_pool_entrypoint alice_trade 0tez in 
    let alice_out_1_bal = query_balance typed_addr_fa3 addr_alice 1n in 

    // alice trades her tokens of id 1n for tokens of id 2n
    let alice_source = Test.set_source addr_alice in 
    let alice_swap_tokens = 
        let trade_in_pool_entrypoint = 
            ((Test.to_entrypoint "trade_in_pool" typed_addr_fa3) : trade_in_pool contract) in 
        let alice_trade : trade_in_pool = 
            { qty = alice_out_1_bal ; token_in = token_out1 ; token_out = token_out2 ; } in
        Test.transfer_to_contract trade_in_pool_entrypoint alice_trade 0tez in 
    let alice_out_2_bal = query_balance typed_addr_fa3 addr_alice 2n in 

    // alice trades her tokens of id 1n for tokens of id 0n
    let alice_source = Test.set_source addr_alice in 
    let alice_swap_tokens = 
        let trade_in_pool_entrypoint = 
            ((Test.to_entrypoint "trade_in_pool" typed_addr_fa3) : trade_in_pool contract) in 
        let alice_trade : trade_in_pool = 
            { qty = alice_out_2_bal ; token_in = token_out2 ; token_out = token_in ; } in
        Test.transfer_to_contract trade_in_pool_entrypoint alice_trade 0tez in 

    // check alice's balance of token_id = 1n
    assert (
        (query_balance typed_addr_fa3 addr_alice 0n <= alice_bal) && // due to rounding error, can be slightly less than
        (query_balance typed_addr_fa3 addr_alice 1n = 0n)        &&
        (query_balance typed_addr_fa3 addr_alice 2n = 0n)    
    )

let trade_test_input : (nat * nat * nat) list = [ 
    (50_000_000n, 996_001n, 1_220_001n) ; 
    (11_000_000n,     001n, 1_245_221n) ; 
    (11_040_200n, 320_001n, 1_220_001n) ; 
    (71_750_020n, 520_001n, 1_220_001n) ;     
    (56_200_111n, 277_550n, 1_245_777n) ; 
    (78_540_670n, 220_111n, 1_220_001n) ; 
    ( 1_132_045n, 220_001n, 1_220_001n) ;     
    (50_000_110n,   2_001n, 1_220_001n) ; 
    (50_021_020n, 234_001n, 1_220_001n) ; 
    (99_102_040n, 1_220_001n, 1_220_001n) ;     
]


let iter_test_trade_in_pool = 
    let test_results = 
        List.map 
        iter_test_trade_in_pool
        trade_test_input
    in assert true

let iter_test_trade_in_pool_reflexive = 
    let test_results = 
        List.map 
        iter_test_trade_in_pool_reflexive
        trade_test_input
    in assert true

let test_trade_in_pool_transitive = 
    let test_results = 
        List.map 
        iter_test_trade_in_pool_transitive
        trade_test_input
    in assert true