// The RPMM contract for pooling tokens

(* =============================================================================
 * Conditionals
 * ============================================================================= *)

(* If the RPMM has minting/burning rights on all tokens in the directory and allows
   for synthetic trades *)
//#define SYNTHETIC_TRADES


(* =============================================================================
 * Storage
 * ============================================================================= *)

type token = { token_addr : address ; token_id : nat ; }
type exchange_rate = nat // always divided by 1_000_000n in the code 

type storage = {
    directory : (token, exchange_rate) big_map ; 
    pool_token : token ; // this contract must have minting/burning rights 
    unpool_fee : nat ; // could be 0n if not worried about arbitrage
    increment : nat ; // the slippage on each trade, divided by 1_000_000n; could be 0n for static exchange rates
    min_rate : int ; // the minimum exchange rate allowed by the RPMM; could be 0
#if SYNTHETIC_TRADES
    synth_trade_fee : nat ; // the fee to make a synthetic trade
#endif
}

type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type transfer_to = [@layout:comb]{ to_ : address ; token_id : nat ; amount : nat ; }
type transfer = 
    [@layout:comb]
    { from_ : address; 
      txs : transfer_to list; }

type mintburn_data = { owner : address ; token_id : nat ; qty : nat ; }
type mintburn = mintburn_data list

type pool = {
    token : token ; 
    qty : nat ; // the qty of tokens to be pooled 
    min_out : nat ; // the min number of pool tokens willing to accept
}
type unpool = {
    token : token ;
    qty : nat ; // the qty of pool tokens being turned in 
    min_out : nat ; // the min number of tokens willing to accept 
}
type trade = {
    token_in : token ; 
    token_out : token ; 
    qty : nat ; // the qty of token_in going in 
    min_out : nat ; // the minimum acceptable for the trade 
}

type entrypoint = 
| Pool of pool
| Unpool of unpool 
| Trade of trade 
#if SYNTHETIC_TRADES
| SyntheticTrade of trade
#endif

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_CONTRACT_NOT_FOUND = 1n
let error_TOKEN_NOT_FOUND = 2n
let error_INSUFFICIENT_FUNDS = 3n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let pool (p : pool) (storage : storage) : result = 
    // mint the pool tokens 
    let qty_to_mint = 
        let rate = 
            match Big_map.find_opt p.token storage.directory with 
            | None -> (failwith error_TOKEN_NOT_FOUND : nat)
            | Some r -> r in 
        rate * p.qty in 
    // check qty is sufficient 
    if qty_to_mint < p.min_out then (failwith error_INSUFFICIENT_FUNDS : result) else
    // make the exchange of tokens
    let op_transfer_in = 
        let txn_data = {
            from_ = Tezos.sender ; 
            txs = [ { to_ = Tezos.self_address ; token_id = p.token.token_id ; amount = p.qty } ; ] ; 
        } in 
        match (Tezos.get_entrypoint_opt "%transfer" p.token.token_addr : transfer contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in 
    let op_mint_pool_tokens = // mints pool tokens in exchange for the tokens transferred in
        let txn_data = [ { owner = Tezos.sender ; token_id = storage.pool_token.token_id ; qty = qty_to_mint ; } ; ] in 
        match (Tezos.get_entrypoint_opt "%mint" storage.pool_token.token_addr : mintburn contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in
    // emit operations
    [ op_transfer_in ; op_mint_pool_tokens ; ], storage

let unpool (p : unpool) (storage : storage) : result = 
    // require a withdrawal fee 
    let () = assert (Tezos.amount = storage.unpool_fee * 1mutez) in 
    // get exchange rate 
    let qty_to_withdraw = 
        // calculate quantity to withdraw 
        let rate = 
            match Big_map.find_opt p.token storage.directory with 
            | None -> (failwith error_TOKEN_NOT_FOUND : nat)
            | Some r -> r in 
        p.qty / rate  in 
    // check the qty is sufficient 
    if qty_to_withdraw < p.min_out then (failwith error_INSUFFICIENT_FUNDS : result) else 
    // transfer the tokens 
    let op_burn_pool_tokens = // burns the pool tokens coming in
        let txn_data = [ { owner = Tezos.sender ; token_id = storage.pool_token.token_id ; qty = p.qty ; } ; ] in 
        match (Tezos.get_entrypoint_opt "%burn" storage.pool_token.token_addr : mintburn contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in
    let op_transfer_out = // unpools the requested tokens
        let txn_data = {
            from_ = Tezos.self_address; 
            txs = [ { to_ = Tezos.sender ; token_id = p.token.token_id ; amount = qty_to_withdraw } ; ] ; 
        } in 
        match (Tezos.get_entrypoint_opt "%transfer" p.token.token_addr : transfer contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in 
    // emit operations
    [ op_burn_pool_tokens ; op_transfer_out ; ], storage

let trade (p : trade) (storage : storage) : result = 
    // get exchange rate 
    let rate_in = 
        match Big_map.find_opt p.token_in storage.directory with 
        | None -> (failwith error_TOKEN_NOT_FOUND : nat)
        | Some r -> r in 
    let rate_out = 
        match Big_map.find_opt p.token_out storage.directory with 
        | None -> (failwith error_TOKEN_NOT_FOUND : nat)
        | Some r -> r in 
    let qty_for_trade = 
        p.qty * rate_in / rate_out in 
    // check qty is sufficient 
    if qty_for_trade < p.min_out then (failwith error_INSUFFICIENT_FUNDS : result) else 
    // execute the trade 
    let op_transfer_in = 
        let txn_data = {
            from_ = Tezos.sender ; 
            txs = [ { to_ = Tezos.self_address ; token_id = p.token_in.token_id ; amount = p.qty } ; ] ; 
        } in 
        match (Tezos.get_entrypoint_opt "%transfer" p.token_in.token_addr : transfer contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in 
    let op_transfer_out = 
        let txn_data = {
            from_ = Tezos.self_address; 
            txs = [ { to_ = Tezos.sender ; token_id = p.token_out.token_id ; amount = qty_for_trade } ; ] ; 
        } in 
        match (Tezos.get_entrypoint_opt "%transfer" p.token_out.token_addr : transfer contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_out_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_out_entrypoint in 
    // new exchange rates
    let directory = 
        let token_in_rate = 
            let r = rate_in - (storage.increment * p.qty / 1_000_000n) in // the token being sold gets cheaper
            if r <= storage.min_rate then abs(storage.min_rate) else abs(r) in 
        let token_out_rate = rate_out + (storage.increment * qty_for_trade / 1_000_000n) in // the token being bought gets more expensive
        Big_map.update p.token_in (Some token_in_rate) (
            Big_map.update p.token_out (Some token_out_rate) storage.directory ) in 
    // emit operations
    [ op_transfer_in ; op_transfer_out ; ], 
    { storage with directory = directory ; }

#if SYNTHETIC_TRADES
let synthetic_trade (p : synthetic_trade) (storage : storage) : result = ...
    // check synth trade fee is being paid
    if Tezos.amount <> storage.synth_trade_fee * 1mutez then (failwith error_INSUFFICIENT_FUNDS : result) else
    // get exchange rate 
    let rate_in = 
        match Big_map.find_opt p.token_in storage.directory with 
        | None -> (failwith error_TOKEN_NOT_FOUND : nat)
        | Some r -> r in 
    let rate_out = 
        match Big_map.find_opt p.token_out storage.directory with 
        | None -> (failwith error_TOKEN_NOT_FOUND : nat)
        | Some r -> r in 
    let qty_for_trade = 
        p.qty * rate_in / rate_out in 
    // check qty is sufficient 
    if qty_for_trade < p.min_out then (failwith error_INSUFFICIENT_FUNDS : result) else 
    // transfer tokens in 
    let op_burn_incoming = 
        let txn_data = 
            [ { owner = Tezos.sender ; token_id = p.token_in.token_id ; amount = p.qty } ; ] in 
        match (Tezos.get_entrypoint_opt "%burn" p.token_in.token_addr : mintburn contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in 
    // mint outgoing 
    let op_mint_outgoing = 
        let txn_data = 
            [ { owner = Tezos.sender ; token_id = p.token_out.token_id ; amount = qty_for_trade } ; ] in 
        match (Tezos.get_entrypoint_opt "%mint" p.token_out.token_addr : mintburn contract option) with
        | None -> (failwith error_CONTRACT_NOT_FOUND : operation)
        | Some token_entrypoint -> 
            Tezos.transaction txn_data 0mutez token_entrypoint in
    // new exchange rates
    let directory = 
        let token_in_rate = 
            let r = rate_in - (storage.increment * p.qty / 1_000_000n) in // the token being sold gets cheaper
            if r <= storage.min_rate then abs(storage.min_rate) else abs(r) in 
        let token_out_rate = rate_out + (storage.increment * qty_for_trade / 1_000_000n) in // the token being bought gets more expensive
        Big_map.update p.token_in (Some token_in_rate) (
            Big_map.update p.token_out (Some token_out_rate) storage.directory ) in 
    // emit operations
    [ op_burn_incoming ; op_mint_outgoing ; ], 
    { storage with directory = directory ; }
#endif

(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// [@view] viewName (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | Pool p -> pool p storage
    | Unpool p -> unpool p storage
    | Trade p -> trade p storage
#if SYNTHETIC_TRADES
    | SyntheticTrade p -> synthetic_trade p storage
#endif


