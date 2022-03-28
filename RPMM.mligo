// An automated market maker for families of semi-fungible tokens 

(* =============================================================================
 * Storage
 * ============================================================================= *)


type storage = {
    exchangeRates : (token, nat) big_map ; // keeps track of exchange rates up to 9 digits (always divided by 1_000_000_000n)
    poolToken : address ; // the pool token address 
    withdrawalFee : nat ; 
}

type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type token = {
    address : address ; 
    token_id : nat ;
}
type deposit = {
    qty : nat ; 
    token : token ; 
    minOut : nat ; // the min number of pool tokens willing to accept
}
type withdraw = {
    qty : nat ;
    token : token ;
    minOut : nat ; // the min number of tokens willing to accept 
}
type trade = {
    qty : nat ; 
    tokenIn : token ; 
    tokenOut : token ; 
    minOut : nat ; // the minimum acceptable for the trade 
}

type entrypoint = 
| Deposit of deposit
| Withdraw of withdraw 
| Trade of trade

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_TOKEN_NOT_IN_FAMILY = 1n
let error_INSUFFICIENT_TOKENS = 2n


(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let deposit (param : deposit) (storage : storage) : result = 
    // mint the tokens 
    let qty_to_mint = 
        let rate = 
            match Big_map.find_opt param.token with 
            | None -> (failwith error_TOKEN_NOT_IN_FAMILY : nat)
            | Some r -> r in 
        rate * param.qty in 
    // check qty is sufficient 
    if qty_to_mint < param.minOut then (failwith error_INSUFFICIENT_TOKENS : result) else
    // transfer the tokens 
    let op_transfer_in = 
        let txn_data = {
            from_ = Tezos.sender ; 
            [ { to_ = Tezos.self ; token_id = param.token.token_id ; amount = param.qty } ; ] ; 
        } in 
        Tezos.transaction txn_data 0mutez param.token.address in 
    let op_transfer_out = 
        let txn_data = { qty = qty_to_mint ; owner = Tezos.sender } in 
        Tezos.transaction txn_data 0mutez storage.poolToken in
    // emit operations
    [ op_transfer_in ; op_transfer_out ; ], storage


let withdraw (param : withdraw) (storage : storage) : result = 
    // require a withdrawal fee 
    let () = assert (Tezos.amount = storage.withdrawalFee * 1mutez) in 
    // get exchange rate 
    let qty_to_withdraw = 
        // calculate quantity to withdraw 
        let rate = 
            match Big_map.find_opt param.token with 
            | None -> (failwith error_TOKEN_NOT_IN_FAMILY : nat)
            | Some r -> r in 
        param.qty / rate  in 
    // check the qty is sufficient 
    if qty_to_withdraw < param.minOut then (failwith error_INSUFFICIENT_TOKENS : result) else 
    // transfer the tokens 
    let op_transfer_in = 
        let txn_data = { qty = param.qty ; owner = Tezos.sender } in 
        Tezos.transaction txn_data 0mutez storage.poolToken in
    let op_transfer_out = 
        let txn_data = {
            from_ = Tezos.self ; 
            [ { to_ = Tezos.sender ; token_id = param.token.token_id ; amount = qty_to_withdraw } ; ] ; 
        } in 
        Tezos.transaction txn_data 0mutez param.token.address in 
    // emit operations
    [ op_transfer_in ; op_transfer_out ; ], storage


let trade (param : trade) (storage : storage) : result = 
    // get exchange rate 
    let qty_for_trade = 
        let rate_in = 
            match Big_map.find_opt param.tokenIn with 
            | None -> (failwith error_TOKEN_NOT_IN_FAMILY : nat)
            | Some r -> r in 
        let rate_out = 
            match Big_map.find_opt param.tokenOut with 
            | None -> (failwith error_TOKEN_NOT_IN_FAMILY : nat)
            | Some r -> r in 
        param.qty * rate_in / rate_out in 
    // check qty is sufficient 
    if qty_to_trade < param.minOut then (failwith error_INSUFFICIENT_TOKENS : result) else 
    // execute the trade 
    let op_transfer_in = 
        let txn_data = {
            from_ = Tezos.sender ; 
            [ { to_ = Tezos.self ; token_id = param.tokenIn.token_id ; amount = param.qty } ; ] ; 
        } in 
        Tezos.transaction txn_data 0mutez param.tokenIn.address in 
    let op_transfer_out = 
        let txn_data = {
            from_ = Tezos.self ; 
            [ { to_ = Tezos.sender ; token_id = param.tokenOut.token_id ; amount = qty_for_trade } ; ] ; 
        } in 
        let tokenOut_contract = 
            // get the contract from param.tokenOut.address
        in 
        Tezos.transaction txn_data 0mutez tokenOut_contract in 
    // new exchange rates
        // TODO : update exchange rates to reflect supply/demand
        // TODO : if not enough tokens for transfer_out then mint synthetic tokens in the family 
    // emit operations
    [ op_transfer_in ; op_transfer_out ; ], storage


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// [@view] view1 (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | Deposit p -> 
        deposit p storage 
    | Withdraw p -> 
        withdraw p storage 
    | Trade p -> 
        trade p storage 


