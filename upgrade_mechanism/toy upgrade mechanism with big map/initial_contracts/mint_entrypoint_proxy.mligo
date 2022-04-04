// A template to start coding a new contract

(* =============================================================================
 * Storage
 * ============================================================================= *)

type storage = {
    is_active : bool ;
    directory : address ;  
}

type result = operation list * storage 

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type update_ledger = { addr_key : address ; nat_value : nat ; }
type mint = { for_ : address ; amt_ : nat ; }

// Mint entrypoint 
type entrypoint = 
| Mint of mint
| Activate of bool

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_PROXY_NOT_FOUND = 1n
let error_INACTIVE_CONTRACT = 2n 
let error_ENTRYPOINT_NOT_FOUND = 3n
let error_INSUFFICIENT_BALANCE = 4n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

let fetch_value (key : address) (acc : operation list) (storage : storage) : nat * (operation list) = 
    match (Tezos.call_view "getProxy" "storage" storage.directory : address option) with 
    | None -> (failwith error_PROXY_NOT_FOUND : nat * (operation list))
    | Some storage_addr -> (
        match (Tezos.call_view "proxy_view_ledger" key storage_addr : (nat option) option) with 
        | None -> (failwith error_PROXY_NOT_FOUND : nat * (operation list))
        | Some nat_option -> (
            match nat_option with 
            | Some n -> n, ([] : operation list) // the current version of storage has a value
            | None -> ( // previous versions of storage may have a value that needs importing 
                match (Tezos.call_view "proxy_view_ledger_telescope" key storage_addr : (nat option) option) with 
                | None -> (failwith error_PROXY_NOT_FOUND : nat * (operation list))
                | Some n -> (
                    match n with
                    | None -> (0n , acc) // there is no record anywhere, so balance is 0n
                    | Some b -> ( // there is a record for addr in a previous storage, so we need to get it and clear previous storage
                        let op_clear_previous_storage = // clear all previous storage 
                            match (Tezos.get_entrypoint_opt "%updateLedgerTelescope" storage_addr : address contract option) with
                            | None -> (failwith error_ENTRYPOINT_NOT_FOUND : operation)
                            | Some addr_entrypoint -> Tezos.transaction key 0tez addr_entrypoint in 
                        (b + 0n, op_clear_previous_storage :: acc)
                    ) 
                )                
            )
        )
    )

let update_ledger (addr_key : address) (nat_value : nat) (storage : storage) : operation = 
    match (Tezos.call_view "getProxy" "storage" storage.directory : address option) with 
    | None -> (failwith error_PROXY_NOT_FOUND : operation)
    | Some addr_storage -> (
        match (Tezos.get_entrypoint_opt "%updateLedger" addr_storage : update_ledger contract option) with 
        | None -> (failwith error_PROXY_NOT_FOUND : operation)
        | Some addr_entrypoint -> (
            Tezos.transaction { addr_key = addr_key ; nat_value = nat_value ; } 0tez addr_entrypoint
        )
    )

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let mint (param : mint) (storage : storage) : result = 
    let (for_, amt_) = (param.for_, param.amt_) in 
    // fetch the balance the storage (potentially needing to update previous contract storage)
    let (new_balance, ops_storage_updates) = 
        let (old_bal, ops) = fetch_value for_ ([] : operation list) storage in 
        (old_bal + amt_, ops) in 
    // update the storage contract
    let op_update_ledger = update_ledger for_ new_balance storage in 
    (op_update_ledger :: ops_storage_updates),
    storage 

let activate (b : bool) (storage : storage) : result = 
    if Tezos.self_address <> storage.directory then (failwith error_PERMISSIONS_DENIED : result) else
    ([] : operation list),
    { storage with is_active = b ; }

(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// [@view] viewName (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    if not storage.is_active then (failwith error_INACTIVE_CONTRACT : result) else
    match param with 
    | Mint p -> mint p storage
    | Activate p -> activate p storage


