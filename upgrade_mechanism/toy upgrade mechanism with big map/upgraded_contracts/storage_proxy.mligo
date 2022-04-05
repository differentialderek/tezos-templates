// A proxy contract for storage 

(* =============================================================================
 * Storage
 * ============================================================================= *)

type storage = {
    is_active : bool ;
    directory : address ;
    ledger : (address, nat) big_map ; 
    previous_storage_contract : address option ; // if None, this is the first storage contract
    next_storage_contract : address option ; // if None, this is the active storage contract
}

type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type update_ledger = {
    addr_key : address ;
    nat_value : nat ; 
}

type entrypoint = 
| UpdateLedger of update_ledger
| UpdateLedgerTelescope of address
| FetchInitStorage // deprecated in this version
| Activate of bool

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_NOT_FOUND = 1n 
let error_CALL_VIEW_FAILED = 2n
let error_INACTIVE_CONTRACT = 3n 

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

let is_proxy (addr : address) (directory : address) : bool = 
    match (Tezos.call_view "isProxy" addr directory : bool option) with 
    | None -> (failwith error_CALL_VIEW_FAILED : bool)
    | Some b -> b

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// proxy contracts can update storage
let update_ledger (param : update_ledger) (storage : storage) : result = 
    let (addr_key, nat_value) = (param.addr_key, param.nat_value) in 
    // contract must be active
    if not storage.is_active then (failwith error_PERMISSIONS_DENIED : result) else
    // only proxy contracts can update the ledger
    if is_proxy Tezos.sender storage.directory then
        ([] : operation list),
        { storage with ledger = 
            Big_map.update addr_key (Some nat_value) storage.ledger ; }
    else
        (failwith error_PERMISSIONS_DENIED : result)

// proxy contracts can lazily import data from the storage of previous big maps
let update_ledger_telescope (addr_key : address) (storage : storage) : result = 
    // only proxy contracts can call this
    if  match storage.next_storage_contract with 
        | None -> // this is the active storage contract, and only other proxy contracts can call this
            not is_proxy Tezos.sender storage.directory
        | Some addr_next_storage -> 
            // this is not the active storage contract, so only the next storage contract in the chain can call this
            // this happens when 
            Tezos.sender <> addr_next_storage
    then (failwith error_PERMISSIONS_DENIED : result) else
    // clear the current storage (this has been imported now)
    let (val_option, storage) = 
        let (v, ledger) = Big_map.get_and_update addr_key (None : nat option) storage.ledger in 
        (v, { storage with ledger = ledger ; }) in 
    // clear the previous contract's storage
    match val_option with 
    // if there is a value here, there is no 
    | Some _ -> ([] : operation list), storage
    // if there's no value here, we may have to clear previous storage
    | None -> (
        match storage.previous_storage_contract with 
        | None -> ([] : operation list), storage
        | Some addr_previous_storage -> (
            match (Tezos.get_entrypoint_opt "%updateLedgerTelescope" addr_previous_storage : address contract option) with
            | None -> (failwith error_NOT_FOUND : result)
            | Some addr_entrypoint -> 
                let op_clear_storage = Tezos.transaction addr_key 0tez addr_entrypoint in 
                [ op_clear_storage ; ], storage
        )
    )

// the initial storage fetch. Because the only thing in storage is a big map, this does nothing 
// due to the structure of the directory contract, every storage contract must have this 
let fetch_init_storage (_ : storage) = 
    ([] : operation list), storage

// the directory contract can activate the storage contract
let activate (b : bool) (storage : storage) : result = 
    if Tezos.sender <> storage.directory then (failwith error_PERMISSIONS_DENIED : result) else
    ([] : operation list),
    { storage with is_active = b ; }


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// proxy contracts can view the integer in storage
[@view] let proxy_view_ledger (addr_key, storage : address * storage) : nat option = 
    if is_proxy Tezos.sender storage.directory then
        Big_map.find_opt addr_key storage.ledger
    else
        (failwith error_PERMISSIONS_DENIED : nat option)

// the recursive backwards call to find storage (and later to lazily import it)
[@view] let proxy_view_ledger_telescope (addr_key, storage : address * storage) : nat option = 
    if is_proxy Tezos.sender storage.directory then
        // first search local storage 
        match Big_map.find_opt addr_key storage.ledger with 
        | Some n -> (Some n)
        | None -> (
            // if nothing shows up, recursively search the previous contract's storage
            match storage.previous_storage_contract with 
            | None -> (None : nat option) // this is the first storage contract
            | Some addr -> (
                match (Tezos.call_view "proxy_view_ledger_telescope" addr_key addr : (nat option) option) with 
                // if it returns None, this means there is no storage contract with a value for this key
                | None -> (None : nat option)
                // if it returns Some b then somewhere in the backwards chain of storage contracts n was stored
                | Some nat_option -> nat_option
            )
        )
    else
        (failwith error_PERMISSIONS_DENIED : nat option)

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | UpdateLedger p -> update_ledger p storage // updates this ledger 
    | UpdateLedgerTelescope p -> update_ledger_telescope p storage // updates previous ledgers
    | FetchInitStorage -> fetch_init_storage storage
    | Activate p -> activate p storage


