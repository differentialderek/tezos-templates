// A proxy contract for storage 

(* =============================================================================
 * Storage
 * ============================================================================= *)

type storage = {
    is_active : bool ;
    directory : address ;
    store : nat ; 

    // for upgrades
    previous_storage_contract : address option ; // if None, this is the first storage contract
}

type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| UpdateStorage of nat
| Activate of bool

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_ADDRESS_NOT_FOUND = 1n 
let error_CALL_VIEW_FAILED = 2n

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
let update_storage (new_store : nat) (storage : storage) : result = 
    if is_proxy Tezos.sender storage.directory then
        ([] : operation list),
        { storage with store = new_store ; }
    else
        (failwith error_PERMISSIONS_DENIED : result)

// the directory contract can activate the storage contract
let activate (b : bool) (storage : storage) : result = 
    if Tezos.self_address <> storage.directory then (failwith error_PERMISSIONS_DENIED : result) else
    ([] : operation list),
    { storage with is_active = b ; }


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// proxy contracts can view the integer in storage
[@view] let proxy_view_storage (_, storage : unit * storage) : nat = 
    if is_proxy Tezos.sender storage.directory then
        storage.store
    else
        (failwith error_PERMISSIONS_DENIED : nat)

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | UpdateStorage n -> update_storage n storage
    | Activate n -> activate n storage


