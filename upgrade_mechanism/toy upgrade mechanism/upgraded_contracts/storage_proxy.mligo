// A proxy contract for storage 

(* =============================================================================
 * Storage
 * ============================================================================= *)

type storage = {
    is_active : bool ;
    directory : address ;
    store : int ; 

    // for upgrades
    previous_storage_contract : address option ; // if None, this is the first storage contract
}

type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| UpdateStorage of int
| FetchStorage 
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
let update_storage (new_store : int) (storage : storage) : result = 
    if is_proxy Tezos.sender storage.directory then
        ([] : operation list),
        { storage with store = new_store ; }
    else
        (failwith error_PERMISSIONS_DENIED : result)

// the function to fetch the storage from the old contract 
let fetch_storage (storage : storage) : result = 
    // can only be done before it's activated 
    if Tezos.sender <> storage.directory || storage.is_active then (failwith error_PERMISSIONS_DENIED : result) else
    // gets the nat through the view, converts it to an int, and stores it 
    match storage.previous_storage_contract with 
    | None -> ([] : operation list), storage 
    | Some storage_addr -> (
        match (Tezos.call_view "proxy_view_storage" () storage_addr : nat option) with 
        | None -> (failwith error_PERMISSIONS_DENIED : result)
        | Some store -> 
            ([] : operation list),
            { storage with store = int(store) ; }
    )

// the directory contract can activate the storage contract
let activate (b : bool) (storage : storage) : result = 
    if Tezos.self_address <> storage.directory then (failwith error_PERMISSIONS_DENIED : result) else
    ([] : operation list),
    { storage with is_active = b ; }


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// proxy contracts can view the integer in storage
[@view] let proxy_view_storage (_, storage : unit * storage) : int = 
    if is_proxy Tezos.sender storage.directory then
        storage.store
    else
        (failwith error_PERMISSIONS_DENIED : int)

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | UpdateStorage n -> update_storage n storage
    | FetchStorage _ -> fetch_storage storage
    | Activate n -> activate n storage


