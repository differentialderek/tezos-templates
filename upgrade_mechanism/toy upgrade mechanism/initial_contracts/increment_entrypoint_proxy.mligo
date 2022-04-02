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

// Increment entrypoint 
type entrypoint = 
| Increment of int
| Activate of bool

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_PROXY_NOT_FOUND = 1n
let error_INACTIVE_CONTRACT = 2n 

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

let view_storage (directory : address) : int = 
    match (Tezos.call_view "getProxy" "storage" directory : address option) with 
    | None -> (failwith error_PROXY_NOT_FOUND : int)
    | Some storage_addr -> (
        match (Tezos.call_view "proxy_view_storage" () storage_addr : int option) with 
        | None -> (failwith error_PERMISSIONS_DENIED : int)
        | Some store -> store
    )

let create_internal_operation (type param_type) (proxy_name : string) (param : param_type) (directory : address) : operation = 
    match (Tezos.call_view "getProxy" proxy_name directory : address option) with 
    | None -> (failwith error_PROXY_NOT_FOUND : operation)
    | Some addr_proxy -> (
        match (Tezos.get_entrypoint_opt "%updateStorage" addr_proxy : param_type contract option) with 
        | None -> (failwith error_PROXY_NOT_FOUND : operation)
        | Some addr_entrypoint -> (
            Tezos.transaction param 0tez addr_entrypoint
        )
    )
(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let increment (param : int) (storage : storage) : result = 
    let new_storage = (view_storage storage.directory) + param in 
    // call the storage contract to update the storage 
    let op_update_storage = create_internal_operation "storage" new_storage storage.directory in
    [ op_update_storage ; ],
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
    | Increment n -> increment n storage
    | Activate n -> activate n storage


