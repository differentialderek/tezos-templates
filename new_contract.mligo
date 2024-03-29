// A template to start coding a new contract

(* =============================================================================
 * Storage
 * ============================================================================= *)


type storage = {
    // storage here 
}


type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| // entrypoints here 

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)




(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// [@view] viewName (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | // entrypoints 


