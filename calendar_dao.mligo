// A DAO is a game encoded on-chain
// a DAO that organizes meeting times 

(* =============================================================================
 * Storage
 * ============================================================================= *)

type mtg_location = 
| DAB of string // room number in the DAB; 0n is default (Keshav/Anil's office)
| WGB of string // room number in the WGB; 0n is default (Keshav's office)
| Pembroke of string // location in Pembroke (encoded as a string), e.g. "plodge"
| Zoom of string // we meet over Zoom. The string is the link

type storage = {
    players : address set ;
    next_mtg_location : mtg_location * timestamp ;
    ack : address set ;   
    // tokens 
    token_ledger : (address, nat) big_map ;
    to_redeem_tokens : address set ;
    last_token_drop : timestamp ; 
    // contract metdata 
    metadata : (string, bytes) big_map;
}

type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| Propose_mtg_location of mtg_location // clears ack 
| Ack_mtg_location of unit // acknowledges 
| Get_tokens of unit  
| Add_player of address 
| Remove_player of address 

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

let propose_mtg_location (param : mtg_location) (storage : storage) : result = 
    if not Set.mem Tezos.sender storage.players then (failwith error_PERMISSIONS_DENIED : result) else 
    // it costs one token to propose 
    let token_ledger = 
        let old_bal =   
            match Big_map.find_opt Tezos.sender storage.token_ledger with 
            | None -> (failwith error_PERMISSIONS_DENIED : nat)
            | Some b -> b in 
        Big_map.update Tezos.sender (Some (abs(old_bal - 1n))) storage.token_ledger in 
    // update storage
    ([] : operation list),
    { storage with 
        next_mtg_location = (param, Tezos.now) ;
        ack = (Set.empty : address set) ;
        token_ledger = token_ledger ;
    }

let ack_mtg_location (_ : unit) (storage : storage) : result = 
    if not Set.mem Tezos.sender storage.players then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with ack = Set.add Tezos.sender storage.ack ; }


let get_tokens (_ : unit) (storage : storage) : result = 
    if not Set.mem Tezos.sender storage.players then (failwith error_PERMISSIONS_DENIED : result) else 
    if Tezos.now < (storage.last_token_drop + 604800) then // one week 
        // the user is redeeming outstanding tokens 
        if Set.mem Tezos.sender storage.to_redeem_tokens then 
        let old_bal =   
            match Big_map.find_opt Tezos.sender storage.token_ledger with 
            | None -> 0n 
            | Some b -> b in 
        ([] : operation list),
        { storage with 
            token_ledger = Big_map.update Tezos.sender (Some (old_bal + 1n)) storage.token_ledger ;
            to_redeem_tokens = Set.remove Tezos.sender storage.to_redeem_tokens ;
        }
        else (failwith error_PERMISSIONS_DENIED : result)
    else // the user wants to trigger a token drop, which can be done every week  
        let storage = {storage with to_redeem_tokens = storage.players} in 
        // the user is redeeming outstanding tokens 
        let old_bal =   
            match Big_map.find_opt Tezos.sender storage.token_ledger with 
            | None -> 0n 
            | Some b -> b in 
        ([] : operation list),
        { storage with 
            token_ledger = Big_map.update Tezos.sender (Some (old_bal + 1n)) storage.token_ledger ;
            to_redeem_tokens = Set.remove Tezos.sender storage.to_redeem_tokens ;
        }


// players can add or remove other players
let add_player (param : address) (storage : storage) : result = 
    if not Set.mem Tezos.sender storage.players then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with  
        players = Set.add param storage.players ; }

let remove_player (param : address) (storage : storage) : result = 
    if not Set.mem Tezos.sender storage.players then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with  
        players = Set.remove param storage.players ; }


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// [@view] view1 (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | Propose_mtg_location p ->
        propose_mtg_location p storage
    | Ack_mtg_location p ->
        ack_mtg_location p storage
    | Get_tokens p ->
        get_tokens p storage 
    | Add_player p ->
        add_player p storage 
    | Remove_player p ->
        remove_player p storage 


