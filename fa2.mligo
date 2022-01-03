(* A generic FA2 contract *)
(* FA2 Proposal TZIP: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md *)
(* FA2 Standard: https://tezos.b9lab.com/fa2 *)

// TODO : Contract views for balances and metadata 
// TODO : How to perform an operation on something in a big map?

(* =============================================================================
 * Storage
 * ============================================================================= *)

type token_id = nat
type qty = nat

type owner = [@layout:comb] { token_owner : address ; token_id : nat ; }
type operator = [@layout:comb] {
    token_owner : address ; 
    token_operator : address ; 
    token_id : nat ;
}

type token_metadata = [@layout:comb]{ token_id : nat ; token_info : (string, bytes) map ; }
type contract_metadata = (string, bytes) big_map

type storage = {
    // admin 
    admin : address ; 

    // the ledger keeps track of who owns what token
    ledger : (owner , qty) big_map ; 
    
    // an operator can trade tokens on behalf of the fa2_owner
    // if the key (owner, operator, token_id) returns some k : nat, this denotes that the operator has (one-time?) permissions to operate k tokens
    // if there is no entry, the operator has no permissions
    // such permissions need to granted, e.g. for the burn entrypoint in the admin
    operators : (operator, nat) big_map;
    
    // token metadata for each token type supported by this contract
    token_metadata : (token_id, token_metadata) big_map;
    // contract metadata 
    metadata : (string, bytes) big_map;
}

type result = (operation list) * storage


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)
type transfer_to = [@layout:comb]{ to_ : address ; token_id : nat ; amount : nat ; }
type transfer = [@layout:comb] { from_ : address; txs : transfer_to list; }

type request = [@layout:comb]{ token_owner : address ; token_id : nat ; }
type callback_data = [@layout:comb]{ request : request ; balance : nat ; }
type balance_of = [@layout:comb]{ requests : request list ; callback : callback_data list contract ; }

type operator_data = [@layout:comb]{ owner : address ; operator : address ; token_id : nat ; qty : nat ; }
type update_operator = 
    | Add_operator of operator_data
    | Remove_operator of operator_data
type update_operators = update_operator list

type mintburn_data = { owner : address ; token_id : nat ; qty : nat ; }
type mintburn = mintburn_data list

type get_metadata = {
    token_ids : nat list ;
    callback : token_metadata list contract ;
}

type entrypoint = 
| Transfer of transfer list // transfer tokens 
| Balance_of of balance_of // query an address's balance
| Update_operators of update_operators // change operators for some address
| Mint of mintburn // mint tokens
| Burn of mintburn // burn tokens 
| Get_metadata of get_metadata // query the metadata of a given token
| Add_token_id of token_metadata list 
| Update_contract_metadata of contract_metadata


(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_FA2_TOKEN_UNDEFINED = 0n // One of the specified token_ids is not defined within the FA2 contract
let error_FA2_INSUFFICIENT_BALANCE = 1n // A token owner does not have sufficient balance to transfer tokens from owner's account
let error_FA2_TX_DENIED = 2n // A transfer failed because of fa2_operatortransfer_policy == No_transfer
let error_FA2_NOT_OWNER = 3n // A transfer failed because fa2_operatortransfer_policy == fa2_ownertransfer and it is invoked not by the token owner
let error_FA2_NOT_OPERATOR = 4n // A transfer failed because fa2_operatortransfer_policy == fa2_owneror_fa2_operatortransfer and it is invoked neither by the token owner nor a permitted operator
let error_FA2_OPERATORS_UNSUPPORTED = 5n // update_operators entrypoint is invoked and fa2_operatortransfer_policy is No_transfer or fa2_ownertransfer
let error_FA2_RECEIVER_HOOK_FAILED = 6n // The receiver hook failed. This error MUST be raised by the hook implementation
let error_FA2_SENDER_HOOK_FAILED = 7n // The sender failed. This error MUST be raised by the hook implementation
let error_FA2_RECEIVER_HOOK_UNDEFINED = 8n // Receiver hook is required by the permission behavior, but is not implemented by a receiver contract
let error_FA2_SENDER_HOOK_UNDEFINED = 9n // Sender hook is required by the permission behavior, but is not implemented by a sender contract
let error_PERMISSIONS_DENIED = 10n // General catch-all for operator-related permission errors
let error_ID_ALREADY_IN_USE = 11n // A token ID can only be used once, error if a user wants to add a token ID that's already there
let error_COLLISION = 12n // A collision in storage 

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// The transfer entrypoint function
// The transfer function creates a list of transfer operations recursively
let rec execute_transfer (param , storage : transfer * storage) : storage = 
    match param.txs with
    | [] -> storage
    | hd :: tl ->
        let (from, to, token_id, qty, operator) = (param.from_, hd.to_, hd.token_id, hd.amount, Tezos.sender) in 
        let owner = from in 
        // update operator permissions to reflect this transfer
        let operators = 
            if Tezos.sender <> from // thus this is an operator
            then 
                let allowed_qty = 
                    match Big_map.find_opt {token_owner = owner; token_operator = operator; token_id = token_id ;} storage.operators with 
                    | None -> 0n | Some q -> q in 
                // failw if operator does not have permissions 
                if allowed_qty < qty then (failwith error_FA2_NOT_OPERATOR : (operator, nat) big_map) else 
                Big_map.update {token_owner = owner; token_operator = operator; token_id = token_id ;} (Some (abs (allowed_qty - qty))) storage.operators
            else storage.operators in
        // update the ledger
        let ledger = 
            // check balances 
            let sender_token_balance =
                match Big_map.find_opt {token_owner = from; token_id = token_id ;} storage.ledger with | None -> 0n | Some b -> b in
            let recipient_balance = 
                match Big_map.find_opt {token_owner = to; token_id = token_id ;} storage.ledger with | None -> 0n | Some b -> b in
            // ensure sufficient funds 
            if (sender_token_balance < qty) then (failwith error_FA2_INSUFFICIENT_BALANCE : (owner, qty) big_map) else
            // update the ledger 
            Big_map.update
            { token_owner = to ; token_id = token_id ; }
            (Some (recipient_balance + qty))
                (Big_map.update 
                 {token_owner = from; token_id = token_id ;}
                 (Some (abs (sender_token_balance - qty))) 
                 storage.ledger) in 
        // recurse with the same from_ address
        execute_transfer (
            { from_ = from ; txs = tl ; }, 
            { storage with ledger = ledger ; operators = operators ; }
        )

let rec transfer (param, storage : transfer list * storage) : result = 
    match param with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let storage = execute_transfer (hd, storage) in 
        transfer (tl, storage)


// the entrypoint to query balance 
let balance_of (param : balance_of) (storage : storage) : result = 
    let (request_list, callback) = (param.requests, param.callback) in 
    let op_balanceOf = 
        Tezos.transaction 
        (
            List.map 
            (
                fun (r : request) ->  
                { request = r ; 
                  balance = 
                    match Big_map.find_opt r storage.ledger with | None -> 0n | Some b -> b ; } 
            )
            request_list 
        )
        0mutez 
        callback in
    ([op_balanceOf], storage)


// The entrypoint where fa2_owner adds or removes fa2_operator from storage.operators
let update_operator (storage, param : storage * update_operator) : storage = 
    match param with
    | Add_operator o ->
        let (owner, operator, token_id, qty) = (o.owner, o.operator, o.token_id, o.qty) in 
        // check permissions        
        if (Tezos.source <> owner) then (failwith error_PERMISSIONS_DENIED : storage) else
        if operator = owner then (failwith error_COLLISION : storage) else // an owner can't be their own operator 
        // update storage
        {storage with operators = 
            let new_qty = 
                let old_qty = 
                    match Big_map.find_opt {token_owner = owner; token_operator = operator; token_id = token_id ;} storage.operators with 
                    | None -> 0n 
                    | Some q -> q in 
                old_qty + qty in 
            Big_map.update {token_owner = owner; token_operator = operator; token_id = token_id ;} (Some new_qty) storage.operators ; }
    | Remove_operator o ->
        let (owner, operator, token_id) = (o.owner, o.operator, o.token_id) in 
        // check permissions
        if (Tezos.source <> owner) then (failwith error_PERMISSIONS_DENIED : storage) else
        // update storage
        {storage with 
            operators = Big_map.update {token_owner = owner; token_operator = operator; token_id = token_id ;} (None : nat option) storage.operators ; }
        
let rec update_operators (param, storage : update_operators * storage) : result = 
    ([] : operation list),
    List.fold update_operator param storage 

// This entrypoint can only be called by the admin
let rec mint_tokens (param, storage : mintburn * storage) : result =
    // check permissions 
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else 
    // mint tokens 
    let minting_list = param in
    match minting_list with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let storage = {storage with ledger = 
            let (owner, token_id, qty) = (hd.owner, hd.token_id, hd.qty) in 
            // update owner balance
            let owner_balance = 
                match Big_map.find_opt {token_owner = owner; token_id = token_id ;} storage.ledger with
                | None -> 0n + qty
                | Some ownerPrevBalance -> ownerPrevBalance + qty in
            Big_map.update {token_owner = owner; token_id = token_id ;} (Some owner_balance) storage.ledger ; } in 
        mint_tokens (tl, storage)

// this is the burn function, which is a transfer function to the burn address 
let burn_tokens (param : mintburn) : transfer = 
    let burn_addr = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) in // standard burn address 
    let from = Tezos.source in 
    // transfer the tokens to the burn address
    {
        from_ = from ;
        txs = List.map
            (fun (b : mintburn_data) : transfer_to -> 
                let () = assert (b.owner = from) in 
                {
                    to_ = burn_addr ;
                    token_id = b.token_id ;
                    amount = b.qty ;
                })
            param ;
    }

// The entrypoint to query token metadata
let get_metadata (param : get_metadata) (storage : storage) : result = 
    let (query, callback) = (param.token_ids, param.callback) in 
    let op_metadata = 
        Tezos.transaction
        (
            List.map 
            (fun (token_id : nat) : token_metadata -> 
                match Big_map.find_opt token_id storage.token_metadata with 
                | None -> (failwith error_FA2_TOKEN_UNDEFINED : token_metadata) 
                | Some m -> {token_id = token_id ; token_info = m.token_info ; })
            query
        )
        0tez 
        callback in 
    ([op_metadata] , storage)

// This entrypoint allows the admin to add token ids to their contract
// If there is a collision on token ids, this entrypoint will return a failwith
let add_token_id (param : token_metadata list) (storage : storage) : result = 
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else
    let storage = 
        List.fold_left
        (fun (s, d : storage * token_metadata) -> 
            { s with token_metadata = 
                match Big_map.get_and_update d.token_id (Some d) s.token_metadata with
                | (None, m) -> m
                | (Some _, m) -> (failwith error_COLLISION : (token_id, token_metadata) big_map) } )
        storage
        param in 
    ([] : operation list), storage


// this entrypoint allows the admin to update the contract metadata
let update_contract_metadata (param : contract_metadata) (storage : storage) : result = 
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else
    ([] : operation list),
    { storage with metadata = param }


(* =============================================================================
 * Main
 * ============================================================================= *)

let rec main ((entrypoint, storage) : entrypoint * storage) : result =
    match entrypoint with
    | Transfer param ->
        transfer (param, storage)
    | Balance_of param -> 
        balance_of param storage
    | Update_operators param ->
        update_operators (param, storage)
    | Mint param -> 
        mint_tokens (param, storage)
    | Burn param ->
        main ( Transfer([burn_tokens param]), storage )
    | Get_metadata param ->
        get_metadata param storage
    | Add_token_id param ->
        add_token_id param storage
    | Update_contract_metadata param ->
        update_contract_metadata param storage