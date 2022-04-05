// the proxy contract for governance 

// A proposal can call entrypoints in the name of the governance contract. That's it.

(* =============================================================================
 * Storage
 * ============================================================================= *)

// the directory types 
type new_proxy = {
    proxy_name : string ;
    proxy_addr : address ; }
type existing_proxy = {
    proxy_name : string ;
    new_proxy_addr : address ; }
type remove_proxy = string 
type update_proxy = 
| ExistingProxy of existing_proxy
| NewProxy of new_proxy
| RemoveProxy of remove_proxy

type disburse_funds = {
    to_ : address ; 
    amt_ : tez ; 
} 
type add_governor = {
    new_governor : address ;
    new_quorum : nat ;
}
type remove_governor = {
    old_governor : address ;
    new_quorum : nat ;    
}
type proposed_txn = 
| UpdateProxy of update_proxy 
| DisburseFunds of disburse_funds
| IncomingGovernor of add_governor 
| OutgoingGovernor of remove_governor

type voter_data = [@layout:comb]{ voter : address ; proposal_id : nat ;}
type vote = 
| Yea
| Nay
| Abstain

type activated = bool
type proposal = proposed_txn list
type proposal_record = {
    proposal_id : nat ;    
    proposal : proposal ; 
    activated : activated ;
}

type storage = {
    is_active : bool ;
    directory : address ; 
    governance : (address, unit) big_map ; // if there's just an admin then this big map has just one individual
    quorum : nat ; // the number of Yea votes required for a pass 
    proposal_history : (nat, proposal_record) big_map ; 
    votes : ({ voter : address ; proposal_id : nat ; }, vote) big_map ;
}

type result = operation list * storage 

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type submit_proposal = {
    proposal_id : nat ;
    proposal : proposal ;
}
type vote_on_proposal = {
    proposal_id : nat ;
    vote : vote ;
}
type execute_proposal = {
    proposal_id : nat ;
    yea_votes : address list ;
}

type entrypoint = 
| SubmitProposal of submit_proposal   // submit proposal 
| VoteonProposal of vote_on_proposal  // vote on proposal 
| ExecuteProposal of execute_proposal // execute proposal 
| AddGovernor of add_governor 
| RemoveGovernor of remove_governor
| Activate of bool 

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n
let error_INACTIVE_CONTRACT = 1n
let error_COLLISION = 2n
let error_PROPOSAL_NOT_FOUND = 3n 
let error_PROPOSAL_ACTIVATED = 4n
let error_NOT_FOUND = 5n
let error_INSUFFICIENT_VOTES = 6n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

(* ====== 
    SUBMIT A PROPOSAL
    A member of governance can submit a proposal, which is a list of transactions
 * ====== *)

let submit_proposal (param : submit_proposal) (storage : storage) : result = 
    let (proposal_id, proposal) = (param.proposal_id, param.proposal) in
    // check that a member of governance submitted the proposal 
    if not Big_map.mem Tezos.sender storage.governance then (failwith error_PERMISSIONS_DENIED : result) else 
    // check for collisions in the ID
    match Big_map.find_opt proposal_id storage.proposal_history with 
    | Some _ -> (failwith error_COLLISION : result)
    | None -> 
        ([] : operation list),
        { storage with proposal_history = 
            Big_map.add 
            proposal_id 
            { proposal_id = proposal_id ; proposal = proposal ; activated = false ; }
            storage.proposal_history
        }

(* ====== 
    VOTE ON A PROPOSAL
    Only members of governance can vote.
    Voting can happen until the proposal is activated. 
    A member of governance can overwrite their vote by voting again
 * ====== *)

let vote_on_proposal (param : vote_on_proposal) (storage : storage) : result = 
    let (proposal_id, vote) = (param.proposal_id, param.vote) in 
    // check that a member of governance is voting
    if not Big_map.mem Tezos.sender storage.governance then (failwith error_PERMISSIONS_DENIED : result) else 
    // record the vote
    match Big_map.find_opt proposal_id storage.proposal_history with
    | None -> (failwith error_PROPOSAL_NOT_FOUND : result)
    | Some proposal_record -> 
        if proposal_record.activated then (failwith error_PROPOSAL_ACTIVATED : result) else 
        ([] : operation list),
        { storage with votes = 
            Big_map.update { voter = Tezos.sender; proposal_id = proposal_id ; } (Some vote) storage.votes } 

(* ====== 
    EXECUTE A PROPOSAL
    Once there are sufficiently many votes, a member of governance
    can execute the proposal by submitting a list of addresses who voted yea
    that form a quorum
 * ====== *)

let rec execute_proposed_txns (proposal, acc, directory : proposal * (operation list) * address) : operation list = 
    match proposal with 
    | [] -> acc
    | proposed_txn :: tl -> (
        match proposed_txn with 
        | UpdateProxy update_proxy -> (
            match (Tezos.get_entrypoint_opt "%updateProxy" directory : update_proxy contract option) with
            | None -> (failwith error_NOT_FOUND : operation list)
            | Some contract_addr -> execute_proposed_txns (tl, Tezos.transaction update_proxy 0tez contract_addr :: acc, directory) )
        | DisburseFunds disburse_funds -> (
            match (Tezos.get_entrypoint_opt "%disburseFunds" directory : disburse_funds contract option) with
            | None -> (failwith error_NOT_FOUND : operation list)
            | Some contract_addr -> execute_proposed_txns (tl, Tezos.transaction disburse_funds 0tez contract_addr :: acc, directory) )
        | IncomingGovernor incoming_governor -> (
            match (Tezos.get_entrypoint_opt "%addGovernor" Tezos.self_address : add_governor contract option) with
            | None -> (failwith error_NOT_FOUND : operation list)
            | Some contract_addr -> execute_proposed_txns (tl, Tezos.transaction incoming_governor 0tez contract_addr :: acc, directory) )
        | OutgoingGovernor outgoing_governor -> (
            match (Tezos.get_entrypoint_opt "%removeGovernor" Tezos.self_address : remove_governor contract option) with
            | None -> (failwith error_NOT_FOUND : operation list)
            | Some contract_addr -> execute_proposed_txns (tl, Tezos.transaction outgoing_governor 0tez contract_addr :: acc, directory) )
    )

let rec voted_yea (yea_votes, proposal_id, votes : (address list) * nat * (({ voter : address ; proposal_id : nat ; }, vote) big_map)) : bool = 
    match yea_votes with 
    | [] -> true 
    | voter :: tl -> (
        match Big_map.find_opt { voter = voter ; proposal_id = proposal_id ; } votes with 
            | None -> (failwith error_NOT_FOUND : bool)
            | Some vote -> (
                match vote with 
                | Yea -> voted_yea (tl, proposal_id, votes)
                | Nay -> false
                | Abstain -> false ))

let execute_proposal (param : execute_proposal) (storage : storage) : result = 
    let (proposal_id, yea_votes) = (param.proposal_id, param.yea_votes) in 
    // check that a member of governance is executing the proposal
    if not Big_map.mem Tezos.sender storage.governance then (failwith error_PERMISSIONS_DENIED : result) else 
    // fetch the proposal record
    match Big_map.find_opt proposal_id storage.proposal_history with 
    | None -> (failwith error_PROPOSAL_NOT_FOUND : result) 
    | Some proposal_record ->
        // check the votes, both that they are all yea and that there are enough for a quorum
        if List.length yea_votes < storage.quorum || not voted_yea (yea_votes, proposal_id, storage.votes) 
            then (failwith error_INSUFFICIENT_VOTES : result) else 
        // there are sufficient votes, so execute the transactions from the proposal 
        execute_proposed_txns (proposal_record.proposal, ([] : operation list), storage.directory),
        { storage with
            proposal_history = (
                Big_map.update 
                proposal_id
                (Some { proposal_record with activated = true ; })
                storage.proposal_history
            ) ; }

(* ====== 
    Add/Remove Governor. Only the governance contract can do this.
 * ====== *)

let add_governor (param : add_governor) (storage : storage) : result = 
    let (new_governor, new_quorum) = (param.new_governor, param.new_quorum) in 
    // only governance can vote to add/remove governors
    if Tezos.sender <> Tezos.self_address then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with 
        governance = Big_map.add new_governor () storage.governance ; 
        quorum = new_quorum ; }

let remove_governor (param : remove_governor) (storage : storage) : result = 
    let (old_governor, new_quorum) = (param.old_governor, param.new_quorum) in 
    // only governance can vote to add/remove governors
    if Tezos.sender <> Tezos.self_address then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with 
        governance = Big_map.add old_governor () storage.governance ; 
        quorum = new_quorum ; }

(* ====== 
    Activate the contract. Only the directory contract can do this.
 * ====== *)

let activate (b : bool) (storage : storage) : result = 
    if Tezos.sender <> storage.directory then (failwith error_PERMISSIONS_DENIED : result) else
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
    // members of governance can submit, vote on, and execute proposals, which are lists of transactions
    | SubmitProposal p ->
        submit_proposal p storage
    | VoteonProposal p ->
        vote_on_proposal p storage
    | ExecuteProposal p ->
        execute_proposal p storage
    // governance can alter some internal parameters here
    | AddGovernor p ->
        add_governor p storage 
    | RemoveGovernor p -> 
        remove_governor p storage
    // the directory contract can activate this contract 
    | Activate p ->
        activate p storage


