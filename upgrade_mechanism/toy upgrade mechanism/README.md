# First iteration 

This is the first and simplest iteration of the upgrade mechanism. The contracts in the `initial_contracts` folder encode the following smart contract into the proxy contract framework: 

```
// Initial Contract

type storage = nat

type parameter =
| Increment of nat
| Reset

type return = operation list * storage

let add (store, delta : storage * nat) : storage = store + delta

let main (action, store : parameter * storage) : return =
 ([] : operation list), 
 (match action with
 | Increment (n) -> add (store, n)
 | Reset         -> 0n)
```

We then upgrade to the following contracts, encoded in the `upgraded_contracts` folder, upgrading the number of entrypoints, the entrypoint types, and the types in the storage:

```
// Upgraded Contract
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
 ```

## Upgrades 

To run this, do the following:

### Initial Contracts 

Deploy each of the contracts in `initial_contracts`, starting with the `directory` contract. This is the contract that will never change, encoding the fundamental and immutable structure of the smart contract, which is simply the fact that there are entrypoints, that there is a storage, and there is a governing entity (whether it be an admin, or a governing body that has to vote.

Deploy `directory.mligo` with the following initial storage:
```
{
   proxy_addresses : (Big_map.empty : (string, address) big_map) ;
   proxy_addresses_unit : (Big_map.empty : (address, unit) big_map) ; 
}
```

Then with the address of the `directory` contract as `addr_directory : address`, and the address deploying the contracts `addr_deploy : address`:

Deploy `governance.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ; 
   governance = ([ (addr_deploy, ()) ; ] : (address, unit) big_map);
   quorum = 1n ; 
   proposal_history = (Big_map.empty (nat, proposal_record) big_map) ; 
   votes = (Big_map.empty : ({ voter : address ; proposal_id : nat ; }, vote) big_map) ;
}
```

Deploy `storage.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ;
   store = 0n ; // actual initial storage value  
   previous_storage_contract = (None : address option) ;
}
```

Deploy `increment_entrypoint_proxy.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ;  
}
```

Deploy `reset_entrypoint_proxy.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ;  
}
```

Then `addr_deploy` can call the `%addNewProxy` entrypoint of the directory contract four times with the following payloads:
```
{ proxy_name = "storage" ; proxy_addr : addr_storage ; }
{ proxy_name = "%increment" ; proxy_addr : addr_storage ; }
{ proxy_name = "%reset" ; proxy_addr : addr_storage ; }
{ proxy_name = "governance" ; proxy_addr : addr_storage ; }
```
(Once the directory contract has a proxy address for `"governance"` then it can only be updated by a call from the governance contract.)

The contract should now function like the Initial Contract above. You can call the `%increment` entrypoint of `increment_entrypoint_proxy` and the `%reset` entrypoint from `reset_entrypoint_proxy`. Both of these address can be gotten from the `getProxy` contract view of the directory contract at any time (regardless of upgrades).

### Upgrade the Contracts

To upgrade the contracts, a member of governance needs to deploy the upgraded contracts, in this case the storage and three entrypoint contracts, and then submit a proposal to the governance contract to upgrade to the new contracts.

Deploy `increment_entrypoint_proxy.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ;  
}
```

Deploy `decrement_entrypoint_proxy.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ;  
}
```

Deploy `reset_entrypoint_proxy.mligo` with the following initial storage:
```
{
   is_active = false ;
   directory = addr_directory ;  
}
```

Deploy `storage_proxy.mligo` with the following initial storage (where `addr_old_storage` is the old storage address):
```
{
   is_active = false ;
   directory = addr_directory ;
   store = 0n ; 
   previous_storage_contract = Some addr_old_storage ; 
}
```
Upgrading will fetch the actual storage, turn it into an `int`, and then replace `0n` in the storage contract above.


Then submit a proposal to the governance contract with the following payload:
```
{
   proposal_id = 0n ;
   proposal = [
      ExistingProxy({ proxy_name = "storage" ; proxy_addr = new_storage_addr ; });
      ExistingProxy({ proxy_name = "%increment" ; proxy_addr = new_increment_addr ; });
      ExistingProxy({ proxy_name = "%reset" ; proxy_addr = new_reset_addr ; });
      NewProxy({ proxy_name = "%decrement" ; proxy_addr = decrement_addr });
   ] ;
}
```
Here `new_storage_addr`, `new_increment_addr`, `new_reset_addr`, and `decrement_addr` are the addresses of the newly deployed upgrade contracts.

A quorum of governors has to then vote `yea`, after which a member of governance can call `%executeProposal` with the proposal ID and a list indicating a quorum of voters that voted `yea`.

If this happens, the old contracts get deactivated (`is_active => false` in storage), the `big_map` in the directory contract gets updated to change the contract pointers, the new storage contract fetches the old storage, and then the new contracts get activated (`is_active => true` in storage).

They should then behave like the Upgraded Contract indicated above, where anyone can call the `%increment`, `%decrement`, and `%reset` entrypoints (and fetch those entrypoint addresses from the directory contract). The old entrypoint and storage contracts are no longer active, and their entrypoints cannot be called. The only thing that still works is the storage contract's views into its storage, which we will see is important for when the contract storage has a big map and the storage has to be transferred lazily.