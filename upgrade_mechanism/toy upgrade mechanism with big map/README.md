# Second iteration : lazily importing big maps

This is the second iteration of the upgrade mechanism. It takes a contract with a ledger (`type storage = (address, nat) big_map`) and a minting mechanism (`type entrypoint = | Mint of { for_ : address ; amt_ : nat ; }`) to one which manages the same ledger, but with a transfer mechanism (`type entrypoint = | Mint of { for_ : address ; amt_ : nat ; } | Transfer of { to_ : address ; amt_ : nat ; }`). (This is not an FA1.2 or FA2 contract.)

The key feature here is that the big map storage is lazily imported.

(More documentation to come soon.)