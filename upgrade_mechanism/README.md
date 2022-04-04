# Upgrade Mechanism

This is a repository that illustrates a dynamic upgrade mechanism for smart contracts that mimics the Tezos blockchain's upgrade mechanism.

In contrast to current upgradeable smart contracts it supports:
- Entrypoint type upgrades
- Storage type upgrades

The example in `toy upgrade mechanism` is the simplest example of the upgrade mechanism, which illustrates the governance, directory, storage, and entrypoint proxy contract structure. Instructions on deploying the upgrades are in the `README` there.

The second example, in `toy upgrade mechanism with big map`, shows how big maps can be lazily imported into an upgraded smart contract. This is done through the "telescope" mechanisms, namely the `proxy_view_ledger_telescope`, for fetching data from previous big maps, and the `update_ledger_telescope` entrypoint, for clearing the data from previous big maps. It recurses backwards, having the key property that if a key has a value in any storage contract's big map, then it necessarily does not have one in any previous storage contract. This makes it possible to safely upgrade the storage contract with big maps.

If you want to upgrade storage types, you need a function in `old_storage_type -> new_storage_type`. This is given in the `fetch_storage` entrypoint of the first contracts, which transforms the `nat` in storage into an `int`.

This can be done with big maps too, and the keys can be upgraded. As long as you have a function from `new_key_type -> old_key_type` and a function from `old_value_type -> new_value_type`, then you can lazily port over data from the old smart contract's big map into the new.

This is a WIP. Further documentation and examples will be coming (hopefully soon).

[See more documentation at this HackMD doc.](https://hackmd.io/@durlicc/r16q6D1Zq)