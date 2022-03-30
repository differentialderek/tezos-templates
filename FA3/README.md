# FA3 Token Standard 

This is a somewhat experimental attempt for a a *pooled* multi-token standard, which we're provisionally writing here as FA3 ([there is some discussion about a new token standard here](https://forum.tezosagora.org/t/fa2-1-fa3-its-time/3704)).

The point of this token standard is that it can be useful to pool together families of semi-fungible tokens. This token standard allows a single FA2-style smart contract to manage multiple (semi-fungible) tokens, and allowing for tokens to be pooled together. 

To do this the contract keeps an internal exchange rate so that the tokens in the family can be traded for one another.

## Two Versions of the FA3 Contracts

There are two versions of the FA3 contracts here: `FA3.mligo` and `FA3_2.mligo`

`FA3.mligo`: 
This token contract allows users to pool their tokens. The pooled tokens provide liquidity for exchanges. Once a user pools their tokens, they receive *pool tokens* in exchange, where the quantity is given by the internal exchange rate. This can be useful to pool together tokens in a semi-fungible family to, for example, get more liquidity on-chain.

`FA3_2.mligo`: 
This token contract allows users to burn their tokens in exchange for tokens in the same semi-fungible family, but of different types. The idea is that these are all roughly equivalent, and it doesn't matter which you hold, since they all have a fixed value relative to each other. It might still be useful to have a designated "pool token" for similar reasons above like in order to get high liquidity on-chain. This may not be suitable for some use cases, depending on the minting conditions of each token in the semi-fungible family.

## Future work

An update to this will come so that the internal exchange rate can change over time to meet supply/demand