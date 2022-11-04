# <p align="center">Hydra Pay</p>

## :dango: Introduction

The **Hydra Pay** project will provide an open-source library and framework for cardano light wallet developers to make use of [Hydra (Head)](https://hydra.family/head-protocol/).

This will cover:

* Spinning up a network (This includes starting and managing hydra nodes)
* An API to make the common operations simple to execute:
  * Initialization
  * Commiting
  * Transactions
  * Querying
  * Closing
  * Disputing
  * Fanout
* A real-world example application

## 👷🏾‍♂️ Running Hydra Pay

Hydra Pay is currently under construction, detailed steps about running and deploying Hydra Pay as well as endpoint and configuration documentation is on its way!

We plan to support docker, and nix out of the box for those who want to deploy and run their own Hydra Pay service, no knowledge of Obelisk, Haskell or any other underlying technology will be necessary.

The client library will be javascript and typescript and will make interacting with Hydra Heads through the Hydra Pay service a breeze.

## 👩🏿‍💻 Hacking on Hydra Pay
Hydra Pay is written in Haskell using [Obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk) so to contribute you must have Obelisk installed.

Once you have Obelisk installed hacking on Hydra Pay is as easy as running `ob run` in the root directory.

Some visual UI like the demo can then be viewed in Chrome at `http://localhost:8000/`.

## :construction: Under Construction

**Hyra Pay** is in-progress, and subject to change as the hydra-head protocol and development requirements evolve to meet the demands of light wallet and dApp developers. 

**Hydra Pay** is **NOT** yet ready for production and mainnet.

## 🎉 Features

Foundational:
- [x] Spawn devnet node
- [x] Standup Hydra network from participant names
- [x] Automatic seeding under devnet
- [x] Hydra Node communication API for participants
- [x] Simple frontend for Head interaction
- [x] Complete frontend for Head interaction
- [x] Coin selection for participants
- [x] How To Run
- [x] Head Creation
- [x] Head Managment
- [x] Head Init and Commit
- [x] Head network spawning and monitoring
- [x] Convenience API for adding funds and creating fuel (CIP-30, and hardware wallet compatible) 
- [x] REST Endpoints
- [ ] Websocket support
- [ ] TTL on add funds and fuel pre-built transactions
- [ ] Integration of hydra libraries
- [ ] Choice of network: Devnet, Preview, Preprod, Mainnet
- [ ] API Documentation
- [ ] Initial client interaction library
- [ ] How to Contribute

## 🗝 API

### Head Creation

`POST /hydra/heads`

To create a Head you will give it a friendly name and list the addresses that will become the participants.
Creating the head starts the Hydra network.

Example payload:
``` json
{
  "headCreate_name": "test",
  "headCreate_participants": [
    "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp"
  ],
}
```

### Head Status

`GET /hydra/head/:head-name`

Get the status of the Head on-chain and the status of the network of hydra-nodes.

Example Response:
``` json
{
  "headStatus_name": "test",
  "headStatus_running": true,
  "headStatus_status": "Status_Open"
}
```

### Head Init

`POST /hydra/init`

Post the inital state of your Head on chain, and start waiting for Commitments from the participants.

Example Payload:
``` json
{
  "headInit_name": "test",
  "headInit_participant": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp"
}
```

### Head Commit

`POST /hydra/commit`

Commit the funds at your Proxy Address to the named Head.

Example Payload:
``` json
{
  "headCommit_participant": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp",
  "headCommit_name": "test"
}
```

### Add Funds

`GET /hydra/add-funds/:cardano-address`

Get a CIP-30 compatible CBOR transaction that will fund your Proxy Address.

Example Response:
``` json
{
  "cborHex": "86a30081825820754f4ebbc6c18f083f417052881c99ef6e05dbd725b2367cfe5ab51839717640010182a200581d604391e312f4ce9ebbd5b6566d1ecade6e8545a7d56ec429e5c23810d3011a35a25b4ba300581d606b4e68b0955fbfd0be9b76527da8fc425fcc80fd47f40fd2d2b2d548011a05f5e1000282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3021a00028db59fff8080f5f6",
  "type": "TxBodyBabbage",
  "description": ""
}
```

### Add Fuel

`GET /hydra/add-fuel/:cardano-address`

Get a CIP-30 compatible CBOR transaction that will create a Fuel UTXO at your Proxy Address. 

Example Response:
``` json
{
  "cborHex": "86a30081825820754f4ebbc6c18f083f417052881c99ef6e05dbd725b2367cfe5ab51839717640010182a200581d604391e312f4ce9ebbd5b6566d1ecade6e8545a7d56ec429e5c23810d3011a35a25b4ba300581d606b4e68b0955fbfd0be9b76527da8fc425fcc80fd47f40fd2d2b2d548011a05f5e1000282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3021a00028db59fff8080f5f6",
  "type": "TxBodyBabbage",
  "description": ""
}
```

### Send funds on Head

`POST /hydra/submit-tx/:cardano-address`

Send funds from your Proxy Address to another participant identified by their L1 Address.

Example Payload:
``` json
{
  "headSubmitTx_name": "test",
  "headSubmitTx_toAddr": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp",
  "amount": 1000000
}
```

### Query head funds

`GET /hydra/head-balance/:head-name/:cardano-address`

Get the amount of Lovelace available on the head at your (Proxy) Address.

Example Response:
``` json
10000000
```

### Close

`POST /hydra/close/:head-name`

Close the head.

Example Response:
``` json
{
  "headStatus_name": "test",
  "headStatus_running": true,
  "headStatus_status": "Status_Closed"
}
```


### Withdraw

`POST /hydra/withdraw/`

Withdraw funds from your Proxy Address to your main address.

Example Payload:

``` json
{
  "withdraw_address": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp",
  "withdraw_amount": "5000000"
}
```

### Query L1 funds

`GET /hydra/l1-balance/:cardano-address`

Get the amount of Lovelace available on the head at your (Proxy) Address.

Example Response:
``` json
10000000
```


## 🦾 Proxy Addresses

Hydra Pay simplifies the creation and managment of Heads to facilitate easy creation of Hydra Head based features for Light Wallet and DApp developers. One way we aid feature creation is through our Proxy Address structure.

Instead of participating directly in a Head, any participant will actually be mapped to a "Proxy Address" this is a regular cardano address that is created to hold funds and fuel for said participant in Hydra Pay Head.

We have a couple important reasons for using this Proxy Address mapping:

- More security: Participants no longer need to provide their private keys to the hydra-node acting on their behalf. This means that developers won't need to ask potential participants for their seed phrase just to be able to join a Head. This keeps in line with the security principle of Hydra: The only funds you can lose are those you commit.

- More convenient fund management: Instead of having to prompty commit funds only when a Head is starting; users can add funds to their proxy address. This gives developers the freedom to orchestrate the timing of head creation and closing however they like, and users the confidence to participate meaningfully with ever Init/Fanout cycle without having to actively micro-manage their funds and Head commitments.

## 🤔 FAQ

### What is Hydra Pay?

A library and service for Light Wallet (and eventually DApp developers) to integrate payment channels allowing them fast finality and low fees through the use of Hydra Heads.

### I am a developer and have feature that I want to implement that uses Hydra. Should I use Hydra Heads directly or Hydra Pay?

The API of Hydra Pay is designed around Hydra Head creation, interaction, and participation. It manages the Heads and the underlying Hydra Nodes and provides a HTTP/Websocket interface, if your feature doesn't need Plutus Scripts to run on the Head, Hydra Pay is likely a good fit, and if it isn't, feel free to reach out about what you need and how we can get it there! 

### What will I need to do to run/deploy a Hydra Pay service?

Once Hydra Pay is available, you will simply run either the provided docker container for the service, or use the provided nix expression to include Hydra Pay service with whatever infrastructure you are using. 

### Can I run plutus scripts on a Head using Hydra Pay?

Not initially, we will be guaging the need/interest in this feature as Hydra Pay evolves and we get feedback from developers.
