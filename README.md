# <p align="center">Hydra for Payments (Hydra Pay)</p>

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

## Prerequisites

1. [Install Nix](https://nixos.org/nix/). If you already have Nix installed, make sure you have version 2.0 or higher. To check your current version, run nix-env --version.

2. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```nix
        nix.binaryCaches = [ "s3://obsidian-open-source" ];
        nix.binaryCachePublicKeys = [ "obsidian-open-source:KP1UbL7OIibSjFo9/2tiHCYLm/gJMfy8Tim7+7P4o0I=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    2. If you are using another operating system or Linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```nix
        binary-caches = https://cache.nixos.org s3://obsidian-open-source
        binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= obsidian-open-source:KP1UbL7OIibSjFo9/2tiHCYLm/gJMfy8Tim7+7P4o0I=
        binary-caches-parallel-connections = 40
        ```

## 👷🏾‍♂️ Running Hydra Pay

Hydra Pay is currently under construction, detailed steps about running and deploying Hydra Pay as well as endpoint and configuration documentation is on its way!

We plan to support docker, and nix out of the box for those who want to deploy and run their own Hydra Pay service, no knowledge of Obelisk, Haskell or any other underlying technology will be necessary.

To deploy hydra-pay locally you can build and deploy the app as described below.

Build Hydra Pay:

```bash
nix-build -A exe --no-out-link
```

Copy the result to a new directory, add configuration, and run!

```bash
mkdir test-app
ln -s $(nix-build -A exe --no-out-link)/* test-app/
cp -r config test-app
(cd test-app && ./backend)
```

Visit the live documentation and confirm your key and requests presented work as expected.

### API Key

Your API Key should be provided in config/backend/api-key if no key is provided we generate a random one the aformentioned file. 

DO NOT USE THIS KEY IN PRODUCTION.

### Live Documentation

Once you have hydra pay running visit [localhost:8000](http://localhost:8000) to see the Live Documentation which gives you a realtime way to test and learn about the Hydra Pay API and Best Practices.

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
- [x] Websocket support
- [x] API Documentation
- [x] Authentication support
- [x] Tested on Preview
- [x] Tested over https and wss
- [ ] Deployment Guide
- [ ] Docker Deployment Guide
- [ ] Best Practices
- [ ] TTL on add funds and fuel pre-built transactions
- [ ] Integration of hydra libraries
- [ ] Choice of network: Devnet, Preview, Preprod, Mainnet
- [ ] How to Contribute

## 🗝 API

The Hydra Pay API is a websocket based API that gives you all that you need to manage and monitor heads, securely. The endpoint to connect to the Hydra Pay websocket is /hydra/api.

Once you are connected you must authenticate via your Authentication Token you have set for your Hydra Pay Instance.

In this section Client refers to the developer, and Server refers to the running Hydra Pay instance reachable at /hydra/api/.

### WebSocket API Design

The Hydra Pay API allows both Request/Response and subscription based server-pushed information for managing your Heads and Payment Channels.

Anytime you make a request to the Server you must Tag it with a request-id, and you will receieve a response with the same request-id. It is the Client's responsiblity to provide unique requests ids for any in-flight requests. The simplest way to achieve this is track the last request-id starting from 0 and increment whenever any request is made to the Server.

All requests to the Server must be tagged.

### Tagging Requests, and Request/Response in Hydra Pay

All communication the Client does with the Server through the WebSocket must be Tagged. For example you may say hello to the server with a `ClientHello` message:

``` json
{ "tag" : "ClientHello" }
```

To Tag this message, we need the unique request-id, if we were sending this as the first message to our Server we may use 0 as the request-id. Tagging is as simple as wrapping the above message like so:

``` json
{ "tagged_payload" : { "tag" : "ClientHello" }, "tagged_id" : 0 }
```

This request can now be sent through the websocket, at which point we should recieve a Tagged Response as follows:

``` json
{ "tagged_payload" : { "tag" : "ServerHello", "version" : "0.1.0" }, "tagged_id" : 0 }
```

Remember that all communication the Client makes with the Server must be tagged. This forms the request response part of the API.

### Subscriptions

There are requests in the Hydra Pay API that tell Hydra Pay that you are interested in receiving timely information about a Head or Heads. When Hydra Pay knows you are interested in this data, you will recieve un-tagged (as opposed to Tagged as you would see in request response), payloads delivering the subscribed to information update.

Information might include a Head changing state, an error or failure, a restart of a Node, or the inability to settle or fanout based on the fuel of one or more participants in a Head.

This allows Light Wallet and DApp developers to have their implementation react and respond to these issues in a timely and automatic way.

### Authentication

When you launch or deploy a Hydra Pay instance you will need to provide an API Key to authenticate against, this is a secret that should be only known to your DApp/LightWallet and your Hydra Pay instance. 

Upon opening a websocket connection to your HydraPay instance, you should immediately Authenticate by sending a Tagged `Authenticate` request (see below).

### Request and Response Payloads

Here is a list of request/response payloads. Remember that you must Tag (see above), these payloads when communicating with the Server. 

#### ClientHello

Say hello to the server, it will report its version when it says hello back:

``` json
{ "tag": "ClientHello" }
```

Example Response:
``` json
{ "tag": "ServerHello", "version": "0.1.0" }
```

#### Authentication 

To start making legitimate requests to your Server(Hydra Pay Instance), you must first authenticate yourself on the open websocket connection.
This will use the API Key you set up when you deployed the Server.

``` json
{ "tag": "Authenticate", "contents": "KbYei/+ymqAeqgXCiS+pfn88xMkkfXHhe8d/YHU3kGM=" }
```

Example Response:
``` json
{ "tag": "AuthenticationResult", "contents": true }
```

#### Subscribe to Head

Subscribing to a Head means you are interested in getting timely information about this Head's operation, including state changes, errors, and status updates like snapshot confirmation, node status and restarts. You will recieve *un-tagged* requests on the websocket where you subscribed.

This makes it easier to implement logic where you wait for say a head to close and fanout, without having to poll the socket.

```json
{ "tag": "SubscribeTo", "contents": "test" }
```

Example Response:
``` json
{ "tag": "SubscriptionStarted", "contents": "test" }
```

#### Head Creation

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

#### Head Status

Get the status of the Head on-chain and the status of the network of hydra-nodes.

Example Response:
``` json
{
  "headStatus_name": "test",
  "headStatus_running": true,
  "headStatus_status": "Status_Open"
}
```

#### Head Init

Post the inital state of your Head on chain, and start waiting for Commitments from the participants.

Example Payload:
``` json
{
  "headInit_name": "test",
  "headInit_participant": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp"
}
```

#### Head Commit

Commit the funds at your Proxy Address to the named Head.

Example Payload:
``` json
{
  "headCommit_participant": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp",
  "headCommit_name": "test"
}
```

#### Add Funds

Get a CIP-30 compatible CBOR transaction that will fund your Proxy Address.

Example Response:
``` json
{
  "cborHex": "86a30081825820754f4ebbc6c18f083f417052881c99ef6e05dbd725b2367cfe5ab51839717640010182a200581d604391e312f4ce9ebbd5b6566d1ecade6e8545a7d56ec429e5c23810d3011a35a25b4ba300581d606b4e68b0955fbfd0be9b76527da8fc425fcc80fd47f40fd2d2b2d548011a05f5e1000282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3021a00028db59fff8080f5f6",
  "type": "TxBodyBabbage",
  "description": ""
}
```

#### Add Fuel

Get a CIP-30 compatible CBOR transaction that will create a Fuel UTXO at your Proxy Address. 

Example Response:
``` json
{
  "cborHex": "86a30081825820754f4ebbc6c18f083f417052881c99ef6e05dbd725b2367cfe5ab51839717640010182a200581d604391e312f4ce9ebbd5b6566d1ecade6e8545a7d56ec429e5c23810d3011a35a25b4ba300581d606b4e68b0955fbfd0be9b76527da8fc425fcc80fd47f40fd2d2b2d548011a05f5e1000282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3021a00028db59fff8080f5f6",
  "type": "TxBodyBabbage",
  "description": ""
}
```

#### Send funds on Head

Send funds from your Proxy Address to another participant identified by their L1 Address.

Example Payload:
``` json
{
  "headSubmitTx_name": "test",
  "headSubmitTx_toAddr": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp",
  "amount": 1000000
}
```

#### Query Head Balance

Get the amount of Lovelace available on the head at your (Proxy) Address.
``` json
{ "tag" : "GetHeadBalance", "head": "test", "addr" : "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp" }
```

Example Response:
``` json
10000000
```

#### Close

Close the head.

Example Response:
``` json
{
  "headStatus_name": "test",
  "headStatus_running": true,
  "headStatus_status": "Status_Closed"
}
```


#### Withdraw

Withdraw funds from your Proxy Address to your main address.

Example Payload:

``` json
{
  "withdraw_address": "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp",
  "withdraw_amount": "5000000"
}
```


#### Query L1 Balance

Get the amount of Lovelace available on the head at your (Proxy) Address on L1.

``` json
{ "tag" : "GetL1Balance", "addr" : "addr_test1vpperccj7n8faw74ketx68k2mehg23d864hvg209cgupp5c4r47hp" }
```

Example Response:
``` json
10000000
```


## 🦾 Proxy Addresses

Hydra Pay simplifies the creation and managment of Heads to facilitate easy creation of Hydra Head based features for Light Wallet and DApp developers. One way we aid feature creation is through our Proxy Address structure.

Instead of participating directly in a Head, any participant will actually be mapped to a "Proxy Address". This is a regular cardano address that is created to hold funds and fuel for said participant in Hydra Pay Head.

We have a couple important reasons for using this Proxy Address mapping:

- More security: Participants no longer need to provide their private keys to the hydra-node acting on their behalf. This means that developers won't need to ask potential participants for their seed phrase just to be able to join a Head. This keeps in line with the security principle of Hydra: The only funds you can lose are those you commit.

- More convenient fund management: Instead of having to promptly commit funds only when a Head is starting; users can add funds to their proxy address. This gives developers the freedom to orchestrate the timing of head creation and closing however they like, and users the confidence to participate meaningfully with ever Init/Fanout cycle without having to actively micro-manage their funds and Head commitments.

The proxy address scheme will be updated/deprecated based on future changes to Hydra Pay like the upcoming [Incremental De/Commit](https://github.com/orgs/input-output-hk/projects/21) on the Hydra roadmap. 

## 🤔 FAQ

### What is Hydra Pay?

A library and service for Light Wallet (and eventually DApp developers) to integrate payment channels allowing them fast finality and low fees through the use of Hydra Heads.

### I am a developer and have feature that I want to implement that uses Hydra. Should I use Hydra Heads directly or Hydra Pay?

The API of Hydra Pay is designed around Hydra Head creation, interaction, and participation. It manages the Heads and the underlying Hydra Nodes and provides a HTTP/Websocket interface, if your feature doesn't need Plutus Scripts to run on the Head, Hydra Pay is likely a good fit, and if it isn't, feel free to reach out about what you need and how we can get it there! 

### What will I need to do to run/deploy a Hydra Pay service?

Once Hydra Pay is available, you will simply run either the provided docker container for the service, or use the provided nix expression to include Hydra Pay service with whatever infrastructure you are using. 

### Can I run plutus scripts on a Head using Hydra Pay?

Not initially, we will be guaging the need/interest in this feature as Hydra Pay evolves and we get feedback from developers.
