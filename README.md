# <p align="center">Hydra for Payments (Hydra Pay)</p>

## :dango: Introduction

The **Hydra Pay** project will provide an open-source library and framework for cardano light wallet developers to make use of [Hydra (Head)](https://hydra.family/head-protocol/).

This will cover:

* Spinning up a network (This includes starting and managing Hydra Nodes)
* An API to make the common operations simple to execute:
  * Initialization
  * Committing
  * Transactions
  * Querying
  * Closing
  * Disputing
  * Fanout
* A real-world example application

We assume at this time you are knowledgeable about Hydra and the basic Hydra Head lifecycle. If not take some time to familiarize yourself with [here](https://hydra.family/head-protocol/core-concepts/). 

## 👷🏾‍♂️ Running Hydra Pay

Hydra Pay can be ran via Docker or Nix.
By default it wil run in live documentation mode on port 8000 which gives you a realtime way to test and learn about the Hydra Pay API and Best Practices.

### With Docker

Pull the docker image, run it and visit [localhost:8000](http://localhost:8000) to see the Live Documentation:

```bash
docker pull obsidiansys/hydra-pay:latest

docker run -p 127.0.0.1:8000:8000/tcp --name hydra-pay obsidiansys/hydra-pay:latest
```

Hydra Pay will generate an *API key* which can be read out with:

```bash
docker exec -it hydra-pay cat /hydrapay/config/backend/api-key
```

To use a custom configuration, mount a copy of the `config` directory from this repository to `/hydrapay/config`:

```bash
docker run -p 127.0.0.1:8000:8000/tcp --volume /path/to/hydra-pay/config:/hydrapay/config obsidiansys/hydra-pay:latest
```

### With Nix

1. [Install Nix](https://nixos.org/nix/). If you already have Nix installed, make sure you have version 2.0 or higher. To check your current version, run nix-env --version.

2. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```nix
        nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
        nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    2. If you are using another operating system or Linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```nix
        binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
        binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
        binary-caches-parallel-connections = 40
        ```

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
(cd test-app && ./hydra-pay)
```

Visit the live documentation and confirm your key and requests presented work as expected.

### Running on a Managed Devnet

By default Hydra Pay will run a managed devnet, this devnet allows us to verify the API behavior of hydra pay in an efficient way, and is not for active development. 

When Hydra Pay launches, and you haven't configured it to utilize a cardano node that you are running, it will spawn a node, configure it as a local devnet, and seed 10 addresses with 10k ADA.

These are made readily available to you via the API and the within the Live Docs to get your footing and start using and interacting with Hydra Pay.

### Running on Preview

You can configure Hydra Pay to use an existing node instead of creating a devnet node, and this allows you to interact with other cardano environments like Preview.

To run on preview there is much more involved as Hydra Pay isn't managing address creation, key storage, and funding. To make things easier on developers the Live Documentation will detect it is connected to an external cardano node and adjust its instructions accordingly.

#### Assumptions

We assume you have a cardano node already running on Preview, and that it is running on the same machine you will be running Hydra Pay. As like other tools in the cardano ecosystem, the Hydra Nodes require access to a cardano node socket. Along with the socket you will need the shelley genesis file that was used to bootstrap your node, this should be in the directory your node is running in, and you can also find up to date versions of these genesis files [here](https://book.world.dev.cardano.org/environments.html).

We also assume you have addresses to use in your testing and those addresses have funds, you can use the [Testnets Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet) if you need funds (just remember to send them back when you are done!).

#### Hydra Scripts on Preview

Hydra in part works by using a Plutus smart contract on L1 and that contract must be deployed on the network you are using, here is the txid for the Hydra Scripts on Preview:
`4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d`

#### Hydra Protocol Parameters

Just like a Cardano Node, Hydra Nodes have a set of protocol parameters to control various aspects of the Node and the Network. However the Hydra Node doesn't need to (and usually shouldn't) use the same protocol parameters of the Cardano Node. If you don't know the exact protocol parameters you want, we recommend using the parameters found in hydra-protocol-parameters.json.example in this repo, they give a reasonable default to compliment the benefits of Hydra.

#### Run configuration for Preview

So with all that in mind, and the assumptions met, here is how you run Hydra Pay on Preview:

```bash
hydra-pay \
    --testnet-magic 2 \
    --node-socket cardano-node.socket \
    --ledger-genesis cardano-node-byron-genesis.json \
    --hydra-scripts-tx-id 4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d \
    --hydra-ledger-protocol-parameters hydra-protocol-parameters.json \
    --hydra-ledger-genesis cardano-node-genesis-shelley.json
```

The testnet magic for `Preview` is `2`. The node socket is commonly found at `/run/cardano-node/node.socket` but this will depend on exactly how your cardano node is configured.

As you can see you must provide some of the configuration files used for your node like the byron genesis file and the shelley genesis file.

Once again we recommend using the Hydra parameters found in hydra-protocol-parameters.json.example unless you know exactly what you are doing.

### API Key

Your API Key should be provided in `config/backend/api-key` if no key is provided we generate a random one the aformentioned file. 

DO NOT USE THIS KEY IN PRODUCTION.

## 👩🏿‍💻 Hacking on Hydra Pay
Hydra Pay is written in Haskell using [Obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk) so to contribute you must have Obelisk installed.

Once you have Obelisk installed hacking on Hydra Pay is as easy as running `ob run` in the root directory.

The live demo can then be viewed in Chrome at `http://localhost:8000/`.

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
- [x] Live Documentation: Devnet, Preview
- [x] Full lifecycle guide
- [x] Deployment Guide
- [x] Docker Deployment Guide
- [x] Best Practices
- [x] TTL on add funds and fuel pre-built transactions
- [x] Choice of network: Devnet, Preview
- [x] Default Hydra Node configuration

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

Example Subscription:
``` json
{
    "contents": "test",
    "tag": "SubscribeTo"
}
```

Example Response:
``` json
{
    "tag": "SubscriptionStarted",
    "contents": {
        "headStatus_name": "test",
        "headStatus_balances": [],
        "headStatus_running": true,
        "headStatus_status": "Status_Pending"
    }
}
```

### Authentication

When you launch or deploy a Hydra Pay instance you will need to provide an API Key to authenticate against, this is a secret that should be only known to your DApp/LightWallet and your Hydra Pay instance. 

Upon opening a websocket connection to your HydraPay instance, you should immediately Authenticate by sending a Tagged `Authenticate` request (see below).

### Errors and Error Messages

Hydra Pay manages state and does its best to communicate when you issue API requests that are not at appropriate times, or would have no meaning in the current state. When this happens you are likely to get responses like the following:

``` json
{ "tag": "ApiError", "contents": ""Head is closing, please wait for the contestation period to be over and the fanout to complete." }
```
This error message was generated when trying to remove/terminate a head while the nodes are fanning out funds.

### Request and Response API

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

Expected Response:
``` json
{ "tag": "AuthResult", "contents": true }
```

#### Head Creation

To create a Head you will give it a friendly name and list the addresses that will become the participants.
Creating the head starts the Hydra network.

Example payload:
``` json
{
    "tag": "CreateHead",
    "contents": {
        "headCreate_name": "test",
        "headCreate_participants": ["addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease"]
    }
}
```

Expected Response:
``` json
{
    "tag": "OperationSuccess"
}
```

#### Subscribing to State Changes

A lot of the time the logic for your DApp or Light Wallet will include waiting for certain state changes to take place. To be convenient for developers and to avoid unnecessary complications and resource usage of polling, you can subscribe to state changes of certain heads. If you create a head you should subscribe to it as well.

Example payload:

``` json
{
    "tag": "SubscribeTo",
    "contents": "test"
}
```

Example response:
``` json
{
    "tag": "SubscriptionStarted",
    "contents": {
        "headStatus_name": "test",
        "headStatus_balances": [],
        "headStatus_running": true,
        "headStatus_status": "Status_Pending"
    }
}
```

#### Head Init

This creates an L1 transaction (using fuel given to a Proxy Address) that places the initial head state on chain.

Example Payload:
``` json
{
    "tag": "InitHead",
    "contents": {
        "headInit_name": "test",
        "headInit_contestation": 3
    }
}
```

Expected Response:
``` json
{
    "tag": "OperationSuccess"
}
```

### 🦾 Proxy Addresses

Hydra Pay simplifies the creation and management of Heads to facilitate easy creation of Hydra Head based features for Light Wallet and DApp developers. One way we aid feature creation is through our Proxy Address structure.

Instead of participating directly in a Head, any participant will actually be mapped to a "Proxy Address". This is a regular cardano address that is created to hold funds and fuel for said participant in Hydra Pay Head.

We have a couple important reasons for using this Proxy Address mapping:

- More security: Participants no longer need to provide their private keys to the Hydra Node acting on their behalf. This means that developers won't need to ask potential participants for their seed phrase just to be able to join a Head. This keeps in line with the security principle of Hydra: The only funds you can lose are those you commit.

- More convenient fund management: Instead of having to promptly commit funds only when a Head is starting; users can add funds to their proxy address. This gives developers the freedom to orchestrate the timing of head creation and closing however they like, and users the confidence to participate meaningfully with ever Init/Fanout cycle without having to actively micro-manage their funds and Head commitments.

The proxy address scheme will be updated/deprecated based on future changes to Hydra Pay like the upcoming [Commit from External Wallet](https://github.com/orgs/input-output-hk/projects/21/views/7) on the Hydra roadmap. 

#### Head Commit

Commit the funds at your Proxy Address to the named Head.

Example Payload:
``` json
{
    "contents": {
        "headCommit_name": "test",
        "headCommit_participant": "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
        "headCommit_amount": 100000000
    },
    "tag": "CommitHead"
}
```

Expected Response:
``` json
{
    "tag": "OperationSuccess"
}
```


#### Proxy Address Information

Sometimes you want to inspect or be aware of the Proxy Address' funds and fuel, maybe to automate filling those up in your business logic. The Proxy Info includes the address of the proxy address, and the balance and fuel of that proxy address.

Example payload:
``` json
{
    "tag": "GetProxyInfo",
    "contents": "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease"
}
```

Example response:
``` json
{
    "tag": "ProxyAddressInfo",
    "contents": {
        "proxyInfo_address": "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
        "proxyInfo_proxyAddress": "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
        "proxyInfo_fuel": 50000000,
        "proxyInfo_balance": 50000000
    }
}
```

#### Funding Proxy Addresses | Funds & Fuel
To participate in a Hydra Pay managed Head you need to transfer funds from your address to your Proxy Address. You must transfer both regular Ada and a specially tagged Fuel transaction yourself.

For your convenience Hydra Pay provides endpoints which return draft transactions for both types of funds. However, you yourself must sign and submit these transactions with the signing key for your address.

To get a CIP-30 compatible CBOR transaction that will fund your Proxy Address:

Example Payload:
``` json
{
    "contents": [
        "Funds",
        "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
        100000000
    ],
    "tag": "GetAddTx"
}
```

Example Response:
``` json
{
    "tag": "FundsTx",
    "contents": {
        "cborHex": "Ledger Cddl Format",
        "type": "Unwitnessed Tx BabbageEra",
        "description": "84a3008182582005bbe2c33e4bd787a8778b63bfbf007fae7b47b8153e75586df0ab59936d6c3c000182a300581d60e04a63ce5112f1b75c66a13375daf937e5ed9177caa8e9536392119f011a002dc6c00282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3a200581d60d31a9209c0da931b7e72f45bc612dc85fae49249619f5f80639d2f50011b0000000253db8f8b021a00028db5a0f5f6"
    }
}
```

#### Adding Fuel to a Proxy Addresses | Funds & Fuel

Get a CIP-30 compatible CBOR transaction that will create a Fuel UTXO at your Proxy Address.

Example Payload:
``` json
{
    "contents": [
        "Fuel",
        "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
        100000000
    ],
    "tag": "GetAddTx"
}
```

Example response:
``` json
{
    "tag": "FundsTx",
    "contents": {
        "cborHex": "Ledger Cddl Format",
        "type": "Unwitnessed Tx BabbageEra",
        "description": "84a3008182582005bbe2c33e4bd787a8778b63bfbf007fae7b47b8153e75586df0ab59936d6c3c000182a300581d60e04a63ce5112f1b75c66a13375daf937e5ed9177caa8e9536392119f011a002dc6c00282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3a200581d60d31a9209c0da931b7e72f45bc612dc85fae49249619f5f80639d2f50011b0000000253db8f8b021a00028db5a0f5f6"
    }
}
```

#### Transaction in a Hydra Head

Once a Head is Open (all participants have commited UTxOs), you are free to move funds around within the head by creating transactions. 

Example Payload:
``` json
{
    "contents": [
        "",
        {
            "headSubmitTx_name": "test",
            "headSubmitTx_toAddr": "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
            "amount": 3000000
        }
    ],
    "tag": "SubmitHeadTx"
}
```

``` json
{
    "tag": "TxConfirmed",
    "contents": 1.0e-3
}
```
The contents are an amount in seconds that it took to complete the transaction (full roundtrip).

#### Closing a Head

After you have done all the transactions you needed to in the Hydra Head, you can close it and when Hydra Pay detects the Fanout period has occurred, it will Fanout your Head for you Automatically.

Example Payload:
``` json
{
    "contents": "test",
    "tag": "CloseHead"
}
```

Example Response:
``` json
{
    "tag": "OperationSuccess"
}
```


#### Withdraw from a Proxy Address

Withdraw funds from your Proxy Address to your main address. Takes the
Proxy Address and a boolean determining whether to withdraw fuel as well.

Example Payload:
``` json
{
  "tag": "Withdraw",
  "contents": [
      "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease",
      true
  ]
}
```

Example Response:
``` json
{
    "tag": "WithdrawSubmitted",
    "contents": "e174c1033009de66ccc577743ae4542c9d5e6c8220acfcd55c1c4cf330b7ca04"
}
```
The contents are a txid you can track on a block explorer like cardanoscan.

## 🤔 FAQ

### What is Hydra Pay?

A library and service for Light Wallet (and eventually DApp developers) to integrate payment channels allowing them fast finality and low fees through the use of Hydra Heads.

### I am a developer and have feature that I want to implement that uses Hydra. Should I use Hydra Heads directly or Hydra Pay?

The API of Hydra Pay is designed around Hydra Head creation, interaction, and participation. It manages the Heads and the underlying Hydra Nodes and provides a HTTP/Websocket interface, if your feature doesn't need Plutus Scripts to run on the Head, Hydra Pay is likely a good fit, and if it isn't, feel free to reach out about what you need and how we can get it there! 

### What will I need to do to run/deploy a Hydra Pay service?

Once Hydra Pay is available, you will simply run either the provided docker container for the service, or use the provided nix expression to include Hydra Pay service with whatever infrastructure you are using. 

### Can I run plutus scripts on a Head using Hydra Pay?

Not initially, we will be guaging the need/interest in this feature as Hydra Pay evolves and we get feedback from developers.
