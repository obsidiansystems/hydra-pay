# <p align="center">Hydra for Payments (Hydra Pay)</p>
* [:dango: Introduction](#dango-introduction)
* [🎉 Features](#-features)
  * [Foundational Features](#foundational-features)
  * [Next Steps](#next-steps)
    + [Providing Performance Metrics and Benchmarking](#providing-performance-metrics-and-benchmarking)
    + [Integration of External/De-commit](#integration-of-externalde-commit)
* [👷🏾‍♂️ Running a Hydra Pay Instance](#running-a-hydra-pay-instance)
  * [Instances](#instances)
    + [Standardization](#standardization)
  * [Running in Docker](#running-in-docker)
  * [Running with Nix](#running-with-nix)
* [🗝 API](#-api)
  * [Create Payment Channel](#create-payment-channel)
  * [Locking Funds in a Payment Channel](#locking-funds-in-a-payment-channel)
  * [Sending ADA in a Payment Channel](#sending-ada-in-a-channel)
  * [Closing a Payment Channel](#closing-a-payment-channel)
  * [Removing a Payment Channel](#removing-a-payment-channel)
* [🦾 Proxy Addresses](#-proxy-addresses)
* [Haskell Library](#haskell-library)
* [👩🏿‍💻 Hacking on Hydra Pay](#-hacking-on-hydra-pay)
* [🤔 FAQ](#-faq)
  * [What is Hydra Pay?](#what-is-hydra-pay)
  * [I am a developer and have feature that I want to implement that uses Hydra. Should I use Hydra Heads directly or Hydra Pay?](#i-am-a-developer-and-have-feature-that-i-want-to-implement-that-uses-hydra-should-i-use-hydra-heads-directly-or-hydra-pay)
  * [Can I run plutus scripts on a Head using Hydra Pay?](#can-i-run-plutus-scripts-on-a-head-using-hydra-pay)
* [Contributing](#contributing)

## :dango: Introduction

**Hydra Pay** provides an open source application and suite of libraries for builders on cardano to make use of [Hydra (Head)](https://hydra.family/head-protocol/). The `hydra-pay` executable allows you to create instances and manage payment channels on the main cardano networks.

The WebSocket API provies builders with the ability to create, manage and monitor payment channels so they can focus on their usecase.

The optional `hydra-pay` and `hydra-pay-core` libraries allow builders using Haskell to more directly integrate facets of **Hydra Pay** into their applications.

**Hydra Pay** does all the work to manage and monitor Heads, it automatically moves them through their lifecycle and persists their configurations so they can be automatically restarted.

Simply ask for a payment channel, send funds in that channel and close it when you are done. **Hydra Pay** will do all the work to manage communication and synchronization with the Hydra Nodes. It even generates bespoke internal wallets with each node, and will ensure funds in the internal wallets are returned to the payment channel participants on Head finalization.

To learn more about the underlying L2 technology of Hydra you can look [here](https://hydra.family/head-protocol/core-concepts/).

## 🎉 Features

###  Foundational Features
- [x] Payment channel first API with easy configuration
- [x] API over WebSocket support for easy integration
- [x] Automatic Head lifecycle management
- [x] Cardano node creation and management
- [x] Hydra Node internal wallet creation and management
- [x] Automatic restarting of Hydra Nodes 
- [x] Robust API documentation
- [x] Deployment Guide (Nix & Docker)

###  Next Steps

#### Providing Performance Metrics and Benchmarking

Understanding the upper bound level of managed payment channels in terms of how much each Head wants will give builders sufficient information to realize their usecases. Providing a wide array of different examples like number of instances vs number of payment channels is something we would like to see happen.

#### Integration of External/De-commit

This is a prominent upcoming feature that will require some changes to how Hydra Pay works under the hood. We would want to provide high level of user experience just as we have done for commit from external wallet.

## Running a Hydra Pay Instance

A Hydra Pay instance can be ran via Docker or Nix. On Nix we provide the `hydra-pay` executable directly that can be used to run an instance or act as a client to an instance. The docker container takes your configuration and runs automatically.

### Instances

Hydra Pay is instanced! Each running Hydra Pay instance manages the Payment Channels and Hydra Heads associated with it. This includes: Running a cardano node, Monitoring the chain, Creating transactions, and ensuring that Heads move through their lifecycle successfully and promptly. To do this each Hydra Pay instance isn't simply running the Hydra Head networks, but tracking internal wallets, payment channel statuses, and relating that info back to the Hydra Nodes to automatically resolve issues that arise. It is also persisting the configuration of nodes so upon a restart of the instance all Hydra Nodes stay in working order.

This means that payment channels exist tied to an instance. The underlying Head is simply a network of Hydra Nodes, but just the information needed to run the Hydra Nodes isn't currently enough.

#### Standardization

Currently nothing is stopping Hydra Pay Instances from cross communicating, but as outlined above there is more information on top of Hydra that Hydra Pay adds. This means that it isn't enough to make the Hydra Nodes aware of each other across instances. A standard must exist to facilitiate the communication of payment channel names, internal wallet addresses etc. This would allow not only communication across Hydra Pay Instances, but also allow other applications to communicate with Hydra Pay instances.

### Running in Docker

Pull the docker image, run it and use http://127.0.0.1:8010/ to talk to your Docker instance.

```bash
docker pull obsidiansys/hydra-pay:latest

docker run -p 127.0.0.1:8010:8010/tcp --name hydra-pay obsidiansys/hydra-pay:latest
```

To use a custom configuration, mount a copy of the `config` directory from this repository to `/hydrapay/config`:

```bash
docker run -p 127.0.0.1:8010:8010/tcp --volume /path/to/hydra-pay/config:/hydrapay/config obsidiansys/hydra-pay:latest
```

### Running with Nix

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
nix-build -A hydra-pay --no-out-link
```

Copy the result to a new directory, add configuration, and run!

```bash
mkdir test-instance
ln -s $(nix-build -A hydra-pay --no-out-link)/* test-instance
cp -r config test-instance
(cd test-instance && ./hydra-pay instance preview)
```

## 🗝 API

The Hydra Pay API is a websocket based API that gives you all that you need to create, manage, and monitor Payment Channels. The endpoint is `127.0.0.1:8010` by default


### Create Payment Channel

To create a payment channel you will provide the name, and addresses of the participants. For example:

``` json
{
  "tag" : "create",
  "name" : "<name-of-your-payment-channel>"
  "addresses" : 
   [ "<address>"
   , "<address>"
   ]
}
```

or through the `hydra-pay` built in client, you can also do:

`hydra-pay channel new <name> <list-of-addresses>`


### Getting The Status of A Payment Channel

To get the status you will provide the name.

``` json
{
  "tag" : "status",
  "name" : "<name-of-your-payment-channel>",
}
```

or through the `hydra-pay` built in client:

`hydra-pay channel status <name>`

### Locking Funds in a Payment Channel

Locking funds in a payment channel can be a multi-step process, Hydra Pay helps guide you through executing the proper steps.

To lock, start by sending a request like so:
``` json
{
  "tag" : "lock",
  "name" : "<name-of-your-payment-channel>",
  "address" : "<address-of-participant>",
  "amount" : <amount-in-lovelace>
}
```

or through the `hydra-pay` built in client:

`hydra-pay channel lock <name> <amount> <address>`

Depending on the state of your payment channel you will receieve a response and information to either:
- Fund the internal wallet of the Hydra Node.
- Create a UTxO with the right amount of ADA.
- Recieve a Commit from External Wallet transaction, to sign and submit.

As Hydra Pay knows the state, you just run this command multiple times, and it will remind you of where you are, and what you need to do to get to the next step.

### Sending ADA in a Channel

Once both parties have locked in a Payment Channel the parties can send within a payment channel.

To send ADA you will make this request, and receieve a transaction for the Hydra Head to sign:

``` json
{
  "tag" : "send",
  "name" : "<name-of-your-payment-channel>",
  "address" : "<address-of-sender>",
  "amount" : <amount-in-lovelace>
}
```

with the `hydra-pay` client:

`hydra-pay channel send <name> <amount> <address>`

sign this transaction and give it back to Hydra Pay, so your ADA can be transferred within the payment channel.

``` json
{
  "tag" : "submit",
  "name" : "<name-of-your-payment-channel>"
  "tx" : "<signed-transaction-body>"
}
```

with the `hydra-pay` client this becomes:

`hydra-pay channel submit <name> <signed-tx-file>`

### Closing a Payment Channel

Eventually you will want to close a payment channel, and ensure funds are given back to the participants. This is accomplished from this single close request.

``` json
{
  "tag" : "close",
  "name" : "<name-of-your-payment-channel",
}
```

with the `hydra-pay` client this is:

`hydra-pay channel close <name>`

The channel will be closed, and Hydra Pay will monitor the underlying Head for closing, contestation, fanout, and finalization (the stage at which the participants funds have exited the Head) and once finalization is met, Hydra Pay will automatically have the internal wallets pay back their balances to the respective participants.

### Removing a Payment Channel

If you want to remove a payment channel completely you can use the remove request.

``` json
{
  "tag" : "remove",
  "name" : "<name-of-your-payment-channel>"
}
```

with the `hydra-pay` client this is:

`hydra-pay channel remove <name>`

## 🦾 Proxy Addresses

Some builders may prefer a custodial design that maximally streamlines their UI/UX.

For these cases, an optional Proxy Address design is available.

Instead of participating directly in a Head, any participant will actually be mapped to a "Proxy Address". This is a regular cardano address that is created to hold funds for said participant in the Hydra Pay Head.

The support for the proxy address scheme will be maintained alongside [Commit from External Wallet](https://github.com/orgs/input-output-hk/projects/21/views/7) update that allows user wallets to participate directly with Heads.

to enable proxies run your `hydra-pay` instance with the `--use-proxies` flag:

`hydra-pay instance preview --use-proxies`

If you as a builder want to handle fees and batch transactions, add bank keys to `config/backend/faucet` and your hydra-pay instance will utilize that wallet for funding internal wallets and batching transactions within your instance.

## Haskell Library

First party haskell libraries `hydra-pay-core` and `hydra-pay` provide direct access to powerful features like:
* A simple, powerful, and customizable logger with automatic rotation, file size limits, and file management.
* A set of workers to carry out tasks in parallel, maximizing throughput. 
* Automated transaction batching to have the least downtime when waiting for L1 transactions to be observed on-chain.
* An automated process for moving heads through the lifecycle based on indexer and on-chain information.
* Automated refunding of L1 addresses when failures are detected, ensuring your funds are always sent back to you.
* Automatic restarting of Hydra Nodes so you never lose your state (or funds).
* An internal indexer that tracks head state in high detail.
* Automated node management with integrated logging and error tracking, providing a convenient interface for interaction and information.
* A typesafe GADT based interface into cardano-cli that allows easy:
  * Tip and Protocol Parameter queries
  * Balance and UTxO queries
  * Key/Address generation
  * Transaction submission
  * Transaction completion detection and waiting
  This part of the library plays nice with any cardano-nodes you run, by intelligently waiting for the cardano node's
  socket before performing any actions.
* Handy Orphans for communicating cardano and hydra node information over json.
* Drop in database backing for proxies, hydra heads, and payment channels, with customizable persistence and simple 
  typeclasses .
* Direct payment channel API
  This API gives you first party payment channels, and if this fits your usecase, greatly simplifies your experience interacting with Hydra Heads.
* Port allocation utilities, used for the node and the various payment channels, but can simplify logic within a dapp or light wallet. Simply ask for ports, and they are automatically returned.

These libraries help you kickstart DApp or LightWallet development by handling logging, state management, persistence, node and head interactions, and even port allocation. `hydra-pay-core` provides functionality that can be used to build frontend clients, while `hydra-pay` is focused on backend support and database interaction.

## 👩🏿‍💻 Hacking on Hydra Pay

Working on hydra-pay means changing either the `hydra-pay` or `hydra-pay-core` libraries. To get into a nix shell with everything you need run `nix-shell default.nix -A shells.ghc`, from there you can run `cabal repl <hydra-pay/hydra-pay-core>` to hack away.

## 🤔 FAQ

### What is Hydra Pay?

A library and service for Light Wallet and DApp developers to integrate payment channels allowing them fast finality and low fees through the use of Hydra Heads.

### I am a developer and have feature that I want to implement that uses Hydra. Should I use Hydra Heads directly or Hydra Pay?

The API of Hydra Pay is designed around Hydra Head creation, interaction, and participation. It manages the Heads and the underlying Hydra Nodes and provides a HTTP/Websocket interface, if your feature doesn't need Plutus Scripts to run on the Head, Hydra Pay is likely a good fit, and if it isn't, feel free to reach out about what you need and how we can get it there! 

### Can I run plutus scripts on a Head using Hydra Pay?

Not initially, we will be guaging the need/interest in this feature as Hydra Pay evolves and we get feedback from developers.

## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.
