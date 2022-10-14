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

## Hydra Heads Demo
This repository currently contains an extended, graphical re-implementation of the original [Hydra Heads demo](https://github.com/input-output-hk/hydra-poc/tree/master/demo) done with [Obelisk](https://github.com/obsidiansystems/obelisk).
It allows starting and closing a head with an arbitrary number of nodes, each with some initial amount of Ada to perform transactions within the head.

### Running
To run the demo install [Obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk) and run `ob run` in the root of the repository.
The demo can then be viewed in Chrome at `http://localhost:8000/`.

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
- [ ] Coin selection for participants
- [x] How To Run
- [ ] Integration of hydra libraries
- [ ] Choice of network: Devnet, Preview, Preprod, Mainnet
- [ ] API Documentation
- [ ] Initial client interaction library
- [ ] How to Contribute
