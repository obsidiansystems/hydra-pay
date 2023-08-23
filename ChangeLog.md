# Revision History for Hydra Pay

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released.

## Version v1.1.1

* Fix outdated commands in README
* Prevent same named channels to avoid ambiguity
* Patch serialization of amounts using Int32
* Add remove command
* Let users know if they have already committed

## Version v1.1.0

* Rework nix-pinned cardano dependencies
* *Breaking Change* Update aeson instances
* *Breaking Change* Update to cardano node 8.1.2
* *Breaking Change* Bump to hydra master 1e13b60a

## Version v1.0.0

* Extend the instance executable to act as client
* *Breaking Change* Reduce dependencies
* *Breaking Change* Unify features of core libraries and application
* Introduce instances
* Make proxies optional
* Ensure all processes are correctly bracketed on close
* Payment channel first API
* Integrate external commit
* *Breaking Change* Bump to hydra 0.11.0

## Version v0.3.0

* Ensure Hydra Node interaction threads currently executing blocking calls when asked to close; close immediately when execution is yielded back to them
* Proxies now pay directly back to their L1 counterpart when they receive the UTxO from a finalized head
* Automatic transaction batching, when HydraPay is instructed to pay to or out of proxy addresses; instead of submitting separate transactions it will now batch to the best of its ability to limit transaction wait times, and avoid contention 
* Automated state progression: When a head is started, we use information from the proxy addresses, and the hydra nodes to automatically send Client inputs to the head, meaning the lifecycle is now self-managing and self-repairing
* Internal indexer: We now do our best to scrutinize and persist events observed by each node on each head
* Add a worker to handle refunds when nodes fail to start for any reason
* Introduce Task Workers: A way for various internal HydraPay actions to happen in parallel with reliable execution and smart retries 
* More reliable and less resource intensive Hydra Node/Head status tracking: The communication interface uses less threads and a single and output channel
* Automatically restart nodes at runtime
* Rework proxy system to have one proxy per node to be in line with Hydra best practices
* Ensure absolute paths for cardano-cli & hydra-tools executables
* Delegate SIGINT to cardano-node process
* Reduce the number of reconnections to Hydra Node websocket api
* Bump to hydra 0.10.0

## Version v0.2.0

* *Breaking change*: Split haskell library into two parts: hydra-pay-core and hydra-pay. hydra-pay includes database components that cannot be included in frontend clients, while hydra-pay-core includes common components that are compatible with both frontend and backend clients.
* Add fanout to L1 workflow
* Add hydra balance retrieval
* Various stability improvements

## Version v0.1.1

* First party haskell library with extensible logging, payment channel api, upgraded persistence, upgraded node and head management, wallet and key generation facilities

##  Version v0.1.0

* Automated spin-up and shut down of Hydra Node networks
* Proxy addresses to streamline fund managment and enforce security invariants
* Static nix binaries for deployment
* Docker image for alternative deployment
* Request/Response Websocket API for Head management
* Live Documentation for education and testing of all Hydra Pay API requests
* Mutliplexed Websocket realtime status information
* Managed devnet creation and funding, with automatic transaction submitting
* Capability for running on Preview (tested)
* CIP-30 compatible Transaction generation for use in Light Wallets
* Proxy address key management
* Automatic Proxy Address generation and mapping
* Script to automate Docker Deployment
* Head transaction speed measurements
* Deep error propagation: From internal nodes and processes out to API messages

