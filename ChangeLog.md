# Revision History for Hydra Pay

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released.

### Unreleased

* Ensure absolute paths for cardano-cli & hydra-tools executables

### Version v0.2.0

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

