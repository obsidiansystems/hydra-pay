# Revision History for Hydra Pay

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released.

## Unreleased

* Moved nix executables to $out/bin to meet nix best practices for automated build input discovery
* Run cardano-submit-api when running in devnet mode for easier wallet integration and testing
* Generate mnemonics and cardano-address private keys along with signing and verification keys for devnet

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

