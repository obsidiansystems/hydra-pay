-- |

module HydraPay.Network where

{-
When we actually run a Hydra Head network, we need to have some things in place
to be able to see what is up

Of course we need a type to describe this network, and we need to be able to monitor one
of the nodes or actually all of the nodes to know what is going on, and that means pubsub

This means that standing up a network also constitutes monitoring the nodes for information
and updating the status, which means we need a monitoring thread...

The network is using the Proxy addresses internally but maps to actual outside addresses
-}
