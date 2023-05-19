{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HydraPay.Orphans where

import Data.Aeson.GADT.TH
import qualified Data.HexString as Hex
import qualified Cardano.Api as Api
import qualified Cardano.Address as Address
import qualified Data.Aeson.Types as Aeson
import GHC.Generics
import Data.Constraint.Extras.TH
import Data.Proxy
import Data.Type.Equality

deriving newtype instance Aeson.FromJSON Address.NetworkTag
deriving newtype instance Aeson.FromJSONKey Address.NetworkTag
deriving newtype instance Aeson.ToJSONKey Address.NetworkTag
deriving newtype instance Ord Address.NetworkTag

instance Aeson.ToJSON Api.AddressAny where
  toJSON = Aeson.toJSON . Api.serialiseAddress

instance Aeson.FromJSON Api.AddressAny where
  parseJSON t = do
    txt <- Aeson.parseJSON t
    case Api.deserialiseAddress Api.AsAddressAny txt of
      Nothing -> fail $ "Could not deserialise address: " <> show txt
      Just addr -> return addr

deriving stock instance Generic Api.AssetId
instance Aeson.ToJSON Api.AssetId
instance Aeson.ToJSONKey Api.AssetId
instance Aeson.FromJSON Api.AssetId
instance Aeson.FromJSONKey Api.AssetId

-- | General conversion for types that convert to CBOR making them also convert to JSON
-- these are just functions and not an instance to avoid overlapping instances
-- just use them as the body of toJSON and fromJSON if your type has a SerializeAsCBOR instance
toJSONCBOR :: (Api.SerialiseAsCBOR (f era)) => f era -> Aeson.Value
toJSONCBOR = Aeson.toJSON . Hex.fromBytes . Api.serialiseToCBOR

parseJSONCBOR :: (Api.SerialiseAsCBOR (f era)) => Aeson.Value -> Aeson.Parser (f era)
parseJSONCBOR v = do
  bs <- Hex.toBytes <$> Aeson.parseJSON v
  case Api.deserialiseFromCBOR (Api.proxyToAsType Proxy) $ bs of
    Left decoderError -> Aeson.parseFail $ show decoderError
    Right r -> pure r

instance Api.IsCardanoEra era => Aeson.ToJSON (Api.TxBody era) where
  toJSON = toJSONCBOR

instance (Api.HasTypeProxy era, Api.IsCardanoEra era) => Aeson.FromJSON (Api.TxBody era) where
  parseJSON = parseJSONCBOR

deriving stock instance Generic (Api.BalancedTxBody era)
instance (Aeson.ToJSON era, Api.IsCardanoEra era) => Aeson.ToJSON (Api.BalancedTxBody era)
instance (Api.IsShelleyBasedEra era) => Aeson.FromJSON (Api.BalancedTxBody era)

instance Api.IsCardanoEra era => Aeson.ToJSON (Api.Tx era) where
  toJSON = toJSONCBOR
instance Api.IsCardanoEra era => Aeson.FromJSON (Api.Tx era) where
  parseJSON = parseJSONCBOR

deriving stock instance Generic (Api.AlonzoEra)
instance Aeson.ToJSON Api.AlonzoEra

deriveArgDict ''Api.CardanoEra
deriveFromJSONGADT ''Api.CardanoEra

deriveArgDict ''Api.ShelleyBasedEra
deriveJSONGADT ''Api.ShelleyBasedEra

instance TestEquality Api.ShelleyBasedEra where
    testEquality Api.ShelleyBasedEraShelley Api.ShelleyBasedEraShelley = Just Refl
    testEquality Api.ShelleyBasedEraAllegra Api.ShelleyBasedEraAllegra = Just Refl
    testEquality Api.ShelleyBasedEraMary    Api.ShelleyBasedEraMary    = Just Refl
    testEquality Api.ShelleyBasedEraAlonzo  Api.ShelleyBasedEraAlonzo  = Just Refl
    testEquality Api.ShelleyBasedEraBabbage Api.ShelleyBasedEraBabbage = Just Refl
    testEquality _          _                                          = Nothing
