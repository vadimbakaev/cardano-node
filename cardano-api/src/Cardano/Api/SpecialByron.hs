{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Special Byron values that we can submit to a node to propose an update proposal
-- or to vote on an update proposal. These are not transactions.
--
module Cardano.Api.SpecialByron
  ( ByronUpdateProposal(..),
    serialiseByronUpdateProposal,
    deserialiseByronUpdateProposal,
    ByronVote(..),
    serialiseByronVote,
    deserialiseByronVote,
  ) where

import           Cardano.Prelude (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Prelude

import qualified Cardano.Binary as CBOR
import           Cardano.Chain.Update.Proposal hiding (annotation)
import           Cardano.Chain.Update.Vote hiding (annotation)

-- TODO: Bring over ledger functions to create the underlying types
-- so you can expose a better API

newtype ByronUpdateProposal = ByronUpdateProposal { unByronUpdateProposal :: AProposal ByteString}

serialiseByronUpdateProposal :: AProposal () -> ByteString
serialiseByronUpdateProposal proposal = CBOR.serialize' proposal

deserialiseByronUpdateProposal :: ByteString -> Either CBOR.DecoderError ByronUpdateProposal
deserialiseByronUpdateProposal bs =
  let lBs = LB.fromStrict bs
  in case CBOR.decodeFull lBs of
      Left deserFail -> Left deserFail
      Right proposal -> Right . ByronUpdateProposal $ annotateProposal proposal lBs
 where
  annotateProposal :: AProposal CBOR.ByteSpan -> LB.ByteString -> AProposal ByteString
  annotateProposal proposal bs' = CBOR.annotationBytes bs' proposal

newtype ByronVote = ByronVote { unByronVote :: AVote ByteString}

serialiseByronVote :: AVote () -> ByteString
serialiseByronVote vote = CBOR.serialize' (void vote)

deserialiseByronVote :: ByteString -> Either CBOR.DecoderError ByronVote
deserialiseByronVote bs =
  let lBs = LB.fromStrict bs
  in case CBOR.decodeFull lBs of
       Left deserFail -> Left deserFail
       Right vote -> Right . ByronVote $ annotateVote vote lBs
 where
  annotateVote :: AVote CBOR.ByteSpan -> LB.ByteString -> AVote ByteString
  annotateVote vote bs' = CBOR.annotationBytes bs' vote



