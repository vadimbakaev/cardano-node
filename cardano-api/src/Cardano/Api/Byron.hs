-- | This module provides a library interface that is intended to be
-- the complete API for Byron covering everything, including exposing
-- constructors for the lower level types.
--

module Cardano.Api.Byron
  ( module Cardano.Api,
    AsType(..),

    -- * Cryptographic key interface
    -- $keys
    VerificationKey(..),
    SigningKey(..),

    -- * Payment addresses
    -- | Constructing and inspecting Byron payment addresses
    Address(ByronAddress),
    NetworkId(Mainnet, Testnet),

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(ByronTxBody),
    TxId(TxId),
    TxIn(TxIn),
    TxOut(TxOut),
    TxIx(TxIx),
    Lovelace(Lovelace),
    SlotNo(SlotNo),

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(ByronTx),

    -- ** Incremental signing and separate witnesses
    Witness (ByronKeyWitness),
    WitnessNetworkIdOrByronAddress
      ( WitnessNetworkId
      , WitnessByronAddress
      ),

    -- *** Reading one of several key types
    FromSomeType(..),

    -- * Errors
    Error(..),
    FileError(..),

    -- ** Low level protocol interaction with a Cardano node
    LocalNodeConnectInfo(LocalNodeConnectInfo),
    ByronMode,
    CardanoMode,
    NodeConsensusMode
      ( ByronMode
      , CardanoMode
      ),
    LocalNodeClientProtocols(LocalNodeClientProtocols),
    withNodeProtocolClient,

    -- *** Chain sync protocol
    ChainSyncClient(..),

    -- *** Local tx submission
    LocalTxSubmissionClient(LocalTxSubmissionClient),

    -- *** Local state query
    LocalStateQueryClient(..),

    -- * Address
    NetworkMagic(..),

    -- * Update Proposal
    ByronUpdateProposal(..),
    ByronProtocolParametersUpdate (..),
    makeByronUpdateProposal,
    toByronLedgerUpdateProposal,
    makeProtocolParametersUpdate,

    -- * Vote
    ByronVote(..),
    makeByronVote,
    toByronLedgertoByronVote,

    -- ** Conversions
    toByronNetworkMagic,
    toByronProtocolMagicId,
    toByronRequiresNetworkMagic,
  ) where

import           Cardano.Api
import           Cardano.Api.SpecialByron
import           Cardano.Api.Typed
