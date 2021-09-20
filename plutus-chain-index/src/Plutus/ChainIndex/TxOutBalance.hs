{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
module Plutus.ChainIndex.TxOutBalance where

import           Control.Lens                (view)
import           Data.FingerTree             (Measured (measure))
import qualified Data.FingerTree             as FT
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Ledger                      (TxIn (txInRef), TxOutRef (..))
import           Plutus.ChainIndex.Tx        (ChainIndexTx (..), citxInputs, citxTxId, txOutsWithRef)
import           Plutus.ChainIndex.TxIdState (transactionStatus)
import           Plutus.ChainIndex.Types     (BlockNumber, Point (..), Tip (..), TxIdState, TxOutBalance (..),
                                              TxOutState (..), TxOutStatus, TxStatusFailure (TxOutBalanceStateInvalid),
                                              pointsToTip, tobSpentOutputs, tobUnspentOutputs)
import           Plutus.ChainIndex.UtxoState (RollbackFailed (OldPointNotFound, RollbackNoTip, TipMismatch, foundTip, targetPoint),
                                              RollbackResult (RollbackResult, newTip, rolledBackIndex), UtxoIndex,
                                              UtxoState (UtxoState, _usTip, _usTxUtxoData), tip, usTxUtxoData, viewTip)

-- | Given the current block, compute the status for the given transaction
-- output by getting the state of the transaction that produced it and checking
-- if the output is spent or unspent.
transactionOutputStatus
  :: BlockNumber
  -- ^ Current block number for inspecting the state of the transaction output
  -> TxIdState
  -- ^ Information on the state of a transaction. Needed for determining its
  -- status.
  -> TxOutBalance
  -- ^ Balance of spent and unspent transaction outputs.
  -> TxOutRef
  -- ^ Target transaction output for inspecting its state.
  -> Either TxStatusFailure TxOutStatus
transactionOutputStatus currentBlock txIdState txOutBalance txOutRef@TxOutRef { txOutRefId } =
  let isSpent = txOutRef `Set.member` Map.keysSet (_tobSpentOutputs txOutBalance)
      isUnspent = txOutRef `Set.member` _tobUnspentOutputs txOutBalance
      txOutState
          | isSpent = Just (Spent txOutRefId)
          | isUnspent = Just Unspent
          | otherwise = Nothing
   in do
     -- Get the status of the tx which produced the target tx output
     txStatus <- transactionStatus currentBlock txIdState txOutRefId
     case txOutState of
       Just s -> Right $ fmap (const s) txStatus
       _      -> Left $ TxOutBalanceStateInvalid currentBlock txOutRef txOutBalance

fromTx :: ChainIndexTx -> TxOutBalance
fromTx tx =
    TxOutBalance
        { _tobUnspentOutputs = Set.fromList $ fmap snd $ txOutsWithRef tx
        , _tobSpentOutputs =
          Map.fromSet (const $ view citxTxId tx)
                      $ Set.mapMonotonic txInRef (view citxInputs tx)
        }

-- | Whether a 'TxOutRef' is a member of the UTXO set (ie. unspent)
isUnspentOutput :: TxOutRef -> UtxoState TxOutBalance -> Bool
isUnspentOutput r = Set.member r . unspentOutputs

-- | The UTXO set
unspentOutputs :: UtxoState TxOutBalance -> Set TxOutRef
unspentOutputs = view (usTxUtxoData . tobUnspentOutputs)

-- | Whether a 'TxOutRef' is a member of the spent tx output set.
isSpentOutput :: TxOutRef -> UtxoState TxOutBalance -> Bool
isSpentOutput r = Set.member r . spentOutputs

-- | The spent output set
spentOutputs :: UtxoState TxOutBalance -> Set TxOutRef
spentOutputs = Map.keysSet . view (usTxUtxoData . tobSpentOutputs)

-- | 'UtxoIndex' for a single block
fromBlock :: Tip -> [ChainIndexTx] -> UtxoState TxOutBalance
fromBlock tip_ transactions =
    UtxoState
            { _usTxUtxoData = foldMap fromTx transactions
            , _usTip        = tip_
            }

-- | Perform a rollback on the utxo index
rollback :: Point
         -> UtxoIndex TxOutBalance
         -> Either RollbackFailed (RollbackResult TxOutBalance)
rollback _ (viewTip -> TipAtGenesis) = Left RollbackNoTip
rollback targetPoint idx@(viewTip -> currentTip)
    -- The rollback happened sometime after the current tip.
    | not (targetPoint `pointLessThanTip` currentTip) =
        Left TipMismatch{foundTip=currentTip, targetPoint}
    | otherwise = do
        let (before, _) = FT.split (pointLessThanTip targetPoint . tip) idx

        case tip (measure before) of
            TipAtGenesis -> Left $ OldPointNotFound targetPoint
            oldTip | targetPoint `pointsToTip` oldTip ->
                       Right RollbackResult{newTip=oldTip, rolledBackIndex=before}
                   | otherwise                        ->
                       Left  TipMismatch{foundTip=oldTip, targetPoint=targetPoint}
    where
      pointLessThanTip :: Point -> Tip -> Bool
      pointLessThanTip PointAtGenesis  _               = True
      pointLessThanTip (Point pSlot _) (Tip tSlot _ _) = pSlot < tSlot
      pointLessThanTip _               TipAtGenesis    = False
