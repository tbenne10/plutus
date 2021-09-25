module Examples.PureScript.VendorPassthrough
  ( contractTemplate
  , fullExtendedContract
  , metaData
  , fixedTimeoutContract
  , defaultSlotContent
  ) where

import Prelude
import Data.BigInteger (BigInteger, fromInt)
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple.Nested (type (/\), (/\))
import Examples.Metadata as Metadata
import Marlowe.Extended (Action(..), Case(..), Contract(..), Payee(..), Timeout(..), Value(..))
import Marlowe.Extended.Metadata (MetaData, ContractTemplate)
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Semantics (Bound(..), ChoiceId(..), Party(..), Token(..), ChoiceName)

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract: fullExtendedContract }

fixedTimeoutContract :: Contract
fixedTimeoutContract =
  fillTemplate
    ( TemplateContent
        { slotContent: defaultSlotContent
        , valueContent: Map.empty
        }
    )
    fullExtendedContract

defaultSlotContent :: Map String BigInteger
defaultSlotContent =
  Map.fromFoldable
    [ "Lessee A/R timeout" /\ fromInt 1800
    , "Vendor -> Lessor A/R timeout" /\ fromInt 3600
    , "Lessor -> Lessee A/P timeout" /\ fromInt 1800
    , "Lessor -> Vendor A/P timeout" /\ fromInt 1800
	, "Response timeout" /\ fromInt 5400
    ]  
	
metaData :: MetaData
metaData = Metadata.vendorpassthrough

ada :: Token
ada = Token "" ""

lessor :: Party
lessor = Role "Lessor"

lessee :: Party
lessee = Role "Lessee"

vendor :: Party
vendor = Role "vendor"

percentFee :: Value
percentFee = ConstantParam "Percent Fee"

fixedFee :: Value
fixedFee = ConstantParam "Fixed Fee"

lessorRevenue :: Value
lessorRevenue = ConstantParam "lessor revenue"

passthroughRevenue :: Value
passthroughRevenue = ConstantParam "passthrough revenue"

lesseeRecievablesTimeout :: Timeout
lesseeRecievablesTimeout = SlotParam "Lessee A/R timeout"

vendorRecievablesTimeout :: Timeout
vendorRecievablesTimeout = SlotParam "Vendor -> Lessor A/R timeout"

lesseePayablesTimeout :: Timeout
lesseePayablesTimeout = SlotParam "Lessor -> Lessee A/P timeout"

vendorPayablesTimeout  :: Timeout
vendorPayablesTimeout = SlotParam "Lessor -> Vendor A/P timeout"

answerTimeout :: Timeout
answerTimeout = SlotParam "Response timeout"

choice :: ChoiceName -> Party -> BigInteger -> Contract -> Case
choice choiceName chooser choiceValue =
  Case
    ( Choice (ChoiceId choiceName chooser)
        [ Bound choiceValue choiceValue ]
    )

choices :: Timeout -> Party -> Contract -> Array (BigInteger /\ ChoiceName /\ Contract) -> Contract
choices timeout chooser timeoutContinuation list =
  When
    ( do
        (choiceValue /\ choiceName /\ continuation) <- list
        pure $ choice choiceName chooser choiceValue continuation
    )
    timeout
    timeoutContinuation
	
refundLessor :: Contract
refundLessor
 | explicitRefunds = Pay lessor (Party lessor) ada servicePrice Close
 | otherwise = Close

refundLessee :: Contract
refundLessee
 | explicitRefunds = Pay lessee (Party lessee) ada servicePrice Close
 | otherwise = Close

frontVendorFunds :: Contract
frontVendorFunds
 | explicitRefunds = Pay vendor (Party lessor) ada servicePrice Close
 | otherwise = Close


transferToLessee :: Timeout -> Value -> Contract -> Contract
transferToLessee  timeout amount continuation =
    When [ Case (Deposit lessee lessor ada amount) continuation ]
         timeout
         Close

transferFromLessee :: Timeout -> Value -> Contract -> Contract
transferFromLessee  timeout amount continuation =
    When [ Case (Deposit lessor lessee ada amount) continuation ]
         timeout
         Close

transferFromVendor :: Timeout -> Value -> Contract -> Contract
transferFromVendor timeout amount continuation =
    When [ Case (Deposit lessor vendor ada amount) continuation ]
         timeout
         Close

transferToVendor :: Timeout -> Value -> Contract -> Contract
transferToVendor timeout amount continuation =
    When [ Case (Deposit vendor lessor ada amount) continuation ]
         timeout
         Close

		 
		 
		 
contract :: Contract 
contract = 
           choices lesseeRecievablesTimeout vendor Close
              [ (zero /\ "Vendor performs service without issue"
                /\ transferFromLessee lesseeRecievablesTimeout servicePrice 
                $ transferToVendor vendorPayablesTimeout passthroughRevenue
                Close
                )
                , (one, "Vendor performs service with dispute from lessee"
                /\  choices answerTimeout lessee frontVendorFunds
                     [ (one /\ "Refund requested"
                       /\ transferToLessee lesseePayablesTimeout servicePrice
                       $ transferFromVendor vendorRecievablesTimeout servicePrice
                       Close
                       )
                     , (zero /\ "Dispute terminated"
                        /\ transferFromLessee lesseeRecievablesTimeout servicePrice 
                        $ transferToVendor vendorPayablesTimeout passthroughRevenue
                        Close
                        
                     
                       )
                     ]
                )
              ]
