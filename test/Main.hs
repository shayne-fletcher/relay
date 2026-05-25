{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Map.Strict qualified as Map
import Data.UUID.Types (fromWords)
import Relay.Buffer
import Relay.ReceiverHalf
import Relay.SenderHalf
import Relay.Session
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "relay" [senderHalfTests, receiverHalfTests, bufferTests]

-- SenderHalf

senderHalfTests :: TestTree
senderHalfTests =
  testGroup
    "SenderHalf"
    [ testCase "S2: emptySender starts at 1" $
        nextSeq emptySender @?= 1,
      testCase "S1: assign advances seq by 1" $
        let (_, sh') = assign emptySender
         in nextSeq sh' @?= nextSeq emptySender + 1
    ]

-- ReceiverHalf

instance (Arbitrary a) => Arbitrary (ReceiverHalf a) where
  arbitrary = sized $ \sz -> do
    e <- choose (1, fromIntegral sz + 1)
    pairs <- listOf $ do
      offset <- choose (1, fromIntegral sz + 1)
      v <- arbitrary
      pure (e + offset, v)
    pure ReceiverHalf {expected = e, pending = Map.fromList pairs}

receiverHalfTests :: TestTree
receiverHalfTests =
  testGroup
    "ReceiverHalf"
    [ testGroup
        "unit"
        [ testCase "in-order frame is delivered" $ do
            let (rh', delivered) = receive 1 'a' emptyReceiver
            delivered @?= ['a']
            expected rh' @?= 2
            pending rh' @?= Map.empty,
          testCase "duplicate frame is dropped" $ do
            let (rh1, _) = receive 1 'a' emptyReceiver
                (rh2, delivered) = receive 1 'b' rh1
            delivered @?= []
            rh2 @?= rh1,
          testCase "out-of-order frame is buffered" $ do
            let (rh', delivered) = receive 2 'b' emptyReceiver
            delivered @?= []
            expected rh' @?= 1
            pending rh' @?= Map.fromList [(2, 'b')],
          testCase "in-order arrival flushes consecutive pending" $ do
            let (rh1, _) = receive 2 'b' emptyReceiver
                (rh2, _) = receive 3 'c' rh1
                (rh3, delivered) = receive 1 'a' rh2
            delivered @?= ['a', 'b', 'c']
            expected rh3 @?= 4
            pending rh3 @?= Map.empty
        ],
      testGroup
        "laws"
        [ testProperty "R1: expected never decreases" $
            \n (v :: Char) rh ->
              expected (fst (receive n v rh)) >= expected rh,
          testProperty "R2: in-order frame always produces output" $
            \(v :: Char) rh ->
              let n = expected rh
               in snd (receive n v rh) /= [],
          testProperty "R3: duplicate frame produces no output and no state change" $
            \n (v :: Char) rh ->
              n < expected rh ==> receive n v rh == (rh, []),
          testProperty "R4: pending keys are strictly above expected" $
            \n (v :: Char) rh ->
              let rh' = fst (receive n v rh)
               in all (> expected rh') (Map.keys (pending rh')),
          testProperty "R5: delivered count equals advance in expected" $
            \n (v :: Char) rh ->
              let (rh', delivered) = receive n v rh
               in toInteger (length delivered)
                    == toInteger (expected rh') - toInteger (expected rh)
        ]
    ]

-- Buffer

instance Arbitrary SessionId where
  arbitrary =
    SessionId <$> (fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance (Arbitrary a) => Arbitrary (Buffer a) where
  arbitrary = Buffer . Map.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)

bufferTests :: TestTree
bufferTests =
  testGroup
    "Buffer"
    [ testGroup
        "laws"
        [ testProperty "B1: distinct sessions commute" $
            \sid1 sid2 n1 n2 (v1 :: Char) v2 buf ->
              sid1 /= sid2 ==>
                fst (deliver sid1 n1 v1 (fst (deliver sid2 n2 v2 buf)))
                  == fst (deliver sid2 n2 v2 (fst (deliver sid1 n1 v1 buf)))
        ]
    ]
