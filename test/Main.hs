{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (forConcurrently_, withAsync)
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.UUID.Types (fromWords)
import Relay.Buffer
import Relay.ReceiverHalf
import Relay.SenderHalf
import Relay.Session
import System.Random (randomRIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "relay"
    [senderHalfTests, receiverHalfTests, bufferTests, concurrentTests, multiSenderTests]

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

-- Concurrent test harness

shuffleIO :: [a] -> IO [a]
shuffleIO [] = pure []
shuffleIO (x : xs) = do
  ys <- shuffleIO xs
  i <- randomRIO (0, length ys)
  let (pre, post) = splitAt i ys
  pure (pre ++ [x] ++ post)

-- | Write frames to the wire channel. The caller is responsible for
-- writing the closing Nothing when all senders are done.
senderWorker :: SessionId -> [a] -> Chan (Maybe (Frame a)) -> IO ()
senderWorker sid payloads wire = go emptySender payloads
  where
    go _ [] = pure ()
    go sh (p : ps) = do
      let (n, sh') = assign sh
      writeChan wire (Just (Frame sid n p))
      go sh' ps

chaosWorker :: Int -> Chan (Maybe (Frame a)) -> Chan (Maybe (Frame a)) -> IO ()
chaosWorker windowSize input output = go []
  where
    flush window = shuffleIO window >>= mapM_ (writeChan output . Just)
    go window =
      readChan input >>= \case
        Nothing -> flush window >> writeChan output Nothing
        Just frame ->
          let window' = frame : window
           in if length window' >= windowSize
                then flush window' >> go []
                else go window'

receiverWorker :: Chan (Maybe (Frame a)) -> IORef (Buffer a) -> IO [a]
receiverWorker input bufRef = go []
  where
    go acc =
      readChan input >>= \case
        Nothing -> pure acc
        Just frame -> do
          buf <- readIORef bufRef
          let (buf', delivered) = deliver (frameSession frame) (frameSeq frame) (framePayload frame) buf
          writeIORef bufRef buf'
          go (acc ++ delivered)

receiverWorkerTagged :: Chan (Maybe (Frame a)) -> IORef (Buffer a) -> IO [(SessionId, a)]
receiverWorkerTagged input bufRef = go []
  where
    go acc =
      readChan input >>= \case
        Nothing -> pure acc
        Just frame -> do
          buf <- readIORef bufRef
          let sid = frameSession frame
              (buf', delivered) = deliver sid (frameSeq frame) (framePayload frame) buf
          writeIORef bufRef buf'
          go (acc ++ map (sid,) delivered)

runProtocol :: SessionId -> [a] -> Int -> IO [a]
runProtocol sid payloads windowSize = do
  wire <- newChan
  reordered <- newChan
  bufRef <- newIORef emptyBuffer
  withAsync (chaosWorker windowSize wire reordered) $ \_ -> do
    senderWorker sid payloads wire
    writeChan wire Nothing
    receiverWorker reordered bufRef

runMultiProtocol :: [(SessionId, [a])] -> Int -> IO [(SessionId, a)]
runMultiProtocol sessions windowSize = do
  wire <- newChan
  reordered <- newChan
  bufRef <- newIORef emptyBuffer
  withAsync (chaosWorker windowSize wire reordered) $ \_ -> do
    forConcurrently_ sessions $ \(sid, ps) -> senderWorker sid ps wire
    writeChan wire Nothing
    receiverWorkerTagged reordered bufRef

fixedSid :: SessionId
fixedSid = SessionId (fromWords 0 0 0 1)

concurrentTests :: TestTree
concurrentTests =
  testGroup
    "concurrent"
    [ testCase "single-sender protocol: in-order delivery under chaos" $ do
        let payloads = [1 .. 20] :: [Int]
        result <- runProtocol fixedSid payloads 5
        result @?= payloads,
      testProperty "single-sender protocol law" $
        \payloads (Positive windowSize) ->
          ioProperty $
            (== payloads) <$> runProtocol fixedSid (payloads :: [Int]) windowSize
    ]

multiSenderTests :: TestTree
multiSenderTests =
  testGroup
    "multi-sender"
    [ testCase "session isolation: each session's payloads delivered in order" $ do
        let sessions =
              [ (SessionId (fromWords 0 0 0 1), [1 .. 10] :: [Int]),
                (SessionId (fromWords 0 0 0 2), [11 .. 20]),
                (SessionId (fromWords 0 0 0 3), [21 .. 30])
              ]
        result <- runMultiProtocol sessions 5
        forM_ sessions $ \(sid, payloads) ->
          [v | (s, v) <- result, s == sid] @?= payloads,
      testProperty "session isolation law" $
        \ps1 ps2 ps3 (Positive windowSize) -> ioProperty $ do
          let sessions =
                filter
                  (not . null . snd)
                  [ (SessionId (fromWords 0 0 0 1), ps1 :: [Int]),
                    (SessionId (fromWords 0 0 0 2), ps2),
                    (SessionId (fromWords 0 0 0 3), ps3)
                  ]
          result <- runMultiProtocol sessions windowSize
          pure $ all (\(sid, ps) -> [v | (s, v) <- result, s == sid] == ps) sessions
    ]
