-- |
-- Module      : Relay.ReceiverHalf
-- Description : Receiver half of the sequencing protocol.
module Relay.ReceiverHalf
  ( ReceiverHalf (..),
    emptyReceiver,
    receive,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Relay.Session (Nat)

-- | Local state for one session's in-order delivery.
data ReceiverHalf a = ReceiverHalf
  { expected :: Nat,
    pending :: Map Nat a
  }
  deriving (Show, Eq)

-- | Initial receiver state. The first expected sequence number is 1.
emptyReceiver :: ReceiverHalf a
emptyReceiver = ReceiverHalf {expected = 1, pending = Map.empty}

-- | Process one arriving frame and return the updated state and any
-- newly deliverable payloads.
--
-- * Duplicate (@n < expected@): dropped silently, no state change.
-- * Out-of-order (@n > expected@): buffered in @pending@, no output.
-- * In-order (@n == expected@): delivered, then consecutive entries
--   flushed from @pending@.
receive :: Nat -> a -> ReceiverHalf a -> (ReceiverHalf a, [a])
receive n v rh
  | n < expected rh = (rh, [])
  | n > expected rh = (rh {pending = Map.insert n v (pending rh)}, [])
  | otherwise = flush rh {expected = expected rh + 1} [v]
  where
    flush rh' acc =
      case Map.minViewWithKey (pending rh') of
        Just ((k, a), rest)
          | k == expected rh' ->
              flush rh' {expected = k + 1, pending = rest} (acc ++ [a])
        _ -> (rh', acc)
