-- |
-- Module      : Relay.Buffer
-- Description : Multi-session reorder buffer.
module Relay.Buffer
  ( Buffer (..),
    emptyBuffer,
    deliver,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Relay.ReceiverHalf (ReceiverHalf, emptyReceiver, receive)
import Relay.Session (Nat, SessionId)

-- | A reorder buffer: one 'ReceiverHalf' per session.
newtype Buffer a = Buffer (Map SessionId (ReceiverHalf a))
  deriving (Show, Eq)

-- | An empty buffer with no sessions.
emptyBuffer :: Buffer a
emptyBuffer = Buffer Map.empty

-- | Route a frame to the correct 'ReceiverHalf', inserting
-- 'emptyReceiver' on first contact with a new session.
deliver :: SessionId -> Nat -> a -> Buffer a -> (Buffer a, [a])
deliver sid n v (Buffer m) =
  let rh = Map.findWithDefault emptyReceiver sid m
      (rh', delivered) = receive n v rh
   in (Buffer (Map.insert sid rh' m), delivered)
