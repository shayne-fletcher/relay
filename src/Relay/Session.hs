-- |
-- Module      : Relay.Session
-- Description : Session identifiers, sequence numbers, and wire frames.
module Relay.Session
  ( SessionId (..),
    Nat,
    Frame (..),
  )
where

import Data.UUID.Types (UUID)

-- | One per sending actor instance.
newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord)

-- | Sequence number type.
type Nat = Word

-- | A sequenced frame on the wire.
data Frame a = Frame
  { frameSession :: SessionId,
    frameSeq :: Nat,
    framePayload :: a
  }
  deriving (Show, Eq)
