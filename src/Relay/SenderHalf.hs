-- |
-- Module      : Relay.SenderHalf
-- Description : Sender half of the sequencing protocol.
module Relay.SenderHalf
  ( SenderHalf (..),
    emptySender,
    assign,
  )
where

import Relay.Session (Nat)

-- | Local state of one sending session.
newtype SenderHalf = SenderHalf
  { nextSeq :: Nat
  }
  deriving (Show, Eq)

-- | Initial sender state. The first assigned sequence number is 1.
emptySender :: SenderHalf
emptySender = SenderHalf {nextSeq = 1}

-- | Return the next sequence number and advance the counter.
assign :: SenderHalf -> (Nat, SenderHalf)
assign sh = (nextSeq sh, sh {nextSeq = nextSeq sh + 1})
