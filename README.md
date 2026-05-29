<p align="center">
  <img src="./images/logo.png" width="340" alt="relay logo">
</p>
<h1 align="center">relay</h1>
<p align="center">
  an executable model of ordered delivery
</p>
<p align="center">
  <a href="https://github.com/shayne-fletcher/relay/actions/workflows/ci.yml">
    <img src="https://github.com/shayne-fletcher/relay/actions/workflows/ci.yml/badge.svg" alt="haskell ci">
  </a>
  <a href="https://shayne-fletcher.github.io/relay/">
    <img src="https://img.shields.io/badge/docs-github.io-blue" alt="docs">
  </a>
</p>

`relay` is a Haskell model of ordered message delivery. It keeps the
sequencing/reorder-buffer protocol close to its laws, and then runs those
laws through a real concurrent test harness.

## Protocol

Messages can arrive out of order. The sender stamps each payload with a
sequence number; the receiver releases payloads only when the next
sequence number is available.

```haskell
newtype SessionId = SessionId UUID
type Nat = Word

data Frame a = Frame
  { frameSession :: SessionId
  , frameSeq     :: Nat
  , framePayload :: a
  }

newtype SenderHalf = SenderHalf
  { nextSeq :: Nat
  }

data ReceiverHalf a = ReceiverHalf
  { expected :: Nat
  , pending  :: Map Nat a
  }

newtype Buffer a = Buffer
  (Map SessionId (ReceiverHalf a))
```

A `Frame` is the wire fact: session, sequence number, payload. `SessionId`
names the sending instance. An ordered stream is determined by a sender
session and a destination stream.

`SenderHalf` is the sender-local half of one such stream: for a fixed
sender session and destination, it owns `nextSeq`.

`ReceiverHalf` is the receiver-local half of the same stream: for that
same sender session and destination, it owns `expected` and `pending`.

The two sides see the product from opposite ends. A sender session may
have many `SenderHalf`s, one per destination stream. A receiver-side
`Buffer` sits at one destination stream and contains many `ReceiverHalf`s,
one per sender `SessionId`.

## Dual Half-Sessions

The protocol is modelled as two complementary halves rather than a shared
runtime object.

`SenderHalf` owns sequence assignment:

```haskell
assign :: SenderHalf -> (Nat, SenderHalf)
```

`assign` returns the current counter and advances it.

`ReceiverHalf a` owns in-order release:

```haskell
receive :: Nat -> a -> ReceiverHalf a -> (ReceiverHalf a, [a])
```

`receive n v rh` processes one arriving frame:

- `n < expected`: duplicate — dropped silently, no state change.
- `n > expected`: out-of-order — inserted into `pending`, no output.
- `n == expected`: in-order — delivered immediately, then any buffered
  frames whose sequence numbers now immediately follow are released.

`Buffer a` is the multi-session receiver:

```haskell
deliver :: SessionId -> Nat -> a -> Buffer a -> (Buffer a, [a])
```

`deliver` routes each arriving frame to the correct half, inserting an
empty receiver on first contact.

The important separation is ownership: the sender advances its own
counter, and the receiver advances its own delivery frontier. No runtime
value needs to be touched by both halves.

## Laws

### Sender

- `assign` advances `nextSeq` by exactly one.
- `emptySender` starts at sequence `1`.

### Receiver

- `expected` never decreases.
- An in-order frame produces output.
- A duplicate frame produces no output and no state change.
- `pending` contains only sequence numbers strictly above `expected`.
- The number of delivered payloads equals the advance in `expected`.

### Buffer

Frames from distinct sessions commute:

```text
deliver s1 f1 . deliver s2 f2 == deliver s2 f2 . deliver s1 f1
```

when `s1 /= s2`.

This is the important multi-sender fact. Each session is an independent
cell in the buffer; delivering to one session does not disturb another
session's delivery state.

The light categorical view is that `Buffer a` is a product of independent
receiver halves:

```haskell
Buffer a ≅ Map SessionId (ReceiverHalf a)
```

A delivery for session `s` updates only the `s` component of that
product. Updates to different components commute, which is why distinct
sessions can share one buffer without interfering with each other.

### Global Protocol Law

If a sender assigns sequences `1..n` and all `n` frames eventually reach
the receiver in any order, the receiver releases exactly the original
payload stream. Neither half can observe this law locally; it is checked
by the concurrent model.

## Concurrent Check

The local laws are stated as QuickCheck properties and unit tests. The
end-to-end law runs through concurrent sender, chaos, and receiver tasks:

Many sender halves feed the same wire; the receiver releases outputs in
session order.

Three tasks run as concurrent threads connected by channels. The sender
iterates its payload list, stamps each item with `assign`, and writes
frames to the wire channel. The chaos worker accumulates a sliding window
of configurable size, shuffles it, and forwards the reordered frames. The
receiver calls `deliver` for each arriving frame and collects the results.

All three tasks are live at the same time, and each blocks at `readChan`
when its input is empty. The OS scheduler decides the interleaving of
channel operations, on top of the chaos window shuffle. The assertion is
that the output matches the original payload list exactly.

Multi-sender tests run several sender tasks concurrently into the same
chaos and receiver tasks. `forConcurrently_` runs all senders in parallel
and waits for every one to finish before writing the single end-of-stream
sentinel. The assertion is per-session: the subsequence of deliveries
belonging to each session must match that session's original payload
order. Cross-session interleaving is unconstrained — which is exactly
what B1 permits. The chaos worker models reordering; duplicate handling is
tested directly in the receiver tests.
