# 6. Immutable events

Date: 2019-06-15

## Status

Accepted

## Context

Events declared by the consumer should be notified for all events occuring, having the ability to update or drop them would make the logic too complex.

## Decision

Once an event is declared, it can not be modified or removed.

## Consequences

Notification logic will be simpler to describe and to implement.
