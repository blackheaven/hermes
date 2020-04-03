# 8. Notifications

Date: 2019-12-28

## Status

Accepted

## Context

The goal of hermes is to provide a way to collect notifications.

With previous ADR, we now know which events are registered and who watch them.

## Decision

Create an endpoint the get the notifications.

## Consequences

Create a projection and a persisting strategy to track resets.
