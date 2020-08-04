# 9. Push notifications

Date: 2020-04-12

## Status

Accepted

## Context

The goal of hermes is to provide a way to collect notifications.

With previous ADR, we can retrieve notification.
In order to have it "in real time", we should poll, it generates a lot of
useless resources consumption.

The solution is to offer a push endpoint.

## Decision

Create an endpoint the get the notifications in push mode.

## Consequences

Create an endpoint pushing notifications via HTTP streams (WebSockets has been
considered too heavy).

Override the Persistent implementation to deal with RabbitMQ after the current
one.
