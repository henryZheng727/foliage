# Foliage Specification

This document discusses the implementation of the Foliage proof checker. It
outlines the foundation and implementation of the proof checker, which can be
found in the `foliage-model` subdirectory of the root. The implementation uses,
as a reference, the following texts:

- _Type Theory and Functional Programming_ by Simon Thompson (Thompson91)
- _The Little Typer_ by Daniel P. Friedman and David Thrane Christiansen
(FriedmanChristiansen18)
- _Types and Programming Languages_ by Benjamin Pierce (Pierce02)

## Logical Foundations

Foliage uses intuitionistic type theory as its model, with proofs written in a
Fitch-style natural deduction notation. For each compound logical formula, we
have _introduction_ rules to construct them, or _elimination_ rules to
destructure them.

### Propositional Logic

### First Order Logic

## Proof Format

See examples in `docs/examples`.
