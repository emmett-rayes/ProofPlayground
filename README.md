# Proof Playground

An interactive proof editor for constructing and verifying formal proofs, built in Scala 3.

## Features

- **Natural Deduction** and **Sequent Calculus** proof systems for second-order propositional logic
- Interactive TUI for navigating and building proof trees
- Inference rule application with unification and side condition checking
- Interactive resolution of unresolved meta-variables during rule application
- Formula parsing and pretty-printing with standard logical notation

## Limitations

- Meta-variable resolution during rule application is not yet fully supported under Sequent Calculus

## Running

```sh
sbt run
```

## Project Structure

- `core.logic` — Propositional logic formulas and symbols
- `core.proof` — Proof trees, inference rules, and proof rule application
- `core.meta` — Meta-variables, patterns, and unification
- `parser` — Logic formula parsing
- `zipper` — Generic tree and zipper data structures for proof tree navigation
- `frontend` — TUI presentation and interaction
