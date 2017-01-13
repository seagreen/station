# Installation

## If Using Nixpkgs and Stack

`stack build` / `stack test`

## If Using Something Else

1. Install the external dependencies listed in `shell.nix` under `buildInputs`.

2. Install the extra Haskell dependencies listed in `stack.yml`. This is each item with `extra-deps: true` under the `packages` section.

# Roadmap

## 1

Make the API as elegant as possible for single-user applications.

## 2

Expand the API so it can be used as the main datastore on a computer, replacing the filesystem (at least for important data). This involves getting authorship tracking, sharing, tagging, etc. right while hopefully maintaining elegance.

## 3

Make it not horribly inefficient in storage and speed.
