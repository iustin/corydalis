# Development notes

A few notes, mostly for myself.

## Haskell code

* bump stack version in `stack.yaml`, run `make devel`, fix issues
* recommend to commit minimal diff to fix building with newer version, and run
  proper cleanup afterwards.

## Node/javascript code

Security problems:

* run `npm audit`, `npm audit fix --dry-run`, then without dry run.
* run `npm outdated` and see what can be updated.

Then check behaviour, since there are no UI tests.
