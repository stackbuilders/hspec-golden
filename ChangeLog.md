# Changelog for hspec-golden
## 0.2.1.1
#### Add
* Add colors to hspec-golden CLI tool.

## 0.2.1.0
#### Add
* Instance for IO Golden type. Kudos to @dbalseiro

## 0.2.0.1
#### Add
* Update docs.

## 0.2.0.0
#### Add
* More flexible file locations
* Contributed by: @bitc and @christianheyn

## 0.1.0.2
#### Add
* Lowerbound for `hspec-core` in `hspec-golden` library

## 0.1.0.2
#### Add
* CLI updates tests recursively #19
* Allow first execution to fail #16

## 0.1.0.1
#### Add
* CLI changed to work with optparse-applicative.

## 0.1.0.0
#### Add
* CLI to update `actual` files to `golden`
* Basic functionality to have Golden tests in `hspec`.
* `Golden str` type to write your own golden tests for the kind of
type that you need and a variable output folder.
* `defaultGolden` helper to create golden tests when your results are simple `String`s.
