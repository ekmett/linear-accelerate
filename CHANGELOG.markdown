# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning Policy
(PVP)](https://pvp.haskell.org)

## [0.6.0.0] - 2018-04-03
### Changed
  * update for accelerate-1.2

### Fixed
  * Disambiguate uses of `fromInteger` and `fromRational`.

## [0.5.0.1] - 2017-09-29
### Fixed
  * doctest failure with accelerate-1.1.1.0 ([#10])

## [0.5] - 2017-08-06
### Fixed
  * "impossible evaluation" error with accelerate-llvm-native ([#25][acc-llvm#25])

## [0.4.1] - 2017-07-29
### Added
  * Support `doctest-0.12`.

## [0.4] - 2017-06-16
### Changed
  * Revamp `Setup.hs` to use `cabal-doctest`. This makes `linear-accelerate` build with `Cabal-2.0`.
  * Re-enable the `doctest`s.

### Fixed
  * Fix handedness of `(-^)` and `(^-)`.

## [0.3] - 2017-04-01
### Changed
  * Expanded API coverage

## [0.2] - 2014-09-15
### Fixed
  * Move `Data.Complex` instances into the `accelerate` package ([#1])

## [0.1] - 2014-03-28
  * Repository initialized


[0.6.0.0]:          https://github.com/ekmett/linear-accelerate/compare/v0.5.0.1...v0.6.0.0
[0.5.0.1]:          https://github.com/ekmett/linear-accelerate/compare/v0.5...v0.5.0.1
[0.5]:              https://github.com/ekmett/linear-accelerate/compare/v0.4.1...v0.5
[0.4.1]:            https://github.com/ekmett/linear-accelerate/compare/v0.4...v0.4.1
[0.4]:              https://github.com/ekmett/linear-accelerate/compare/v0.3...v0.4
[0.3]:              https://github.com/ekmett/linear-accelerate/compare/v0.2...v0.3
[0.2]:              https://github.com/ekmett/linear-accelerate/compare/v0.1...v0.2
[0.1]:              https://github.com/ekmett/linear-accelerate/compare/3db20f05af0a1488fcbc3ea28f8561ce73289b73...v0.1

[#1]:               https://github.com/ekmett/linear-accelerate/issues/1
[#10]:              https://github.com/ekmett/linear-accelerate/issues/10
[acc-llvm#25]:      https://github.com/AccelerateHS/accelerate-llvm/issues/25

