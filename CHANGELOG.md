Unreleased
==========

* Fix: a nightly snapshot missing from the database (e.g. a date for which
  no nightly was published) was reported as `✓ up to date`.
  Now its GHC version is inferred from the closest known nightly.
* Snapshots that cannot be analyzed are now reported as `? unknown snapshot`
  instead of `✓ up to date`.  This includes snapshots newer than the database
  (previously a downgrade could be suggested), unknown LTS major versions,
  and resolvers that are not Stackage snapshots.
* New subcommands `stacker info repo` and `stacker info snapshots`
  that print just the repository path and just the snapshot list, respectively.
* Shipped snapshot database updated (latest: lts-24.63, nightly-2026-10-12).

0.0.0.0
=======

Initial release.
* Tested with GHC 9.4.8 - 9.12.4.

_Andreas Abel, 2026-04-08_
