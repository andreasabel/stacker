# stack-snapshots

Bump snapshots (aka resolvers) in `stack*.yaml` files in the current directory.

The tool `stack-snapshots` goes through the `stack*.yaml` files in the current directory,
inspects the value of the `snapshot:` (or `resolver:`) field and upgrades it in the following way:

1. If the value is pointing to a LTS snapshot, e.g., `lts-24.10`,
   it will bump it to the latest version of this LTS
   (at the time of writing this is `lts-24.23`).

2. If the value is pointing to a nightly release and this nightly
   belongs to a GHC major version for which an LTS exists,
   it will bump it to the latest LTS for this GHC version.
   For instance, `nightly-2024-12-15` is for GHC 9.10.1,
   but there is LTS series 24 for GHC 9.10,
   so the snapshot will be bumped to `lts-24.23`,
   the latest of the LTS series 24.

3. If the value is pointing to a nightly release for a GHC major version
   that has no LTS yet, it will be bumped to the latest nightly snapshot
   for this GHC major version.
   For instance, snapshot `nightly-2025-10-10` is for GHC 9.12.2,
   and there is no LTS series for GHC 9.12 yet (at the time of writing),
   so the snapshot value will be bumped to `nightly-2025-12-09`,
   the latest nightly (at the time of writing this).

## Synopsis

`stack-snapshots` understands the following essential commands:

| Command           | Description                                                                         |
|-------------------|-------------------------------------------------------------------------------------|
| `bump`            | Updates `stack*.yaml` as described above                                            |
| `dry-run`         | Only describes the updates `bump` would perform (default if no command is given)    |
| `config`          | Configure `stack-snaphots`                                                          |
| `update`          | Updates its database of stackage snapshots                                          |
| `info`            | Prints table mapping GHC major versions to their latest snapshots                   |

These can be modified with the following option(s):

| Option    | Values                  | Description                       |
|-----------|-------------------------|-----------------------------------|
| `--color` | `always`,`never`,`auto` | Colored output? (Default: `auto`) |

It also has the following inessential commands (standard options) that just give information about itself:

| Command           | Alternatives        | Description                                                                         |
|-------------------|---------------------|-------------------------------------------------------------------------------------|
| `version`         | `--version`, `-V`   | Prints its version and copyright info                                               |
| `numeric-version` | `--numeric-version` | Prints just its version number                                                      |
| `license`         | `--license`         | Prints the license text (BSD 3-clause)                                              |
| `help`            | `--help`, `-h`      | Prints `--version`, one paragraph description of functionality, and list of options |

`stack-snapshots` runs on the 3 major platforms Linux, macOS, and Windows.

## Configuration

`stack-snapshots` generates its information about GHC versions and Stackage snapshots from the repository
https://github.com/commercialhaskell/stackage-snapshots .

By default, `stack-snapshots` stores information about the stackage snapshots
in its XDG-conform application directory (`$XDG_STATE_HOME/stack-snapshots`),
by creating a shallow git clone of said repository there
(typically `$XDG_STATE_HOME/stack-snapshots/stackage-snapshots`).

With the command
```
   stack-snapshots config repo path/to/dir
```
the stackage-snapshots repository will be searched for in `path/to/dir`.
The effect of the `config` command is to write file `config.yaml`
in the XDG-conform configuration directory (`$XDG_CONFIG_HOME/stack-snapshots`)
with the following content:
```yaml
# stack-snapshots configuration

repo: /absolute/path/to/dir
```
where `/absolute/path/to/dir` is the `path/to/dir` turned into an absolute path.

Whenever `stack-snapshots` is started with an essential command,
it will attempt to read this configuration file and extract the value of `repo`, if it is given.
We shall refer to this value as `$USER_REPO`.
If `$REPO` shall be the same as `$USER_REPO` except when it is empty, then it shall be
`$XDG_STATE_HOME/stack-snapshots/stackage-snapshots`.

## Data source and storage

Whenever `stack-snapshots` is started with an essential command, and `$REPO` does not exist,
it will be created as a shallow git clone of https://github.com/commercialhaskell/stackage-snapshots .

Once created, it will only be updated by `stack-snapshots update`, and then only if its `master` branch is checked out.
If another branch is checked out, the `update` command should exit with an error message explaining the situation.

From the files in the `$REPO` three CSVs file are created in `$XDG_STATE_HOME/stack-snapshots/`:

1. `lts.csv` containing a map from LTS version to GHC version, e.g.
   ```
   ...
   22.42,9.6.6
   22.43,9.6.6
   22.44,9.6.7
   23.1,9,8.4
   23.2,9.8.4
   ...
   23.28,9.8.4
   24.1,9.10.1
   ...
   ```

2. `nightly.csv` containing a map from nightly version to GHC version, e.g.
   ```
   ...
   2024-01-01,9.8.1
   ...
   2025-07-07,9.10.2
   ...
   2025-12-08,9.12.2
   2025-12-09,9.12.2
   ```

3. `ghc.csv` containing a map from GHC version to its latest LTS or nightly snapshot, e.g.
   ```
   ...
   9.2.8,lts-20.26
   9.4.5,lts-21.7
   ...
   9.4.8,lts-21.25
   9.6.3,lts-22.6
   ...
   9.6.6,lts-22.43
   9.6.7,lts-22.44
   ...
   9.10.3,lts-24.23
   9.12.2,nightly-2025-12-09
   ```

These files are created/updated whenever

- `$REPO` is cloned for the first time, or
- `update` is run, or
- they do not exist despite `$REPO` is present.

How to extract the information for these files from the `$REPO` is described and implemented in
https://github.com/commercialhaskell/stackage-snapshots/blob/master/stackage-history-chart/src/Main.hs .

## `update` command

`stack-snapshots update` prints the path to the repo (value of `$REPO`) and updates the repo by pulling as described above.

## `info` command

`stack-snapshots info` prints the following things in YAML format:
1. `repo: $REPO`
2. A map between each latest major GHC version and its corresponding latest LTS or nightly snapshot,
   as read from `ghc.csv`.  Example:
   ```yaml
   snapshots:
     ...
     9.2.8: lts-20.26
     9.4.8: lts-21.25
     9.6.7: lts-22.44
     9.8.4: lts-23.28
     9.10.3: lts-24.23
     9.12.2: nightly-2025-12-09
   ```

## `bump` command

`stack-snapshots bump` looks for all `stack*.yaml` files in the current directory and updates them as described above.
Only the content of the `snapshot:` and `resolver:` fields will be updated,
everything else will remain literally the same, including comments and whitespace.

## `dry-run` command

This command is also run if no command is given to `stack-snapshots`.
It prints a table with all `stack*.yaml` files in the current directory,
their `snapshot` (which can also be given in the `resolver` field), and what action to take (if any).
Example:
```
stack-9.2.yaml    lts-20.26            ✓ up to date
stack-9.4.yaml    lts-21.25            ✓ up to date
stack-9.6.yaml    lts-22.43            → bump to lts-22.44
stack-9.8.yaml    lts-23.1             → bump to lts-23.28
stack-9.10.yaml   nightly-2024-07-07   → bump to lts-24.23
stack-9.12.yaml   nightly-2025-10-03   → bump to nightly-2025-12-09
```
The table should be suitably colored if coloring is on:
the `stack*.yml` should be printed in bold, `✓ up to date` in green and any bump action in bright yellow.

Internally, `bump` and `dry-run` share the analysis phase that reads the `stack*.yaml` and creates for each an action plan,
yet `dry-run` just pretty-prints this plan as detailed above whereas `bump` executes it.

The action plan should be implemented as a record containing all the necessary information to execute the action or pretty-print it:
1. File name.
2. Old snapshot string extracted from `snapshot` or `resolver` field (and boolean which of these).
3. Maybe new snapshot to replace the old one, or nothing if old one is up-to-date.
4. Character span of old snapshot string that shall be replaced by new snapshot string.
   (Depending how the faithful replacement is implemented, we might also retain the original line where we found the old snapshot string.)


## Options and inessential commands

These have standard implementations, see as a model:

- https://github.com/teach-plt/visualize-type-inference/blob/master/src/Options.hs
- https://github.com/teach-plt/visualize-type-inference/blob/master/src/License.hs
- https://github.com/teach-plt/visualize-type-inference/blob/master/src/ColorOption.hs


## Documentation

This project comes with user documentation explaining the user-facing functionalty in readthedocs format in markdown flavor.

## Testing

This project is accompanied by a golden value testsuite (framework `tasty-golden`).
Dry-run tests are directories with example `stack*.yaml` files and the respective golden value is the output of the `dry-run` command.
System tests are also directories with example `stack*.yaml` files; yet there the golden value is a directory containing these files as modified by the `bump` command.
