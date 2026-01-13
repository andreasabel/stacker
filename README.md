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
| `config`          | Configures `stack-snaphots`                                                         |
| `update`          | Updates its database of stackage snapshots                                          |
| `info`            | Prints table mapping GHC major versions to their latest snapshots                   |

These can be modified with the following option(s):

| Option    | Values                  | Description                       |
|-----------|-------------------------|-----------------------------------|
| `--color` | `always`,`never`,`auto` | Colored output? (Default: `auto`) |

When `--color=auto` is used (the default), colors are disabled if the `NO_COLOR` environment variable is set to any value, following the standard described at https://no-color.org.
Colors can still be forced on with `--color=always` even when `NO_COLOR` is set.

It also has the following inessential commands (standard options) that just give information about itself:

| Command           | Alternatives        | Description                                                                         |
|-------------------|---------------------|-------------------------------------------------------------------------------------|
| `version`         | `--version`, `-V`   | Prints version and copyright info                                                   |
| `numeric-version` | `--numeric-version` | Prints just the version number                                                      |
| `license`         | `--license`         | Prints the license text (BSD 3-clause)                                              |
| `help`            | `--help`, `-h`      | Prints `--version`, one paragraph description of functionality, and list of options |

`stack-snapshots` runs on the 3 major platforms Linux, macOS, and Windows.

## Data files

`stack-snapshots` comes with 3 data files that are copied into its
XDG-conform application directory (`$XDG_STATE_HOME/stack-snapshots`)
whenever the program is invoked and they do not exist there yet.

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

These data files determine the results of `bump` (and its preview by `dry-run`).

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
When coloring is on, the `stack*.yml` column is printed in bold, `✓ up to date` in green and any bump action in bright yellow.

Internally, `bump` and `dry-run` share the analysis phase that reads the `stack*.yaml` and creates for each an action plan,
yet `dry-run` just pretty-prints this plan as detailed above whereas `bump` executes it.

## Configuration (optional)

`stack-snapshots update` regenerates its information about GHC versions and Stackage snapshots from the repository
https://github.com/commercialhaskell/stackage-snapshots .

If you have a clone of this repository already, you might want to configure `stack-snapshots` to use it
so it does not make its own clone (which takes ~2GB of disk space).

By default, `stack-snapshots` stores information about the stackage snapshots
in its XDG-conform application directory (`$XDG_STATE_HOME/stack-snapshots`),
by creating a shallow git clone of said repository there
(typically `$XDG_STATE_HOME/stack-snapshots/stackage-snapshots`).

With the command
```
   stack-snapshots config repo path/to/dir
```
the stackage-snapshots repository will be instead searched for in `path/to/dir`.
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
We shall refer to this value as `$REPO`.
It defaults to `$XDG_STATE_HOME/stack-snapshots/stackage-snapshots` if not configured.

## `update` command

`stack-snapshots update` first prints the path to the repo (value of `$REPO`).
If the repo does not exist,
it will be created as a shallow git clone of https://github.com/commercialhaskell/stackage-snapshots .

Once created, it will only be updated by `stack-snapshots update`, and then only if its `master` branch is checked out.
If another branch is checked out, the `update` command exits with an error.

The `update` command then regenerates then three files `lts.csv`, `nightly.csv` and `ghc.csv` in `$XDG_STATE_HOME/stack-snapshots/`.

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

## Testing

This project is accompanied by a golden value testsuite (framework `tasty-golden`) that tests the `dry-run` and `bump` commands.

## License

BSD 3-clause.
