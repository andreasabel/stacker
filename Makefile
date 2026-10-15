# Maintenance tasks for stacker

STACKER = stack exec -- stacker

.PHONY: update

# Update the shipped snapshot database data/*.csv from the configured stackage-snapshots repo.
# We run `stacker update` with temporary XDG directories,
# so that the user's own state (database) is left untouched.
update:
	stack build
	tmp=$$(mktemp -d) && \
	repo=$$($(STACKER) info repo) && \
	mkdir -p $$tmp/config/stacker && \
	echo "repo: $$repo" > $$tmp/config/stacker/config.yaml && \
	XDG_CONFIG_HOME=$$tmp/config XDG_STATE_HOME=$$tmp/state $(STACKER) update && \
	cp $$tmp/state/stacker/ghc.csv $$tmp/state/stacker/lts.csv $$tmp/state/stacker/nightly.csv data/ && \
	rm -rf $$tmp
