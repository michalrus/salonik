.PHONY: local ci build lint run devloop autoformat autoformat-check

local: autoformat       build lint
ci:    autoformat-check build lint

sources=$(shell find . -name '*.hs' -a -not -path '*/.*' -a -not -path './dist-newstyle/*' | sort)

inShell=./nix-shell-persistent --pure --run

build:
	@nix-build -j$$(nproc)
lint:
	@$(inShell) 'exec $(MAKE) _lint'
run:
	@$(inShell) "exec $(MAKE) _run"
devloop:
	@$(inShell) "exec $(MAKE) testOpts='$(subst ','\'',$(testOpts))' -j$$(nproc) _devloop"
autoformat:
	@$(inShell) "exec $(MAKE) -j$$(nproc) _autoformat"
autoformat-check: autoformat
	@$(inShell) 'status=$$(git status --porcelain | grep -v "^M ") ; [ -z "$$status" ] || { printf >&2 "%s\n%s\n" "fatal: some files are unformatted (or repo unclean):" "$$status" ; exit 1 ; }'


#———————————————————————————————————nix-shell———————————————————————————————————


.PHONY: _autoformat _devloop _run _lint _seed _test

_autoformat: $(patsubst %,dist-newstyle/autoformat/%_fmt,$(sources))
_lint:       $(patsubst %,dist-newstyle/lint/%_fmt,$(sources))

dist-newstyle/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@hindent -XTypeApplications "$<" \
		&& stylish-haskell --inplace "$<" \
		&& mkdir -p "$(dir $@)" && touch "$@" \
		|| true # we want to see real compilation errors

dist-newstyle/lint/%_fmt: %
	@echo "Linting $<..."
	@hlint "$<" \
		&& mkdir -p "$(dir $@)" && touch "$@"

# Cf. running an autoformatter in a loop: <https://github.com/sublee/trueskill/issues/26#issuecomment-396212691>.
_devloop:
	watchexec -rs SIGKILL "printf '\n\n\n\n\n--\n\n\n\n\n\n' ; export | grep -F WATCHEXEC ; exec $(MAKE) -j1 _lint _run"
_run:
	cabal new-run salonik -- -c ./config/local.dhall
