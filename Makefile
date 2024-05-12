project_name = universal-json

DUNE = opam exec -- dune
opam_file = $(project_name).opam

.PHONY: help
help:
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build
build:
	$(DUNE) build @all

.PHONY: build-prod
build-prod:
	$(DUNE) build --profile=prod @all

.PHONY: build-dev
dev:
	$(DUNE) build -w @all

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean

.PHONY: test
test: ## Run the unit tests
	$(DUNE) build @runtest
	yarn jest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build @runtest -w

.PHONY: format
format: 
	$(DUNE) build @fmt --auto-promote

.PHONY: format-check
format-check:
	$(DUNE) build @fmt

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.1.1 --deps-only --with-test -y

.PHONY: install
install:
	$(DUNE) build @install
	opam install . --deps-only --with-test

.PHONY: pin
pin: ## Pin dependencies
	@opam pin add -y quickjs "https://github.com/ml-in-barcelona/quickjs.ml.git#0.1.1"

.PHONY: init
init: create-switch pin install ## Create a local dev enviroment
