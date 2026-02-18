.PHONY: all install dev build build-cli test setup clean help

AGENT_DIR = openmainframe-agent

all: install dev

setup: ## First-time setup: env files + install + build CLI
	@$(MAKE) -C $(AGENT_DIR) setup

install: ## Install frontend + agent dependencies
	@$(MAKE) -C $(AGENT_DIR) install

dev: ## Start frontend (port 3000) + agent (port 8123)
	@$(MAKE) -C $(AGENT_DIR) dev

build: ## Build frontend for production
	@$(MAKE) -C $(AGENT_DIR) build

build-cli: ## Build OpenMainframe Rust CLI binary
	cargo build --release

test: ## Run integration tests
	@$(MAKE) -C $(AGENT_DIR) test

clean: ## Clean all build artifacts
	@$(MAKE) -C $(AGENT_DIR) clean

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
