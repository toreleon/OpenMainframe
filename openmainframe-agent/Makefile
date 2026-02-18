.PHONY: all install install-frontend install-agent dev dev-frontend dev-agent build build-cli test test-agent lint clean setup help

PYTHON ?= python3
VENV = agent/.venv
VENV_BIN = $(VENV)/bin
PIP = $(VENV_BIN)/pip
UVICORN = $(VENV_BIN)/uvicorn

# Default target
all: install dev

# ──────────────────────────────────────────────
# Virtual environment
# ──────────────────────────────────────────────

$(VENV):
	$(PYTHON) -m venv $(VENV)
	$(PIP) install --upgrade pip

# ──────────────────────────────────────────────
# Install
# ──────────────────────────────────────────────

install: install-frontend install-agent ## Install all dependencies

install-frontend: ## Install frontend (Node.js) dependencies
	npm install

install-agent: $(VENV) ## Install agent (Python) in venv
	$(PIP) install -e agent/
	$(PIP) install -e "agent/[dev]"

install-agent-postgres: $(VENV) ## Install agent with PostgreSQL support
	$(PIP) install -e "agent/[postgres]"

# ──────────────────────────────────────────────
# Development
# ──────────────────────────────────────────────

dev: ## Start frontend + agent concurrently
	npx concurrently \
		-n "frontend,agent" \
		-c "cyan,green" \
		"npm run dev:frontend" \
		"$(UVICORN) main:app --host 0.0.0.0 --port 8123 --reload --app-dir agent"

dev-frontend: ## Start frontend only (port 3000)
	npm run dev:frontend

dev-agent: $(VENV) ## Start agent only (port 8123)
	$(UVICORN) main:app --host 0.0.0.0 --port 8123 --reload --app-dir agent

# ──────────────────────────────────────────────
# Build
# ──────────────────────────────────────────────

build: ## Build frontend for production
	npm run build

build-cli: ## Build OpenMainframe Rust CLI
	cd .. && cargo build --release

# ──────────────────────────────────────────────
# Test
# ──────────────────────────────────────────────

test: test-agent ## Run all tests

test-agent: $(VENV) ## Run Python integration tests
	$(VENV_BIN)/python -m pytest tests/integration -v

# ──────────────────────────────────────────────
# Utilities
# ──────────────────────────────────────────────

lint: ## Lint frontend code
	npm run lint

clean: ## Remove build artifacts and venv
	rm -rf .next node_modules $(VENV)
	find agent -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true

# ──────────────────────────────────────────────
# Setup (first time)
# ──────────────────────────────────────────────

setup: ## First-time setup: env files + install + build CLI
	@test -f .env.local || cp .env.example .env.local
	@test -f agent/.env || cp agent/.env.example agent/.env
	@echo ""
	@echo "==> Environment files created. Edit these with your API keys:"
	@echo "    .env.local      (AGENT_URL)"
	@echo "    agent/.env      (OPENAI_API_KEY)"
	@echo ""
	@echo "==> Then run:"
	@echo "    make install    # install all dependencies"
	@echo "    make build-cli  # build Rust CLI (cargo build --release)"
	@echo "    make dev        # start frontend + agent"

# ──────────────────────────────────────────────

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
