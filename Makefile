.PHONY: all build compile test clean distclean deploy install uninstall dialyzer cover help

REBAR3 := rebar3
INSTALL_DIR := /usr/local/bin
BINARY_NAME := cre

help:
	@echo "CRE Makefile - Collaborative Runtime Environment"
	@echo ""
	@echo "Available targets:"
	@echo "  make build        - Compile the project using rebar3"
	@echo "  make compile      - Alias for build"
	@echo "  make test         - Run all tests"
	@echo "  make deploy       - Build escript binary"
	@echo "  make install      - Build and install binary to $(INSTALL_DIR)"
	@echo "  make uninstall    - Remove installed binary"
	@echo "  make clean        - Clean build artifacts"
	@echo "  make distclean    - Remove build and deps directories"
	@echo "  make dialyzer     - Run Dialyzer type checker"
	@echo "  make cover        - Generate test coverage report"
	@echo "  make all          - Build, test, and generate escript"
	@echo "  make help         - Show this help message"

all: build test deploy
	@echo "Build complete!"

build: compile
	@echo "Build successful!"

compile:
	@echo "Compiling Erlang source code..."
	$(REBAR3) compile
	@echo "Compilation complete!"

test:
	@echo "Running tests..."
	$(REBAR3) eunit
	$(REBAR3) ct
	@echo "Tests complete!"

deploy:
	@echo "Building escript..."
	$(REBAR3) escript:build
	@echo "Escript built at _build/default/bin/cre"

install: deploy
	@echo "Installing binary to $(INSTALL_DIR)/$(BINARY_NAME)..."
	@if [ -d "_build/default/bin" ]; then \
		cp _build/default/bin/$(BINARY_NAME) $(INSTALL_DIR)/$(BINARY_NAME); \
		chmod +x $(INSTALL_DIR)/$(BINARY_NAME); \
		echo "Installation complete! Use '$(BINARY_NAME)' from anywhere."; \
	else \
		echo "Error: Binary not found in _build/default/bin/"; \
		exit 1; \
	fi

uninstall:
	@echo "Removing $(BINARY_NAME) from $(INSTALL_DIR)..."
	@if [ -f "$(INSTALL_DIR)/$(BINARY_NAME)" ]; then \
		rm -f $(INSTALL_DIR)/$(BINARY_NAME); \
		echo "Uninstall complete!"; \
	else \
		echo "Binary not found in $(INSTALL_DIR)"; \
	fi

clean:
	@echo "Cleaning build artifacts..."
	$(REBAR3) clean
	rm -f erl_crash.dump
	@echo "Clean complete!"

distclean: clean
	@echo "Removing dependencies and build directories..."
	rm -rf _build rebar.lock
	@echo "Distclean complete!"

dialyzer:
	@echo "Running Dialyzer type checker..."
	$(REBAR3) dialyzer
	@echo "Dialyzer complete!"

cover:
	@echo "Generating test coverage report..."
	$(REBAR3) do eunit, ct --cover
	$(REBAR3) cover
	@echo "Coverage report generated!"
