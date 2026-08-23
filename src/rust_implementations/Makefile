# Makefile for CRE Rust NIF bindings
#
# This Makefile handles building the Rust NIF library that integrates
# with the Erlang CRE application.

# Erlang and Rust configuration
ERL = erl
ERLC = erlc
REBAR3 = rebar3

CARGO = cargo
RUSTUP = rustup

# Directories
RUST_DIR = $(CURDIR)
NIF_DIR = $(RUST_DIR)
PRIV_DIR = $(CURDIR)/priv
SO_DIR = $(PRIV_DIR)

# Platform-specific settings
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    SO_EXT = so
    PLATFORM = linux
endif
ifeq ($(UNAME_S),Darwin)
    SO_EXT = dylib
    PLATFORM = darwin
endif

# NIF library name and output
NIF_LIB_NAME = cre_rust_nif
NIF_SO = $(SO_DIR)/lib$(NIF_LIB_NAME).$(SO_EXT)

# Rust target for NIF compilation
# The target must match the Erlang VM architecture
RUST_TARGET = $(shell $(ERL) -noshell -eval 'io:format("~p~n", [erlang:system_info(system_architecture)]).' -s init stop)

# Build targets
.PHONY: all build clean release test help check-deps install

# Default target
all: build

# Build the NIF library
build: check-deps
	@echo "Building CRE Rust NIF for $(PLATFORM)..."
	@mkdir -p $(PRIV_DIR)
	$(CARGO) build --release
	@echo "Copying NIF library to $(NIF_SO)..."
	@cp target/release/lib$(NIF_LIB_NAME).$(SO_EXT) $(NIF_SO) || \
	 cp target/release/lib$(NIF_LIB_NAME).dll $(NIF_SO) || \
	 cp target/release/$(NIF_LIB_NAME).dll $(NIF_SO)
	@echo "NIF library built successfully: $(NIF_SO)"

# Development build (with debug symbols)
dev:
	@echo "Building CRE Rust NIF in debug mode..."
	@mkdir -p $(PRIV_DIR)
	$(CARGO) build
	@cp target/debug/lib$(NIF_LIB_NAME).$(SO_EXT) $(NIF_SO) 2>/dev/null || \
	 cp target/debug/lib$(NIF_LIB_NAME).dll $(NIF_SO)
	@echo "Debug NIF library built: $(NIF_SO)"

# Check dependencies
check-deps:
	@command -v $(CARGO) >/dev/null 2>&1 || { \
		echo "Error: cargo not found. Please install Rust from https://rustup.rs/"; \
		exit 1; \
	}
	@$(CARGO) --version | head -n 1
	@echo "Checking Rust toolchain for NIF compilation..."
	@rustup show | grep -q "rustler" || { \
		echo "Warning: rustler not explicitly installed. Installing now..."; \
		$(CARGO) install rustler; \
	}

# Install the NIF library (for use by Erlang)
install: build
	@echo "NIF library installed at: $(NIF_SO)"
	@file $(NIF_SO) || echo "Warning: Unable to verify library type"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	$(CARGO) clean
	rm -f $(NIF_SO)
	@echo "Clean complete"

# Release build with full optimization
release: clean
	@echo "Building release-optimized NIF..."
	CARGO_PROFILE_RELEASE_LTO=fat CARGO_PROFILE_RELEASE_CODEGEN_UNITS=1 \
	$(CARGO) build --release
	@mkdir -p $(PRIV_DIR)
	@cp target/release/lib$(NIF_LIB_NAME).$(SO_EXT) $(NIF_SO)
	@echo "Release NIF built: $(NIF_SO)"

# Run Rust tests
test:
	@echo "Running Rust tests..."
	$(CARGO) test

# Run with Clippy for linting
lint:
	@echo "Running Clippy..."
	$(CARGO) clippy -- -D warnings

# Format code with rustfmt
format:
	@echo "Formatting Rust code..."
	$(CARGO) fmt

# Check formatting
check-format:
	@echo "Checking code formatting..."
	$(CARGO) fmt -- --check

# Run doctests
doctest:
	@echo "Running Rust doctests..."
	$(CARGO) test --doc

# Build documentation
docs:
	@echo "Building documentation..."
	$(CARGO) doc --no-deps --open

# Run benchmarks
bench:
	@echo "Running benchmarks..."
	$(CARGO) bench

# Show NIF information
info:
	@echo "CRE Rust NIF Build Information:"
	@echo "  Platform: $(PLATFORM)"
	@echo "  Library: $(NIF_SO)"
	@echo "  Rust target: $(RUST_TARGET)"
	@echo "  Erlang arch: $(shell $(ERL) -noshell -eval 'io:format("~p~n", [erlang:system_info(system_architecture)]).' -s init stop)"
	@cargo --version
	@rustc --version

# Full CI pipeline (lint, test, build)
ci: check-deps lint test build
	@echo "CI pipeline completed successfully"

# Development help
help:
	@echo "CRE Rust NIF Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the NIF library (default)"
	@echo "  build     - Build the NIF library (same as all)"
	@echo "  dev       - Build in debug mode with symbols"
	@echo "  release   - Build optimized release version"
	@echo "  clean     - Remove build artifacts"
	@echo "  test      - Run Rust tests"
	@echo "  lint      - Run Clippy linter"
	@echo "  format    - Format code with rustfmt"
	@echo "  docs      - Generate and open documentation"
	@echo "  bench     - Run benchmarks"
	@echo "  info      - Show build information"
	@echo "  ci        - Run full CI pipeline"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make              # Build the NIF"
	@echo "  make dev          # Debug build"
	@echo "  make test         # Run tests"
	@echo "  make clean build  # Clean rebuild"
