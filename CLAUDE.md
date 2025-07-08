# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is `tacaml`, OCaml bindings for the TA-Lib (Technical Analysis Library) at https://ta-lib.org/. The project provides both raw C bindings and higher-level safe wrappers for technical analysis functions used in financial markets.

## Architecture

### Core Components

- **src/tacaml.ml**: Main module that exposes the public API with submodules (C, Safe, Input, Output, Defaults, Indicator)
- **src/wrappers.ml**: Type definitions for all indicators using GADTs, including `to_string`, `pp`, and `lookback` functions
- **src/calculate.ml**: Core calculation engine that handles the actual TA-Lib function calls for each indicator
- **src/pack.ml**: Type-safe packing system that matches input sources to output destinations for each indicator
- **src/functions.ml**: Enumeration of all supported TA-Lib functions (160+ indicators)
- **src/type.ml**: Type definitions for different output types (Float, Int, Bool modules)

### Key Design Patterns

- **GADT-based type safety**: The `Wrappers.t` type uses GADTs to ensure compile-time matching of input/output types
- **Ctypes bindings**: Direct C library integration using the ctypes library
- **Bigarray integration**: Uses bigarrays for efficient array operations with C code
- **Error handling**: Consistent Result types for TA-Lib return codes

### Data Flow

1. **Input Sources** (`Input_source.t`): Wrapper around different input types (OHLCV data, float arrays, etc.)
2. **Indicators** (`Wrappers.t`): Type-safe indicator definitions with parameters
3. **Pack system** (`Pack.t`): Combines indicator with input/output types
4. **Calculation**: `Pack.calculate` function calls `Calculate.calculate` to execute the indicator with proper type checking
5. **Output Destinations** (`Output_destination.t`): Type-safe output containers

### Code Organization

The codebase has been refactored to separate concerns:

- **wrappers.ml**: Contains only the GADT type definitions, helper functions (`to_string`, `pp`, `lookback`), and type declarations
- **calculate.ml**: Contains the actual calculation logic with pattern matching on all 160+ indicators
- **pack.ml**: Imports `Calculate.calculate` and handles the type-safe dispatch between input sources and output destinations

## Build System

This project uses **Dune** as the build system with the following commands:

### Essential Commands

```bash
# Build the project
dune build

# Install locally
dune install

# Format code
dune build @fmt --display=quiet --auto-promote

# Generate dependency graph
odep dune | dot -Tsvg > dune-odep.svg
```

### Just Commands (via justfile)

```bash
# Format code
just format

# Generate dependency graph
just deps

# Build and install
just install
```

## Development

### Dependencies

- **ocaml**: Core OCaml compiler
- **dune**: Build system (>= 3.17)
- **ctypes**: C bindings library
- **containers**: Extended standard library
- **ta-lib**: External C library (must be installed separately)

### Key Files for Development

- **dune-project**: Project configuration and opam metadata
- **src/dune**: Library configuration with ctypes integration
- **src/ta_func.h**: C header definitions for TA-Lib functions

### Type System

The project uses a sophisticated type system to ensure correctness:

- Indicators are defined as GADTs in `wrappers.ml`
- Input/output types are strictly enforced at compile time
- The `Pack` module handles type-safe composition

### Error Handling

All functions that can fail return `Result` types:
- `Ok (start_idx, num_elements)` for successful calculations
- `Error (`TALibCode err)` for TA-Lib errors
- `ta_initialize()` must be called before using any functions

## Testing

The project uses dune's built-in test framework. Run tests with:

```bash
dune runtest
```

## Documentation

Generate documentation with:

```bash
dune build @doc
```

The project includes comprehensive type signatures and the main API is exposed through the `Tacaml` module.