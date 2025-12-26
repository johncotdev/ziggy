# Ziggy DBL Technical Documentation

This directory contains technical documentation for AI agents and developers working on the Ziggy DBL project.

## Project Overview

Ziggy DBL is a Zig implementation of Synergy DBL (Dibol Business Language), a legacy business application language used primarily in enterprise environments. The project aims to provide:

1. A modern, memory-safe implementation of DBL
2. A custom ISAM (Indexed Sequential Access Method) database
3. A bytecode compiler and virtual machine for portable execution
4. Compatibility with SynergyDE semantics where practical

## Documentation Index

| Document | Description |
|----------|-------------|
| [architecture.md](./architecture.md) | High-level system architecture and module relationships |
| [bytecode.md](./bytecode.md) | Bytecode format, compiler, and VM documentation |
| [isam.md](./isam.md) | ISAM database implementation details |
| [ast.md](./ast.md) | Abstract Syntax Tree structure |
| [runtime.md](./runtime.md) | Runtime interpreter documentation |

## Quick Reference

### Project Structure

```
ziggy/
├── src/
│   ├── main.zig           # CLI entry point
│   ├── lexer/             # Tokenizer
│   ├── parser/            # Recursive descent parser
│   ├── ast/               # AST definitions
│   ├── runtime/           # Tree-walking interpreter
│   ├── bytecode/          # Bytecode compiler + VM
│   │   ├── compiler.zig   # AST → Bytecode
│   │   ├── vm.zig         # Bytecode executor
│   │   ├── opcodes.zig    # Instruction set
│   │   ├── module.zig     # Binary format
│   │   └── disasm.zig     # Disassembler
│   └── isam/              # ISAM database
│       ├── isam.zig       # File operations
│       └── btree.zig      # B+ tree index
├── docs/                  # User documentation
├── tech-docs/             # Technical docs (this directory)
└── examples/              # Example DBL programs
```

### Key Design Decisions

1. **Dual Execution Modes**: Programs can run via tree-walking interpreter (development) or bytecode VM (production)
2. **Stack-Based VM**: Simple, proven design with 100+ opcodes
3. **Two-File ISAM**: Separate index (.ism) and data (.is1) files
4. **B+ Tree Indexes**: High-order (128) for fewer disk seeks
5. **SynergyDE Compatibility**: Channel-based I/O, XCALL semantics

### Building

```bash
zig build           # Build all
zig build test      # Run tests
zig build run       # Run CLI
```

### Running Examples

```bash
./zig-out/bin/ziggy examples/hello.dbl
./zig-out/bin/ziggy examples/isam_demo.dbl
```

## For AI Agents

When working on this codebase:

1. **Read the architecture doc first** - Understand module boundaries
2. **Check existing patterns** - The codebase follows consistent conventions
3. **Run tests after changes** - `zig build test` catches regressions
4. **ISAM is complex** - The B+ tree and file format are tightly coupled
5. **AST structure matters** - The compiler depends on exact AST types

### Common Tasks

- **Adding a new opcode**: Edit `opcodes.zig`, add to `operandSize()`, implement in `vm.zig`
- **Adding a statement**: Update `ast.zig`, `parser.zig`, `compiler.zig`, `runtime.zig`
- **ISAM changes**: Test with `examples/isam_demo.dbl` to verify behavior
