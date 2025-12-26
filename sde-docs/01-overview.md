# SynergyDE Platform Overview

## What is Synergy/DE?

Synergy/DE is a full suite of development tools for creating event-driven business applications. It was originally derived from Digital Equipment Corporation's DIBOL and has evolved to support modern programming paradigms.

## Core Philosophy

The platform is built on the principle that programming languages should maintain consistent operational characteristics across different operating systems. This enables code portability through recompilation and relinking.

## Platform Components

### Professional Series

| Component | Description |
|-----------|-------------|
| **Synergy DBL** | High-level business programming language with structured, modular, system-independent code |
| **Synergy DBMS** | High-speed keyed (ISAM) and sequential access database |
| **UI Toolkit** | Subroutines for environment, menu, input, text, and list processing |
| **Composer** | Visual interface design tool (Windows only) |
| **Repository** | Centralized data definition and management system |
| **ReportWriter** | End-user reporting application |
| **Synergy Runtime** | Execution support environment |

### Connectivity Series

| Component | Description |
|-----------|-------------|
| **SQL Connection** | RDBMS access (Oracle, MySQL, SQL Server) |
| **SQL OpenNet** | Remote database middleware |
| **xfODBC** | Third-party ODBC compatibility |
| **xfServer** | Network data requests |
| **xfServerPlus** | Remote routine execution |
| **xfNetLink** | Java and .NET integration |

## Runtime Environments

### Traditional Synergy Runtime
Native runtime providing execution and system-level services for conventional deployments.

### Synergy .NET
Targets .NET Framework and .NET 6+ on Windows and Linux, enabling integration with third-party .NET libraries.

## Cross-Platform Support

- Windows
- Linux/Unix
- OpenVMS

## Key Capabilities

- Native XML, HTTPS, SSL, JSON support
- Built-in high-performance ISAM database
- Visual Studio integration with IntelliSense and debugging
- Advanced profiling tools
- Async/await patterns

## Ziggy DBL Scope

For Ziggy DBL, we will focus on:

1. **Synergy DBL Language** - The core language compiler/interpreter
2. **Synergy DBMS** - Custom ISAM implementation
3. **Runtime** - Execution environment

We will NOT implement:
- UI Toolkit (platform-specific)
- Composer (Windows-only visual tool)
- Repository (can be separate tooling)
- Connectivity Series (separate project scope)
