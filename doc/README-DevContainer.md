# Development Container for COBOL Application Modernization Workshop

This document provides an overview of the Development Container (Dev Container) configuration used in this workshop. The Dev Container provides a consistent development environment with all the necessary tools pre-installed, ensuring that all participants can work with the same setup regardless of their local operating system.

## Overview

The Dev Container for this workshop is configured using Docker and the VS Code Remote - Containers extension. It provides:

- A GnuCOBOL development environment
- Required tools and libraries
- Appropriate VS Code extensions and settings for COBOL development

## Container Configuration Files

The `.devcontainer` folder contains the following configuration files:

### 1. `Dockerfile`

This file defines the Docker image used for the development environment:

- Base image: `ubuntu:22.04`
- Uses a multi-stage build approach for optimization
- Installs GnuCOBOL compiler and necessary dependencies
- Creates a non-root user (`vscode`) for better security
- Configures sudo access for convenience

Key packages installed:
- `gnucobol` - The GnuCOBOL compiler
- `libxml2` - XML processing library
- `git` - Version control system
- `curl` - Command-line tool for data transfer
- `make` - Build automation tool

### 2. `devcontainer.json`

This file configures how VS Code interacts with the Dev Container:

- Sets the container name as "COBOL Development Environment"
- Configures the Docker Compose file to use
- Specifies the workspace folder mapping
- Configures VS Code extensions to be installed automatically
- Sets editor preferences optimized for COBOL development

VS Code Extensions installed:
- `bitlang.cobol` - COBOL language support
- `streetsidesoftware.code-spell-checker` - Spell checking
- `broadcom.cobol-language-support` - Enhanced COBOL language support
- `kainino.backgroundcopy` - Background file copying
- `ms-vscode.makefile-tools` - Makefile support

Editor settings include:
- Column rulers at positions 6, 7, 72 (matching COBOL fixed format)
- Tab size of 1 with space insertion
- COBOL-specific formatting and styling options

### 3. `compose.yaml`

This file defines how Docker Compose should run the container:

- Sets up a service named "cobol"
- Maps the repository root to the container's `/workspace` directory
- Configures resource limits:
  - Memory limit: 1GB
  - CPU shares: 1024
- Keeps the container running with `sleep infinity`

## Using the Dev Container

When you open the project in VS Code with the Dev Containers extension installed, you'll be prompted to "Reopen in Container". After clicking this option, VS Code will:

1. Build the Docker image defined in the Dockerfile (if not already built)
2. Start the container using Docker Compose
3. Connect VS Code to the running container
4. Set up the specified extensions and configurations
5. Mount your project files to the container's workspace directory

Once inside the container, you can use the terminal to run COBOL commands or use VS Code tasks to build and run the application.

## Benefits of Using Dev Containers

- **Consistency**: All workshop participants use the identical development environment
- **Isolation**: The development environment is separate from your local system
- **Simplicity**: All required tools are pre-installed and configured
- **Portability**: Works the same way across Windows, macOS, and Linux
- **Reproducibility**: The environment can be recreated with a simple command

## Customizing the Dev Container

If needed, you can customize the Dev Container by modifying the configuration files:

- Add additional packages to the Dockerfile
- Configure more VS Code extensions in devcontainer.json
- Adjust resource limits in compose.yaml

After making changes, you can rebuild the container using the "Rebuild Container" command in VS Code.
