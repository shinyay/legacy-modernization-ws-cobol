# COBOL Legacy Application Modernization Workshop

This repository contains workshop materials for learning how to analyze and modernize legacy COBOL applications. The workshop guides participants through the process of understanding an existing COBOL application and planning its modernization.

## Workshop Overview

This workshop simulates a real-world scenario where documentation for a legacy COBOL application has been lost, and participants must analyze, understand, and plan modernization approaches for the application. The workshop is structured as a series of tasks that build upon each other.

## Prerequisites

- [Visual Studio Code](https://code.visualstudio.com/)
- [Docker](https://www.docker.com/)
- [Dev Containers Extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
- [GitHub CLI](https://cli.github.com/) (optional, for working with workshop issues)

## Getting Started

1. Fork this repository to your own GitHub account:
   - Click the "Fork" button in the upper right of the repository page
   - Complete the fork creation process

2. Clone your forked repository:
   ```bash
   git clone https://github.com/your-username/legacy-modernization-ws-cobol-2503.git
   cd legacy-modernization-ws-cobol-2503
   ```

3. Open the project in Visual Studio Code:
   ```bash
   code .
   ```

4. When prompted "Reopen in Container" by VS Code, click on it.
   - Alternatively, press `F1`, type "Dev Containers: Reopen in Container" and press Enter

5. Wait for the Dev Container to build and start. This may take a few minutes the first time.

## Workshop Structure

The workshop is divided into several main sections, each with specific learning objectives:

### 0. Repository Setup
- Fork and clone the repository
- Set up the development environment using VS Code Dev Containers

### 1. Initial Analysis
- Analyze the workshop structure and objectives
- Explore the project structure
- Identify technologies used in the project
- Begin application analysis

### 2. Application Exploration
- Understand the processing flow of the application
- Explore each module's role and functionality
- Analyze application architecture

### 3. Subroutine Investigation
- Identify subroutines in each module
- Understand relationships between modules
- Map dependencies

### 4. Design Documentation
- Create templates for design documentation
- Document the existing application architecture
- Use diagrams (including mermaid notation) to visualize structures

### 5. Modernization Planning
- Research appropriate technologies for modernization
- Consider alternative modernization approaches
- Design a modernized architecture overview

## Building and Running the Application

Inside the Dev Container, you can build and run the application using VS Code tasks:

### Using VS Code Command Palette (Ctrl+Shift+P or Cmd+Shift+P):
1. Build the application:
   - Open Command Palette and type "Tasks: Run Build Task" or press `Ctrl+Shift+B`

2. Run the application:
   - Open Command Palette
   - Type "Tasks: Run Task"
   - Select "run"

### Using Terminal in VS Code:
1. Build the application:
   ```bash
   make
   ```

2. Run the application:
   ```bash
   make run
   ```

3. Clean build artifacts:
   ```bash
   make clean
   ```

## Working with Workshop Issues

This workshop is structured around GitHub Issues with the "workshop" label. Each issue represents a task or milestone in the workshop. You can view these issues using:

```bash
gh issue list --label workshop
```

To view details of a specific issue:

```bash
gh issue view ISSUE_NUMBER
```

## Legacy Application Overview

The repository contains a legacy COBOL application that participants will discover and analyze throughout the workshop. By exploring the source code and program structure, participants will gradually understand the application's purpose and functionality.

The workshop provides a realistic example for exploring modernization approaches for legacy systems commonly found in enterprise environments.

## License

Released under the [MIT license](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)

## Author

- github: <https://github.com/shinyay>
- twitter: <https://twitter.com/yanashin18618>
- mastodon: <https://mastodon.social/@yanashin>
