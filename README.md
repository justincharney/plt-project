# PLT Project

## Project Structure
- `parser.mly`: OCamlyacc parser
- `scanner.mll`: Ocamllex scanner
- `hello.ml`: Sample "Hello World" program
- `dune`: Dune build configuration
- `dune-project`: Dune project settings
-  `tests/`: Directory containing test files
- `Dockerfile`: Docker configuration
- `docker-compose.yml`: Docker Compose configuration

## Getting Started

### Prerequisites

1. This project uses Docker for containerization. If you don't have it already, download and install Docker Desktop from:
```
https://docs.docker.com/desktop/
```

2. Docker Compose is included with Docker Desktop. Verify that it is installed by running `docker-compose version` in your terminal.

### Docker Commands Overview

- **`docker-compose build`**: Builds the Docker image (run once initially and whenever you change dependencies or Dockerfile)
- **`docker-compose up -d ocaml`**: Starts a persistent container in the background
- **`docker-compose exec ocaml <command>`**: Runs a command in the already-running container
- **`docker-compose run --rm ocaml <command>`**: Creates a temporary container, runs the command, then removes the container
- **`docker-compose down`**: Stops and removes the running container

### Running the Project

#### Option 1: One-off commands

1. Build the Docker image (only needed once, or when changing Docker configuration):
   ```
   docker-compose build
   ```

2. Run the sample program:
   ```
   # Uses the --rm flag to automatically remove the container when it exits
   docker-compose run --rm ocaml dune exec ./hello.exe
   ```

3. Access the Docker container shell:
   ```
   docker-compose run --rm ocaml bash
   ```

#### Option 2: Persistent development container

```bash
# Build the image first (only needed once initially)
docker-compose build

# Start a container in the background
docker-compose up -d ocaml

# Execute commands in the running container
docker-compose exec ocaml dune exec ./hello.exe

# Open a shell in the container
docker-compose exec ocaml bash
# Then inside the container
dune exec ./hello.exe
# Or try utop and execute something like 2 * 24;;
utop

# When finished, stop the container
docker-compose down
```

### Building and Testing the Project

This project uses Dune for building and running tests.

#### Building the Project

To build the project:

```bash
# If you're using a persistent container
docker-compose exec ocaml dune build

# Or for a one-off command
docker-compose run --rm ocaml dune build
```

#### Running Tests

There are several ways to run the tests:

1. Run all tests:
   ```bash
   docker-compose exec ocaml dune runtest
   ```

2. Run a specific test executable:
   ```bash
   docker-compose exec ocaml dune exec tests/test_scanner.exe
   ```

3. Rebuild and run tests in one command:
   ```bash
   docker-compose exec ocaml dune build @runtest --auto-promote
   ```

4. For interactive testing during development:
   ```bash
   # Enter the Docker container
   docker-compose exec ocaml bash

   # Build the project
   dune build

   # Run specific tests
   dune exec tests/test_scanner.exe

   # Or run all tests
   dune runtest
   ```
