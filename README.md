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

1. Build the Docker image (only needed once, or when changing Docker configuration):
   ```
   docker-compose build
   ```

2. Start the container
  ```bash
  docker-compose up -d ocaml
  ```

3. Open a shell in the container
  ```bash
  docker-compose exec ocaml bash
  ```

3. Compile and Run a P.A.T Test Progran (e.g., `funky.pat`):
  This command will first build your P.A.T. compiler (`plt.exe`) if it hasn't been built yet, then use it to compile `funky.pat`. The compiler will output the generated LLVM IR to a `.ll` file and print instructions to compile and run it.
   ```
   dune exec ./plt.exe -- ./tests/test_programs/funky.pat
   ```

  After running the above, follow the `llc-14` and `clang-14` commands printed by `plt.exe` to create and run the executable.

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

There are several ways to run the tests from the container's shell:

0. Run a shell in the docker container. Then perform subsequent steps.
  ```bash
   docker-compose exec ocaml bash
  ```

1. Run all tests:
   ```bash
   dune test
   ```

2. Run a specific test executable:
   ```bash
   dune exec tests/test_scanner.exe
   ```
