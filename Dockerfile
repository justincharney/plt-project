FROM ocaml/opam:debian-ocaml-4.14

# Set DEBIAN_FRONTEND to noninteractive to avoid prompts during apt-get install
ARG DEBIAN_FRONTEND=noninteractive

# Switch to root user to install system packages
USER root

# Install system dependencies:
# - LLVM development libraries (version 14 as an example)
# - Clang (often useful with LLVM)
# - pkg-config (helper for C bindings)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    llvm-14-dev \
    libllvm14 \
    clang-14 \
    pkg-config \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Copy project files
COPY . /app/

# Set the working directory
WORKDIR /app/

# Switch to the opam user for opam operations
USER opam

# Install dependencies
RUN opam update && \
    opam install -y dune ocaml-lsp-server odoc ocamlformat utop ounit2 llvm.14.0.6

# Add OPAM environment setup to bashrc so it's loaded automatically
RUN echo 'eval $(opam env)' >> ~/.bashrc

# Default command
CMD ["/bin/bash"]
