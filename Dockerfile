FROM ocaml/opam:debian-ocaml-4.14

# Copy project files
COPY . .

# Install dependencies
RUN opam install -y dune ocaml-lsp-server odoc ocamlformat utop ounit2

# Add OPAM environment setup to bashrc so it's loaded automatically
RUN echo 'eval $(opam env)' >> ~/.bashrc

# Default command
CMD ["/bin/bash"]
