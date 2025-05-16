# PAT Syntax Highlighter (`highlight_pat.py`)

This script generates LaTeX-formatted syntax highlighting for `.pat` source files using a custom Pygments lexer.

## Prerequisites

- Python 3.7+
- [Pygments](https://pygments.org/) (`pip install pygments`)

## Usage

Run the script from the `highlighting/` directory:

```bash
python highlight_pat.py <filename.pat> [--codedir DIR] [--outdir DIR] [--stylefile FILE]
```

### Arguments

- `<filename.pat>`: Name of the `.pat` file to highlight (must be in the code directory).
- `--codedir DIR`: Directory containing `.pat` files (default: `code/`).
- `--outdir DIR`: Directory for output `.tex` files and style file (default: `output/`).
- `--stylefile FILE`: Name of the LaTeX style file to generate (default: `pat_style.tex`).

### Example

```bash
python highlight_pat.py test.pat
```

This will:
- Read `code/test.pat`
- Write highlighted LaTeX to `output/test.tex`
- Write style macros to `output/pat_style.tex`

## Output

- `output/<filename>.tex`: LaTeX code with syntax-highlighted source.
- `output/pat_style.tex`: LaTeX macros for syntax highlighting.

## Including in LaTeX

In your LaTeX document, include the style file and the highlighted code:

```latex
\usepackage{fancyvrb}
\input{output/pat_style.tex} % In preamble
...
\input{output/test.tex}
```

## Notes

- The script uses the custom [`PATLexer`](pat_lexer.py) for P.A.T. syntax.
- Output directories are created automatically if they do not exist.