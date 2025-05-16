# highlight_pat.py
import argparse
from pathlib import Path
from pygments import highlight
from pygments.formatters import LatexFormatter
from pat_lexer import PATLexer

# --- CLI setup ---
parser = argparse.ArgumentParser(description="Generate LaTeX highlighting for a P.A.T. file.")
parser.add_argument("filename", help=".pat filename (e.g. test.pat)")
parser.add_argument("--codedir", default="code", help="Directory containing .pat files (default: code/)")
parser.add_argument("--outdir", default="output", help="Directory for output .tex files (default: output/)")
parser.add_argument("--stylefile", default="pat_style.tex", help="Name of the style file (default: pat_style.tex)")

args = parser.parse_args()

# --- Resolve paths ---
codedir = Path(args.codedir)
outdir = Path(args.outdir)
input_path = codedir / args.filename
output_tex_path = outdir / (input_path.stem + ".tex")
style_path = outdir / args.stylefile

# --- Make sure output dir exists ---
outdir.mkdir(parents=True, exist_ok=True)

# --- Read code and highlight ---
formatter = LatexFormatter(style="colorful", linenos=True)

with input_path.open("r") as f:
    code = f.read()

highlighted = highlight(code, PATLexer(), formatter)

with output_tex_path.open("w") as out_file:
    out_file.write(highlighted)

with style_path.open("w") as style_file:
    style_file.write(formatter.get_style_defs())

# --- Success messages ---
print(f"Highlighted LaTeX written to: {output_tex_path}")
print(f"Style macros written to:     {style_path}")
