#!/usr/bin/env python3
"""
Create combined Emacs icons with overlay (MSys2, Cygwin, Windows) in the lower-right corner.
Uses rsvg-convert for SVG rendering and Pillow for compositing.
"""

import subprocess
import io
import os
from PIL import Image

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
INPUT_DIR = os.path.join(SCRIPT_DIR, "input")
SIZE = 256
OVERLAY_SIZE = 150
MARGIN = 1        # small margin from the edge


def svg_to_image(svg_path, size):
    """Render an SVG file to a PIL Image via rsvg-convert."""
    result = subprocess.run(
        ["rsvg-convert", "--width", str(size), "--height", str(size),
         "--keep-aspect-ratio", "--format", "png", svg_path],
        capture_output=True, check=True
    )
    return Image.open(io.BytesIO(result.stdout)).convert("RGBA")


def load_image(path, size):
    """Load a PNG or SVG as a PIL RGBA image at the given size."""
    if path.lower().endswith(".svg"):
        return svg_to_image(path, size)
    img = Image.open(path).convert("RGBA")
    img = img.resize((size, size), Image.LANCZOS)
    return img


def combine(base_img, overlay_img, canvas_size, overlay_size, margin):
    """Paste overlay onto base at lower-right corner."""
    canvas = base_img.copy()
    overlay = overlay_img.resize((overlay_size, overlay_size), Image.LANCZOS)
    x = canvas_size - overlay_size - margin
    y = canvas_size - overlay_size - margin
    canvas.paste(overlay, (x, y), overlay)
    return canvas


def main():
    emacs_path   = os.path.join(INPUT_DIR, "emacs.svg")
    msys2_path   = os.path.join(INPUT_DIR, "msys2.png")
    cygwin_path  = os.path.join(INPUT_DIR, "cygwin.svg")
    windows_path = os.path.join(INPUT_DIR, "windows.png")

    print("Loading Emacs base icon...")
    emacs = load_image(emacs_path, SIZE)

    overlays = [
        ("msys2",   msys2_path),
        ("cygwin",  cygwin_path),
        ("windows", windows_path),
    ]

    for name, path in overlays:
        print(f"Creating emacs-{name}.ico ...")
        overlay = load_image(path, OVERLAY_SIZE)
        combined = combine(emacs, overlay, SIZE, OVERLAY_SIZE, MARGIN)

        out_path = os.path.join(SCRIPT_DIR, f"emacs-{name}.ico")
        combined.save(out_path, format="ICO", sizes=[(256, 256)])
        print(f"  -> {out_path}")

    print("Done.")


if __name__ == "__main__":
    main()
