"""Execute plain Python notebook cells sequentially; never save cell outputs."""
from pathlib import Path
import ast
import json
import os
import sys

root = Path(__file__).resolve().parents[1]
os.chdir(root)
for path in sorted((root / "notebooks").glob("*.ipynb")):
    notebook = json.loads(path.read_text(encoding="utf-8"))
    assert notebook["nbformat"] == 4 and notebook["nbformat_minor"] == 5
    assert isinstance(notebook["metadata"], dict)
    namespace = {"__name__": "__main__"}
    for cell in notebook["cells"]:
        assert cell["cell_type"] in {"code", "markdown"}
        assert isinstance(cell["id"], str) and isinstance(cell["metadata"], dict)
        source = "".join(cell["source"])
        if cell["cell_type"] == "code":
            assert cell["outputs"] == [] and cell["execution_count"] is None
            ast.parse(source)
            exec(compile(source, str(path), "exec"), namespace)
    print(f"PASS {path.name}")
