"""Read existing survey PDFs; preserve original bytes and their page context."""
import argparse
import hashlib
import json
from pathlib import Path
from pypdf import PdfReader
import fitz

p = argparse.ArgumentParser()
p.add_argument("root", type=Path)
args = p.parse_args()
out = args.root / "data/derived/party-drift/cached-poll-inspection"
out.mkdir(parents=True, exist_ok=True)
rows = []
for source in sorted((args.root / "data/sources/polls/history").glob("*.pdf")):
    rows.append({"file": source.name, "sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
                 "pages": [{"page": i+1, "text": page.extract_text()} for i,page in enumerate(PdfReader(source).pages)]})
    if source.stem in {"Sondaggio12951", "Sondaggio12952"}:
        doc = fitz.open(source)
        for i in range(1,len(doc)):
            doc[i].get_pixmap(matrix=fitz.Matrix(1.5,1.5)).save(str(out / f"{source.stem}-page-{i+1}.png"))
(out/"transcripts.json").write_text(json.dumps(rows,ensure_ascii=False,indent=2)+"\n")
print(json.dumps({"directory":str(out),"pdfs":len(rows)}))
