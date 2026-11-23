#!/usr/bin/env python3
"""Split the former docs/migration-history.md while preserving section content.

The one-time migration is recorded by docs/history/migration-history-map.json.
Run with --check after the split to verify every mapped file and heading.
"""
from __future__ import annotations

import hashlib
import json
import re
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MAP = ROOT / "docs/history/migration-history-map.json"


def slug(text: str) -> str:
    text = re.sub(r"<[^>]*>", "", text)
    text = re.sub(r"\[([^]]+)\]\([^)]*\)", r"\1", text)
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode()
    text = re.sub(r"[^\w -]", "", text.lower())
    return re.sub(r"\s+", "-", text).strip("-")


def check() -> None:
    data = json.loads(MAP.read_text(encoding="utf-8"))
    errors = []
    section_names = [
        section["heading"] if isinstance(section, dict) else section
        for entry in data["entries"]
        for section in entry["sections"]
    ]
    if len(section_names) != data["section_count"]:
        errors.append(f"mapped {len(section_names)} sections; expected {data['section_count']}")
    if len(set(section_names)) != len(section_names):
        errors.append("duplicate source section names in the mapping")
    for entry in data["entries"]:
        path = ROOT / entry["path"]
        if not path.is_file():
            errors.append(f"missing: {entry['path']}")
            continue
        text = path.read_text(encoding="utf-8")
        if hashlib.sha256(text.encode()).hexdigest() != entry["output_sha256"]:
            errors.append(f"content changed since split: {entry['path']}")
        if entry["heading"] not in text:
            errors.append(f"heading missing: {entry['path']}")
    if errors:
        raise SystemExit("\n".join(errors))
    print(f"Verified {len(section_names)} source sections in {len(data['entries'])} history files; source SHA-256 {data['source_sha256']}.")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true", help="verify the recorded split")
    args = parser.parse_args()
    if not args.check:
        parser.error("the one-time split is complete; use --check")
    check()
