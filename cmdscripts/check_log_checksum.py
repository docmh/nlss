#!/usr/bin/env python3
import hashlib
import json
import sys
from collections import Counter
from pathlib import Path


def xor_hex(left, right):
    if not left or not right:
        return ""
    left = left.strip().lower()
    right = right.strip().lower()
    if len(left) != len(right) or len(left) % 2 != 0:
        return ""
    try:
        left_bytes = bytes.fromhex(left)
        right_bytes = bytes.fromhex(right)
    except ValueError:
        return ""
    out = bytes(a ^ b for a, b in zip(left_bytes, right_bytes))
    return out.hex()


def hashlib_md5_bytes(data):
    return hashlib.md5(data).hexdigest()


def hashlib_md5_text(text):
    try:
        value = int(text)
    except (TypeError, ValueError):
        return ""
    if value < 0:
        return ""
    return hashlib_md5_bytes(str(value).encode("utf-8"))


def load_entries(path):
    with path.open("rb") as handle:
        for raw in handle:
            if not raw:
                continue
            if raw.endswith(b"\r\n"):
                line = raw[:-2]
                line_ending = b"\r\n"
            elif raw.endswith(b"\n"):
                line = raw[:-1]
                line_ending = b"\n"
            else:
                line = raw
                line_ending = b""
            if not line.strip():
                continue
            yield line, line_ending


def main():
    if len(sys.argv) < 2 or sys.argv[1] in ("-h", "--help"):
        print("Usage: check_log_checksum.py <analysis_log.jsonl>", file=sys.stderr)
        sys.exit(2)

    log_path = Path(sys.argv[1])
    if not log_path.exists():
        print(f"Missing log: {log_path}", file=sys.stderr)
        sys.exit(2)

    counts = Counter()

    prev_line_bytes = None
    prev_line_ending = b""

    for line_bytes, line_ending in load_entries(log_path):
        if not line_ending:
            line_ending = b"\n"
        try:
            line_text = line_bytes.decode("utf-8")
        except UnicodeDecodeError:
            line_text = line_bytes.decode("latin-1")
        try:
            entry = json.loads(line_text)
        except json.JSONDecodeError:
            prev_line_bytes = line_bytes
            prev_line_ending = line_ending
            continue
        combined = entry.get("checksum")
        if not isinstance(combined, str) or not combined:
            prev_line_bytes = line_bytes
            prev_line_ending = line_ending
            continue
        if not line_bytes.endswith(b"}"):
            prev_line_bytes = line_bytes
            prev_line_ending = line_ending
            continue
        if not line_bytes.endswith(b'"}'):
            prev_line_bytes = line_bytes
            prev_line_ending = line_ending
            continue
        # Remove trailing ,"checksum":"..."
        idx = line_bytes.rfind(b',"checksum":"')
        if idx == -1:
            prev_line_bytes = line_bytes
            prev_line_ending = line_ending
            continue
        base_bytes = line_bytes[:idx] + b"}"
        entry_checksum = hashlib_md5_bytes(base_bytes + line_ending)
        reverted = xor_hex(combined, entry_checksum)
        try:
            version = int(entry.get("checksum_version", 1))
        except (TypeError, ValueError):
            version = 1
        if version >= 2 and prev_line_bytes is not None:
            prev_checksum = hashlib_md5_bytes(prev_line_bytes + prev_line_ending)
            reverted = xor_hex(reverted, prev_checksum)
        if version >= 3:
            seq_value = entry.get("log_seq")
            seq_checksum = hashlib_md5_text(seq_value)
            if seq_checksum:
                reverted = xor_hex(reverted, seq_checksum)
        if reverted:
            counts[reverted] += 1
        prev_line_bytes = line_bytes
        prev_line_ending = line_ending

    if not counts:
        print("No checksum entries found.")
        return 0

    for checksum, count in sorted(counts.items()):
        print(f"{checksum} {count}")

    if len(counts) > 1:
        print("WARNING: multiple reverted checksums found.", file=sys.stderr)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
