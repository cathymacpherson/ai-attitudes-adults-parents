"""Engine for rendering the LaTeX table fragments as a single HTML page.

Parses the LaTeX conventions write_apa_table() (R/utils.R) emits: booktabs
rules, threeparttable notes, multicolumn/cmidrule spanner headers, and its
math/macro vocabulary. render_tables.py supplies TABLES_DIR, TABLE_ORDER,
OUTPUT_HTML and a title, then calls render_tables().
"""

import re

ROW_SEP = "\\\\"
MULTICOL_RE = re.compile(r"\\multicolumn\{(\d+)\}\{[^}]*\}\{(.*)\}")
CMIDRULE_RE = re.compile(r"\\cmidrule\([^)]*\)\{[^}]*\}")
ALIGN_NAMES = {"l": "left", "c": "center", "r": "right"}


def convert_math(inner):
    inner = inner.replace("\\Delta", "\u0394").replace("\\chi", "\u03c7")
    inner = inner.replace("\\alpha", "\u03b1").replace("\\times", "\u00d7")
    inner = inner.replace("\\beta", "\u03b2")
    inner = inner.replace("\\leq", "\u2264").replace("\\geq", "\u2265")
    inner = inner.replace("\\rightarrow", "\u2192")
    # \sqrt{x} has no plain-text equivalent that stays readable; an overline
    # on the radicand is what makes the extent of the root unambiguous.
    inner = re.sub(r"\\sqrt\{([^}]*)\}",
                   "\u221a" + r'<span class="radicand">\1</span>', inner)
    inner = re.sub(r"\^\{([^}]*)\}", r"<sup>\1</sup>", inner)
    inner = re.sub(r"\^(\S)", r"<sup>\1</sup>", inner)
    inner = re.sub(r"_\{([^}]*)\}", r"<sub>\1</sub>", inner)
    inner = re.sub(r"_(\S)", r"<sub>\1</sub>", inner)
    return inner


def render_math(match):
    inner = match.group(1)
    if inner == "<":
        return "&lt;"
    if inner == "-":
        return "\u2212"
    return f"<i>{convert_math(inner)}</i>"


DOLLAR_PLACEHOLDER = "\x00DOLLAR\x00"


def latex_to_html(text, labels):
    # Escaped literal dollar signs (income brackets etc.) must be protected
    # before the math-mode regex runs, or "\$0 - \$31,199" gets misread as
    # math content between two delimiters -- the backslash doesn't stop the
    # regex from matching on the bare "$" characters.
    text = text.replace("\\$", DOLLAR_PLACEHOLDER)
    text = re.sub(r"\$([^$]+)\$", render_math, text)
    text = text.replace(DOLLAR_PLACEHOLDER, "$")
    text = text.replace("\\%", "%")
    text = text.replace("\\&", "&amp;")
    text = re.sub(r"\\textsuperscript\{([^}]*)\}", r"<sup>\1</sup>", text)
    text = re.sub(r"\\emph\{([^}]*)\}", r"<em>\1</em>", text)
    text = re.sub(r"\\textit\{([^}]*)\}", r"<i>\1</i>", text)
    text = re.sub(r"\\texttt\{([^}]*)\}", r"<code>\1</code>", text)
    text = text.replace("\\quad", '<span class="indent"></span>')
    text = text.replace("--", "\u2013")
    text = re.sub(
        r"\\ref\{([^}]+)\}",
        lambda m: f'<a href="#{m.group(1)}">{labels.get(m.group(1), m.group(1))}</a>',
        text,
    )
    return text.strip()


def split_cells(row):
    # Negative lookbehind so an escaped "\&" (a literal ampersand in a cell,
    # e.g. a citation like "Maas \& Hox") isn't mistaken for a column
    # separator.
    return [c.strip() for c in re.split(r"(?<!\\)&", row)]


def parse_table(tex_path):
    text = tex_path.read_text(encoding="utf-8")

    caption = re.search(r"\\caption\{(.*?)\}\s*\n", text).group(1)
    label = re.search(r"\\label\{([^}]*)\}", text).group(1)

    colspec = re.search(r"\\begin\{tabular\}\{([^}]*)\}", text).group(1)
    aligns = [ALIGN_NAMES[c] for c in colspec if c in ALIGN_NAMES]

    tabular = re.search(
        r"\\begin\{tabular\}\{[^}]*\}(.*?)\\end\{tabular\}", text, re.DOTALL
    ).group(1)
    tabular = tabular.replace("\\toprule", "").replace("\\bottomrule", "")
    tabular = CMIDRULE_RE.sub("", tabular)

    header_part, body_part = tabular.split("\\midrule", 1)
    header_rows = [r.strip() for r in header_part.split(ROW_SEP) if r.strip()]
    body_rows = [r.strip() for r in body_part.split(ROW_SEP) if r.strip()]

    note_match = re.search(
        r"\\item\s*\\textit\{Note\.\}\s*(.*?)\s*\\end\{tablenotes\}", text, re.DOTALL
    )
    note = note_match.group(1) if note_match else None

    return {
        "label": label,
        "caption": caption,
        "aligns": aligns,
        "header_rows": header_rows,
        "body_rows": body_rows,
        "note": note,
    }


def render_header(header_rows, labels):
    if len(header_rows) == 1:
        cells = split_cells(header_rows[0])
        tds = "".join(f"<th>{latex_to_html(c, labels)}</th>" for c in cells)
        return f"<thead><tr>{tds}</tr></thead>"

    top_cells = split_cells(header_rows[0])
    sub_cells = split_cells(header_rows[1])

    top_html = []
    sub_html = []
    sub_idx = 0
    for cell in top_cells:
        match = MULTICOL_RE.match(cell)
        if match:
            span, spanner_label = match.groups()
            span = int(span)
            top_html.append(f'<th colspan="{span}">{latex_to_html(spanner_label, labels)}</th>')
            for _ in range(span):
                sub_html.append(f"<th>{latex_to_html(sub_cells[sub_idx], labels)}</th>")
                sub_idx += 1
        else:
            # Plain column: spans both header rows. The row-2 cell beneath it
            # is a blank placeholder (kept only so column counts line up), so
            # it's consumed here but never rendered.
            top_html.append(f'<th rowspan="2">{latex_to_html(cell, labels)}</th>')
            sub_idx += 1

    return f"<thead><tr>{''.join(top_html)}</tr><tr>{''.join(sub_html)}</tr></thead>"


def render_body(body_rows, aligns, labels):
    rows_html = []
    n_cols = len(aligns)
    for row in body_rows:
        cells = split_cells(row)
        # A group-header row arrives as a single spanning cell (write_apa_table
        # emits \multicolumn for these so the header text doesn't set the width
        # of the first column). Render it as one colspan'd cell rather than
        # letting the raw LaTeX fall through as text.
        if len(cells) == 1:
            match = MULTICOL_RE.match(cells[0])
            if match:
                span, spanner_label = match.groups()
                span = min(int(span), n_cols) if n_cols else int(span)
                rows_html.append(
                    f'<tr class="category-row"><td colspan="{span}" '
                    f'style="text-align:left">{latex_to_html(spanner_label, labels)}</td></tr>'
                )
                continue
        is_category_row = len(cells) > 1 and all(c == "" for c in cells[1:])
        row_class = ' class="category-row"' if is_category_row else ""

        tds = []
        for i, cell in enumerate(cells):
            align = aligns[i] if i < len(aligns) else "left"
            tds.append(f'<td style="text-align:{align}">{latex_to_html(cell, labels)}</td>')
        rows_html.append(f"<tr{row_class}>{''.join(tds)}</tr>")
    return f"<tbody>{''.join(rows_html)}</tbody>"


def render_table_section(parsed, labels, filename):
    table_html = (
        f'<table>{render_header(parsed["header_rows"], labels)}'
        f'{render_body(parsed["body_rows"], parsed["aligns"], labels)}</table>'
    )
    note_html = ""
    if parsed["note"]:
        note_html = f'<p class="note"><i>Note.</i> {latex_to_html(parsed["note"], labels)}</p>'
    return (
        f'<section id="{parsed["label"]}">'
        f'<h2>{latex_to_html(parsed["caption"], labels)}</h2>'
        f'<p class="filename">{filename}</p>'
        f'<div class="table-scroll">{table_html}</div>'
        f"{note_html}"
        f"</section>"
    )


PAGE_TEMPLATE = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>{title}</title>
<style>
  :root {{
    color-scheme: light dark;
    --bg: #ffffff; --fg: #1a1a1a; --muted: #666666;
    --border: #cccccc; --stripe: #f5f5f5; --link: #0b5fff;
  }}
  @media (prefers-color-scheme: dark) {{
    :root {{ --bg: #1a1a1a; --fg: #e8e8e8; --muted: #a0a0a0;
             --border: #444444; --stripe: #242424; --link: #6ea8ff; }}
  }}
  body {{ background: var(--bg); color: var(--fg); font-family: -apple-system, "Segoe UI", Helvetica, Arial, sans-serif;
          max-width: 900px; margin: 2rem auto; padding: 0 1.5rem; line-height: 1.5; }}
  h1 {{ font-size: 1.5rem; }}
  h2 {{ font-size: 1.1rem; margin-top: 3rem; border-top: 1px solid var(--border); padding-top: 1.5rem; }}
  .generated-note {{ color: var(--muted); font-size: 0.85rem; }}
  .filename {{ color: var(--muted); font-family: ui-monospace, Menlo, monospace; font-size: 0.8rem; margin-top: -0.5rem; }}
  nav {{ background: var(--stripe); border: 1px solid var(--border); border-radius: 6px; padding: 1rem 1.5rem; }}
  nav ol {{ margin: 0.5rem 0 0; padding-left: 1.2rem; columns: 2; }}
  nav a, .note a {{ color: var(--link); }}
  .table-scroll {{ overflow-x: auto; }}
  table {{ border-collapse: collapse; width: 100%; font-size: 0.9rem; }}
  caption {{ caption-side: top; }}
  th, td {{ padding: 0.35rem 0.7rem; border-bottom: 1px solid var(--border); white-space: nowrap; }}
  thead th {{ border-bottom: 2px solid var(--fg); vertical-align: bottom; }}
  tbody tr:last-child td {{ border-bottom: 2px solid var(--fg); }}
  tr.category-row td:first-child {{ font-weight: 600; }}
  .indent {{ display: inline-block; width: 1.5em; }}
  .radicand {{ border-top: 1px solid currentColor; padding: 0 0.15em; }}
  .note {{ color: var(--muted); font-size: 0.85rem; max-width: 70ch; white-space: normal; }}
  .note i {{ color: var(--fg); }}
  code {{ font-family: ui-monospace, Menlo, monospace; font-size: 0.9em; }}
  sup, sub {{ font-size: 0.75em; }}
</style>
</head>
<body>
<h1>{title}</h1>
<p class="generated-note">{generated_note}</p>
<nav>
<strong>Contents</strong>
<ol>
{toc}
</ol>
</nav>
{sections}
</body>
</html>
"""


def render_tables(tables_dir, table_order, output_html, title, generated_note):
    """Render every .tex fragment in tables_dir to a single HTML page.

    table_order fixes the presentation order; any .tex file present but not
    listed is appended at the end (and flagged), so a new table can't
    silently go missing. A table_order entry with no file on disk is skipped
    with a warning rather than failing outright -- the pipeline's own
    RUN_POWER=0 mode intentionally leaves the power tables ungenerated, and a
    preview of everything else is still useful without them. Returns the
    number of tables rendered.
    """
    available = {p.name for p in tables_dir.glob("*.tex")}
    missing = [f for f in table_order if f not in available]
    if missing:
        print(f"Warning: table(s) in table_order not found, skipped: {missing}")
    present_order = [f for f in table_order if f in available]

    extra = sorted(available - set(table_order))
    if extra:
        print(f"Warning: table(s) not in table_order, appended at the end: {extra}")
    filenames = present_order + extra

    parsed_tables = [(f, parse_table(tables_dir / f)) for f in filenames]
    labels = {parsed["label"]: parsed["caption"] for _, parsed in parsed_tables}

    toc = "\n".join(
        f'<li><a href="#{parsed["label"]}">{latex_to_html(parsed["caption"], labels)}</a></li>'
        for _, parsed in parsed_tables
    )
    sections = "\n".join(
        render_table_section(parsed, labels, filename) for filename, parsed in parsed_tables
    )

    output_html.write_text(
        PAGE_TEMPLATE.format(title=title, generated_note=generated_note, toc=toc, sections=sections),
        encoding="utf-8",
    )
    print(f"Wrote {output_html} ({len(parsed_tables)} tables)")
    return len(parsed_tables)
