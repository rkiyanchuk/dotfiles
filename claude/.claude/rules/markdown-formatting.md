# Markdown formatting

Applies to every project — every `.md` file (CLAUDE.md, READMEs, docs,
plans, notes).

## Line wrapping

Wrap markdown files at **80 columns**. Never write long unwrapped lines.
Break at sentence/clause boundaries where possible.

This rule does **not** apply to:

- fenced code blocks,
- tables,
- lines containing a single URL that would be broken by wrapping.

## Tables

Always format tables with **columns aligned**: pad every cell with trailing
spaces to the width of the widest cell in its column, and size the separator
row's dashes to match, so the `|` delimiters line up vertically in the
source. For example:

```
| Path            | What it does     |
| --------------- | ---------------- |
| `settings.json` | Main config.     |
| `CLAUDE.md`     | Global instructions. |
```

becomes:

```
| Path            | What it does         |
| --------------- | -------------------- |
| `settings.json` | Main config.         |
| `CLAUDE.md`     | Global instructions. |
```

Tables are exempt from the 80-column wrap rule, so widen them as needed to
keep columns aligned.
