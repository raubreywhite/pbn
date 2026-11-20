# AGENTS.md

This project uses the R package pbn. Before you change any code, read how to use pbn: https://www.rwhite.no/pbn/articles/pbn.md

## Before you report a change

- Run `pbn::verify("vNN")` on the newest version. Continue until the verdict is PASS. Each finding of a rule names its fix.
- A PASS without the scenario `real` covers the fake scenarios only. Say so when you report.
- Add what changed and why to `CHANGELOG.md`, under the section of the version.

## What a person decides

- Propose the fake scenarios and their expected values. A person confirms them before they count. After that, do not change an expected value in an `expect_` function, or add or remove a fake scenario, without asking.
- Do not change `pbn.yml`. Ask.
- Do not freeze a version or start a new one. Ask.

## Manuscripts

- A hand edit, by a person or by an agent for a person, is saved as `manuscript/manual/v(NN+1)_<INITIALS>.docx`. NN is the version it starts from. INITIALS are those of the person responsible.
- Never edit or overwrite a file that is already in `manuscript/manual/` or `manuscript/built/`. Save a new file.
- Add a `## v(NN+1)` section to `CHANGELOG.md` with the file name, who edited it, the version it starts from, and what changed.
- `verify()` then fails on `manual_not_ahead` until a person starts the next run version. Do not start it; ask.
- A built manuscript reads `read_results()` of its own version only.
