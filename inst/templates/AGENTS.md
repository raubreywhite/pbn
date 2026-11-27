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

## Results

- In a one-file project, the run on this computer writes the scenario `real` to `vNN_results/fake/`. A one-file project is one whose `vNN_run.R` gives `one_file_home` to `pbn::initialize_project()`.
- `fake/` stands in for the real results, and its numbers are fake. It is not one of the fake scenarios, which have planted effects.
- `vNN_results/real/` holds only the real results that come back from the server. In a one-file project, that is `export_blocks.txt`, which a person saves, and the `blocks.qs2` that `pbn::import_blocks()` writes from it. Write nothing else into it.

## Manuscripts

- Save your own draft as `manuscript/draft/vNN_<INITIALS>.docx`, and its supporting information as `manuscript/draft/vNN_<INITIALS>_si.docx`. NN is the version it starts from. A draft is outside the version counter, so you MAY overwrite it.
- A hand edit, by a person or by an agent for a person, is saved as `manuscript/manual/v(NN+1)_<INITIALS>.docx`. NN is the version it starts from. INITIALS are those of the person responsible.
- Never edit or overwrite a file that is already in `manuscript/manual/` or `manuscript/built/`. Save a new file.
- Add a `## v(NN+1)` section to `CHANGELOG.md` with the file name, who edited it, the version it starts from, and what changed.
- `verify()` then fails on `manual_not_ahead` until a person starts the next run version. Do not start it; ask.
- A built manuscript reads its own version only. In a one-file project it reads `pbn::read_blocks()` or `pbn::txt()`, and in another project `read_results()` or `pbn::txt()`.
- A one-file project has fake numbers until `pbn::import_blocks()` writes `vNN_results/real/blocks.qs2` from real pages. Until then, save its manuscript in `manuscript/draft/`.
