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
