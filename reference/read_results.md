# Read the results object of a project version

`read_results()` reads the `results.qs2` of one scenario, and the
`results_gitexclude.qs2` beside it, the files that
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes. The default reads the real results.

## Usage

``` r
read_results(version, scenario = "real", sim = NULL, home = ".")
```

## Arguments

- version:

  The version, for example `"v01"`.

- scenario:

  The scenario. The default is `"real"`.

- sim:

  `NULL`, or the number of one simulation.

- home:

  The project folder.

## Value

The results object, a named list. See
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).

## Details

With `sim = NULL`, `read_results()` reads
`<home>/<version>_results/<scenario>/results.qs2`. That is the results
of a scenario with 1 simulation, or the simulations that
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stacked. With a number `k`, it reads
`<home>/<version>_results/<scenario>/sims_gitexclude/sim_<kkkk>.qs2`,
where `<kkkk>` is `k` with four digits. git ignores that folder, so a
clone of the repository has no such file. Then `read_results()` stops,
and the error says to run the scenario with
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
For a scenario with 1 simulation, `sim = 1` reads its `results.qs2`, the
results of that simulation. The section The files of
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
shows each file.

In a one-file project, `<scenario>` is `fake` for the scenario `"real"`.
So `read_results(version)` there reads `<version>_results/fake/`. The
section The folder fake/ of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
gives the rule.

`read_results()` stops when
`<home>/<version>_results/<scenario>/check.csv` exists and holds a value
of `pass` that is not `TRUE`, also when it reads one simulation. It also
stops when that file holds no assertion, or has no column `pass` that R
can read.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
still writes such results, so that you can find the fault. The error
names the file and how to read it with
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html).

For `"real"`, `read_results()` also stops when
`manifest$failed_fake_checks` names a fake scenario.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes into it each fake scenario of the same run with a failed
assertion. The real data and the fake scenarios run through the same
code. So a fake scenario with a failed assertion can show a fault in the
code of the real results. The error names each such scenario, the path
of its `check.csv` and the fix.

The run that writes the real results writes this record. A run of
`"real"` alone records no fake scenario with a failed assertion. A later
run of a fake scenario alone does not change the record of the real
results. Real results of pbn 2026.11.24 or older have no
`failed_fake_checks`, and `read_results()` reads them as before.

`read_results()` returns one list with the elements of both files. The
order is that of a results object: `manifest`, `exclusions`, and the
analysis kinds in the order of `manifest$analyses`. These rules apply to
`results_gitexclude.qs2`, and to `sim_<kkkk>_gitexclude.qs2` for one
simulation:

- When the manifest names kinds in it and the file is missing, it
  returns the elements of the results file. A clone of the repository
  has no such file. It gives one warning that names the missing kinds
  and the file.

- It stops when the file holds other kinds than the manifest names, also
  when the manifest names none.

A manuscript MUST read the real results only.
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
fails a call in `manuscript/built/` that gives a `scenario` other than
`"real"`.

## See also

Other results:
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
[`render_run()`](https://www.rwhite.no/pbn/reference/render_run.md),
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)

## Examples

``` r
# See the example of save_results().
```
