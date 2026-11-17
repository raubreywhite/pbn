# Read the results object of a project version

`read_results()` reads the `results.qs2` of one scenario, the file that
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
`<home>/<version>_results/<scenario>/sim_<kkkk>/results.qs2`, where
`<kkkk>` is `k` with four digits.

`read_results()` stops when
`<home>/<version>_results/<scenario>/check.csv` exists and holds a value
of `pass` that is not `TRUE`, also when it reads one simulation. It also
stops when that file holds no assertion, or has no column `pass` that R
can read.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
still writes such results, so that you can find the fault. The error
names the file and how to read it with
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html).

A manuscript MUST read the real results only.
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
fails a call in `manuscript/built/` that gives a `scenario` other than
`"real"`.

## See also

Other results:
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)

## Examples

``` r
# See the example of save_results().
```
