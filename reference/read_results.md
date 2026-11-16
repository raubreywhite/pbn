# Read the results object of a project version

`read_results()` reads `<home>/<version>_results/results.qs2`, the file
that
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes.

## Usage

``` r
read_results(version, home = ".")
```

## Arguments

- version:

  The version, for example `"v01"`.

- home:

  The project folder.

## Value

The results object, a named list. See
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).

## See also

Other results:
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)

## Examples

``` r
# See the example of save_results().
```
