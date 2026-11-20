# List the installed Table 1 styles

`table1_styles()` gives the names of the Table 1 styles that pbn
installs. Give one of these names to the argument `style` of
[`table1()`](https://www.rwhite.no/pbn/reference/table1.md).

## Usage

``` r
table1_styles()
```

## Value

A character vector of style names, such as `"name_2026_12_01"`, sorted
in the C locale. It is `character(0)` when pbn installs no style.

## See also

Other shared helpers:
[`table1()`](https://www.rwhite.no/pbn/reference/table1.md)

## Examples

``` r
pbn::table1_styles()
#> character(0)
```
