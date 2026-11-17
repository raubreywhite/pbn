# Mark a figure of fake data

`stamp_fake()` adds a large diagonal label "FAKE DATA" to a ggplot when
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
calls the output function for a fake scenario. Otherwise it returns the
figure as it is.

## Usage

``` r
stamp_fake(q)
```

## Arguments

- q:

  A ggplot.

## Value

`q`, with the label for a fake scenario.

## Details

Call it in the figure code of the output function, before the figure is
saved: `q <- pbn::stamp_fake(q)`. For `"real"`, and outside a call of
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md),
it returns `q` unchanged. The label is on each panel.

For a fake scenario, `stamp_fake()` needs the package ggplot2. It stops
with an error when ggplot2 is not installed, and when `q` is not a
ggplot.

## See also

Other output:
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)

## Examples

``` r
# Outside make_outputs(), stamp_fake() returns the figure as it is.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  q <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y))
  identical(pbn::stamp_fake(q), q)
}
#> [1] TRUE
```
