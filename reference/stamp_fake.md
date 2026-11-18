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

  A ggplot, or a patchwork of ggplots.

## Value

`q`, with the label for a fake scenario.

## Details

Call it in the figure code of the output function, before the figure is
saved: `q <- pbn::stamp_fake(q)`. For `"real"` on real data, and outside
a call of
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md),
it returns `q` unchanged. For `"real"` whose manifest has
`fake_real_data` `TRUE`, it adds the label.

On a ggplot, the label is on each panel. On a patchwork, it is one label
over the whole figure. `stamp_fake()` wraps the patchwork in one element
with
[`patchwork::wrap_elements()`](https://patchwork.data-imaginist.com/reference/wrap_elements.html),
and puts the label on it with
[`patchwork::inset_element()`](https://patchwork.data-imaginist.com/reference/inset_element.html).
The label has no background, so each panel stays visible. Call
`stamp_fake()` last, just before the figure is saved. A `+` after it
applies to the label, and a theme with a background then hides the
figure.

For a fake scenario, `stamp_fake()` needs the package ggplot2, and
patchwork for a patchwork. It stops when the package is not installed.
It also stops when `q` is not a ggplot or a patchwork, for example a
grob or a gtable of `gridExtra::arrangeGrob()`. Stamp each ggplot before
you combine it in such an object.

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
