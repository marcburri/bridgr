# bridgr Plot Theme and Color Scales

Plot styling helpers for `bridgr` graphics, based on the visual defaults
used in the `reviser` package.

## Usage

``` r
theme_bridgr(
  base_size = 12,
  legend.position = "bottom",
  legend.direction = "horizontal"
)

colors_bridgr()

scale_color_bridgr(...)

scale_fill_bridgr(...)
```

## Arguments

- base_size:

  Base text size for the plot theme.

- legend.position:

  Legend position passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- legend.direction:

  Legend direction passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Additional arguments passed to the ggplot2 scale functions.

## Value

A ggplot2 theme, color palette, or scale.
