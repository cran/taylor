## ----chunk-options, include=FALSE---------------------------------------------
if (requireNamespace("pkgdown", quietly = TRUE) && pkgdown::in_pkgdown()) {
  tiny_width <- small_width <- med_width <- 7
  large_width <- 8
} else {
  tiny_width <- small_width <- med_width <- 5
  large_width <- 5.5
}

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.asp = 0.618,
  fig.width = small_width,
  fig.align = "center",
  out.width = "90%"
)

if (capabilities("cairo") && Sys.info()[['sysname']] != "Darwin") {
  knitr::opts_chunk$set(
    dev = "png",
    dev.args = list(type = "cairo")
  )
}

## -----------------------------------------------------------------------------
library(taylor)

my_pal <- color_palette(c("firebrick", "turquoise", "#0051ba"))
my_pal

## -----------------------------------------------------------------------------
my_big_pal <- color_palette(my_pal, n = 10)
my_big_pal

## -----------------------------------------------------------------------------
my_small_pal <- color_palette(my_big_pal, n = 5)
my_small_pal

## -----------------------------------------------------------------------------
album_palettes

## -----------------------------------------------------------------------------
album_palettes$fearless_tv

## -----------------------------------------------------------------------------
album_compare

## ----message = FALSE, fig.alt = "A heatmap showing a positive relationship between the waiting time between eruptions and the length of eruptions at the Old Faithful geyser. The heat map is colored using the palette based on Fearless (Taylor's Version), which moves from a dark golden brown for low density combinations up to bright gold for high density combinations."----
library(ggplot2)

p <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  theme_minimal()

p + scale_fill_taylor_c(album = "Fearless (Taylor's Version)")

## ----fig.alt = "The same heatmap as the previous figure, but instead of the fill using a palette based on Fearless (Taylor's Version), the color palette goes from light green to dark green."----
green_pal <- color_palette(c("#E5F5E0", "#A1D99B", "#31A354"))
green_pal

ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_gradientn(colors = green_pal) +
  theme_minimal()

## ----message = FALSE, warning = FALSE, fig.alt = "A scatter plot with bill length on the x-axis and bill depth on the y-axis. The shape and color of the points correspond to the species of penguin, with colors derived from our custom color palette. Adelie penguins are shown in red circles, Chinstrap penguins in yellow triangles, and Gentoo penguins in blue squares."----
library(palmerpenguins)

penguin_pal <- color_palette(c(Adelie = "firebrick",
                               Chinstrap = "goldenrod",
                               Gentoo = "navy"))
penguin_pal

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(shape = species, color = species), size = 3) +
  scale_color_manual(values = penguin_pal) +
  theme_minimal()

