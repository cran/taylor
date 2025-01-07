## ----chunk-options, include=FALSE---------------------------------------------
library(taylor)

if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
  tiny_width <- small_width <- med_width <- 7
  large_width <- 9
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

## ----warning = FALSE, message = FALSE, fig.alt = "A bar graph with song valence on the x-axis and song title on the y-axis. Each bar is a different color, with colors following a rainbow-like palette."----
library(taylor)
library(ggplot2)

evermore <- subset(taylor_album_songs, album_name == "evermore")
evermore$track_name <- factor(evermore$track_name, levels = evermore$track_name)

p <- ggplot(evermore, aes(x = valence, y = track_name, fill = track_name)) +
  geom_col(show.legend = FALSE) +
  expand_limits(x = c(0, 1)) +
  labs(y = NULL) +
  theme_minimal()
p

## ----warning = FALSE, fig.alt = "The same bar graph as the previous figure, but the colors of the bars have been updated to use a palette inspired by the album cover of evermore. The palette starts with a dark brown, moving to orange, and finally to a light gray."----
p + scale_fill_taylor_d(album = "evermore")

## ----warning = FALSE, fig.alt = "The same bar graph as the previous two figures, but the colros of the bars have been updated to use a palette inspired by the album cover of Speak Now. The palette starts with a dark burnt red and then moves to purple and finally a light pink."----
p + scale_fill_taylor_d(album = "Speak Now")

## ----warning = FALSE, fig.alt = "A scatter plot with bill length on the x-axis and bill depth on the y-axis. The shape and color of the points correspond to the species of penguin, with colors derived from the color palette for Lover."----
library(palmerpenguins)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(shape = species, color = species), size = 3) +
  scale_color_taylor_d(album = "Lover") +
  theme_minimal()

## ----warning = FALSE, fig.alt = "A heatmap showing a positive relationship between the waiting time between eruptions and the length of eruptions at the Old Faithful geyser. The heat map is colored using the palette based on Fearless (Taylor's Version), which moves from a dark golden brown for low density combinations up to bright gold for high density combinations."----
p <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  theme_minimal()

p + scale_fill_taylor_c(album = "Fearless (Taylor's Version)")

## ----warning = FALSE, fig.alt = c("The same heatmap as the previous figure, but the color palette has been changed to use colors inspired by reputation. Less frequent values are show in dark grey, and more common values appear as white.", "The same heatmap as the previous two figures, but the color palette has been changed to use colors inspired by folklore. Less frequent values are show in dark grey, and more common values appear as white.")----
p + scale_fill_taylor_c(album = "reputation")
p + scale_fill_taylor_c(album = "folklore")

## ----warning = FALSE, fig.alt = "The same heat map as the previous figures, but instead of a smooth continuous color scale, values have been binned into four categories, with color inspired by the evermore album cover."----
p + scale_fill_taylor_b(album = "evermore")

## -----------------------------------------------------------------------------
taylor_albums

## ----warning = FALSE, fig.alt = "A bar graph with the Metacritic rating on the x-axis and the album name on the y-axis. Color has been assigned to each bar such that each bar is filled with a color. The colors follow the ggplot2 default, resulting in a rainbow-like palette."----
metacritic <- taylor_albums

# Not Taylor's best work, so we'll give it a 72
metacritic$metacritic_score[1] <- 72L
metacritic <- subset(metacritic, !is.na(metacritic_score))
metacritic$album_name <- factor(metacritic$album_name,
                                levels = album_levels)

ggplot(metacritic, aes(x = metacritic_score, y = album_name)) +
  geom_col(aes(fill = album_name), show.legend = FALSE) +
  labs(x = "Metacritic Rating", y = NULL) +
  theme_minimal()

## ----warning = FALSE, fig.alt = "The same bar graph as the previous image, instead of using the default colors, each bar for each album is filled with a color from that album's cover."----
ggplot(metacritic, aes(x = metacritic_score, y = album_name)) +
  geom_col(aes(fill = album_name), show.legend = FALSE) +
  scale_fill_albums() +
  labs(x = "Metacritic Rating", y = NULL) +
  theme_minimal()

## ----warning = FALSE, fig.alt = "The same bar graph as the previous image, but only showing five albums, and the ordering of the y-axis has been made random. However, the fill of the bar for each album still corresponds to that album's cover."----
rand_critic <- metacritic[sample(seq_len(nrow(metacritic)), 5), ]
rand_critic$album_name <- factor(rand_critic$album_name,
                                 levels = sample(rand_critic$album_name,
                                                 size = nrow(rand_critic)))

ggplot(rand_critic, aes(x = metacritic_score, y = album_name)) +
  geom_col(aes(fill = album_name), show.legend = FALSE) +
  scale_fill_albums() +
  labs(x = "Metacritic Rating", y = NULL) +
  theme_minimal()

## ----warning = FALSE, fig.alt = "A bar graph with the Metacritic rating on the x-axis and the album name on the y-axis. Color has been assigned to each bar such that each bar is filled with a color. The colors for each bar a based on the ablum cover. On y-axis, evermore, folklore, and repuation, have been spelled in title case, rather than lower case, resulting in no bar showing for these albums."----
upper_critic <- metacritic
upper_critic$album_name <- factor(upper_critic$album_name,
                                  levels = album_levels,
                                  labels = title_case(album_levels))

ggplot(upper_critic, aes(x = metacritic_score, y = album_name)) +
  geom_col(aes(fill = album_name), show.legend = FALSE) +
  scale_fill_albums() +
  labs(x = "Metacritic Rating", y = NULL) +
  theme_minimal()

