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

## ----message = FALSE----------------------------------------------------------
library(taylor)
library(dplyr)

track_lyrics <- taylor_album_songs %>%
  select(album_name, track_name, lyrics)

track_lyrics

## -----------------------------------------------------------------------------
track_row <- which(track_lyrics$track_name == "Cruel Summer")
track_lyrics$lyrics[[track_row]]

## -----------------------------------------------------------------------------
library(tidyr)

track_lyrics %>%
  unnest(lyrics)

## -----------------------------------------------------------------------------
track_lyrics %>%
  filter(track_name == "Cruel Summer") %>%
  unnest(lyrics)

## -----------------------------------------------------------------------------
track_lyrics %>%
  filter(album_name == "Lover") %>%
  mutate(lines = vapply(lyrics, nrow, integer(1)))

