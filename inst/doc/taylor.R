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

## ----setup--------------------------------------------------------------------
library(taylor)

## -----------------------------------------------------------------------------
taylor_all_songs

## -----------------------------------------------------------------------------
taylor_albums

## -----------------------------------------------------------------------------
eras_tour_surprise

## -----------------------------------------------------------------------------
album_palettes$lover

## -----------------------------------------------------------------------------
album_compare

## ----eras-plot, eval = FALSE--------------------------------------------------
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# leg_labels <- unique(eras_tour_surprise$leg)
# leg_labels <- gsub("South America", "South\nAmerica", leg_labels)
# 
# surprise_song_count <- eras_tour_surprise %>%
#   nest(dat = -c(leg, date, city, night)) %>%
#   arrange(date) %>%
#   mutate(leg = factor(leg, levels = unique(eras_tour_surprise$leg),
#                       labels = leg_labels)) %>%
#   mutate(show_number = seq_len(n()), .after = night) %>%
#   unnest(dat) %>%
#   left_join(distinct(taylor_album_songs, track_name, album_name),
#             join_by(song == track_name),
#             relationship = "many-to-one") %>%
#   count(leg, date, city, night, show_number, album_name) %>%
#   complete(nesting(leg, date, city, night, show_number), album_name) %>%
#   mutate(n = replace_na(n, 0)) %>%
#   arrange(album_name, date, night) %>%
#   mutate(surprise_count = cumsum(n), .by = album_name) %>%
#   left_join(select(taylor_albums, album_name, album_release),
#             by = "album_name") %>%
#   mutate(surprise_count = case_when(
#     album_name == "THE TORTURED POETS DEPARTMENT" &
#       date < album_release ~ NA_integer_,
#     .default = surprise_count
#   )) %>%
#   add_row(leg = factor("Europe"), album_name = "THE TORTURED POETS DEPARTMENT",
#           show_number = 83.5, surprise_count = 0L) %>%
#   mutate(album_name = replace_na(album_name, "Other"),
#          album_group = album_name,
#          album_name = factor(album_name, c(album_levels, "Other"),
#                              labels = c(gsub("POETS DEPARTMENT",
#                                              "POETS\nDEPARTMENT",
#                                              album_levels), "Other")))
# 
# ggplot(surprise_song_count) +
#   facet_wrap(~ album_name, ncol = 3) +
#   geom_line(data = ~select(.x, -album_name),
#             aes(x = show_number, y = surprise_count, group = album_group),
#             color = "grey80", na.rm = TRUE) +
#   geom_line(aes(x = show_number, y = surprise_count, color = album_group),
#             show.legend = FALSE, linewidth = 2, na.rm = TRUE) +
#   scale_color_albums(na.value = "grey80") +
#   scale_x_continuous(breaks = c(1, seq(20, 500, 20))) +
#   labs(x = "Show", y = "Songs Played") +
#   theme_minimal() +
#   theme(strip.text.x = element_text(hjust = 0, size = 10),
#         axis.title = element_text(size = 9))

## ----eras-plot, echo = FALSE, message = FALSE, warning = FALSE----------------
library(dplyr)
library(tidyr)
library(ggplot2)

leg_labels <- unique(eras_tour_surprise$leg)
leg_labels <- gsub("South America", "South\nAmerica", leg_labels)

surprise_song_count <- eras_tour_surprise %>%
  nest(dat = -c(leg, date, city, night)) %>%
  arrange(date) %>%
  mutate(leg = factor(leg, levels = unique(eras_tour_surprise$leg),
                      labels = leg_labels)) %>%
  mutate(show_number = seq_len(n()), .after = night) %>%
  unnest(dat) %>%
  left_join(distinct(taylor_album_songs, track_name, album_name),
            join_by(song == track_name),
            relationship = "many-to-one") %>%
  count(leg, date, city, night, show_number, album_name) %>%
  complete(nesting(leg, date, city, night, show_number), album_name) %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(album_name, date, night) %>%
  mutate(surprise_count = cumsum(n), .by = album_name) %>%
  left_join(select(taylor_albums, album_name, album_release),
            by = "album_name") %>%
  mutate(surprise_count = case_when(
    album_name == "THE TORTURED POETS DEPARTMENT" &
      date < album_release ~ NA_integer_,
    .default = surprise_count
  )) %>%
  add_row(leg = factor("Europe"), album_name = "THE TORTURED POETS DEPARTMENT",
          show_number = 83.5, surprise_count = 0L) %>%
  mutate(album_name = replace_na(album_name, "Other"),
         album_group = album_name,
         album_name = factor(album_name, c(album_levels, "Other"),
                             labels = c(gsub("POETS DEPARTMENT",
                                             "POETS\nDEPARTMENT",
                                             album_levels), "Other")))

ggplot(surprise_song_count) +
  facet_wrap(~ album_name, ncol = 3) +
  geom_line(data = ~select(.x, -album_name),
            aes(x = show_number, y = surprise_count, group = album_group),
            color = "grey80", na.rm = TRUE) +
  geom_line(aes(x = show_number, y = surprise_count, color = album_group),
            show.legend = FALSE, linewidth = 2, na.rm = TRUE) +
  scale_color_albums(na.value = "grey80") +
  scale_x_continuous(breaks = c(1, seq(20, 500, 20))) +
  labs(x = "Show", y = "Songs Played") +
  theme_minimal() +
  theme(strip.text.x = element_text(hjust = 0, size = 10),
        axis.title = element_text(size = 9))

## ----eras-1989, eval = FALSE--------------------------------------------------
# library(patchwork)
# 
# missing_firsts <- tibble(date = as.Date(c("2023-11-01",
#                                           "2024-02-01",
#                                           "2024-05-01",
#                                           "2024-10-01")))
# day_ones <- surprise_song_count %>%
#   slice_min(date, by = c(leg, album_name)) %>%
#   select(leg, date, album_name) %>%
#   mutate(date = date - 1)
# 
# surprise_dat <- surprise_song_count %>%
#   bind_rows(missing_firsts) %>%
#   arrange(date) %>%
#   fill(leg, .direction = "up") %>%
#   bind_rows(day_ones) %>%
#   arrange(album_name, date) %>%
#   group_by(album_name) %>%
#   fill(surprise_count, .direction = "down")
# 
# tour1 <- surprise_dat %>%
#   filter(leg %in% c("North America (Leg 1)", "South\nAmerica")) %>%
#   ggplot() +
#   facet_grid(cols = vars(leg), scales = "free_x", space = "free_x") +
#   geom_line(aes(x = date, y = surprise_count, group = album_name),
#             color = "grey80", na.rm = TRUE) +
#   geom_line(data = ~filter(.x, album_name == "1989 (Taylor's Version)"),
#             aes(x = date, y = surprise_count, color = album_name),
#             show.legend = FALSE, linewidth = 2, na.rm = TRUE) +
#   scale_color_albums() +
#   scale_x_date(breaks = "month", date_labels = "%b\n%Y", expand = c(.02, .02)) +
#   expand_limits(y = c(0, 37)) +
#   labs(x = NULL, y = "Songs Played") +
#   theme_minimal() +
#   theme(strip.text.x = element_text(hjust = 0, size = 10),
#         axis.title = element_text(size = 9))
# 
# tour2 <- surprise_dat %>%
#   filter(!leg %in% c("North America (Leg 1)", "South\nAmerica")) %>%
#   ggplot() +
#   facet_grid(cols = vars(leg), scales = "free_x", space = "free_x") +
#   geom_line(aes(x = date, y = surprise_count, group = album_name),
#             color = "grey80", na.rm = TRUE) +
#   geom_line(data = ~filter(.x, album_name == "1989 (Taylor's Version)"),
#             aes(x = date, y = surprise_count, color = album_name),
#             show.legend = FALSE, linewidth = 2, na.rm = TRUE) +
#   scale_color_albums() +
#   scale_x_date(breaks = "month", date_labels = "%b\n%Y", expand = c(.02, .02)) +
#   expand_limits(y = c(0, 37)) +
#   labs(x = NULL, y = "Songs Played") +
#   theme_minimal() +
#   theme(strip.text.x = element_text(hjust = 0, size = 10),
#         axis.title = element_text(size = 9))
# 
# tour1 / tour2 + plot_layout(axes = "collect")

## ----eras-1989, echo = FALSE, message = FALSE, warning = FALSE----------------
library(patchwork)

missing_firsts <- tibble(date = as.Date(c("2023-11-01",
                                          "2024-02-01",
                                          "2024-05-01",
                                          "2024-10-01")))
day_ones <- surprise_song_count %>%
  slice_min(date, by = c(leg, album_name)) %>%
  select(leg, date, album_name) %>%
  mutate(date = date - 1)

surprise_dat <- surprise_song_count %>%
  bind_rows(missing_firsts) %>%
  arrange(date) %>%
  fill(leg, .direction = "up") %>%
  bind_rows(day_ones) %>%
  arrange(album_name, date) %>%
  group_by(album_name) %>%
  fill(surprise_count, .direction = "down")

tour1 <- surprise_dat %>%
  filter(leg %in% c("North America (Leg 1)", "South\nAmerica")) %>%
  ggplot() +
  facet_grid(cols = vars(leg), scales = "free_x", space = "free_x") +
  geom_line(aes(x = date, y = surprise_count, group = album_name),
            color = "grey80", na.rm = TRUE) +
  geom_line(data = ~filter(.x, album_name == "1989 (Taylor's Version)"),
            aes(x = date, y = surprise_count, color = album_name),
            show.legend = FALSE, linewidth = 2, na.rm = TRUE) +
  scale_color_albums() +
  scale_x_date(breaks = "month", date_labels = "%b\n%Y", expand = c(.02, .02)) +
  expand_limits(y = c(0, 37)) +
  labs(x = NULL, y = "Songs Played") +
  theme_minimal() +
  theme(strip.text.x = element_text(hjust = 0, size = 10),
        axis.title = element_text(size = 9))

tour2 <- surprise_dat %>%
  filter(!leg %in% c("North America (Leg 1)", "South\nAmerica")) %>%
  ggplot() +
  facet_grid(cols = vars(leg), scales = "free_x", space = "free_x") +
  geom_line(aes(x = date, y = surprise_count, group = album_name),
            color = "grey80", na.rm = TRUE) +
  geom_line(data = ~filter(.x, album_name == "1989 (Taylor's Version)"),
            aes(x = date, y = surprise_count, color = album_name),
            show.legend = FALSE, linewidth = 2, na.rm = TRUE) +
  scale_color_albums() +
  scale_x_date(breaks = "month", date_labels = "%b\n%Y", expand = c(.02, .02)) +
  expand_limits(y = c(0, 37)) +
  labs(x = NULL, y = "Songs Played") +
  theme_minimal() +
  theme(strip.text.x = element_text(hjust = 0, size = 10),
        axis.title = element_text(size = 9))

tour1 / tour2 + plot_layout(axes = "collect")

## ----examples, echo = FALSE, results = "asis", eval = identical(Sys.getenv("IN_PKGDOWN"), "true")----
# examples <- read.csv("data/example-uses.csv")
# cells <- paste("<td>",
#                paste0("  <a href=\"", examples$href, "\">"),
#                paste0("    <img src=\"", examples$preview, "\" ",
#                       "alt=\"", examples$description, "\" width=\"100%\"/>"),
#                "  </a>",
#                "</td>",
#                sep = "\n")
# 
# needed_rows <- ceiling(length(cells) / 3)
# rows <- vapply(seq_len(needed_rows),
#                function(x) {
#                  paste("<tr>",
#                        paste(cells[((x * 3) - 2):(x * 3)], collapse = "\n"),
#                        "</tr>",
#                        sep = "\n")
#                },
#                character(1))
# 
# tab <- paste("<table class=\"taylor-examples\" width=\"100%\">",
#              paste(rows, collapse = "\n"),
#              "</table>",
#              sep = "\n")
# 
# cat(tab)

