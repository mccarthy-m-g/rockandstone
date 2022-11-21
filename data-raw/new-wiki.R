# If I want to scrape the new wiki, the code below accomplishes that

library(rvest)
library(xml2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

"https://deeprockgalactic.wiki.gg/wiki/Voicelines"

mission_control_wiki_url <- "https://deeprockgalactic.wiki.gg/wiki/Mission_Control/Quotes"

mission_control_wiki_page <- read_html(mission_control_wiki_url)

xpath_table <- "//table[@class='wikitable']"
xpath_heading <- "//table[@class='wikitable']/preceding-sibling::h2"

# By default all voicelines on the wiki page should be retrieved
mission_control <- html_elements(
  mission_control_wiki_page,
  xpath = paste0(xpath_table, " | ", xpath_heading)
)

mission_control_headings <- mission_control |>
  html_elements(".mw-headline") |>
  html_text() # or `html_attr("id")`

headings_index <- str_detect(mission_control, "h2")
groups_vector <- cumsum(headings_index)[!headings_index]
groups_vector <- c(mission_control_headings, groups_vector)[
  match(groups_vector, c(1:length(mission_control_headings), groups_vector))
]

table_groups <- split(
  mission_control[!headings_index],
  groups_vector # Replace with heading IDs
)

mission_control_voicelines <- map_dfr(
  table_groups, ~{
    map_dfr(
      .x, ~{
        html_table(.x) |>
          rename_with(~ c("upon", "quote", "audio"))
      }
    )
  },
  .id = "category"
)
