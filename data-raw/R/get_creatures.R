get_creatures_data <- function(creatures_wiki_url) {

  creatures_wiki_page <- read_html(creatures_wiki_url)

  xpath_table <- "//table[@class='wikitable']"
  xpath_heading <- "//table[@class='wikitable']/preceding-sibling::h2"

  creatures <- html_elements(creatures_wiki_page, ".cargoTable")

  creatures |>
    html_table() |>
    pluck(1) |>
    janitor::clean_names() |>
    select(
      name,
      species,
      biome,
      base_health,
      health_scaling,
      pathfinder_type,
      pathfinder_preference,
      burn_temp,
      freeze_temp
    ) |>
    mutate(
      across(where(is.character), ~ na_if(.,"")),
      base_health = str_remove(base_health, " • .*+"),
      base_health = as.integer(base_health)
    )

}
