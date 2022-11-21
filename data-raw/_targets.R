# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
library(rvest)
library(xml2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

# Set target options:
tar_option_set(
  packages = c(
    "rvest",
    "xml2",
    "purrr",
    "dplyr",
    "tidyr",
    "stringr"
  ), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# Load the R scripts with your custom functions:
for (file in list.files("R", full.names = TRUE)) source(file)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    voicelines_wiki_url,
    # FIXME: wiki moved to https://deeprockgalactic.wiki.gg/wiki/Voicelines
    # after August 2022, so if new voicelines are to be added they need to come
    # from there, and some of the rvest code in get_voicelines_table() may need
    # to be changed.
    "https://deeprockgalactic.fandom.com/wiki/Voicelines?mobileaction=toggle_view_mobile"
  ),
  tar_target(
    voicelines_all,
    get_voicelines_table(voicelines_wiki_url, "all")
  ),
  tar_target(
    voicelines_removed,
    get_voicelines_table(voicelines_wiki_url, "removed")
  ),
  tar_target(
    voicelines_final,
    anti_join(
      voicelines_all,
      voicelines_removed,
      by = c("upon", "quote", "audio", "file", "path", "nsfw", "url")
    )
  ),
  tar_target(
    voicelines_files_ogg,
    download_voicelines(voicelines_final$url, voicelines_final$path),
    pattern = map(voicelines_final),
    format = "file"
  ),
  tar_target(
    internal_data,
    write_internal_data(voicelines_final = voicelines_final),
    format = "file"
  )
)
