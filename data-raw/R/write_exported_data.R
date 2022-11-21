#' Write exported data for package
#'
#' @param ... Unquoted names of existing targets objects to save. Arguments must
#'   be named (e.g., my_data = my_data).
#'
#' @return A file path to `R/sysdata.rda`.
write_exported_data <- function(...) {

  # Code from adapted from:
  # https://github.com/ropensci/targets/discussions/588#discussioncomment-1141571
  args <- list(...)
  envir <- environment()
  purrr::walk2(
    .x = names(args),
    .y = args,
    ~assign(x = .x, value = .y, envir = envir)
  )
  usethis::use_data(..., overwrite = TRUE)

  # Save exported data here
  paste0("../data/", names(args), ".rda")

}
