#' Download voiceline files from wiki
#'
#' @param url The download URL.
#' @param destination The destination to save to.
#'
#' @return Character vector of the file location.
download_voicelines <- function(url, destination) {

  destination <- paste0("../", destination)

  download.file(
    url,
    destination
  )

  # Targets needs the file path in order to track it.
  destination

}
