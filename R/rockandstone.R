#' Play a random voiceline
#'
#' `rockandstone()` plays a random voiceline. This is useful, for example, to
#' to get notified when a script is finished.
#'
#' @param category An optional character vector specifying the voiceline
#'   categories to include for playback. See `voiceline_categories()` for
#'   options.
#' @param sentiment An optional character vector specifying the voiceline
#'   sentiments to include for playback. Options are "positive", "negative",
#'   and "neutral".
#' @param nsfw A logical specifying whether to include "Not Safe For Work"
#'   voicelines that include profanity. Default is `FALSE`.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#'
#' # By default the voiceline pool includes all "Safe For Work" voicelines
#' rockandstone()
#'
#' # "Not Safe For Work Voicelines" (with profanity) are opt-in
#' rockandstone(nsfw = TRUE)
#'
#' # The voiceline pool can be reduced to certain categories and/or sentiments
#' # to suit your use case
#' rockandstone(category = "salute")
#' rockandstone(sentiment = "negative")
rockandstone <- function(
  category = NULL,
  sentiment = NULL,
  nsfw = FALSE
) {

  voiceline <- select_voiceline(category, sentiment, nsfw)

  play_voiceline(
    system.file("audio", voiceline, package = "rockandstone")
  )

  # TODO: Send an optional desktop notification, email notification, text
  # message, etc. For text/email, make the sender "Mission Control".

}

#' Select a random voiceline
#'
#' Same arguments as `rockandstone()`.
#'
#' @keywords internal
select_voiceline <- function(
  category = NULL,
  sentiment = NULL,
  nsfw = FALSE
) {
  # Note: `voicelines_final` is internal data stored in `R/sysdata.rda`.

  # Profanity should be excluded by default to be user-friendly.
  if (!nsfw) voicelines_final <- dplyr::filter(voicelines_final, !nsfw)

  # All voicelines should be used by default. The voiceline pool should only be
  # reduced to specific categories if the user specifies them.
  if (!is.null(category)) {
    categories <- rlang::arg_match(
      category, voiceline_categories(), multiple = TRUE
    )
    voicelines_final <- dplyr::filter(
      voicelines_final, category %in% categories
    )
  }

  # All voicelines should be used by default. The voiceline pool should only be
  # reduced to specific sentiments if the user specifies them.
  if (!is.null(sentiment)) {
    sentiments <- rlang::arg_match(
      sentiment, c("positive", "negative", "neutral"), multiple = TRUE
    )
    voicelines_final <- dplyr::filter(
      voicelines_final, sentiment %in% sentiments
    )
  }

  # Some combinations of argument parameters will not return any voicelines, so
  # this needs to be checked explicitly.
  if(nrow(voicelines_final) == 0) {
    # FIXME: Instead of throwing an error, consider making this a warning, and
    # return the unaltered `voicelines_final` data instead.
    rlang::abort(
      "No voicelines are available for this combination of argument parameters."
    )
  }

  # After optional filtering of the voiceline pool, a single random voiceline
  # should be returned for playback.
  voicelines_final |>
    dplyr::slice_sample(n = 1) |>
    dplyr::pull(file)

}

#' Play voicelines
#'
#' @param file_ogg The .ogg file path
#'
#' @return `NULL`
#' @keywords internal
play_voiceline <- function(file_ogg) {

  # Storing .wav versions of the voicelines takes over 120 MB of space, which
  # is too big for an R package. Meanwhile, the .ogg files only take up around
  # 9 MB of space. Since the voicelines are short, they can just be converted
  # on the fly as a temporary .wav file which is removed after playback. They
  # still play instantaneously so it isn't perceptible to the user.
  file_wav <- stringr::str_replace(file_ogg, ".ogg", ".wav")

  withr::with_tempfile(file_wav, {
    file_wav <- av::av_audio_convert(
      file_ogg,
      file_wav,
      verbose = FALSE
    )
    beepr::beep(file_wav)
  })

  invisible(NULL)

}


#' Get voiceline categories
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' # Get voiceline categories
#' voiceline_categories()
voiceline_categories <- function() {
  unique(voicelines_final$category)
}
