test_that("selecting a voiceline works", {
  # No negative voicelines exist for this category
  expect_error(select_voiceline("salute", "negative"))
})

test_that("playing a voiceline returns NULL", {
  expect_equal(rockandstone(), NULL)
  expect_equal(
    play_voiceline(
      system.file("audio", select_voiceline(), package = "rockandstone")
    ),
    NULL
  )
})

test_that("voiceline categories are the correct type", {
  expect_type(voiceline_categories(), "character")
})
