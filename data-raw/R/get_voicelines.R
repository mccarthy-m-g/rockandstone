#' Get wiki voicelines table
#'
#' @param voicelines_wiki_page URL of the mobile wiki page for voicelines.
#' @param which A specific voicelines table to return. Currently supports
#'   "removed".
#'
#' @return A tibble.
get_voicelines_table <- function(voicelines_wiki_url, which = NULL) {

  voicelines_wiki_page <- read_html(voicelines_wiki_url)

  # By default all voicelines on the wiki page should be retrieved
  voicelines <- html_elements(voicelines_wiki_page, "article")

  # Removed voicelines should not be shipped with the package, so we need a way
  # to extract them separately. Later an anti-join can be used to remove them
  # from the table of voicelines.
  if (which == "removed") {
    voicelines <- html_elements(
      voicelines_wiki_page,
      "#Removed_Voicelines-collapsible-section"
    )
  }

  voicelines <- voicelines |>
    html_table() |>
    pluck(1) |>
    # When getting the table for all voicelines, an empty column is added. Make
    # sure only the columns we need are included.
    select(upon = Upon, quote = Quote, audio = Audio) |>
    # The table is a little bit messy on import, with some redundant cells which
    # can be identified by the cells containing values they should not.
    filter(
      !if_any(everything(), is.na),
      !upon %in% c("Upon", "Quote", "Audio", ""),
      !quote %in% c("Upon", "Quote", "Audio", ""),
      !audio %in% c("Upon", "Quote", "Audio", "")
    ) |>
    # Some Audio cells in the HTML table contain URLs for more than one file
    # because the quote is the same for both files; these cells need to be
    # separated into multiple rows. In the table, these cells just end up
    # concatenating the URLS one after the other, so they can be separated by the
    # (empty) "space" between them.
    separate_rows(audio, sep = "(?<=.ogg)(.?)(?=https)") |>
    mutate(
      file = str_extract(audio, "(?<=File:).*+"),
      path = paste0("inst/audio/", file),
      # Some strings have line breaks in them that should not be there.
      upon = str_replace_all(upon, "\n", " "),
      # Some users might want to use this package without hearing any profanity,
      # a "not safe for work" flag can be added to data which can be used by
      # functions and arguments further down the pipeline.
      nsfw = str_detect(
        quote,
        regex("(arse|ass|bastard|damn)", ignore_case = TRUE)
      )
    )

  # The links to the audio files in the "audio" column of the previous table do
  # not point to the correct location. The correct links are stored in the audio
  # elements on the wiki page.
  voicelines_files <- voicelines_wiki_page |>
    html_elements("audio") |>
    html_attr("src") |>
    as_tibble() |>
    rename(url = value) |>
    mutate(file = str_extract(url, "[\\w\\-]+(?:.ogg)"))

  voicelines <- voicelines |>
    # Include the correct links in the table so the files can be downloaded later
    # on in the pipeline.
    left_join(voicelines_files, by = "file") |>
    # Voicelines should be given sentiments (positive, neutral, negative) so
    # they can be used for different contexts (e.g., code finished running,
    # error thrown, etc.). Same thing with categories (based on "upon").
    left_join(voicelines_labelled(), by = c("quote", "upon"))

  # Some of the voicelines would feel out of place here, so they can be removed.
  voicelines_1 <- filter(
    voicelines,
    !upon %in% remove_category(),
    !quote %in% remove_voiceline()
  )

  # However, a select number of voicelines from the removed categories are
  # fitting, so they should stay in.
  voicelines_2 <- filter(
    voicelines,
    quote %in% keep_voiceline()
  )

  bind_rows(voicelines_1, voicelines_2)

}

# Helpers ---------------------------------------------------------------------
remove_category <- function() {
  c(
    "When Dwarf gets Downed",
    "Calling the M.U.L.E.",
    "Depositing into the M.U.L.E.",
    'During the "Extraction" Sequence',
    "Ordering a Resupply",
    "Resupply Arrival",
    "Being Grabbed by a Mactera Grabber",
    "Being Grabbed by a Cave Leech",
    "Shared by Both",
    "Dwarf Shout in Space Rig",
    "Grappling",
    "Placing a Satchel Charge",
    "Throwing a Cluster Grenade",
    "Throwing a Cryo Grenade",
    "Aquarq",
    "Dystrum",
    "Enor Pearl",
    "Gold",
    "Jadiz",
    "Nitra"
  )
}

remove_voiceline <- function() {
  c(
    # When the Drop Pod Leaves and the Dwarf is Left Behind ---
    "Get back here you bastards!"
  )
}

keep_voiceline <- function() {
  c(
    # Calling the M.U.L.E. ---
    "Just take your sweet time old lady! I've got all day!",
    "Deep Rock seriously need to invest in some better equipment!",
    # Depositing into the M.U.L.E. ---
    "You're a good mule, Molly!",
    "Another day at the office!",
    "Getting closer to our quota!",
    "Bum bum bum!",
    "Mmm... Could really go for a cold brew right now!",
    "This is thirsty work!",
    "<Yawn> I could need a good rest!",
    "When we get back... It's sandwich time!",
    "Mining is hard work, good thing we're dwarves!",
    # Being Grabbed by a Mactera Grabber ---
    "Noooooo!",
    "HELP!",
    # Grappling ---
    "From A to D, skipping B and C!",
    "Wooosh!",
    "Watch me fly!",
    "Ludicrous speed!"
  )
}

voicelines_labelled <- function() {
  tibble::tribble(
    ~quote,                                                                                     ~sentiment, ~category, ~upon,
    # A Dwarf Salute --------------------------------------------------------------------------------------------------
    "Rock on!",                                                                                 "positive", "salute", "A Dwarf Salute",
    "Rock and Stone... Yeeaaahhh!",                                                             "positive", "salute", "A Dwarf Salute",
    "Rock and Stone forever!",                                                                  "positive", "salute", "A Dwarf Salute",
    "ROCK... AND... STONE!",                                                                    "positive", "salute", "A Dwarf Salute",
    "Rock and Stone!",                                                                          "positive", "salute", "A Dwarf Salute",
    "Rock and Stone!",                                                                          "positive", "salute", "A Dwarf Salute",
    "For Rock and Stone!",                                                                      "positive", "salute", "A Dwarf Salute",
    "We are unbreakable!",                                                                      "positive", "salute", "A Dwarf Salute",
    "Rock and roll!",                                                                           "positive", "salute", "A Dwarf Salute",
    "Rock and roll and stone!",                                                                 "positive", "salute", "A Dwarf Salute",
    "That's it lads! Rock and Stone!",                                                          "positive", "salute", "A Dwarf Salute",
    "Like that! Rock and Stone!",                                                               "positive", "salute", "A Dwarf Salute",
    "Yeaahhh! Rock and Stone!",                                                                 "positive", "salute", "A Dwarf Salute",
    "None can stand before us!",                                                                "positive", "salute", "A Dwarf Salute",
    "Rock solid!",                                                                              "positive", "salute", "A Dwarf Salute",
    "Rock solid!",                                                                              "positive", "salute", "A Dwarf Salute",
    "Stone and Rock! ...Oh, wait...",                                                           "positive", "salute", "A Dwarf Salute",
    "Come on guys! Rock and Stone!",                                                            "positive", "salute", "A Dwarf Salute",
    "If you don't Rock and Stone, you ain't comin' home!",                                      "positive", "salute", "A Dwarf Salute",
    "We fight for Rock and Stone!",                                                             "positive", "salute", "A Dwarf Salute",
    "We rock!",                                                                                 "positive", "salute", "A Dwarf Salute",
    "Rock and Stone everyone!",                                                                 "positive", "salute", "A Dwarf Salute",
    "Stone.",                                                                                   "positive", "salute", "A Dwarf Salute",
    "Yeah, yeah, Rock and Stone.",                                                              "positive", "salute", "A Dwarf Salute",
    "Rock and Stone in the Heart!",                                                             "positive", "salute", "A Dwarf Salute",
    "For Teamwork!",                                                                            "positive", "salute", "A Dwarf Salute",
    "Did I hear a Rock and Stone?",                                                             "positive", "salute", "A Dwarf Salute",
    "Rock and Stone!",                                                                          "positive", "salute", "A Dwarf Salute",
    "Rock and Stone!",                                                                          "positive", "salute", "A Dwarf Salute",
    "Rock and Stone, Brother!",                                                                 "positive", "salute", "A Dwarf Salute",
    "Rock and Stone to the Bone!",                                                              "positive", "salute", "A Dwarf Salute",
    "For Karl!",                                                                                "positive", "salute", "A Dwarf Salute",
    "Leave No Dwarf Behind!",                                                                   "positive", "salute", "A Dwarf Salute",
    "By the Beard!",                                                                            "positive", "salute", "A Dwarf Salute",
    # Activating the Jukebox ------------------------------------------------------------------------------------------
    "Yeah, that's nice!",                                                                       "positive", "jukebox", "Activating the Jukebox",
    "Ohh, I like that!",                                                                        "positive", "jukebox", "Activating the Jukebox",
    "Not that...",                                                                              "negative", "jukebox", "Activating the Jukebox",
    "Hidden gems my ass!",                                                                      "negative", "jukebox", "Activating the Jukebox",
    "Now that is real music!",                                                                  "positive", "jukebox", "Activating the Jukebox",
    "That reminds me of my first love..",                                                       "positive", "jukebox", "Activating the Jukebox",
    "I feel like dancing!",                                                                     "positive", "jukebox", "Activating the Jukebox",
    "Can't stand still to this!",                                                               "positive", "jukebox", "Activating the Jukebox",
    "Nice tune!",                                                                               "positive", "jukebox", "Activating the Jukebox",
    "That sucks!",                                                                              "negative", "jukebox", "Activating the Jukebox",
    "Let's dance!",                                                                             "positive", "jukebox", "Activating the Jukebox",
    "Bang your head to this!",                                                                  "positive", "jukebox", "Activating the Jukebox",
    "Dance, monkeys!",                                                                          "positive", "jukebox", "Activating the Jukebox",
    "That's sweet!",                                                                            "positive", "jukebox", "Activating the Jukebox",
    # When Dwarf gets Downed ------------------------------------------------------------------------------------------
    "Hah! <Grunting>",                                                                          "neutral",  "dwarf downed", "When Dwarf gets Downed",
    # When Falling ----------------------------------------------------------------------------------------------------
    "I fell!",                                                                                  "negative", "falling", "When Falling",
    "Help!",                                                                                    "negative", "falling", "When Falling",
    "Ahh!",                                                                                     "negative", "falling", "When Falling",
    "Oops I slipped!",                                                                          "negative", "falling", "When Falling",
    "Nooooo!",                                                                                  "negative", "falling", "When Falling",
    "Mommy!",                                                                                   "negative", "falling", "When Falling",
    # Friendly Fire ---------------------------------------------------------------------------------------------------
    "Ouch! Watch it!",                                                                          "negative", "friendly fire", "Friendly Fire",
    "Careful!",                                                                                 "negative", "friendly fire", "Friendly Fire",
    "Careful!",                                                                                 "negative", "friendly fire", "Friendly Fire",
    "Trigger discipline, you whale piper!",                                                     "negative", "friendly fire", "Friendly Fire",
    "It's ME, you arsewipe!",                                                                   "negative", "friendly fire", "Friendly Fire",
    "Friendlies! FRIENDLIES!",                                                                  "negative", "friendly fire", "Friendly Fire",
    "Friendly fire!",                                                                           "negative", "friendly fire", "Friendly Fire",
    "Watch it you blind-sided pig!",                                                            "negative", "friendly fire", "Friendly Fire",
    "Hey! I'm not the enemy!",                                                                  "negative", "friendly fire", "Friendly Fire",
    "Watch where you're shooting! Moron!",                                                      "negative", "friendly fire", "Friendly Fire",
    "Stop shooting me!",                                                                        "negative", "friendly fire", "Friendly Fire",
    "I'm your friend! And let's keep it that way!",                                             "negative", "friendly fire", "Friendly Fire",
    "I hope that wasn't on purpose!",                                                           "negative", "friendly fire", "Friendly Fire",
    "Don't shoot me!",                                                                          "negative", "friendly fire", "Friendly Fire",
    "Watch your fire, moron!",                                                                  "negative", "friendly fire", "Friendly Fire",
    "Watch it!",                                                                                "negative", "friendly fire", "Friendly Fire",
    "Stop shooting you pointy-eared Leaf Lover!",                                               "negative", "friendly fire", "Friendly Fire",
    "Shoot them! Not me!",                                                                      "negative", "friendly fire", "Friendly Fire",
    "OW! That hurt!",                                                                           "negative", "friendly fire", "Friendly Fire",
    "I'm with you! You beardless blunderbuss!",                                                 "negative", "friendly fire", "Friendly Fire",
    "Hold your fire you addle-brain lump of lard!",                                             "negative", "friendly fire", "Friendly Fire",
    "I'm not one of them you knob head!",                                                       "negative", "friendly fire", "Friendly Fire",
    "You interplanetary goat! Why are you shooting at me!",                                     "negative", "friendly fire", "Friendly Fire",
    "Why are you shooting me? You slack-jaw dimwit!",                                           "negative", "friendly fire", "Friendly Fire",
    "Don't shoot me!",                                                                          "negative", "friendly fire", "Friendly Fire",
    "Stop attacking me!",                                                                       "negative", "friendly fire", "Friendly Fire",
    # When the Drop Pod Leaves and the Dwarf is Left Behind -----------------------------------------------------------
    "Get back here you bastards!",                                                              "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Okay, this is not happening!",                                                             "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Don't leave me!",                                                                          "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Damn you Deep Rock Galactic!",                                                             "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "I've lived like a Dwarf, and I'm gonna die like a Dwarf!",                                 "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Buggers!",                                                                                 "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Damn! That's unfortunate!",                                                                "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Well... Sometimes you win, sometimes you die!",                                            "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Game over boys!",                                                                          "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Game over lads!",                                                                          "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "Oh shit!",                                                                                 "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "This is it boys!",                                                                         "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "This is it lads!",                                                                         "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    "We were so close - damn it!",                                                              "negative", "drop pod", "When the Drop Pod Leaves and the Dwarf is Left Behind",
    # Random Comments -------------------------------------------------------------------------------------------------
    "I think we actually have a chance to make it out alive!",                                  "positive", "random comments", "Random Comments",
    "Maybe we're not gonna die after all!",                                                     "positive", "random comments", "Random Comments",
    "I actually think we're gonna make it!",                                                    "positive", "random comments", "Random Comments",
    "I'll beat your record this time - just watch me!",                                         "positive", "random comments", "Random Comments",
    "Wonder what slop they're serving in the canteen tonight!",                                 "positive", "random comments", "Random Comments",
    "This place is almost nice compared to last time!",                                         "positive", "random comments", "Random Comments",
    "This is a friggin' hell hole!",                                                            "negative", "random comments", "Random Comments",
    "God dammit there is a pebble in my boot!",                                                 "positive", "random comments", "Random Comments",
    "I think we're actually doing alright so far!",                                             "positive", "random comments", "Random Comments",
    "That guy at mission control, he really has a cozy job!",                                   "negative", "random comments", "Random Comments",
    "I'm wondering if fighting bugs and moving dirt is the best way to make a living.",         "negative", "random comments", "Random Comments",
    "I'm having the best time in my life right now!",                                           "positive", "random comments", "Random Comments",
    "Next time, let's go somewhere nice!",                                                      "negative", "random comments", "Random Comments",
    # Calling the M.U.L.E. --------------------------------------------------------------------------------------------
    "Where is that damn tin can?!",                                                             "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Molly! Come here!",                                                                        "neutral",  "calling mule", "Calling the M.U.L.E.",
    "I called for Molly!",                                                                      "neutral",  "calling mule", "Calling the M.U.L.E.",
    "I called for the mule!",                                                                   "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Molly is on the way!",                                                                     "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Where is that damn mule!",                                                                 "neutral",  "calling mule", "Calling the M.U.L.E.",
    "I need the mule - Right here! Right now!",                                                 "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Come here!",                                                                               "neutral",  "calling mule", "Calling the M.U.L.E.",
    "I need the damn minecart on legs! Where is it!",                                           "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Donkey! Come here!",                                                                       "neutral",  "calling mule", "Calling the M.U.L.E.",
    "I got to deposit!",                                                                        "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Molly! Over here!",                                                                        "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Move your tin ass over here and hurry please!",                                            "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Molly!",                                                                                   "neutral",  "calling mule", "Calling the M.U.L.E.",
    "Just take your sweet time old lady! I've got all day!",                                    "negative", "calling mule", "Calling the M.U.L.E.",
    "Deep Rock seriously need to invest in some better equipment!",                             "negative", "calling mule", "Calling the M.U.L.E.",
    # Depositing into the M.U.L.E. ------------------------------------------------------------------------------------
    "Down it goes!",                                                                            "positive", "depositing", "Depositing into the M.U.L.E.",
    "Depositing minerals!",                                                                     "positive", "depositing", "Depositing into the M.U.L.E.",
    "You're a good mule, Molly!",                                                               "positive", "depositing", "Depositing into the M.U.L.E.",
    "The tin can is surely hungry today!",                                                      "positive", "depositing", "Depositing into the M.U.L.E.",
    "Making a deposit!",                                                                        "positive", "depositing", "Depositing into the M.U.L.E.",
    "Another day at the office!",                                                               "positive", "depositing", "Depositing into the M.U.L.E.",
    "Getting closer to our quota!",                                                             "positive", "depositing", "Depositing into the M.U.L.E.",
    "Down the hatch!",                                                                          "positive", "depositing", "Depositing into the M.U.L.E.",
    "Depositing!",                                                                              "positive", "depositing", "Depositing into the M.U.L.E.",
    "Whoopsie doopsie!",                                                                        "positive", "depositing", "Depositing into the M.U.L.E.",
    "<Whistling>",                                                                              "positive", "depositing", "Depositing into the M.U.L.E.",
    "<Grunting>",                                                                               "positive", "depositing", "Depositing into the M.U.L.E.",
    "Bum bum bum!",                                                                             "positive", "depositing", "Depositing into the M.U.L.E.",
    "Mmm... Could really go for a cold brew right now!",                                        "positive", "depositing", "Depositing into the M.U.L.E.",
    "This is thirsty work!",                                                                    "positive", "depositing", "Depositing into the M.U.L.E.",
    "<Yawn> I could need a good rest!",                                                         "positive", "depositing", "Depositing into the M.U.L.E.",
    "<Grunt> Air's kinda heavy down here today!",                                               "negative", "depositing", "Depositing into the M.U.L.E.",
    "When we get back... It's sandwich time!",                                                  "positive", "depositing", "Depositing into the M.U.L.E.",
    "Sometimes I wonder if mining is all there is to life... Then I punch myself in the nose!", "positive", "depositing", "Depositing into the M.U.L.E.",
    "Mining is hard work, good thing we're dwarves!",                                           "positive", "depositing", "Depositing into the M.U.L.E.",
    "Need to get my pickaxe sharpened when we get back!",                                       "positive", "depositing", "Depositing into the M.U.L.E.",
    # During the "Extraction" Sequence --------------------------------------------------------------------------------
    "Let's follow the mule, it's heading for the drop pod!",                                    "neutral",  "extraction", 'During the "Extraction" Sequence',
    "Follow the mule!",                                                                         "neutral",  "extraction", 'During the "Extraction" Sequence',
    "Follow Molly!",                                                                            "neutral",  "extraction", 'During the "Extraction" Sequence',
    "The mule has been called back to the drop pod!",                                           "neutral",  "extraction", 'During the "Extraction" Sequence',
    # Ordering a Resupply ---------------------------------------------------------------------------------------------
    "Resupply ordered!",                                                                        "neutral",  "resupply order", "Ordering a Resupply",
    "I ordered a resupply!",                                                                    "neutral",  "resupply order", "Ordering a Resupply",
    "More ammo is on the way!",                                                                 "neutral",  "resupply order", "Ordering a Resupply",
    # Resupply Arrival ------------------------------------------------------------------------------------------------
    "Resupply arrived! Go restock!",                                                            "neutral",  "resupply arrival", "Resupply Arrival",
    "The resupply is here.",                                                                    "neutral",  "resupply arrival", "Resupply Arrival",
    "Time to restock!",                                                                         "neutral",  "resupply arrival", "Resupply Arrival",
    "Resupply pod is here! Everybody collect ammunition.",                                      "neutral",  "resupply arrival", "Resupply Arrival",
    # Being Grabbed by a Mactera Grabber ------------------------------------------------------------------------------
    "Help, it took me!",                                                                        "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "Let me go!",                                                                               "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "Noooooo!",                                                                                 "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "Let me go, Creep!",                                                                        "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "It took me!",                                                                              "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "Put me down!",                                                                             "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "I'm afraid of heights!",                                                                   "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "Stupid bug! Put me down!",                                                                 "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    "HELP!",                                                                                    "negative", "grabber", "Being Grabbed by a Mactera Grabber",
    # Being Grabbed by a Cave Leech -----------------------------------------------------------------------------------
    "Help! I'm stuck in the ceiling!",                                                          "negative", "cave leech", "Being Grabbed by a Cave Leech",
    "The tentacle got me!",                                                                     "negative", "cave leech", "Being Grabbed by a Cave Leech",
    "I'm paralyzed!",                                                                           "negative", "cave leech", "Being Grabbed by a Cave Leech",
    "Can't... move!",                                                                           "negative", "cave leech", "Being Grabbed by a Cave Leech",
    "Help! I'm stuck!",                                                                         "negative", "cave leech", "Being Grabbed by a Cave Leech",
    # Shared by Both --------------------------------------------------------------------------------------------------
    "It got me!",                                                                               "negative", "grabbed", "Shared by Both",
    "I'm trapped!",                                                                             "negative", "grabbed", "Shared by Both",
    "Please, shoot it!",                                                                        "negative", "grabbed", "Shared by Both",
    "Get me free!",                                                                             "negative", "grabbed", "Shared by Both",
    # Dwarf Shout on Hoxxes (Field) -----------------------------------------------------------------------------------
    "Over here!",                                                                               "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "With me!",                                                                                 "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "Follow me!",                                                                               "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "I know the way!",                                                                          "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "Move out!",                                                                                "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "This way!",                                                                                "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "Hello!",                                                                                   "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "Hey!",                                                                                     "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    "Come here!",                                                                               "positive", "hoxxes shout", "Dwarf Shout on Hoxxes (Field)",
    # Dwarf Shout in Space Rig ----------------------------------------------------------------------------------------
    "<Dwarf spitting>",                                                                         "negative", "space rig shout", "Dwarf Shout in Space Rig",
    "<Dwarf spitting>",                                                                         "negative", "space rig shout", "Dwarf Shout in Space Rig",
    "<Dwarf flatulence>",                                                                       "negative", "space rig shout", "Dwarf Shout in Space Rig",
    "<Dwarf flatulence>",                                                                       "negative", "space rig shout", "Dwarf Shout in Space Rig",
    "<Dwarf burp>",                                                                             "negative", "space rig shout", "Dwarf Shout in Space Rig",
    "<Dwarf burp>",                                                                             "negative", "space rig shout", "Dwarf Shout in Space Rig",
    # Dwarf Shout when Downed -----------------------------------------------------------------------------------------
    "Save me!",                                                                                 "negative", "downed shout", "Dwarf Shout when Downed",
    "I can't feel my beard, help!",                                                             "negative", "downed shout", "Dwarf Shout when Downed",
    "Please, rescue me!",                                                                       "negative", "downed shout", "Dwarf Shout when Downed",
    "I'm down!",                                                                                "negative", "downed shout", "Dwarf Shout when Downed",
    "Bastards got me!",                                                                         "negative", "downed shout", "Dwarf Shout when Downed",
    "Help! They've got me!",                                                                    "negative", "downed shout", "Dwarf Shout when Downed",
    "I'm hurt! I need help!",                                                                   "negative", "downed shout", "Dwarf Shout when Downed",
    "Help! Please hury!",                                                                       "negative", "downed shout", "Dwarf Shout when Downed",
    "I could use a hand!",                                                                      "negative", "downed shout", "Dwarf Shout when Downed",
    "Somebody help!",                                                                           "negative", "downed shout", "Dwarf Shout when Downed",
    "Come and rescue me!",                                                                      "negative", "downed shout", "Dwarf Shout when Downed",
    "Need a hand here!",                                                                        "negative", "downed shout", "Dwarf Shout when Downed",
    "Somebody patch me up!",                                                                    "negative", "downed shout", "Dwarf Shout when Downed",
    # Dwarf Shout While Holding a Beer --------------------------------------------------------------------------------
    "For Karl!",                                                                                "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Rock and Stone!",                                                                          "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Rock and Stone to the bone!",                                                              "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Bottoms up, friends!",                                                                     "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Fortune and glory!",                                                                       "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To a successful mission.",                                                                 "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To our continued survival ... yeah right! Hahaha!",                                        "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Long live the Dwarves!",                                                                   "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To the fallen.",                                                                           "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To the fallen.",                                                                           "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "For gold!",                                                                                "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "For teamwork!",                                                                            "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To Karl!",                                                                                 "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To darkness!",                                                                             "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To the empires of old!",                                                                   "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To danger.",                                                                               "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To mates, to darkness, and to making it back alive!",                                      "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To those we lost!",                                                                        "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Teamwork and beer will keep us together!",                                                 "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Skål!",                                                                                    "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Last one to finish is a pointy-eared Leaf Lover.",                                         "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "To Rock and Stone!",                                                                       "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Nothing will stop us now!",                                                                "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Darkness is our friend!",                                                                  "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Hello darkness, my old friend!",                                                           "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Miners - the lowest, and the highest.",                                                    "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Cheers, everyone!",                                                                        "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Cheers!",                                                                                  "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "Karl would approve of this.",                                                              "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    "May your beards be thick and your gold satchels heavy!",                                   "positive", "beer shout", "Dwarf Shout While Holding a Beer",
    # Overheating -----------------------------------------------------------------------------------------------------
    "Damn drills are overheating!",                                                             "negative", "overheating", "Overheating",
    "Not again!",                                                                               "negative", "overheating", "Overheating",
    "I gotta watch the overheating!",                                                           "negative", "overheating", "Overheating",
    "It's overheated!",                                                                         "negative", "overheating", "Overheating",
    "Rubbish equipment!",                                                                       "negative", "overheating", "Overheating",
    "Deep Rock really need to provide better equipment!",                                       "negative", "overheating", "Overheating",
    "God damn these drills!",                                                                   "negative", "overheating", "Overheating",
    "That goddamn overheating!",                                                                "negative", "overheating", "Overheating",
    # Grappling -------------------------------------------------------------------------------------------------------
    "Zip!",                                                                                     "positive", "grappling", "Grappling",
    "Zippity!",                                                                                 "positive", "grappling", "Grappling",
    "From A to D, skipping B and C!",                                                           "positive", "grappling", "Grappling",
    "Better than flying!",                                                                      "positive", "grappling", "Grappling",
    "See ya, suckers!",                                                                         "positive", "grappling", "Grappling",
    "Take-off!",                                                                                "positive", "grappling", "Grappling",
    "Gotta go fast!",                                                                           "positive", "grappling", "Grappling",
    "Wooosh!",                                                                                  "positive", "grappling", "Grappling",
    "You snooze, you lose!",                                                                    "positive", "grappling", "Grappling",
    "I'm outta here!",                                                                          "positive", "grappling", "Grappling",
    "I love this Grappling Hook!",                                                              "positive", "grappling", "Grappling",
    "Watch me fly!",                                                                            "positive", "grappling", "Grappling",
    "I'm off!",                                                                                 "positive", "grappling", "Grappling",
    "Ludicrous speed!",                                                                         "positive", "grappling", "Grappling",
    # Placing a Satchel Charge ----------------------------------------------------------------------------------------
    "Things about to go boom!",                                                                 "positive", "satchel charge", "Placing a Satchel Charge",
    "Explosives places!",                                                                       "neutral",  "satchel charge", "Placing a Satchel Charge",
    "Ready to blow!",                                                                           "positive", "satchel charge", "Placing a Satchel Charge",
    # Throwing a Cluster Grenade --------------------------------------------------------------------------------------
    "Cluster Grenade!",                                                                         "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "CLUSTER GRENADE!",                                                                         "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Grenade!",                                                                                 "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Let it rain!",                                                                             "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Incoming!",                                                                                "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "All clear!",                                                                               "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Out of the way!",                                                                          "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Multi-boom!",                                                                              "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Cluster out! Save yourself!",                                                              "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Watch for the Cluster Grenade!",                                                           "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "Time for fireworks!",                                                                      "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    "I call it my \"grenade-grenade\"!",                                                        "neutral",  "cluster grenade", "Throwing a Cluster Grenade",
    # Throwing a Cryo Grenade -----------------------------------------------------------------------------------------
    "Cryo grenaade!",                                                                           "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Cryo!",                                                                                    "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Enemies frozen!",                                                                          "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Freeze, creep!",                                                                           "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Freeze!",                                                                                  "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Cryo Grenade!",                                                                            "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Chill!",                                                                                   "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Pickaxe and chill!",                                                                       "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Chill factor!",                                                                            "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Cold shoulder for you!",                                                                   "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    "Cold as ice!",                                                                             "neutral",  "cryo grenade", "Throwing a Cryo Grenade",
    # Driller ---------------------------------------------------------------------------------------------------------
    "I'll drill through anything!",                                                             "positive", "driller", "Driller",
    "Ain't no obstacle I can't clear!",                                                         "positive", "driller", "Driller",
    "Let me handle the digging!",                                                               "positive", "driller", "Driller",
    "I eat rock for breakfast!",                                                                "positive", "driller", "Driller",
    "Diggings' my middle name!",                                                                "positive", "driller", "Driller",
    "Step aside, wimps!",                                                                       "positive", "driller", "Driller",
    "Obstacle? More like a granite smoothie once I'm done!",                                    "positive", "driller", "Driller",
    "If it ain't drillable, it's probably flammable.",                                          "positive", "driller", "Driller",
    "Fueled up and raring to go!",                                                              "positive", "driller", "Driller",
    "Heavy duty excavation, ain't no finer thing!",                                             "positive", "driller", "Driller",
    # Engineer --------------------------------------------------------------------------------------------------------
    "Elementary!",                                                                              "positive", "engineer", "Engineer",
    "Infestation problems? Automated turret, mate.",                                            "positive", "engineer", "Engineer",
    "Turrets and explosions! You've come to the right place, mate.",                            "positive", "engineer", "Engineer",
    "Let's play this smart for once, huh?",                                                     "positive", "engineer", "Engineer",
    "Cut 'em off, cut 'em down. That's the way!",                                               "positive", "engineer", "Engineer",
    "It's all about securing the caves, ya know.",                                              "positive", "engineer", "Engineer",
    "Identify targets and exterminate. That's how it's done.",                                  "positive", "engineer", "Engineer",
    "Quotas and objectives to fulfill. Let's get 'er done, lads!",                              "positive", "engineer", "Engineer",
    "Time for a cold one once this is done.",                                                   "positive", "engineer", "Engineer",
    "All in a day's work.",                                                                     "positive", "engineer", "Engineer",
    # Gunner ----------------------------------------------------------------------------------------------------------
    "Just show me where to shoot!",                                                             "positive", "gunner", "Gunner",
    "I'll kill anything with more legs than two!",                                              "positive", "gunner", "Gunner",
    "Lock and load!",                                                                           "positive", "gunner", "Gunner",
    "Pumped up and ready to kill!",                                                             "positive", "gunner", "Gunner",
    "Two-thousand rounds of depleted uranium, aw yeah!",                                        "positive", "gunner", "Gunner",
    "So what if I like really big guns?!",                                                      "positive", "gunner", "Gunner",
    "It ain't a gun if it don't weight at least one-hundred pounds!",                           "positive", "gunner", "Gunner",
    "Time to turn some aliens into thin, green paste!",                                         "positive", "gunner", "Gunner",
    "Trigger finger is itchy again.",                                                           "positive", "gunner", "Gunner",
    "Born to kill, baby!",                                                                      "positive", "gunner", "Gunner",
    # Scout -----------------------------------------------------------------------------------------------------------
    "The truth is out there, so is the gold.",                                                  "positive", "scout", "Scout",
    "Let's move it.",                                                                           "positive", "scout", "Scout",
    "Time to light this shit up.",                                                              "positive", "scout", "Scout",
    "Allow me to illuminate the situation! Hahahaha!",                                          "positive", "scout", "Scout",
    "Darkness? Ain't nothing!",                                                                 "positive", "scout", "Scout",
    "Darkness, here I come!",                                                                   "positive", "scout", "Scout",
    "I can get anywhere, any time.",                                                            "positive", "scout", "Scout",
    "Keep them shites off me, I'll keep the team alive!",                                       "positive", "scout", "Scout",
    "Afraid of the dark? No need! You've got me!",                                              "positive", "scout", "Scout",
    "Yeah, I've got a grappling hook. What are you going to do about it?",                      "positive", "scout", "Scout",
    "Yeah, I don't so much scout as make this operation possible.",                             "positive", "scout", "Scout",
    # Aquarq ----------------------------------------------------------------------------------------------------------
    "Aquarq here!",                                                                             "neutral",  "aquarq", "Aquarq",
    "Found an Aquarq!",                                                                         "neutral",  "aquarq", "Aquarq",
    "There's an Aquarq here!",                                                                  "neutral",  "aquarq", "Aquarq",
    # Bittergem & Compressed Gold -------------------------------------------------------------------------------------
    "We're rich!",                                                                              "positive", "we're rich", "Bittergem & Compressed Gold",
    # Dystrum ---------------------------------------------------------------------------------------------------------
    "I found some Dystrum!",                                                                    "neutral",  "dystrum", "Dystrum",
    "There's Dystrum here!",                                                                    "neutral",  "dystrum", "Dystrum",
    "Dystrum found!",                                                                           "neutral",  "dystrum", "Dystrum",
    "Dystrum!",                                                                                 "neutral",  "dystrum", "Dystrum",
    "Dystrum!",                                                                                 "neutral",  "dystrum", "Dystrum",
    # Enor Pearl ------------------------------------------------------------------------------------------------------
    "Found an Enor Pearl!",                                                                     "neutral",  "enor pearl", "Enor Pearl",
    "Enor Pearl here!",                                                                         "neutral",  "enor pearl", "Enor Pearl",
    "There's an Enor Pearl here!",                                                              "neutral",  "enor pearl", "Enor Pearl",
    # Gold ------------------------------------------------------------------------------------------------------------
    "Gold!",                                                                                    "neutral",  "gold", "Gold",
    "There is gold here!",                                                                      "neutral",  "gold", "Gold",
    "There is gold!",                                                                           "neutral",  "gold", "Gold",
    "I found a gold vein!",                                                                     "neutral",  "gold", "Gold",
    # Jadiz -----------------------------------------------------------------------------------------------------------
    "Jadiz here!",                                                                              "neutral",  "jadiz", "Jadiz",
    "Found a Jadiz!",                                                                           "neutral",  "jadiz", "Jadiz",
    "There's a Jadiz here!",                                                                    "neutral",  "jadiz", "Jadiz",
    # Nitra -----------------------------------------------------------------------------------------------------------
    "There is Nitra over here!",                                                                "neutral",  "nitra", "Nitra",
    "Found some Nitra!",                                                                        "neutral",  "nitra", "Nitra",
    "Nitra!",                                                                                   "neutral",  "nitra", "Nitra",
    "Anyone needs ammo? There's Nitra here!",                                                   "neutral",  "nitra", "Nitra",
    "Mine the Nitra for ammo!",                                                                 "neutral",  "nitra", "Nitra",
    # Removed ---------------------------------------------------------------------------------------------------------
    "I'm not a monster you dickhead!",                                                          "negative", "removed", "Friendly Fire",
    "It's me you drunk bastard!",                                                               "negative", "removed", "Friendly Fire",
    "*I'm on your side you mangy packer!",                                                      "negative", "removed", "Friendly Fire",
    "Stone and Rock!",                                                                          "positive", "removed", "Dwarf Salute",
    "Could you lazy bastards get me the fuck up!",                                              "negative", "removed", "Dwarf Shout when Downed",
    "Help me you fuckers!",                                                                     "negative", "removed", "Being Grabbed by a Cave Leech",
    "Who needs ziplines?",                                                                      "positive", "removed", "Grappling",
    "Fuck!",                                                                                    "negative", "removed", "Overheating Weapon or Tool",
    "Work you piece of shit!",                                                                  "negative", "removed", "Overheating Weapon or Tool",
    "Fucking piece of...",                                                                      "positive", "removed", "Overheating Weapon or Tool",
    "That will do!",                                                                            "positive", "removed", "Placing a Satchel Charge",
    "Armed and ready!",                                                                         "positive", "removed", "Placing a Satchel Charge",
    "This shit is dangerous!",                                                                  "negative", "removed", "Placing a Satchel Charge",
    "Things about to get serious!",                                                             "negative", "removed", "Placing a Satchel Charge",
    "Here we go!",                                                                              "negative", "removed", "Placing a Satchel Charge",
    "Done!",                                                                                    "positive", "removed", "Placing a Satchel Charge",
    "Ready to blow!",                                                                           "positive", "removed", "Placing a Satchel Charge",
    "Armed.",                                                                                   "positive", "removed", "Placing a Satchel Charge",
    "Get to safety, folks!",                                                                    "negative", "removed", "Placing a Satchel Charge"
  )
}
