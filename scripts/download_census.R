library(rRace)

download_census(
  .key = "8d569f4a70e1abf84cd23350522b479216d008c4",
  .geo = "tract",
  .use_age = TRUE,
  .use_gen = TRUE,
  .dir = "_debug_data/census/",
  .workers = 5,
  .retry = 5,
  .progress = TRUE,
  .states = rRace:::get_states()
)
