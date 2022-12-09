
library(magrittr)

classic_monty <- function(doors = 3
                          , prize = "car"
                          , other = "goat"
                          ) {
  
  prizes <- c(prize, paste0(other, seq(1, doors - 1))) %>%
    sample()
  
  # Pick a door
  original <- sample(prizes, 1)
  
  # monty options
  monty_options <- prizes[!prizes %in% c(original, "car")]
  
  # Monty picks a door
  monty <- sample(monty_options, 1)
  
  # change options
  change_options <- prizes[!prizes %in% c(original, monty)]
  
  #change choice
  change <- sample(change_options, 1)
  
  tibble::tibble(original = original
         , monty = monty
         , change = change
         )

  
}

runs <- tibble::tibble(run = 1:10000) %>%
  dplyr::mutate(result = purrr::map(run, ~classic_monty(3))) %>%
  tidyr::unnest(cols = c(result))

result <- runs %>%
  dplyr::summarise(change = sum(change == "car") / nrow(.)
                   , original = sum(original == "car") / nrow(.)
                   )

result

