# Blackjack Game

# libraries
library(dplyr)
library(magrittr)
library(tidyverse)
library(forcats)

# Deck simulation (Tanja)
deck_count <- 5
cardtype <- c("Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King")
cardvalue <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)
# make a vector of frequencies - in the beginning 4 cards per type and deck
current_deckfreq <- rep(deck_count*4, 13)
card_probs <- vector()

# faulty atm - will calculate probability of card at next draw, is updated constantly
card_probs <- 
  for (i in cardtype){
    card_probs[i] <- current_deckfreq / sum(current_deckfreq)
  }

# make the deck from given data
deck <- data.frame(cardtype, cardvalue, current_deckfreq, stringsAsFactors = TRUE)


# Dealer's game - hardfast rule

# Player strategies (Mushahid)

# Func: drawing without replacement
# use sample()
# handle zero case: if current_deckfreq of a card is already 0, it will stay zero

# shuffling

# counting for winning
