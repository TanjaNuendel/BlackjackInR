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
players <- list()
playersDF <- data.frame(cards = vector(), numericValues = vector(), total = vector())

dfPopulate <- function(numPlayers) {
  for (i in 1:numPlayers) {
    players[[i]] <- playersDF
  }
  return(players)
}

# win <- function() {
#   
# }
# 
# gameEnd <- function() {
#   if (win) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }

#Updates the data frame for the player, given the player number, card type, and card number
updateTotal <- function(player_num, card_type, card_val) {
  typeVec <- append(players[[player_num]][, 1], card_type)
  valVec <- append(players[[player_num]][, 2], card_val)
  
  len <- length(players[[player_num]][, 3])
  if (len == 0) {
    totalVec <- append(players[[player_num]][, 3], card_val)
  } else {
    total = card_val + players[[player_num]][, 3][len]
    totalVec <- append(players[[player_num]][, 3], total)
  }

  players[[player_num]] <- data.frame(cards = typeVec, numericValues = valVec, total = totalVec)
  return(players)
}

#Basic way to determine whether to hit or stand
HitOrStand <- function(player_num) {
  total <- players[[player_num]][, 3][length(players[[player_num]][, 3])]
  if (total >= 17) {
    print("Stand")
  } else {
    print("Hit")
  }
}

players <- dfPopulate(4)

#Populating player 1
players <- updateTotal(1, "Ace", 11)
players <- updateTotal(1, "5", 5)
players <- updateTotal(1, "King", 10)

players

# Func: drawing without replacement
# use sample()
# handle zero case: if current_deckfreq of a card is already 0, it will stay zero

# shuffling

# counting for winning
