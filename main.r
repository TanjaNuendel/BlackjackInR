# Blackjack Game

# libraries
library(dplyr)
library(magrittr)
library(tidyverse)
library(forcats)

# Deck simulation
deck_count <- 1
total_cards <- deck_count * 52
cardtype <- c("Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King")
cardvalue <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

# make a vector of frequencies - in the beginning 4 cards per type and deck
current_deckfreq <- rep(deck_count*4, 13)
# init card probabilities with equal proportions of 1/13
card_probs <- rep(1/13, total_cards/4)

# make the deck from given data
deck <- data.frame(cardtype, cardvalue, current_deckfreq, card_probs, stringsAsFactors = TRUE)

# keeps deck updated with card type frequencies and probablilities
updateDeck <- function(deck, drawn_card) {
  # drawn card gets removed from deck
  deck$current_deckfreq[which(cardtype == drawn_card)] <- (deck$current_deckfreq[which(cardtype == drawn_card)] - 1)
  #probabilities are updated
  drawn_card_index <- which(cardtype == drawn_card)
  updateProbs <- function(){
    deck$card_probs[index] <- deck$current_deckfreq[index] / sum(deck$current_deckfreq)
  }
  
  # this throws an error: " Error in FUN(X[[i]], ...) : unused argument (arg1 = drawn_card_index)"
  deck$card_probs <- lapply(deck$card_probs, 
                            function(x) deck$card_probs[drawn_card_index] 
                            <- deck$current_deckfreq[drawn_card_index] / 
                              sum(deck$current_deckfreq))
}

# function to draw a card from the current deck
drawCard <- function(x){
  drawn_card <- sample(deck$cardtype, size = 1, replace = TRUE)
  print(drawn_card)
  # if card is already out, draw again until there is one
  if (deck$current_deckfreq[which(cardtype == drawn_card)] == 0){
    drawn_card <- sample(deck$cardtype, size = 1, replace = TRUE)
  } else if(sum(deck$current_deckfreq) == 0) {
    # make new deck if all cards are empty
    deck <- data.frame(cardtype, cardvalue, current_deckfreq, card_probs, stringsAsFactors = TRUE)
  } else {
    updateDeck(deck, drawn_card)
  }
}

drawCard()

# Dealer's game - hardfast rule

# Player strategies (Mushahid)
players <- list()
playersDF <- data.frame(cards = character(), numericValues = vector(), total = vector(), 
                        status = character())

printf <- function(...) cat(sprintf(...))

dfPopulate <- function(numPlayers) {
  for (i in 1:numPlayers) {
    players[[i]] <- playersDF
  }
  return(players)
}

CheckStatus <- function(player_num, total) {
  status_vec <- vector()
  if (total == 21) {
    status_vec <- append(as.character(players[[player_num]][, 4]), "won")
  } else if (total > 21) { 
    status_vec <- append(as.character(players[[player_num]][, 4]), "lost")
  } else if (total >= 17 &&  total < 21) {
    status_vec <- append(as.character(players[[player_num]][, 4]), "standing")
  } else {
    status_vec <- append(as.character(players[[player_num]][, 4]), "hitting")
  }
  
  return(status_vec)
}

#Basic way to determine whether to hit or stand
HitOrStand <- function(player_num, vec) {
  total <- vec[length(vec)]
  if (total == 21) {
    printf("Player %d: WON!\n", player_num)
    CheckStatus(player_num, total)
  } else if (total > 21) {
    printf("Player %d: BUSTED!\n", player_num)
    CheckStatus(player_num, total)
  } else if (total >= 17 && total < 21) {
    printf("Player %d: Stand\n", player_num)
    CheckStatus(player_num, total)
  } else {
    printf("Player %d: Hit\n", player_num)
    CheckStatus(player_num, total)
  }
}

#This is the function the dealer will call after checking if the player wants to hit or
#stand. The dealer will pass in the player number, the type of card drawn, and the numeric 
#value of that card. From there, the data frame for the player will be updated. 
updateTotal <- function(player_num, card_type, card_val) {
  typeVec <- append(as.character(players[[player_num]][, 1]), card_type)
  valVec <- append(players[[player_num]][, 2], card_val)
  
  len <- length(players[[player_num]][, 3])
  status_vec <- vector()
  
  if (len == 0) {
    totalVec <- append(players[[player_num]][, 3], card_val)
    status_vec <- HitOrStand(player_num, totalVec)
  } else {
    total = card_val + players[[player_num]][, 3][len]
    totalVec <- append(players[[player_num]][, 3], total)
    
    status_vec <- HitOrStand(player_num, totalVec)
  }
  
  players[[player_num]] <- data.frame(cards = typeVec, numericValues = valVec, total = totalVec,
                                      status = status_vec)
  
  return(players)
}

#The number of players n, must be passed in at the start
players <- dfPopulate(4)

#Populating player 1
players <- updateTotal(1, "Ace", 11)
players <- updateTotal(1, "5", 5)
players <- updateTotal(1, "4", 4)

players

# Func: drawing without replacement
# use sample()
# handle zero case: if current_deckfreq of a card is already 0, it will stay zero


# counting for winning
