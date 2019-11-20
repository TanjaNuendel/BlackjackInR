#Tanja Neundel
#Mushahid Hassan
#Conor Falvey

#Case Study 3

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
numPlayers <- 4
finished = FALSE

# make a vector of frequencies - in the beginning 4 cards per type and deck
current_deckfreq <- rep(deck_count*4, 13)
# init card probabilities with equal proportions of 1/13
card_probs <- rep(1/13, total_cards/4)

# make the deck from given data
deck <- data.frame(cardtype, cardvalue, current_deckfreq, card_probs, stringsAsFactors = TRUE)

# keeps deck updated with card type frequencies and probablilities
updateDeck <- function(drawn_card) {
  # drawn card gets removed from deck
  deck$current_deckfreq[which(deck$cardtype == as.factor(drawn_card))] <<- (deck$current_deckfreq[which(deck$cardtype == as.factor(drawn_card))] - 1)
  
  #probabilities are updated
  for (i in 1:length(deck$cardtype)) {
    deck$card_probs[[i]] <<- deck$current_deckfreq[[i]] / colSums(deck[3])
  }
}

# function to draw a card from the current deck
drawCard <- function(){
  drawn_card <- sample(deck$cardtype, size = 1, replace = TRUE)
  # if card is already out, draw again until there is one
  if (deck$current_deckfreq[which(cardtype == drawn_card)] == 0){
    drawn_card <- sample(deck$cardtype, size = 1, replace = TRUE)
  } else if(sum(deck$current_deckfreq) == 0) {
    # make new deck if all cards are empty
    deck <<- data.frame(cardtype, cardvalue, current_deckfreq, card_probs, stringsAsFactors = TRUE)
  } else {
    updateDeck(drawn_card)
  }
  return(drawn_card)
}

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
    finished <- TRUE
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
    #printf("Player %d: WON!\n", player_num)
    CheckStatus(player_num, total)
  } else if (total > 21) {
    #printf("Player %d: BUSTED!\n", player_num)
    CheckStatus(player_num, total)
  } else if (total >= 17 && total < 21) {
    #printf("Player %d: Stand\n", player_num)
    CheckStatus(player_num, total)
  } else {
    #printf("Player %d: Hit\n", player_num)
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

# Set the number of trials, decks, players, and shuffles to run in the simulation
trials = 1:25
decks = c(1, 2, 3, 4, 5)
player = c(1, 2, 3, 4, 5)
deck_shuffles = c(1, 2, 3, 4, 5)

#Create list to add data to in each pass
odds <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(odds) <- c("decks", "players", "shuffles", "wins")

for (curr_decks in decks) {
  for (curr_players in player) {
    for (curr_deck_shuffles in deck_shuffles) {
      for (trial in trials) {
        deck_count <- curr_decks
        numPlayers <- curr_players
        for (i in curr_deck_shuffles) {
          deck <- data.frame(cardtype, cardvalue, current_deckfreq, card_probs, stringsAsFactors = TRUE)
        }
        
        #The number of players n, must be passed in at the start
        players <- dfPopulate(numPlayers)
        
        #Deal initial two cards to players
        for (i in 1:length(players)) {
          curr <- drawCard()
          players <- updateTotal(i, curr, cardvalue[curr])
          curr <- drawCard()
          players <- updateTotal(i, curr, cardvalue[curr])
        }
        
        #Deal dealers cards
        dealer <- cardvalue[drawCard()] + cardvalue[drawCard()]
        change <- TRUE
        
        #Main loop
        #Finished - Checks if a player has won to end the loop
        #Change - Detects change in player cards to see when players are done
        while(!finished && change) {
          change <- FALSE
          for (i in 1:numPlayers) {
            if (players[[i]]$total[length(players[[i]]$total)] == 21) {
              finished <- TRUE
              break
            } else if (players[[i]]$total[length(players[[i]]$total)] <= 14) {
              curr <- drawCard()
              players <- updateTotal(i, curr, cardvalue[curr])
              change <- TRUE
            } else if (players[[i]]$total[length(players[[i]]$total)] > 21) {
              players[[i]]$status = "lost"
            }
          }
        }
        
        #Determine the player with the highest pair of legal cards
        highest = list(0, 0)
        for (i in 1:numPlayers) {
          if (players[[i]]$total[length(players[[i]]$total)] > highest[2]) {
            highest[1] = i
            highest[2] = players[[i]]$total[length(players[[i]]$total)]
          }
        }
        
        #Make dealer draw until they hit 17 or beat the player with the highest score
        while (dealer <= 17 && dealer < highest[2]) {
          dealer <- dealer + cardvalue[drawCard()]
          if (dealer <= 21 && !finished && dealer > highest[2]) {
            #print("Dealer Won!")
          } else if (dealer > 21) {
            #print("Dealer Busted!")
            #printf("Player %d Won!\n", as.numeric(highest[1]))
          }
        }
        
        #Add results to list
        if (as.numeric(highest[1]) == 1){
          odds <- rbind(odds, c(curr_decks, curr_players, curr_deck_shuffles, 1))
        } else {
          odds <- rbind(odds, c(curr_decks, curr_players, curr_deck_shuffles, 0))
        }
      }
    }
  }
}

#Turn list into data frame
data <- rbind.data.frame(odds)
colnames(data) <- c("decks", "players", "shuffles", "win")
#Filter the data for wins, and count those per each scenario
test <- data %>% filter(win == 1) %>% count(decks, players, shuffles)
print.data.frame(test)