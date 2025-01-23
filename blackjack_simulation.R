########### Blackjack Simulation
# functions for keeping track of wins with what hands dealt
# count of cards tracker
########## Build Deck ########

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
cards <- c("Ace", "Two", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
values <- c(11, 2:9, rep(10, 4))
totalNumOfDecks <- 6

deck <- expand.grid(cards=cards, suits=suits)
deck$value <- values
deck <- droplevels(deck[rep(seq(nrow(deck)), totalNumOfDecks),])

########## Functions ########

library(rlist)
library(dplyr)

shuffle <- function(){
  return(deck[c(sample(312, 312, replace = FALSE)),])}

draw_card = function(y){
  x = shuffle_deck[y,]
  return(x)}

hand_total = function(hand_df){
  sum = sum(hand_df[,3])
  aces <- hand_df %>%
    filter(cards == "Ace") %>%
    summarise(n = n())
  if (sum>21 & aces[1,1]>0){
    sum = sum - 10
    if (sum>21 & aces[1,1]>1){
      sum = sum-10
      if (sum>21 & aces[1,1]>2){
        sum = sum -10}}}
  return(sum)}

results_update = function(overall, in_game){
  overall[1,4] = overall[1,4]+1
  overall[1,in_game] = overall[1,in_game]+1
  return(overall)}

########## Game Play ########

play_setup <- TRUE
while (play_setup == TRUE){
  
  # simulations
  numbers <- as.integer(readline("Enter 2-card hand total: "))
  while (is.na(numbers) | (numbers < 4) | (numbers > 21)){
    print("Invalid number! Please enter a positive value beteen 4 and 21.")
    numbers <- as.integer(readline("Enter 2-card hand total: "))}
  showing <- as.integer(readline("Dealer showing a : "))
  while (is.na(showing) | (showing < 2) | (showing > 11)){
    print("Invalid number! Please enter a positive value beteen 2 and 11.")
    numbers <- as.integer(readline("Enter number that dealer is showing: "))}
  statistics <- list("stand" = data.frame("wins"=0, 'losses'=0, 'pushes'=0, "games"=0),
                     "hit" = data.frame("wins"=0, 'losses'=0, 'pushes'=0, "games"=0))
  number_of_trials <- as.integer(readline("Enter number of trials: "))
  while (is.na(number_of_trials) | number_of_trials<1){
    print("Invalid number! Please enter a positive value.")
    number_of_trials <- as.integer(readline("Enter number of trials: "))} 
  
  # deck shuffle
  shuffle_deck = shuffle()
  card_cut = sample(floor(length(deck[,1])/3):floor(length(deck[,1])*2/3), 1, replace = FALSE)
  current_card = 1
  game = 1
  
  # Main Gameplay
  while (game <= number_of_trials){
    
    # Reset hands
    player_hand = data.frame('cards' = 'setup', 'suits' = 'setup', 'value' = 0) 
    deal_hand = data.frame('cards' = 'setup', 'suits' = 'setup', 'value' = 0) 
    game_results = 0
    
    # Draw 2 cards each
    for (i in 1:2){
      player_hand = rbind(player_hand, draw_card(current_card))
      current_card = current_card + 1
      deal_hand = rbind(deal_hand, draw_card(current_card))
      current_card = current_card + 1
      if (i==1){
        player_hand = player_hand[2,]
        deal_hand = deal_hand[2,]}}
    
    if (hand_total(player_hand)==numbers & showing == hand_total(deal_hand[2,])){
      
      if (hand_total(player_hand)==21){
        if (hand_total(deal_hand)==21){
          game_results = 3}
        else {game_results = 1}} 
    
  
      # Hit/Stay
      if (floor(number_of_trials/2)<game){
        player_hand = rbind(player_hand, draw_card(current_card))
        current_card = current_card + 1}

    
      # Dealer hitting
      while (hand_total(deal_hand)<17){
        deal_hand = rbind(deal_hand, draw_card(current_card))
        #print(paste0("----The dealer drew a ", draw_card(current_card)[1,1], ' of ', draw_card(current_card)[1,2]))        
        #print(paste0("Dealer has ", hand_total(deal_hand), '.'))
        current_card = current_card + 1}
    
  
  
      # End Result
      if (game_results==0){
        if (hand_total(player_hand)>21){game_results=2}
        else if (hand_total(player_hand)<22 & hand_total(deal_hand)>21){game_results=1}
        else if (hand_total(player_hand)==hand_total(deal_hand)){game_results=3}
        else if (hand_total(player_hand)>hand_total(deal_hand)){game_results=1}
        else if (hand_total(player_hand)<hand_total(deal_hand)){game_results=2}
        game = game + 1}
     
      # update statistics
      if (floor(number_of_trials/2)<game-1){
        statistics[[2]] = results_update(statistics[[2]], game_results)}
      if (floor(number_of_trials/2)>=game-1){
        statistics[[1]] = results_update(statistics[[1]], game_results)}
      
    } # end loop of gameplay simulation
    
    # shuffle if needed
    if (card_cut<=current_card){
      shuffle_deck = shuffle()
      card_cut = sample(floor(length(deck[,1])/3):floor(length(deck[,1])*2/3), 1, replace = FALSE)
      current_card = 1}

  } # Game Play loop
  
  for (i in 1:2){
    print(names(statistics)[i])
    print(statistics[[i]])
    print(paste0("Winning Percent: ", 100*statistics[[i]][1,1]/statistics[[i]][1,4]))
    print(paste0("Losing Percent: ", 100*statistics[[i]][1,2]/statistics[[i]][1,4]))
    print(paste0("Push Percent: ", 100*statistics[[i]][1,3]/statistics[[i]][1,4]))
    print("")
  }
  play_setup = FALSE  
} # Game Setup loop
print(statistics)
names(statistics)
