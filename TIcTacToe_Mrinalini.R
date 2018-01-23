##Tic Tac Toe Code 

#Function 1: Display
state<- rep( NA,9)
# Function 1- Display
display<- function(state){
  place<- seq(1:9)
  for( i in 1:9){
    if( is.na(state[i])) { 
      place[i] = i
    }else{ 
        place[i]= state[i]
        }
  }
  cat( place[1], "   | ", place[2] , "| ", place[3], " \n",
       "---+---+---\n" ,
       place[4], "  | ", place[5] , "| " , place[6]  , "\n",
       "---+---+---\n" ,
       place[7], "  | ", place[8] , "| " , place[9]  , "\n")
}

# Function 2: Update --------------------------------------

update<- function( state, who, pos)
{
  if( is.na(state[pos])) {
    state[pos]<-who
  }else if(  state[pos] == "x" | state[pos] == "o") {
   stop("This position has been taken please try again")
  }else{ 
    stop(" You have made an error- please try again")}
  return(state)
  #state<- update( state, who, pos) # Not sure if necessary 
}


# Function #3 ----------------------------------------------
computer_turn<- function(state){
  # the comp needs 3 things: 1) What player number it is, 2) What positions it has played at ; 3) What positions the opponnet has played at # Are any of them lethal pairs if yes then it needs to block or win 
  player<- NA
  turnsyet<- sum( !is.na(state))
  if (turnsyet %% 2==0){ 
    player<- 1}
  
  # Assigning "x" or "o"
  if(!is.na(player)){ 
    c_char<-"x"
    o_char<- "o"
  } else{
    c_char<- "o"
    o_char<- "x"
  }
  
  # The computer "sees" the board
  cstate<- which( state== c_char ) 
  oppstate<- which( state== o_char )
  tracker<- NA
  
  # Any lethal positions: Winners  Test for all 8 winnning
  for( i in 1:8){
    if( sum(cstate %in% triples[[i]])==2) {
      need<- setdiff(triples[[i]], cstate) # Position needed to get the winning side
      tracker<- 1
      break
    }
  }
  
  # Test for opposing pairs     
  if(is.na(tracker)) { 
    for( j in 1:8){
      if (sum( oppstate %in% triples[[j]])==2){
        need<- setdiff(triples[[j]], oppstate) # Position needed to block opponent
        tracker<- 1
        break
      }
    }
  }
  
  # Play random turn-- in this case min number position available(not optimized)
  if( is.na(tracker)){
    need<- min(setdiff(c(1:9), union(cstate, oppstate)))
  }
  return(need) # The position the computer needs to play 
  # return (c(c_char, need)) # to see the position and letter returned by computer
}



#--------------------------------------------------------

#Funtcion 4: Checking winner 


check_winner<- function(state){
  winner<- NA
  f_win<- which( state=="x" )# P1
  s_win<- which( state== "o" )#P2
  for( i in 1:8 ){ # Checking against all triples again 
    if( sum(f_win %in% triples[[i]])==3){
      print("Player 1 wins")
      winner<-1
      break
    } else if(sum(s_win %in% triples[[i]])==3){
      print("Player 2 wins")
      winner<- 2
      break
    }
  }
    if(is.na(winner) && sum(is.na(state))==0) { 
      print("draw") 
      winner<-0
    }
    
  if(is.na(winner)){
    print("No winner yet, next turn")
  }
  invisible(winner)
}


#Function 5-------------------

play<- function(){
  state<- rep( NA,9)
  n<-  readline("Enter 0 if computer is not playing; Enter 1 if the computer is playing and is player 1 and 2 if the computer is player 2")
  winner<- NA
  while( is.na(winner))
    
  {
    if(n== 0){
      display(state)
      pos1<- readline( "What position would you like to play?")
      pos1<- as.integer(pos1)
      
      turnsyet<- sum(!is.na(state))
      if (turnsyet %% 2==0){ 
        who = "x"} else { who= "o"} # Determining whose turn it is
      
      state<- update( state, who, pos1)
      display(state)
      check_winner(state)
      winner<- check_winner(state)
      
      
      pos2<- readline("What position would you like to play?")
      pos2<- as.integer(pos2)
      
      turnsyet<- sum( !is.na(state))
      if (turnsyet %% 2==0){ who = "x"} else { who= "o"} # Determining whose turn it is
      
      state<- update( state, who, pos2)
      display(state)
      check_winner(state)
      winner<- check_winner(state)
      
    }else if ( n == 2){ # Computer playing player 2
      display(state)
      pos1<- readline( "What position would you like to play?")
      pos1<- as.integer(pos1)
      
      state<- update( state, "x", pos1)
      display(state)
      check_winner(state)
      winner<- check_winner(state)
      
      
      pos2<- computer_turn(state)
      
      state<- update( state, "o", pos2)
      display(state)
      check_winner(state)
      winner<- check_winner(state)
      
    }else if ( n ==1){
      
      pos1<- computer_turn(state)
      
      state<- update( state, "x", pos1)
      display(state)
      check_winner(state)
      winner<- check_winner(state)
      
      display(state)
      pos2<- readline( "Your turn?")
      pos2<- as.integer(pos2)
      
      state<- update( state, "o", pos2)
      display(state)
      check_winner(state)
      winner<- check_winner(state)
    }
  }
  print("Game Over")
} 