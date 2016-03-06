setwd("D:/programming/personal/bankingSimulation/")

deck=matrix(c(rep( c(2:10,"J","Q","K","A"),4),rep(c("C","D","H","S"),rep(13,4))), ncol=2,dimnames=list(NULL,c("rank","suit")))

# Eight players is the max number that can sit at a table
numPlay = 8
numBank = 2
stackSize = 100000
minBet = 5
#averageBet = 5
anteBet = 5
playerPairBet = 5
sixCardBonusBet = 5
anteFee = 1
sixCardBonusFee = 1
bankerFee = 3
maxSeatPayout = 30000
maxTablePayout = 100000
# Assuming ten hours a weekend and thirty hands an hour for three months, this is the approximate number of hands: 4000
numHands = 1000

playerPairPayout = c(-1, 1, 3, 6, 30, 40, 200)
sixCardBonusPayout = c(-1, 7, 10, 15, 20, 100, 200, 1000)

numTrials = 10000

deal_hand = function()
{
    return( deck[sample(1:52,3,replace=F),])
}


what_hand = function(hand)
{    
    ranks_acehigh = c(2:10,"J","Q","K","A")
    ranks_acelow = c("A",2:10,"J","Q","K")
    
    rank_i_ah = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acehigh)) )
    rank_i_al = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acelow)) )
    
    is_straight = all( rank_i_ah-min(rank_i_ah)+1 == 1:3 ) | all( rank_i_al-min(rank_i_al)+1 == 1:3 )
    is_flush = length(unique(hand[,"suit"])) == 1
    
# 7 = Royal Flush    
    if (is_straight && is_flush) {
        if (all(c("K","A") %in% hand[,"rank"])){
          handRank = c(7, 13)
          return(handRank)
        }
# 6 = Straight Flush      
        else {
          if(all( rank_i_ah-min(rank_i_ah)+1 == 1:3 )) {
            highCardStraight = max(rank_i_ah)
          }
          else {
            highCardStraight = max(rank_i_al) - 1
          }
          handRank = c(6, highCardStraight)
          return(handRank)
        }
    }
    
# 4 = Straight    
    if (is_straight){
      if(all( rank_i_ah-min(rank_i_ah)+1 == 1:3 )) {
        highCardStraight = max(rank_i_ah)
      }
      else {
        highCardStraight = max(rank_i_al) - 1
      }
      handRank = c(4, highCardStraight)
      return(handRank)
    } 
    if (is_flush){
# 3 = Flush      
      highCard = max(rank_i_ah)
      handRank = c(3, highCard)
      return(handRank)
    }
    
    tab = sort( table(hand[,"rank"]) )
# 5 = Three of a Kind    
    if (length(tab)==1) {
      highCard = max(rank_i_ah)
      handRank = c(5, highCard)
      return(handRank)
    }
    if (length(tab)==2) {
#        if (all(tab == c(1,4))) return( "Four of a kind" )
#        if (all(tab == c(2,3))) return( "Full house")
# 2 = Pair      
      pairCards = table(rank_i_ah)
      highCard = as.integer(names(pairCards[match(2, pairCards)]))
      handRank = c(2, highCard)
      return(handRank)
    }
#    if (length(tab)==3) {
#        if (all(tab == c(1,1,3))) return( "Three of a kind")
#        if (all(tab == c(1,2,2))) return( "Two pair" )
#    }
#    if (length(tab)==4) {
#        return( "Pair" )
#    }
# 1 = No Pair
    highCard = max(rank_i_ah)
    handRank = c(1, highCard)
    return(handRank)
}

#simulate = function(N=10000) 
#{
#    hands = c("Royal flush", "Straight flush", "Four of a kind",
#              "Full house", "Flush", "Straight",
#              "Three of a kind", "Two pair", "Pair", "No pair")

#    hands = c("Royal flush", "Straight flush", "Three of a kind", 
#              "Straight", "Flush", "Pair", "No pair")        
    
#    res = matrix(rep(0,length(hands)),ncol=1)
#    rownames(res) = hands
#    colnames(res) = "Counts"
    
#    pb = txtProgressBar(min = 0, max = N, style = 3)
#    for(i in 1:N) {
#        hand = what_hand(deal_hand())
    
#        res[hand,1] = res[hand,1]+1
#        setTxtProgressBar(pb, i)
#    }
    
#    return(res)
#}

fullHand = function(numPlay)
{
  totalCards = 3 * numPlay
  
  hands = deck[sample(1:52,totalCards,replace=F),]
  
  allHands = vector("list", numPlay)
  for(i in 1:numPlay) {
    initialCard = ((i - 1)*3) + 1
    allHands[[i]] = hands[initialCard:(initialCard+2), 1:2]
  }
  return(allHands)
}

handWinner = function(allHands, numPlay) {
  handRanks = matrix(nrow = numPlay, ncol = 2)
  for(i in 1:numPlay) {
    handRanks[i, ] = what_hand(allHands[[i]])
  }
  
  return(handRanks)
  
}

playerHand = function(allHands, handRanks, playerHandRank, bet, playerNum) {
  dealerHand = handRanks[1, ]
  myHand = playerHandRank
  
  ranks_acehigh = c(2:10,"J","Q","K","A")
    
  dealerHandRanks = sort( sapply(allHands[[1]][,"rank"],function(x) which(x == ranks_acehigh)) )
  playerHandRanks = sort( sapply(allHands[[playerNum]][,"rank"],function(x) which(x == ranks_acehigh)) )
  
  if(myHand[1] > 1 || (myHand[1] == 1 && playerHandRanks[3] > 10) || (myHand[1] == 1 && 
                        playerHandRanks[3] > 10 && playerHandRanks[2] > 5) || (myHand[1] == 1 &&
                        playerHandRanks[3] > 10 && playerHandRanks[2] > 5 && playerHandRanks[1] > 3)) {
    playBet = 2 * bet
  }
  else {
    payout = -bet
    return(payout)
  }
  
  if(dealerHand[1] == 1 && dealerHand[2] < 11) {
    payout = bet
    return(payout)
  }
  
  if(dealerHand[1] > myHand[1]) {
    payout = -playBet
    return(payout)
  }
  
  if(dealerHand[1] == myHand[1] && dealerHand[2] > myHand[2]) {
    payout = -playBet
    return(payout)
  }
  
  if(dealerHand[1] == myHand[1] && dealerHand[2] == myHand[2]) {
    if(dealerHand[1] == 2) {
      dealerPair = table(dealerHandRanks)
      playerPair = table(playerHandRanks)
      dealerHighCard = as.integer(names(dealerPair[match(1, dealerPair)]))
      playerHighCard = as.integer(names(playerPair[match(1, playerPair)]))
      
      if(dealerHighCard > playerHighCard) {
        payout = -playBet
        return(payout)
      }
      
      if(dealerHighCard < playerHighCard) {
        payout = playBet
        return(payout)
      }
    }
    
    if(dealerHand[1] == 1) {
      if(dealerHandRanks[2] > playerHandRanks[2]) {
        payout = -playBet
        return(payout)
      }
      
      if(dealerHandRanks[2] < playerHandRanks[2]) {
        payout = playBet
        return(payout)
      }
      
      if(dealerHandRanks[1] > playerHandRanks[1]) {
        payout = -playBet
        return(payout)
      }
      
      if(dealerHandRanks[1] < playerHandRanks[1]) {
        payout = playBet
        return(payout)
      }
    }
    payout = 0
    return(payout)
  }
  
  payout = playBet
  return(payout)
}

playerRound = function() {
    allHands = fullHand(numPlay)
    handRanks = handWinner(allHands, numPlay)

    playerPayout = (playerHand(allHands, handRanks, handRanks[2, ], minBet, 2) - anteFee)
    
    return(playerPayout)
}

bankRound = function(bankPayoutRound) {
  allHands = fullHand(numPlay)
  handRanks = handWinner(allHands, numPlay)
  
  for(j in 2:numPlay) {
    sixCardHand = rbind(allHands[[1]], allHands[[j]])
    sixCardRank = whatHandSixCard(sixCardHand)
    
    baseWagerPayout = playerHand(allHands, handRanks, handRanks[j, ], anteBet, j)
    
    if(baseWagerPayout == -anteBet) {
      sixCardRank = 1
    }
    
    bankPayoutRound[j - 1] = -baseWagerPayout - playerPairPayout[handRanks[j, 1]] * playerPairBet - sixCardBonusPayout[sixCardRank] * sixCardBonusBet
    
    if(bankPayoutRound[j - 1] < -maxSeatPayout) {
      bankPayoutRound[j - 1] = -maxSeatPayout
      }
  }
  
  return(bankPayoutRound)
}

simulateSession = function(numHands) {
  newStackSize = vector("integer", (numHands + 1))
  bankPayoutRound = vector("integer", (numPlay - 1))
  newStackSize[1] = stackSize
  whoIsBank = rep(1:numBank, length.out = numHands, each = 2)
  
  for (i in 1:numHands) {
    if(whoIsBank[i] != 2) {
      playerPayout = playerRound()
      
      newStackSize[i+1] = newStackSize[i] + playerPayout
    }
    
    if(whoIsBank[i] == 2) {
      bankPayoutRound = bankRound(bankPayoutRound)
      
      totalBankRoundPayout = sum(bankPayoutRound)
      
      if(totalBankRoundPayout < -maxTablePayout) {
        totalBankRoundPayout = -maxTablePayout
        }
      
      newStackSize[i+1] = newStackSize[i] + totalBankRoundPayout - bankerFee
    }
  }

  return(newStackSize)
}

whatHandSixCard = function(hand)
{    
  ranks_acehigh = c(2:10,"J","Q","K","A")
  ranks_acelow = c("A",2:10,"J","Q","K")
  
  rank_i_ah = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acehigh)) )
  rank_i_al = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acelow)) )
  
  if(length(unique(rank_i_ah)) > 4) {
    straightRankAH = unique(rank_i_ah)
    straightRankAL = unique(rank_i_al)
    
    if(length(straightRankAH) == 5) {
      is_straight = all( straightRankAH-min(straightRankAH)+1 == 1:5 ) | all( straightRankAL-min(straightRankAL)+1 == 1:5 )
    }
    else {
      AH1 = straightRankAH[-6]
      AH2 = straightRankAH[-1]
      AL1 = straightRankAL[-6]
      AL2 = straightRankAL[-1]
      is_straight = all( AH1-min(AH1)+1 == 1:5 ) | all( AH2-min(AH2)+1 == 1:5 ) | all( AL1-min(AL1)+1 == 1:5 ) | all( AL2-min(AL2)+1 == 1:5 )
    }
  }
  else {
    is_straight = F
  }
  
#  is_flush = length(unique(hand[,"suit"])) == 1
  is_flush = max(table(hand[ , "suit"])) > 4
  
  isStraightFlush = F
  
  if(length(unique(hand[ , "suit"])) == 1 && is_straight) {
    isStraightFlush = T
  }
  
  if(max(table(hand[ , "suit"])) == 5) {
    suitsSixCardHand = table(hand[ , "suit"])
    
    flushSuit = names(suitsSixCardHand[suitsSixCardHand == 5])
    fiveCardFlush = hand[hand[ , "suit"] == flushSuit, ]
    
    rankAHFiveCard = sort( sapply(fiveCardFlush[,"rank"],function(x) which(x == ranks_acehigh)) )
    rankALFiveCard = sort( sapply(fiveCardFlush[,"rank"],function(x) which(x == ranks_acelow)) )
    
    isStraightFlush = all( rankAHFiveCard-min(rankAHFiveCard)+1 == 1:5 ) | all( rankALFiveCard-min(rankALFiveCard)+1 == 1:5 )
  }
  
  # 8 = Royal Flush    
  if (isStraightFlush) {
    if (all(c("Q", "K","A") %in% hand[,"rank"])){
      handRank = 8
      return(handRank)
    }
    # 7 = Straight Flush      
    else {
      handRank = 7
      return(handRank)
    }
  }
  
  #6 = Four of a Kind
  if(any(table(hand[ , 1]) == 4)) {
    handRank = 6
    return(handRank)
  }
  
  tab = sort( table(hand[,"rank"]) )
  
  #5 = Full House
  
  if(length(tab) == 2) {
    if(all(tab == c(3,3))) {
      handRank = 5
      return(handRank)
    }
  }
  if(length(tab) == 3) {
    if(all(tab == c(1, 2, 3))) {
      handRank = 5
      return(handRank)
    }
  }
  
  if (is_flush){
    # 4 = Flush
    handRank = 4
    return(handRank)
  }
  
  # 3 = Straight    
  if (is_straight){
    handRank = 3
    return(handRank)
  }

  # 2 = Three of a Kind    
  if (max(tab) == 3) {
    handRank = 2
    return(handRank)
  }

  # 1 = No Pair
  handRank = 1
  return(handRank)
}

sessionStats = matrix(nrow = numTrials, ncol = 2, dimnames = list(NULL, c("finalStackSize", "lowestStackSize")))
allStacks = matrix(nrow = (numHands+1), ncol = numTrials)

for(i in 1:numTrials) {
  finalStack = simulateSession(numHands)
  
  allStacks [ , i] = finalStack
  sessionStats[i, ] = c(finalStack[(numHands+1)], min(finalStack))
}


# This code block is for testing the behavior of each bet individually, all in one run of the code.
# The same results can be achieved with the main program by changing the betting amounts, but this version
# let me check each individual bet at once.

# sessionStatsAnteBet = matrix(nrow = numTrials, ncol = 2, dimnames = list(NULL, c("finalStackSizeAnte", "lowestStackSizeAnte")))
# sessionStatsPairBet = matrix(nrow = numTrials, ncol = 2, dimnames = list(NULL, c("finalStackSizePair", "lowestStackSizePair")))
# sessionStatsSixBet = matrix(nrow = numTrials, ncol = 2, dimnames = list(NULL, c("finalStackSizeSix", "lowestStackSizeSix")))
# 
# for(i in 1:numTrials) {
#   anteBet = 5
#   playerPairBet = 0
#   sixCardBonusBet = 0
#   
#   finalStack = simulateSession(numHands)
#   
#   sessionStatsAnteBet[i, ] = c(finalStack[numHands], min(finalStack))
# }
# 
# for(i in 1:numTrials) {
#   anteBet = 0
#   playerPairBet = 5
#   sixCardBonusBet = 0
#   
#   finalStack = simulateSession(numHands)
#   
#   sessionStatsPairBet[i, ] = c(finalStack[numHands], min(finalStack))
# }
# 
# for(i in 1:numTrials) {
#   anteBet = 0
#   playerPairBet = 0
#   sixCardBonusBet = 5
#   
#   finalStack = simulateSession(numHands)
#   
#   sessionStatsSixBet[i, ] = c(finalStack[numHands], min(finalStack))
# }



#This is for visualisation purposes. A handy way to save typing a couple lines of code.

#plot(finalStack)
#summary(sessionStats)

# This is for establishing the size of the stack to use--I want a one percent chance, but ran the simulation
# 1000 times for more reliability.

# ses20 <- read.csv("data/sessionStats75000Stack20Bet1000Hands1000Trials.csv")
# for(i in 1:10) {avgMinSize[i] <- min(ses20[(100*(i-1) + 1):(100*i), 3])}
# summary(avgMinSize)
# (75000-49570) + 15000
# for(i in 1:10) {avgFinSize[i] <- mean(ses20[(100*(i-1) + 1):(100*i), 2])}
# summary(avgFinSize)
# ((88250 - 75000) / 1000) * 35

