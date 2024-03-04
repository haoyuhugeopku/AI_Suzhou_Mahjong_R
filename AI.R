# 6 AI助手

aiNewbee <- function(player) {
  hand <- player_hands[[player]]
  strength <- rep(0, length(player_hands[[player]]))
  
  colortend <- 0
  pongtend <- 0
  windtend <- 0
  avoidtend <- 0
  fasttend <- 0
  potentialtend <- 0
  montecarlotend <- 0
  montecarlotimes <- 0
  tendency <- ""
  tendencyAvoid <- FALSE
  tendencyFast <- FALSE
  tendencyPotential <- FALSE
  tendencyMontecarlo <- FALSE
  
  if (tendency == "onecolor") {
    colortend <- 3
  }
  if (tendency == "pongponghu") {
    pongtend <- 10
  }
  if (tendency == "softflowers") {
    windtend <- 5
  }
  
  for (i in 1:length(player_hands[[player]])) {
    if(hand[i] %in% wind_cards) {
      strength[i] <- 10 + windtend
      for (j1 in 1:length(player_hands[[player]])) {
        if (j1 != i && hand[i] == hand[j1]) {
          strength[i] <- strength[i] + 20 + pongtend + windtend * 2
        }
      }
    }

    else {
      num <- as.numeric(substr(hand[i], 1, 1))
      suit <- substr(hand[i], 2, 2)
      num_cards <-c(circles, myriads, strings)
      for (j2 in 1:length(player_hands[[player]])) {
        if (hand[j2] %in% num_cards) {
          num2 <- as.numeric(substr(hand[j2], 1, 1))
          suit2 <- substr(hand[j2], 2, 2)
          if (j2 != i && suit2 == suit) {
            strength[i] <- strength[i] + 1 + colortend
            diffnum <- abs(num - num2)
            if (diffnum == 0) {
              strength[i] <- strength[i] + 20 + pongtend
            }
            else if (diffnum == 1) {
              strength[i] <- strength[i] + 10
            }
            else if (diffnum == 2) {
              strength[i] <- strength[i] + 5
            }
            else if (diffnum == 3) {
              strength[i] <- strength[i] + 2
            }
            else if (diffnum == 4) {
              strength[i] <- strength[i] + 1
            }
          }
        }
      }

      if (length(player_pongs[[player]]) > 0) {
        for (j3 in 1:length(player_pongs[[player]])) {
          if (hand[j3] %in% num_cards) {
            num3 <- as.numeric(substr(hand[j3], 1, 1))
            suit3 <- substr(hand[j3], 2, 2)
            if (j3 != i && suit3 == suit) {
              strength[i] <- strength[i] + 2 + colortend * 2 
            }
          }
        }
      }
      if (length(player_gangs[[player]]) > 0) {
        for (j4 in 1:length(player_gangs[[player]])) {
          if (hand[j4] %in% num_cards) {
            num4 <- as.numeric(substr(hand[j4], 1, 1))
            suit4 <- substr(hand[j4], 2, 2)
            if (j4 != i && suit4 == suit) {
              strength[i] <- strength[i] + 3 + colortend * 2
            }
          }
        }
      }
    }

    if (tendencyAvoid) {
      if (length(if_can_dianpao(player, hand[i])) > 0) {
        claiming_available[1] <<- FALSE
        claiming_available[2] <<- FALSE
        claiming_available[3] <<- FALSE
        claiming_available[4] <<- FALSE
        strength[i] <- strength[i] + 10 * avoidtend
      }
    }

    if (tendencyFast) {
      if (sum(if_listen(player, hand[-i])$nums) > 0) {
        strength[i] <- strength[i] - sum(if_listen(player, hand[-i])$nums) * 10 * fasttend
      }
    }

    if (tendencyPotential) {
      shorthand <- hand[-i]
      sequential_cards <- shorthand[grepl("[0-9]", shorthand)] 
      adjacent_cards <- c() 
      for (eachcard in sequential_cards) {
        suit <- substr(eachcard, 2, 2)  
        num <- as.numeric(substr(eachcard, 1, 1))  
        if (num > 1) {  
          adjacent_cards <- c(adjacent_cards, paste0(num - 1, suit))
        }  
        if (num < 9) {  
          adjacent_cards <- c(adjacent_cards, paste0(num + 1, suit)) 
        } 
      }
      biggerhand <- unique(c(shorthand, adjacent_cards)) 
      for (mi in 1:length(shorthand)) {
        for (mj in 1:length(biggerhand)) {
          testhand <- c(shorthand[-mi], biggerhand[mj])
          prob <- sum(if_listen(player, testhand)$nums)
          strength[i] <- strength[i] - prob * potentialtend
        }
      }
    }

    if (montecarlotimes > 0) {
      for (mi in 1:montecarlotimes) {
        shorthand <- hand[-i]
        for (mj in 1:length(shorthand)) {
          randomnum <- sample(1:length(remaining_deck),1)
          randomhand <- c(shorthand[-mj], remaining_deck[randomnum])
          prob <- sum(if_listen(player, randomhand)$nums)
          strength[i] <- strength[i] - prob * montecarlotend
        }
      }
    }
  }
  
  for (i in 1:length(player_hands[[player]])) {
    cat (hand[i],"得分",strength[i], "\n")
  }

  min_score_index <- which.min(strength)  
  return(hand[min_score_index])
}
