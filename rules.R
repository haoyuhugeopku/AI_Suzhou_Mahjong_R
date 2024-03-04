# 2 胡牌规则定义

is_doublecard <- function(cards) {  
  card_counts <- table(cards)  
  any(card_counts >= 2)   
} 
 
extract_doublecards <- function(cards) {  
  card_counts <- table(cards)  

  double_pairs <- data.frame(  
    Pair = character(),  
    Count = integer(),  
    stringsAsFactors = FALSE  
  )  

  for (card in names(card_counts)) {  
    count <- card_counts[card]  
    if (count == 4) {  
      double_pairs <- rbind(double_pairs, data.frame(Pair = card, Count = 2))  
    }  
    else if (count == 3) {  
      double_pairs <- rbind(double_pairs, data.frame(Pair = card, Count = 2))  
    }
    else if (count == 2) {  
      double_pairs <- rbind(double_pairs, data.frame(Pair = card, Count = 2))  
    }  
  }  
  
  return(double_pairs)  
}  

is_triplet_or_quad <- function(cards) {  
  card_counts <- table(cards)  
  any(card_counts >= 3) 
}  

extract_triplets <- function(cards) {  
  card_counts <- table(cards)  

  triplets <- data.frame(  
    Triplet = character(),  
    Count = integer(),  
    stringsAsFactors = FALSE  
  )  

  for (card in names(card_counts)) {  
    count <- card_counts[card]  
    if (count == 4) {  
      triplets <- rbind(triplets, data.frame(Triplet = card, Count = 3))  
    }  
    else if (count == 3) {  
      triplets <- rbind(triplets, data.frame(Triplet = card, Count = 3)) 
    }
  }  
  
  return(triplets)  
}  

count_wind_triplets <- function(cards) {  
  winds <- c("东", "南", "西", "北")  
  wind_counts <- sapply(winds, function(wind) sum(cards == wind))  
  triplet_count <- sum(wind_counts >= 3)  
  return(triplet_count)  
} 

count_wind_pairs <- function(cards) {  
  winds <- c("东", "南", "西", "北")  
  wind_counts <- sapply(winds, function(wind) sum(cards == wind))  
  pair_count <- sum(wind_counts == 2)  
  return(pair_count)  
}

count_dragon_triplets <- function(cards) {  
  dragons <- c("红中", "发财", "白板")  
  dragon_counts <- sapply(dragons, function(dragon) sum(cards == dragon))  
  triplet_count <- sum(dragon_counts >= 3)  
  return(triplet_count)  
} 

count_dragon_pairs <- function(cards) {  
  dragons <- c("红中", "发财", "白板")  
  dragon_counts <- sapply(dragons, function(dragon) sum(cards == dragon))  
  pair_count <- sum(dragon_counts == 2)  
  return(pair_count)  
} 

is_straight <- function(cards) {  
  cards <- unique(cards)
  
  suits <- sapply(cards, function(x) substr(x, 2, 2))  
  numbers <- sapply(cards, function(x) as.numeric(substr(x, 1, 1)))  

  valid_cards <- !is.na(numbers)

  suits <- suits[valid_cards]  
  numbers <- numbers[valid_cards]   

  if (length(numbers) < 3) {  
    return(FALSE)  
  }  

  n <- length(numbers)  
  for (i in 1:(n - 2)) {  
    for (j in (i + 1):(n - 1)) {  
      for (k in (j + 1):n) {  
        if (suits[i] == suits[j] && suits[j] == suits[k]) {  
          sorted_nums <- sort(c(numbers[i], numbers[j], numbers[k]))  
          if (sorted_nums[3] - sorted_nums[1] == 2) {  
            return(TRUE) 
          }  
        }  
      }  
    }  
  }  
  
  return(FALSE) 
}

extract_straights <- function(cards) {  
  straights <- list()  
  
  cards <- unique(cards)

  suits <- sapply(cards, function(x) substr(x, 2, 2))  
  numbers <- sapply(cards, function(x) as.numeric(substr(x, 1, 1)))  

  valid_cards <- !is.na(numbers) 

  suits <- suits[valid_cards]  
  numbers <- numbers[valid_cards]   
  cards <- cards[valid_cards] 
 
  n <- length(numbers)  
  for (i in 1:(n - 2)) {  
    for (j in (i + 1):(n - 1)) {  
      for (k in (j + 1):n) {  
        if (suits[i] == suits[j] && suits[j] == suits[k]) {  
          sorted_nums <- sort(c(numbers[i], numbers[j], numbers[k]))  
          if (sorted_nums[3] - sorted_nums[1] == 2) {  
            straight <- sort(c(cards[i], cards[j], cards[k]))
            straights <- c(straights, list(straight))  
          }  
        }  
      }  
    }  
  } 

  straights_count <- length(straights)  
  return(list(straights = straights, count = straights_count))  
} 

is_pinghu14 <- function(cards) {  
  whether_pinghu <- 0

  if (!is_doublecard(cards)) {  
    return(FALSE)  
  }  
  extracteddoubles <- extract_doublecards(cards)

  for (i in 1:(nrow(extracteddoubles))) {  
    current_doublecard_index <- which(cards == extracteddoubles[i,1])[1]
    current_card <- cards[-current_doublecard_index]
    current_doublecard_index <- which(current_card == extracteddoubles[i,1])[1]
    current_card <- current_card[-current_doublecard_index]

    if (is_triplet_or_quad(current_card)) {
      for (j1 in 1:(nrow(extract_triplets(current_card)))) {  
        current_triplet_index <- which(current_card == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card[-current_triplet_index]
        current_triplet_index <- which(current_card_j1 == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card_j1[-current_triplet_index]      
        current_triplet_index <- which(current_card_j1 == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card_j1[-current_triplet_index]   

        if (is_triplet_or_quad(current_card_j1)) {
          for (k1 in 1:(nrow(extract_triplets(current_card_j1)))) {
            current_triplet_index <- which(current_card_j1 == extract_triplets(current_card_j1)[k1,1])[1]
            current_card_k1 <- current_card_j1[-current_triplet_index]
            current_triplet_index <- which(current_card_k1 == extract_triplets(current_card_j1)[k1,1])[1]
            current_card_k1 <- current_card_k1[-current_triplet_index]      
            current_triplet_index <- which(current_card_k1 == extract_triplets(current_card_j1)[k1,1])[1]
            current_card_k1 <- current_card_k1[-current_triplet_index]  

            if (is_triplet_or_quad(current_card_k1)){
              for (m1 in 1:(nrow(extract_triplets(current_card_k1)))) {
                current_triplet_index <- which(current_card_k1 == extract_triplets(current_card_k1)[m1,1])[1]
                current_card_m1 <- current_card_k1[-current_triplet_index]
                current_triplet_index <- which(current_card_m1 == extract_triplets(current_card_k1)[m1,1])[1]
                current_card_m1 <- current_card_m1[-current_triplet_index]      
                current_triplet_index <- which(current_card_m1 == extract_triplets(current_card_k1)[m1,1])[1]
                current_card_m1 <- current_card_m1[-current_triplet_index]  

                if (is_straight(current_card_m1) || is_triplet_or_quad(current_card_m1)) {  
                  whether_pinghu <- whether_pinghu + 1
                }
              }
            }

            else if (is_straight(current_card_k1))  {
              straights_k1 <- extract_straights(current_card_k1)
              for (m2 in 1:straights_k1[[2]]) {
                current_straight_index <- which(current_card_k1 == straights_k1[[1]][[m2]][1])[1]
                current_card_m2 <- current_card_k1[-current_straight_index]
                current_straight_index <- which(current_card_m2 == straights_k1[[1]][[m2]][2])[1]
                current_card_m2 <- current_card_m2[-current_straight_index]
                current_straight_index <- which(current_card_m2 == straights_k1[[1]][[m2]][3])[1]
                current_card_m2 <- current_card_m2[-current_straight_index]

                if (is_straight(current_card_m2)) {  
                  whether_pinghu <- whether_pinghu + 1
                }
              }
            }
            
          }
        }

        else if (is_straight(current_card_j1))  {
          straights_j1 <- extract_straights(current_card_j1)
          for (k2 in 1:straights_j1[[2]]) {
            current_straight_index <- which(current_card_j1 == straights_j1[[1]][[k2]][1])[1]
            current_card_k2 <- current_card_j1[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j1[[1]][[k2]][2])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j1[[1]][[k2]][3])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]

            if (is_straight(current_card_k2)) {
              straights_k2 <- extract_straights(current_card_k2)
              for (m2 in 1:straights_k2[[2]]) {
                current_straight_index <- which(current_card_k2 == straights_k2[[1]][[m2]][1])[1]
                current_card_m2 <- current_card_k2[-current_straight_index]
                current_straight_index <- which(current_card_m2 == straights_k2[[1]][[m2]][2])[1]
                current_card_m2 <- current_card_m2[-current_straight_index]
                current_straight_index <- which(current_card_m2 == straights_k2[[1]][[m2]][3])[1]
                current_card_m2 <- current_card_m2[-current_straight_index]

                if (is_straight(current_card_m2)) {  
                  whether_pinghu <- whether_pinghu + 1
                }
              }
            }
          }
        }
      }
    }

    else if (is_straight(current_card)) {
      straights_i <- extract_straights(current_card)
      for (j2 in 1:straights_i[[2]]) {  
        current_straight_index <- which(current_card == straights_i[[1]][[j2]][1])[1]
        current_card_j2 <- current_card[-current_straight_index]
        current_straight_index <- which(current_card_j2 == straights_i[[1]][[j2]][2])[1]
        current_card_j2 <- current_card_j2[-current_straight_index]
        current_straight_index <- which(current_card_j2 == straights_i[[1]][[j2]][3])[1]
        current_card_j2 <- current_card_j2[-current_straight_index]

        if (is_straight(current_card_j2)) {
          straights_j2 <- extract_straights(current_card_j2)
          for (k2 in 1:straights_j2[[2]]) {
            current_straight_index <- which(current_card_j2 == straights_j2[[1]][[k2]][1])[1]
            current_card_k2 <- current_card_j2[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j2[[1]][[k2]][2])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j2[[1]][[k2]][3])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]

            if (is_straight(current_card_k2)) {
              straights_k2 <- extract_straights(current_card_k2)
              for (m2 in 1:straights_k2[[2]]) {
                current_straight_index <- which(current_card_k2 == straights_k2[[1]][[m2]][1])[1]
                current_card_m2 <- current_card_k2[-current_straight_index]
                current_straight_index <- which(current_card_m2 == straights_k2[[1]][[m2]][2])[1]
                current_card_m2 <- current_card_m2[-current_straight_index]
                current_straight_index <- which(current_card_m2 == straights_k2[[1]][[m2]][3])[1]
                current_card_m2 <- current_card_m2[-current_straight_index]

                if (is_straight(current_card_m2)) {  
                  whether_pinghu <- whether_pinghu + 1
                }
              }
            }
          }
        }
      }
    }
  }  

  if (whether_pinghu > 0) {  
    return(TRUE)  
  }
  else {
    return(FALSE)
  }  
}  

is_pinghu11 <- function(cards) {  
  whether_pinghu <- 0

  if (!is_doublecard(cards)) {  
    return(FALSE)  
  }  
  extracteddoubles <- extract_doublecards(cards)

  for (i in 1:(nrow(extracteddoubles))) {  
    current_doublecard_index <- which(cards == extracteddoubles[i,1])[1]
    current_card <- cards[-current_doublecard_index]
    current_doublecard_index <- which(current_card == extracteddoubles[i,1])[1]
    current_card <- current_card[-current_doublecard_index]

    if (is_triplet_or_quad(current_card)) {
      for (j1 in 1:(nrow(extract_triplets(current_card)))) {  
        current_triplet_index <- which(current_card == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card[-current_triplet_index]
        current_triplet_index <- which(current_card_j1 == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card_j1[-current_triplet_index]      
        current_triplet_index <- which(current_card_j1 == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card_j1[-current_triplet_index]   

        if (is_triplet_or_quad(current_card_j1)) {
          for (k1 in 1:(nrow(extract_triplets(current_card_j1)))) {
            current_triplet_index <- which(current_card_j1 == extract_triplets(current_card_j1)[k1,1])[1]
            current_card_k1 <- current_card_j1[-current_triplet_index]
            current_triplet_index <- which(current_card_k1 == extract_triplets(current_card_j1)[k1,1])[1]
            current_card_k1 <- current_card_k1[-current_triplet_index]      
            current_triplet_index <- which(current_card_k1 == extract_triplets(current_card_j1)[k1,1])[1]
            current_card_k1 <- current_card_k1[-current_triplet_index]  

            if (is_straight(current_card_k1) || is_triplet_or_quad(current_card_k1)) {  
              whether_pinghu <- whether_pinghu + 1
            }
          }
        }

        else if (is_straight(current_card_j1)) {
          straights_j1 <- extract_straights(current_card_j1)
          for (k2 in 1:straights_j1[[2]]) {
            current_straight_index <- which(current_card_j1 == straights_j1[[1]][[k2]][1])[1]
            current_card_k2 <- current_card_j1[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j1[[1]][[k2]][2])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j1[[1]][[k2]][3])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]

            if (is_straight(current_card_k2)) {  
              whether_pinghu <- whether_pinghu + 1
            }
          }
        }
      }
    }
    
    else if (is_straight(current_card)) {
      straights_i <- extract_straights(current_card)
      for (j2 in 1:straights_i[[2]]) {  
        current_straight_index <- which(current_card == straights_i[[1]][[j2]][1])[1]
        current_card_j2 <- current_card[-current_straight_index]
        current_straight_index <- which(current_card_j2 == straights_i[[1]][[j2]][2])[1]
        current_card_j2 <- current_card_j2[-current_straight_index]
        current_straight_index <- which(current_card_j2 == straights_i[[1]][[j2]][3])[1]
        current_card_j2 <- current_card_j2[-current_straight_index]
        
        if (is_straight(current_card_j2)) {
          straights_j2 <- extract_straights(current_card_j2)
          for (k2 in 1:straights_j2[[2]]) {
            current_straight_index <- which(current_card_j2 == straights_j2[[1]][[k2]][1])[1]
            current_card_k2 <- current_card_j2[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j2[[1]][[k2]][2])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]
            current_straight_index <- which(current_card_k2 == straights_j2[[1]][[k2]][3])[1]
            current_card_k2 <- current_card_k2[-current_straight_index]

            if (is_straight(current_card_k2)) {  
              whether_pinghu <- whether_pinghu + 1
            }
          }
        }
      }
    }
  }  

  if (whether_pinghu > 0) {  
    cat("种类", whether_pinghu, "\n")
    return(TRUE)  
  }
  else {
    return(FALSE)
  }  
}  

is_pinghu8 <- function(cards) {  
  whether_pinghu <- 0

  if (!is_doublecard(cards)) {  
    return(FALSE)  
  }
  extracteddoubles <- extract_doublecards(cards)

  for (i in 1:(nrow(extracteddoubles))) {  
    current_doublecard_index <- which(cards == extracteddoubles[i,1])[1]
    current_card <- cards[-current_doublecard_index]
    current_doublecard_index <- which(current_card == extracteddoubles[i,1])[1]
    current_card <- current_card[-current_doublecard_index]

    if (is_triplet_or_quad(current_card)) {
      for (j1 in 1:(nrow(extract_triplets(current_card)))) {  
        current_triplet_index <- which(current_card == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card[-current_triplet_index]
        current_triplet_index <- which(current_card_j1 == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card_j1[-current_triplet_index]      
        current_triplet_index <- which(current_card_j1 == extract_triplets(current_card)[j1,1])[1]
        current_card_j1 <- current_card_j1[-current_triplet_index]   

        if (is_straight(current_card_j1) || is_triplet_or_quad(current_card_j1)) {  
          whether_pinghu <- whether_pinghu + 1
        }
      }
    }

    else if (is_straight(current_card)) {
      straights_i <- extract_straights(current_card)
      for (j2 in 1:straights_i[[2]]) {  
        current_straight_index <- which(current_card == straights_i[[1]][[j2]][1])[1]
        current_card_j2 <- current_card[-current_straight_index]
        current_straight_index <- which(current_card_j2 == straights_i[[1]][[j2]][2])[1]
        current_card_j2 <- current_card_j2[-current_straight_index]
        current_straight_index <- which(current_card_j2 == straights_i[[1]][[j2]][3])[1]
        current_card_j2 <- current_card_j2[-current_straight_index]

        if (is_straight(current_card_j2)) {  
          whether_pinghu <- whether_pinghu + 1
        }
      }
    }
  }  

  if (whether_pinghu > 0) {  
    cat("种类", whether_pinghu, "\n")
    return(TRUE)  
  }
  else {
    return(FALSE)
  }  
}  

is_pinghu5 <- function(cards) {  
  whether_pinghu <- 0

  if (!is_doublecard(cards)) {  
    return(FALSE)  
  }  
  extracteddoubles <- extract_doublecards(cards)

  for (i in 1:(nrow(extracteddoubles))) {  
    current_doublecard_index <- which(cards == extracteddoubles[i,1])[1]
    current_card <- cards[-current_doublecard_index]
    current_doublecard_index <- which(current_card == extracteddoubles[i,1])[1]
    current_card <- current_card[-current_doublecard_index]

    if (is_straight(current_card) || is_triplet_or_quad(current_card)) {  
      whether_pinghu <- whether_pinghu + 1
    }
  }  
 
  if (whether_pinghu > 0) {  
    cat("种类", whether_pinghu, "\n")
    return(TRUE)  
  }
  else {
    return(FALSE)
  }  
}  

is_pinghu <- function(cards) {
  whether_pinghu <- FALSE
  
  if (length(cards) == 14) {
    whether_pinghu <- suppressWarnings(is_pinghu14(cards))
  }
  else if (length(cards) == 11) {
    whether_pinghu <- suppressWarnings(is_pinghu11(cards))
  }
  else if (length(cards) == 8) {
    whether_pinghu <- suppressWarnings(is_pinghu8(cards))
  }
  else if (length(cards) == 5) {
    whether_pinghu <- suppressWarnings(is_pinghu5(cards))
  }
  else if (length(cards) == 2) {
    whether_pinghu <- suppressWarnings(is_doublecard(cards))
  }
  
  return(whether_pinghu)
}

is_seven_pairs <- function(cards) { 
  if (length(cards) != 14) {  
    return(FALSE)  
  }  

  card_counts <- table(cards) 
  num_pairs <- sum(card_counts == 2)  
  num_quads <- sum(card_counts == 4)  

  if (num_pairs == 7) {
    return(TRUE)
  } 
  else if (num_pairs == 5 && num_quads == 1) {
    return(TRUE)
  }
  else if (num_pairs == 3 && num_quads == 2) {
    return(TRUE)
  }
  else if (num_pairs == 1 && num_quads == 3) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}