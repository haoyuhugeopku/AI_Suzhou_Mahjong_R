# 5 打法

draw_available <- c(TRUE, FALSE, FALSE, FALSE)
draw_card <- function(player) {
  if (player_cardcount[[player]]>13) {
    tempmessage <- paste("玩家", player, " 当前不能抓牌！因为其牌数过多")
    showNotification(tempmessage, type = "error")
    return(FALSE)
  }
  
  if (length(remaining_deck) > 0) {
    card <- remaining_deck[1]
    remaining_deck <<- remaining_deck[-1]
    player_hands[[player]] <<- c(player_hands[[player]], card)
    cat("玩家", player, " 抓到了牌 ", card, "\n")
    cat("玩家", player, " 现在的手牌：\n ", player_hands[[player]], "\n")

    current_played <<- "无"
    current_played_player <<- 0
    current_bugang <<- "无"
    current_bugang_player <<- 0
    current_get_player <<- as.numeric(player)
    
    player_hand <- player_hands[[player]]
    temp_remaining_deck <- remaining_deck
    
    current_get <<- card

    while(any(player_hand %in% suzhou_mahjong_flower_cards)) {
      current_flower_card_index <- which(player_hand %in% suzhou_mahjong_flower_cards)[1]
      current_flower_card <- player_hand[current_flower_card_index]

      player_hard_flowers[as.numeric(player)] <<- player_hard_flowers[as.numeric(player)] + 1
      player_hand <- player_hand[-current_flower_card_index]
      player_hards[[player]] <<- c(player_hards[[player]], current_flower_card)

      if (length(temp_remaining_deck) == 0) {  
        cat("剩余牌堆为空，无法继续补花。\n")  
        break 
      } 

      replacement_card <- temp_remaining_deck[1]
      temp_remaining_deck <- temp_remaining_deck[-1]
      
      if (replacement_card %in% suzhou_mahjong_flower_cards) {
        player_hand <- c(player_hand, replacement_card)
        cat("玩家", player, " 进行补花，抓到了花牌 ", replacement_card, " 并继续补花...\n")
      } 
      else {
        player_hand <- c(player_hand, replacement_card)
        cat("玩家", player, " 进行补花，抓到了非花牌 ", replacement_card, " 用于补花。\n")
        current_get <<- replacement_card
      }
    }

    player_hands[[player]] <<- player_hand

    cat("玩家", player, " 的硬花数量：", player_hard_flowers[as.numeric(player)], "\n\n\n")

    remaining_deck <<- temp_remaining_deck

    tempmessage <- paste("玩家", player, " 抓牌成功！")
    showNotification(tempmessage, type = "message")
    player_cardcount[[player]] <<- player_cardcount[[player]] + 1
    
    note <<- paste("玩家", player, " 进行了抓牌\n 抓到的牌是", current_get)
    notepad <<- c(notepad, note)

    player_soft_flowers_fenganke[as.numeric(player)] <<- count_wind_triplets(player_hands[[player]])

    for (player in 1:4) {  
      player_hand <- player_hands[[player]]  
      prioritized_hand <- lapply(player_hand, priority_sort)  
      prioritized_hand_df <- do.call(rbind, lapply(prioritized_hand, function(x) {  
        data.frame(priority = as.numeric(x$priority), card = x$card, stringsAsFactors = FALSE)  
      }))  
      sorted_hand <- prioritized_hand_df[order(prioritized_hand_df$priority), "card"]  
      player_hands[[player]] <<- sorted_hand  
    }
    
    return(TRUE)
  } 
  else if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  else {
    if_huangzhuang <<- 1
    cat("游戏结束了！荒庄！\n")
    showNotification("游戏结束了！荒庄！", type = "error")
    return(FALSE)
  }
}

if_can_zimo <- function(player) {
  hand <- player_hands[[player]]
  pong <- c(player_pongs[[player]],player_pongs[[player]],player_pongs[[player]])
  gang <- c(player_gangs[[player]],player_gangs[[player]],player_gangs[[player]])
  allcards <- c(hand,pong,gang)
  if (suppressWarnings(is_pinghu(hand)) || suppressWarnings(is_seven_pairs(allcards))) {
    if (player_hard_flowers[as.numeric(player)] >= 3) {
      return(TRUE)
    }
    else if (is_god(player, allcards)) {
      return(TRUE)
    }
    else if (is_earth(player, allcards)) {
      return(TRUE)
    }
    else if (is_moon(player, allcards)) {
      return(TRUE)
    }
    else if (is_gang_replacement(player, allcards)) {
      return(TRUE)
    }
    else if (is_big_concealed(player, allcards)) {
      return(TRUE)
    }
    else if (is_small_concealed(player, allcards)) {
      return(TRUE)
    }
    else if (is_diao(player, allcards)) {
      return(TRUE)
    }
    else if (is_pure_onecolor(player, allcards)) {
      return(TRUE)
    }
    else if (is_mixed_onecolor(player, allcards)) {
      return(TRUE)
    }
    else if (is_pongponghu(player, allcards)) {
      return(TRUE)
    }
    else if (is_seven_pairs(allcards)) {
      return(TRUE)
    }
    else if (is_longdragon(player, allcards)) {
      return(TRUE)
    }
    else if (is_small_three_winds(player, allcards)) {
      return(TRUE)
    }
    else if (is_three_winds(player, allcards)) {
      return(TRUE)
    }
    else if (is_small_four_winds(player, allcards)) {
      return(TRUE)
    }
    else if (is_four_winds(player, allcards)) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  else {
    return(FALSE)
  }
}

if_can_zigang <- function(player) {
  hand <- player_hands[[player]]
  unique_cards <- unique(hand)
  for (i in 1:length(unique_cards)) {
    count <- sum(hand == unique_cards[i])
    if (count == 4) {
      return(TRUE)
    }
  }
  return(FALSE)
}

if_can_bugang <- function(player) {
  hand <- player_hands[[player]]  
  pongs <- player_pongs[[player]]  
  for (card in pongs) {  
    count <- sum(hand == card)  
    if (count >= 1) {
      return(TRUE)
    }
  }
  return(FALSE)
}

play_card <- function(player, card) {
  if (player_cardcount[[player]]<14) {
    tempmessage <- paste("玩家", player, " 当前不能打出牌！因为其牌数过少")
    showNotification(tempmessage, type = "error")
    return(FALSE)
  }
  
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  if (card %in% player_hands[[player]]) {
    player_hands[[player]] <<- player_hands[[player]][-which(player_hands[[player]] == card)[1]]
    player_played[[player]] <<- c(player_played[[player]], card)
    cat("玩家", player, " 打出了牌 ", card, "\n")
    cat("玩家", player, " 现在的手牌：\n ", player_hands[[player]], "\n")
    tempmessage <- paste("玩家", player, " 打牌成功！打出了牌 ", card)
    current_played <<- card
    current_played_player <<- as.numeric(player)
    player_cardcount[[player]] <<- player_cardcount[[player]] - 1
    player_soft_flowers_fenganke[as.numeric(player)] <<- count_wind_triplets(player_hands[[player]])
    current_gang_player <<- 0
    current_pong_player <<- 0
    showNotification(tempmessage, type = "message")
    note <<- paste("玩家", player, " 进行了出牌")
    notepad <<- c(notepad, note)
    return(TRUE)
  } else {
    cat("玩家", player, " 没有牌 ", card, "，无法打出。\n")
    showNotification("无法打出这张牌！", type = "error")
    return(FALSE)
  }
}

if_can_ponggang <- function(player, card) {
  for (other_player in 1:4) { 
    if (other_player != player) {
      hand <- player_hands[[other_player]]
      count <- sum(hand == card)
      if (count >= 2) {
        claiming_available[as.numeric(other_player)] <<- TRUE
        return(as.numeric(other_player))
      }
    }
  }
  return(0)
}

if_you_can_pong <- function(player, card) {
  hand <- player_hands[[player]]
  count <- sum(hand == card)
  if (count == 2) {
    return(TRUE)
  }
  return(FALSE)
}

if_you_can_gang <- function(player, card) {
  hand <- player_hands[[player]]
  count <- sum(hand == card)
  if (count == 3) {
    return(TRUE)
  }
  return(FALSE)
}

pong_card <- function(player, card) {
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]
  count <- sum(hand == card)

  if (count >= 2) {
    player_hands[[player]] <<- hand[-which(hand == card)[1:2]]
    player_pongs[[player]] <<- c(player_pongs[[player]], card)
    cat("玩家", player, " 碰了牌 ", card, "\n")
    cat("玩家", player, " 现在的手牌：\n ", player_hands[[player]], "\n")
    tempmessage <- paste("玩家", player, " 碰牌成功！\n 碰了玩家 ", current_played_player, "打出的牌 ", card)
    player_cardcount[[player]] <<- player_cardcount[[player]] + 1
    player_played[[current_played_player]] <<- head(player_played[[current_played_player]] , length(player_played[[current_played_player]] ) - 1)
    current_played <<- "无"
    current_gang_player <<- 0
    current_pong_player <<- as.numeric(player)
    if (card %in% wind_cards) {
      player_soft_flowers_fengpeng[as.numeric(player)] <<- player_soft_flowers_fengpeng[as.numeric(player)] + 1
    } 
    showNotification(tempmessage, type = "message")
    note <<- paste("玩家", player, " 碰了玩家", current_played_player, " 的牌")
    notepad <<- c(notepad, note)
    return(TRUE)
  } else {
    cat("玩家", player, " 不能碰牌 ", card, "\n")
    if (as.numeric(player) == 1) {
      showNotification("无法碰这张牌！", type = "error")
    }
    return(FALSE)
  }
}

gang_card <- function(player, card) {
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]
  count <- sum(hand == card)

  if (count >= 3) {
    player_hands[[player]] <<- hand[-which(hand == card)[1:3]]
    player_gangs[[player]] <<- c(player_gangs[[player]], card)
    cat("玩家", player, " 杠了牌 ", card, "\n")
    cat("玩家", player, " 现在的手牌：\n ", player_hands[[player]], "\n")
    tempmessage <- paste("玩家", player, " 杠牌成功！\n 杠了玩家 ", current_played_player, "打出的牌 ", card)
    player_played[[current_played_player]] <<- head(player_played[[current_played_player]] , length(player_played[[current_played_player]] ) - 1)
    current_played <<- "无"
    current_gang_player <<- as.numeric(player)
    current_pong_player <<- 0
    player_soft_flowers_minggang[as.numeric(player)] <<- player_soft_flowers_minggang[as.numeric(player)] + 1
    if (card %in% wind_cards) {
      player_soft_flowers_fenggang[as.numeric(player)] <<- player_soft_flowers_fenggang[as.numeric(player)] + 1
      player_soft_flowers_fenganke[as.numeric(player)] <<- count_wind_triplets(player_hands[[player]])
    } 
    showNotification(tempmessage, type = "message")
    note <<- paste("玩家", player, " 杠了玩家", current_played_player, " 的牌")
    notepad <<- c(notepad, note)
    return(TRUE)
  } else {
    cat("玩家", player, " 不能杠牌 ", card, "\n")
    if (as.numeric(player) == 1) {
      showNotification("无法杠这张牌！", type = "error")
    }
    return(FALSE)
  }
}

self_gang <- function(player) {
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  if (player_cardcount[[player]]<14) {
    tempmessage <- paste("玩家", player, " 当前不能自杠！因为其牌数过少")
    showNotification(tempmessage, type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]
  unique_cards <- unique(hand)

  for (i in 1:length(unique_cards)) {
    count <- sum(hand == unique_cards[i])
    if (count == 4) {
      player_hands[[player]] <<- hand[-which(hand == unique_cards[i])[1:4]]
      player_gangs[[player]] <<- c(player_gangs[[player]], unique_cards[i])
      cat("玩家", player, " 自杠了牌 ", unique_cards[i], "\n")
      cat("玩家", player, " 现在的手牌：\n ", player_hands[[player]], "\n")
      tempmessage <- paste("玩家", player, " 自杠成功！杠了牌 ", unique_cards[i])
      current_gang_player <<- as.numeric(player)
      current_pong_player <<- 0
      player_cardcount[[player]] <<- player_cardcount[[player]] - 1
      player_soft_flowers_angang[as.numeric(player)] <<- player_soft_flowers_angang[as.numeric(player)] + 1
      if (unique_cards[i] %in% wind_cards) {
        player_soft_flowers_fenggang[as.numeric(player)] <<- player_soft_flowers_fenggang[as.numeric(player)] + 1
        player_soft_flowers_fenganke[as.numeric(player)] <<- count_wind_triplets(player_hands[[player]])
      } 
      showNotification(tempmessage, type = "message")
      note <<- paste("玩家", player, " 进行了自杠")
      notepad <<- c(notepad, note)
      return(TRUE)
    }
  }
  
  cat("玩家", player, " 不能自杠\n")
  if (as.numeric(player) == 1) {
    showNotification("无法自杠任何牌！", type = "error")
  }
  return(FALSE)
}

bu_gang <- function(player) {  
  if (if_any_hu > 0){  
    cat("游戏结束了！已经有玩家胡了！\n")  
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")  
    return(FALSE)  
  }  
  
  if (player == current_pong_player){
    cat("刚刚碰牌后禁止补杠！\n")
    showNotification("刚刚碰牌后禁止补杠！", type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]  
  pongs <- player_pongs[[player]]  

  for (card in pongs) {  
    count <- sum(hand == card)  

    if (count >= 1) {  
      player_hands[[player]] <<- hand[-which(hand == card)[1]] 
      player_pongs[[player]] <<- player_pongs[[player]][-which(player_pongs[[player]] == card)] 
      player_gangs[[player]] <<- c(player_gangs[[player]], card)    
      
      cat("玩家", player, " 补杠了牌 ", card, "\n")  
      cat("玩家", player, " 现在的手牌：\n ", player_hands[[player]], "\n")  
      tempmessage <- paste("玩家", player, " 补杠成功！\n 补杠了之前碰过的牌 ", card)  
      player_soft_flowers_minggang[as.numeric(player)] <<- player_soft_flowers_minggang[as.numeric(player)] + 1
      player_cardcount[[player]] <<- player_cardcount[[player]] - 1 
      current_gang_player <<- as.numeric(player)
      current_pong_player <<- 0
      if (card %in% wind_cards) {
        player_soft_flowers_fengpeng[as.numeric(player)] <<- player_soft_flowers_fengpeng[as.numeric(player)] - 1
        player_soft_flowers_fenggang[as.numeric(player)] <<- player_soft_flowers_fenggang[as.numeric(player)] + 1
      }
      showNotification(tempmessage, type = "message")  
      current_bugang <<- card
      current_bugang_player <<- as.numeric(player)
      note <<- paste("玩家", player, " 进行了补杠")
      notepad <<- c(notepad, note)
      return(TRUE)  
    }  
  }  

  cat("玩家", player, " 没有牌可以补杠\n")  
  if (as.numeric(player) == 1) {
    showNotification("无法补杠任何牌！", type = "error")  
  }
  return(FALSE)  
}

potential_cards <- function(hand) {
  sequential_cards <- hand[grepl("[0-9]", hand)] 
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
  biggerhand <- unique(c(hand, adjacent_cards))  
  return(biggerhand)
}

if_can_dianpao <- function(player, card) {
  can_hu_player <- c()
  other_player <- next_player(player)
  for (i in 1:4) { 
    if (other_player != player) {
      hand <- player_hands[[other_player]]
      pong <- c(player_pongs[[other_player]],player_pongs[[other_player]],player_pongs[[other_player]])
      gang <- c(player_gangs[[other_player]],player_gangs[[other_player]],player_gangs[[other_player]])
      allcards <- c(hand,pong,gang,card)
      can_hu_thisplayer <- FALSE
      biggerhand <- potential_cards(hand) 
      if (card %in% biggerhand) {
        if (suppressWarnings(is_pinghu(c(hand,card))) || suppressWarnings(is_seven_pairs(c(hand,card)))) {
          if (player_hard_flowers[as.numeric(other_player)] >= 4) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_god(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_earth(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_moon(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_gang_replacement(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_big_concealed(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_small_concealed(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_diao(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_pure_onecolor(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_mixed_onecolor(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_pongponghu(other_player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_seven_pairs(allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_longdragon(player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_small_three_winds(player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_three_winds(player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_small_four_winds(player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
          else if (is_four_winds(player, allcards)) {
            can_hu_thisplayer <- TRUE
          }
        }
      }
      if (can_hu_thisplayer) {
        claiming_available[as.numeric(other_player)] <<- TRUE
        can_hu_player <- c(can_hu_player, other_player)
      }
    }
    
    other_player <- next_player(other_player)
  }
  return(can_hu_player)
}

if_can_hupai <- function(player, card) {
  other_player <- player
  hand <- player_hands[[other_player]]
  pong <- c(player_pongs[[other_player]],player_pongs[[other_player]],player_pongs[[other_player]])
  gang <- c(player_gangs[[other_player]],player_gangs[[other_player]],player_gangs[[other_player]])
  allcards <- c(hand,pong,gang,card)
  biggerhand <- potential_cards(hand) 
  if (card %in% biggerhand) {
    if (suppressWarnings(is_pinghu(c(hand,card))) | suppressWarnings(is_seven_pairs(c(hand,card)))) {
      if (player_hard_flowers[as.numeric(other_player)] >= 4) {
        return(TRUE)
      }
      else if (is_god(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_earth(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_moon(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_gang_replacement(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_big_concealed(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_small_concealed(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_diao(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_pure_onecolor(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_mixed_onecolor(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_pongponghu(other_player, allcards)) {
        return(TRUE)
      }
      else if (is_seven_pairs(allcards)) {
        return(TRUE)
      }
      else if (is_longdragon(player, allcards)) {
        return(TRUE)
      }
      else if (is_small_three_winds(player, allcards)) {
        return(TRUE)
      }
      else if (is_three_winds(player, allcards)) {
        return(TRUE)
      }
      else if (is_small_four_winds(player, allcards)) {
        return(TRUE)
      }
      else if (is_four_winds(player, allcards)) {
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    else {
      return(FALSE)
    }
  }
  else {
    return(FALSE)
  }
}

if_listen <- function(player, hand) {  
  all_mahjong_cards <- unique(suzhou_mahjong_normal_cards)  

  if (length(hand) != 1 && length(hand) != 4 && length(hand) != 7 && length(hand) != 10 && length(hand) != 13){  
    cat("玩家当前手牌数量不支持听牌检查！\n")  
    return(list(cards = "不合法", nums = 0))  
  }  

  not_played_cards <- remaining_deck  
  for (i in 1:4) {  
    if (as.numeric(player) != i) {  
      not_played_cards <- c(not_played_cards, player_hands[[i]])  
    }  
  }  

  biggerhand <- potential_cards(hand)  

  listen_cards <- character() 

  for (j in 1:length(biggerhand)) {  
    test_cards <- c(hand, biggerhand[j])  
    if (suppressWarnings(is_pinghu(test_cards)) || suppressWarnings(is_seven_pairs(test_cards))) {  
      listen_cards <- c(listen_cards, biggerhand[j])  
    }  
  }  
  
  if (length(listen_cards) == 0) {
    return(list(cards = character(), nums = numeric()))
  }
  
  prioritized_listen <- lapply(listen_cards, priority_sort)  
  
  prioritized_listen_df <- do.call(rbind, lapply(prioritized_listen, function(x) {  
    data.frame(priority = as.numeric(x$priority), card = x$card, stringsAsFactors = FALSE)  
  }))  
  
  listen_cards <- prioritized_listen_df[order(prioritized_listen_df$priority), "card"]  

  counts <- sapply(listen_cards, function(card) {  
    sum(not_played_cards == card)  
  })  

  hu_cards <- listen_cards[counts > 0]  
  hu_counts <- counts[counts > 0]  

  hu_result <- list(cards = hu_cards, nums = hu_counts)  
  
  return(hu_result)  
}  

hu_card <- function(player, card) {
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]
  pong <- c(player_pongs[[player]],player_pongs[[player]],player_pongs[[player]])
  gang <- c(player_gangs[[player]],player_gangs[[player]],player_gangs[[player]])

  if (if_can_hupai(player, card)) {
    cat("玩家", player, " 胡了，通过牌 ", card, "\n")
    tempmessage <- paste("恭喜！玩家", player, " 胡了！通过牌 ", card)
    player_hands[[player]] <<- c(player_hands[[player]], card)
    player_cardcount[[player]] <<- player_cardcount[[player]] + 1
    showNotification(tempmessage, type = "message")
    current_played <<- paste("玩家", player, " 胡了")
    if_any_hu <<- 1
    player_played[[current_played_player]] <<- head(player_played[[current_played_player]] , length(player_played[[current_played_player]] ) - 1)
    final_cards <<- c(hand,pong,gang,card)
    note <<- paste("玩家", player, " 胡了，点炮者是玩家", current_played_player)
    notepad <<- c(notepad, note)
    return(TRUE)
  } else {
    cat("玩家", player, " 不能胡牌 ", card, "\n")
    if (as.numeric(player) == 1) {
      showNotification("当前没有达成胡牌条件！", type = "error")
    }
    return(FALSE)
  }
}

self_hu <- function(player) {
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  if (player == current_pong_player){
    cat("刚刚碰牌后禁止自摸！\n")
    showNotification("刚刚碰牌后禁止自摸！", type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]
  pong <- c(player_pongs[[player]],player_pongs[[player]],player_pongs[[player]])
  gang <- c(player_gangs[[player]],player_gangs[[player]],player_gangs[[player]])

  if (if_can_zimo(player)) {
    cat("玩家", player, " 胡了，自摸！", "\n")
    tempmessage <- paste("恭喜！玩家", player, " 胡了！自摸！")
    showNotification(tempmessage, type = "message")
    if_any_hu <<- 1
    current_played <<- paste("玩家", player, " 胡了")
    final_cards <<- c(hand,pong,gang)
    note <<- paste("玩家", player, " 胡了，自摸")
    notepad <<- c(notepad, note)
    return(TRUE)
  } else {
    cat("玩家", player, " 不能自摸\n")
    if (as.numeric(player) == 1) {
      showNotification("当前没有达成自摸条件！", type = "error")
    }
    return(FALSE)
  }
}

qiang_gang <- function(player, card) {
  if (if_any_hu > 0){
    cat("游戏结束了！已经有玩家胡了！\n")
    showNotification("游戏结束了！已经有玩家胡了！", type = "error")
    return(FALSE)
  }
  
  hand <- player_hands[[player]]
  pong <- c(player_pongs[[player]],player_pongs[[player]],player_pongs[[player]])
  gang <- c(player_gangs[[player]],player_gangs[[player]],player_gangs[[player]])

  if (is_pinghu(c(hand,card)) | is_seven_pairs(c(hand,card))) {
    cat("玩家", player, " 胡了，通过抢杠 ", card, "\n")
    tempmessage <- paste("恭喜！玩家", player, " 胡了！通过抢杠 ", card)
    player_hands[[player]] <<- c(player_hands[[player]], card)
    player_cardcount[[player]] <<- player_cardcount[[player]] + 1
    showNotification(tempmessage, type = "message")
    current_played <<- paste("玩家", player, " 胡了")
    if_any_hu <<- 1
    final_cards <<- c(hand,pong,gang,card)
    note <<- paste("玩家", player, " 胡了，被抢杠者是玩家", current_bugang_player)
    notepad <<- c(notepad, note)
    return(TRUE)
  } else {
    cat("玩家", player, " 不能抢杠 ", card, "\n")
    if (as.numeric(player) == 1) {
      showNotification("当前没有达成抢杠条件！", type = "error")
    }
    return(FALSE)
  }
}
