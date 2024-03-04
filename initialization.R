# 3 开局抓牌

player_hands <- split(shuffled_deck[1:52], rep(1:4, each = 13))
remaining_deck <- shuffled_deck[53:length(shuffled_deck)]

player_hard_flowers <<- rep(0, 4)
player_hards <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())

for (player in 1:4) {
  player_hand <- player_hands[[player]]
  temp_remaining_deck <- remaining_deck

  cat("玩家", player, " 的原始手牌：\n", player_hand, "\n\n")

  while(any(player_hand %in% suzhou_mahjong_flower_cards)) {
    current_flower_card_index <- which(player_hand %in% suzhou_mahjong_flower_cards)[1]
    current_flower_card <- player_hand[current_flower_card_index]

    player_hard_flowers[player] <- player_hard_flowers[player] + 1
    player_hand <- player_hand[-current_flower_card_index]
    player_hards[[player]] <- c(player_hards[[player]], current_flower_card)

    replacement_card <- temp_remaining_deck[1]
    temp_remaining_deck <- temp_remaining_deck[-1]
    
    if (replacement_card %in% suzhou_mahjong_flower_cards) {
      player_hand <- c(player_hand, replacement_card)
      cat("玩家", player, " 抽到了花牌 ", replacement_card, " 并继续补花...\n")
    } 
    else {
      player_hand <- c(player_hand, replacement_card)
      cat("玩家", player, " 抽到了非花牌 ", replacement_card, " 用于补花。\n")
    }
  }

  player_hands[[player]] <- player_hand

  cat("玩家", player, " 的原始手牌（不含花牌，已补完）：\n", player_hand, "\n")
  cat("玩家", player, " 的硬花数量：", player_hard_flowers[player], "\n\n\n")

  remaining_deck <- temp_remaining_deck
}

cat("牌堆剩余牌数：", length(remaining_deck), "\n\n\n")

roll_dice <- function() {  
  dice1 <- sample(1:6, 1)  
  dice2 <- sample(1:6, 1)  
  return(list(dice1, dice2))  
}  
dice <- roll_dice()

diling <- dice[[1]] == dice[[2]]

priority_sort <- function(card) {  
  types <- c("筒", "万", "条", "东", "南", "西", "北")  
  priorities <- c(1, 2, 3, 4, 5, 6, 7)  
  default_priority <- min(priorities)  

  type_match <- regexpr("(筒|万|条|东|南|西|北)", card)  
  if (type_match == -1) {  
    type <- "未知"  
    priorityA <- default_priority  
  } else {  
    type <- substr(card, type_match, type_match + attr(type_match, "match.length") - 1)  
    priorityA <- match(type, types)*10  
  }  
  
  number_match <- regexpr("\\d+", card)  
  if (number_match == -1) {  
    number <- 0  
  } else {  
    number <- as.numeric(substr(card, number_match, number_match + attr(number_match, "match.length") - 1))  
  }  

  priorityB <- number  
  priority <- as.numeric(priorityA + priorityB)  

  return(data.frame(priority = priority, card = card, stringsAsFactors = FALSE))  
}

for (player in 1:4) {  
  player_hand <- player_hands[[player]]  
  prioritized_hand <- lapply(player_hand, priority_sort)  
  prioritized_hand_df <- do.call(rbind, lapply(prioritized_hand, function(x) {  
    data.frame(priority = as.numeric(x$priority), card = x$card, stringsAsFactors = FALSE)  
  }))  
  sorted_hand <- prioritized_hand_df[order(prioritized_hand_df$priority), "card"]  
  player_hands[[player]] <- sorted_hand  
  cat("玩家", player, " 排序后的手牌：\n", sorted_hand, "\n\n")  
}  

player_soft_flowers_minggang <<- rep(0, 4) 
player_soft_flowers_angang <<- rep(0, 4) 
player_soft_flowers_fengpeng <<- rep(0, 4) 
player_soft_flowers_fenganke <<- rep(0, 4) 
player_soft_flowers_fenggang <<- rep(0, 4) 

for (player in 1:4) {  
  player_soft_flowers_fenganke[as.numeric(player)] <- count_wind_triplets(player_hands[[player]])
}

player_played <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())
player_pongs <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())
player_gangs <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())

if_any_hu <<- 0

if_huangzhuang <<- 0

once_huangzhuang <<- 0

once_multi_hu <<- 0

current_get <<- "无"
current_get_player <<- 0
current_played <<- "无"
current_played_player <<- 0
current_bugang <<- "无"
current_bugang_player <<- 0
current_gang_player <<- 0
current_pong_player <<- 0

player_cardcount <<- list("1" = length(player_hands[[1]]), "2" = length(player_hands[[2]]), "3" = length(player_hands[[3]]), "4" = length(player_hands[[4]]))

claiming_available <<- c(FALSE, FALSE, FALSE, FALSE)

next_player <- function(player) {
  if (player < 4 && player > 0) {
    return(as.numeric(player) + 1)
  }
  else if (player == 4) {
    return(1)
  }
  else {
    return(0)
  }
}

notepad <- c("")
note <- "游戏开始"

new_game <- function(){
  shuffled_deck <- sample(suzhou_mahjong_deck)
  
  player_hands <<- split(shuffled_deck[1:52], rep(1:4, each = 13))
  remaining_deck <<- shuffled_deck[53:length(shuffled_deck)]

  player_hard_flowers <<- rep(0, 4)
  player_hards <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())

  for (player in 1:4) {
    player_hand <- player_hands[[player]]
    temp_remaining_deck <- remaining_deck

    cat("玩家", player, " 的原始手牌：\n", player_hand, "\n\n")

    while(any(player_hand %in% suzhou_mahjong_flower_cards)) {
      current_flower_card_index <- which(player_hand %in% suzhou_mahjong_flower_cards)[1]
      current_flower_card <- player_hand[current_flower_card_index]

      player_hard_flowers[player] <<- player_hard_flowers[player] + 1
      player_hand <- player_hand[-current_flower_card_index]
      player_hards[[player]] <<- c(player_hards[[player]], current_flower_card)

      replacement_card <- temp_remaining_deck[1]
      temp_remaining_deck <- temp_remaining_deck[-1]
      
      if (replacement_card %in% suzhou_mahjong_flower_cards) {
        player_hand <- c(player_hand, replacement_card)
        cat("玩家", player, " 抽到了花牌 ", replacement_card, " 并继续补花...\n")
      }
      else {
        player_hand <- c(player_hand, replacement_card)
        cat("玩家", player, " 抽到了非花牌 ", replacement_card, " 用于补花。\n")
      }
    }

    player_hands[[player]] <<- player_hand

    cat("玩家", player, " 的原始手牌（不含花牌，已补完）：\n", player_hand, "\n")
    cat("玩家", player, " 的硬花数量：", player_hard_flowers[player], "\n\n\n")

    remaining_deck <<- temp_remaining_deck
  }

  cat("牌堆剩余牌数：", length(remaining_deck), "\n\n\n")

  dice <<- roll_dice()

  diling <<- (dice[[1]] == dice[[2]]) || (once_huangzhuang > 0) || (once_multi_hu > 0)

  for (player in 1:4) {  
    player_hand <- player_hands[[player]]  
    prioritized_hand <- lapply(player_hand, priority_sort)  
    prioritized_hand_df <- do.call(rbind, lapply(prioritized_hand, function(x) {  
      data.frame(priority = as.numeric(x$priority), card = x$card, stringsAsFactors = FALSE)  
    }))  
    sorted_hand <- prioritized_hand_df[order(prioritized_hand_df$priority), "card"]  
    player_hands[[player]] <<- sorted_hand  
    cat("玩家", player, " 排序后的手牌：\n", sorted_hand, "\n\n")  
  }

  player_soft_flowers_minggang <<- rep(0, 4) 
  player_soft_flowers_angang <<- rep(0, 4) 
  player_soft_flowers_fengpeng <<- rep(0, 4) 
  player_soft_flowers_fenganke <<- rep(0, 4) 
  player_soft_flowers_fenggang <<- rep(0, 4) 

  for (player in 1:4) {  
    player_soft_flowers_fenganke[as.numeric(player)] <<- count_wind_triplets(player_hands[[player]])
  }

  player_played <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())
  player_pongs <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())
  player_gangs <<- list("1" = character(), "2" = character(), "3" = character(), "4" = character())

  if_any_hu <<- 0

  if_huangzhuang <<- 0
  
  current_get <<- "无"
  current_get_player <<- 0
  current_played <<- "无"
  current_played_player <<- 0
  current_bugang <<- "无"
  current_bugang_player <<- 0
  current_gang_player <<- 0
  current_pong_player <<- 0

  player_cardcount <<- list("1" = length(player_hands[[1]]), "2" = length(player_hands[[2]]), "3" = length(player_hands[[3]]), "4" = length(player_hands[[4]]))

  note <<- "游戏开始"
  notepad <<- c(notepad, note)

  claiming_available <<- c(FALSE, FALSE, FALSE, FALSE)
}
