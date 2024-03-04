# 4 番型

# 番型：大门清 
is_big_concealed <- function(player, card) { 
  if (player_hard_flowers[as.numeric(player)] == 0) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
text_big_concealed <- function(x) {
  if (x) {
    return(paste("\n大门清：", 10, "花"))
  }
}

# 番型：小门清 
is_small_concealed <- function(player, card) { 
  if (!is_seven_pairs(card)) {
    if (length(player_pongs[[player]]) == 0 && player_soft_flowers_minggang[as.numeric(player)] == 0) {
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
text_small_concealed <- function(x) {
  if (x) {
    return(paste("\n小门清：", 5, "花"))
  }
}

# 番型：清一色 
is_pure_onecolor <- function(player, card) { 
  suits <- sapply(card, function(card) strsplit(card, "")[[1]][2])  
  is_all_same_suit <- length(unique(suits)) == 1 
  return(is_all_same_suit)
}
text_pure_onecolor <- function(x) {
  if (x) {
    return(paste("\n清一色：", 10, "花"))
  }
}

# 番型：混一色 
is_mixed_onecolor <- function(player, card) { 
  suits <- sapply(card, function(card) strsplit(card, "")[[1]][2])  
  new_suits <- as.character(na.omit(suits))
  is_all_same_suit <- length(unique(new_suits)) == 1 && length(new_suits) < 14
}
text_mixed_onecolor <- function(x) {
  if (x) {
    return(paste("\n混一色：", 5, "花"))
  }
}

# 番型：碰碰胡 
is_pongponghu <- function(player, card) { 
  if (!is_seven_pairs(card)) {
    pongponghu <- length(unique(card)) == 5 
    return(pongponghu)  
  }
  else {
    return(FALSE)
  }
}
text_pongponghu <- function(x) {
  if (x) {
    return(paste("\n碰碰胡：", 5, "花"))
  }
}

# 番型：大吊车 
is_diao <- function(player, card) { 
  if (!is_seven_pairs(card)) {
    return(length(player_hands[[player]]) < 3)  
  }
  else {
    return(FALSE)
  }
}
text_diao <- function(x) {
  if (x) {
    return(paste("\n大吊车：", 5, "花"))
  }
}

# 番型：杠上开花 
is_gang_replacement <- function(player, card) { 
  if (!is_seven_pairs(card)) {
    return(as.numeric(player) == current_gang_player)  
  }
  else {
    return(FALSE)
  }
}
text_gang_replacement <- function(x) {
  if (x) {
    return(paste("\n杠上开花：", 5, "花"))
  }
}

# 番型：海底捞月 
is_moon <- function(player, card) { 
  return(length(remaining_deck) == 0) 
}
text_moon <- function(x) {
  if (x) {
    return(paste("\n海底捞月：", 5, "花"))
  }
}

# 番型：天胡 
is_god <- function(player, card) { 
  alltimes <- length(player_played[[1]]) + length(player_played[[2]]) + length(player_played[[3]]) + length(player_played[[4]])
  return(alltimes == 0 && as.numeric(player) == 1) 
}
text_god <- function(x) {
  if (x) {
    return(paste("\n天胡：", 50, "花"))
  }
}

# 番型：地胡 
is_earth <- function(player, card) { 
  alltimes <- length(player_played[[1]]) + length(player_played[[2]]) + length(player_played[[3]]) + length(player_played[[4]])
  return(alltimes < 4 && alltimes >= 0 && as.numeric(player) != 1) 
}
text_earth <- function(x) {
  if (x) {
    return(paste("\n地胡：", 28, "花"))
  }
}

# 番型：普通七对 
is_normal_seven_pairs <- function(player, card) { 
  if (is_seven_pairs(card)) {
    card_counts <- table(card) 
    num_pairs <- sum(card_counts == 2)  
    num_quads <- sum(card_counts == 4)  
    if (num_pairs == 7 && num_quads == 0) {
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
text_normal <- function(x) {
  if (x) {
    return(paste("\n普通七对：", 10, "花"))
  }
}

# 番型：豪华七对 
is_luxury_seven_pairs <- function(player, card) { 
  if (is_seven_pairs(card)) {
    card_counts <- table(card) 
    num_pairs <- sum(card_counts == 2)  
    num_quads <- sum(card_counts == 4)  
    if (num_pairs == 5 && num_quads == 1) {
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
text_luxury <- function(x) {
  if (x) {
    return(paste("\n豪华七对：", 20, "花"))
  }
}

# 番型：超豪华七对 
is_super_seven_pairs <- function(player, card) { 
  if (is_seven_pairs(card)) {
    card_counts <- table(card) 
    num_pairs <- sum(card_counts == 2)  
    num_quads <- sum(card_counts == 4)  
    if (num_pairs == 3 && num_quads == 2) {
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
text_super <- function(x) {
  if (x) {
    return(paste("\n超豪华七对：", 40, "花"))
  }
}

# 番型：超超豪华七对 
is_ultra_seven_pairs <- function(player, card) { 
  if (is_seven_pairs(card)) {
    card_counts <- table(card) 
    num_pairs <- sum(card_counts == 2)  
    num_quads <- sum(card_counts == 4)  
    if (num_pairs == 1 && num_quads == 3) {
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
text_ultra <- function(x) {
  if (x) {
    return(paste("\n超超豪华七对：", 80, "花"))
  }
}

# 番型：清龙
is_longdragon <- function(player, card) { 
  suits <- suppressWarnings(sapply(card, function(x) substr(x, 2, 2)))  
  numbers <- suppressWarnings(sapply(card, function(x) as.numeric(substr(x, 1, 1))))  
  valid_cards <- suppressWarnings(!is.na(numbers))
  suits <- suppressWarnings(suits[valid_cards])
  numbers <- suppressWarnings(numbers[valid_cards])
  if (length(numbers) < 9) {  
    return(FALSE)  
  }  
  for (suit in unique(suits)) {  
    suit_numbers <- numbers[suits == suit]  
    if (length(unique(suit_numbers)) == 9) {  
      test_card <- card
      for (i in 1:9) {
        specific_name <- paste0(i, suit)
        index <- which(test_card == specific_name)[1]  
        test_card <- test_card[-index]
      }
      if (suppressWarnings(is_pinghu5(test_card))) {
        return(TRUE)
      }
    }  
  }  
  return(FALSE)  
}
text_longdragon <- function(x) {
  if (x) {
    return(paste("\n清龙：", 10, "花"))
  }
}

# 番型：小三风
is_small_three_winds <- function(player, card) {  
  return(count_wind_triplets(card) == 2 && count_wind_pairs(card) == 1)  
}  
text_small_three_winds <- function(x) {
  if (x) {
    return(paste("\n小三风：", 6, "花"))
  }
}

# 番型：三风刻
is_three_winds <- function(player, card) {  
  return(count_wind_triplets(card) == 3)  
}  
text_three_winds <- function(x) {
  if (x) {
    return(paste("\n三风刻：", 12, "花"))
  }
}

# 番型：小四喜
is_small_four_winds <- function(player, card) {  
  return(count_wind_triplets(card) == 3 && count_wind_pairs(card) == 1)  
}  
text_small_four_winds <- function(x) {
  if (x) {
    return(paste("\n小四喜：", 48, "花"))
  }
}

# 番型：大四喜
is_four_winds <- function(player, card) {  
  return(count_wind_triplets(card) == 4)  
}  
text_four_winds <- function(x) {
  if (x) {
    return(paste("\n大四喜：", 64, "花"))
  }
}

# 番型：花中小三元
is_flower_small_three_dragons <- function(player, card) {
  flower <- player_hards[[player]]
  return(count_dragon_triplets(flower) == 2 && count_dragon_pairs(flower) == 1)  
}  
text_flower_small_three_dragons <- function(x) {
  if (x) {
    return(paste("\n花中小三元：", 16, "花"))
  }
}

# 番型：花中大三元
is_flower_three_dragons <- function(player, card) {
  flower <- player_hards[[player]]
  return(count_dragon_triplets(flower) == 3)  
}  
text_flower_three_dragons <- function(x) {
  if (x) {
    return(paste("\n花中大三元：", 24, "花"))
  }
}

hutypes <- function(player, card) {
  hutype <- "平胡"
  if (is_small_concealed(player, card))
  {
    hutype <- "小门清"
  }
  if (is_diao(player, card))
  {
    hutype <- "大吊车"
  }
  if (is_mixed_onecolor(player, card))
  {
    hutype <- "混一色"
  }
  if (is_pongponghu(player, card))
  {
    hutype <- "碰碰胡"
  }
  if (is_gang_replacement(player, card))
  {
    hutype <- "杠上开花"
  }
  if (is_moon(player, card))
  {
    hutype <- "海底捞月"
  }
  if (is_small_three_winds(player, card))
  {
    hutype <- "小三风"
  }
  if (is_big_concealed(player, card))
  {
    hutype <- "大门清"
  }
  if (is_pure_onecolor(player, card))
  {
    hutype <- "清一色"
  }
  if (is_longdragon(player, card))
  {
    hutype <- "清龙"
  }
  if (is_normal_seven_pairs(player, card))
  {
    hutype <- "七对"
  }
  if (is_three_winds(player, card))
  {
    hutype <- "三风刻"
  }
  if (is_flower_small_three_dragons(player, card))
  {
    hutype <- "花中小三元"
  }
  if (is_luxury_seven_pairs(player, card))
  {
    hutype <- "豪华七对"
  }
  if (is_flower_three_dragons(player, card))
  {
    hutype <- "花中大三元"
  }
  if (is_earth(player, card))
  {
    hutype <- "地胡"
  }
  if (is_super_seven_pairs(player, card))
  {
    hutype <- "超豪华七对"
  }
  if (is_small_four_winds(player, card))
  {
    hutype <- "小四喜"
  }
  if (is_god(player, card))
  {
    hutype <- "天胡"
  }
  if (is_four_winds(player, card))
  {
    hutype <- "大四喜"
  }
  if (is_ultra_seven_pairs(player, card))
  {
    hutype <- "超超豪华七对"
  }
  return(hutype)
}

count_fanxing_flowers <- function(player, card) {
  allflowers <- 0
  if (is_big_concealed(player,card)) {allflowers <- allflowers + 10}
  if (is_small_concealed(player,card)) {allflowers <- allflowers + 5}
  if (is_pure_onecolor(player,card)) {allflowers <- allflowers + 10}
  if (is_mixed_onecolor(player,card)) {allflowers <- allflowers + 5}
  if (is_pongponghu(player,card)) {allflowers <- allflowers + 5}
  if (is_diao(player,card)) {allflowers <- allflowers + 5}
  if (is_gang_replacement(player,card)) {allflowers <- allflowers + 5}
  if (is_moon(player,card)) {allflowers <- allflowers + 5}
  if (is_god(player,card)) {allflowers <- allflowers + 50}
  if (is_earth(player,card)) {allflowers <- allflowers + 28}
  if (is_normal_seven_pairs(player,card)) {allflowers <- allflowers + 10}
  if (is_luxury_seven_pairs(player,card)) {allflowers <- allflowers + 20}
  if (is_super_seven_pairs(player,card)) {allflowers <- allflowers + 40}
  if (is_ultra_seven_pairs(player,card)) {allflowers <- allflowers + 80}
  if (is_longdragon(player,card)) {allflowers <- allflowers + 10}
  if (is_small_three_winds(player,card)) {allflowers <- allflowers + 6}
  if (is_three_winds(player,card)) {allflowers <- allflowers + 12}
  if (is_small_four_winds(player,card)) {allflowers <- allflowers + 48}
  if (is_four_winds(player,card)) {allflowers <- allflowers + 64}
  if (is_flower_small_three_dragons(player,card)) {allflowers <- allflowers + 16}
  if (is_flower_three_dragons(player,card)) {allflowers <- allflowers + 24}
  return (allflowers)
}

# 特殊规则：软花
count_soft_flowers <- function(player) {
  playern <- as.numeric(player)
  soft <- player_soft_flowers_minggang[playern] + player_soft_flowers_angang[playern] * 2 + player_soft_flowers_fengpeng[playern] + player_soft_flowers_fenganke[playern] * 2 + player_soft_flowers_fenggang[playern] * 2
  return(soft)
}

# 特殊规则：滴零
text_diling <- function(x) {
  if (x) {
    return(paste("\n滴零：", 2, "倍"))
  }
}
diling_calculation <- function(x) {
  if (x) {
    return(2)
  }
  else {
    return(1)
  }
}
