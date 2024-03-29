# 1 构建牌库

circles <- rep(c("1筒", "2筒", "3筒", "4筒", "5筒", "6筒", "7筒", "8筒", "9筒"), each = 4)
myriads <- rep(c("1万", "2万", "3万", "4万", "5万", "6万", "7万", "8万", "9万"), each = 4)
strings <- rep(c("1条", "2条", "3条", "4条", "5条", "6条", "7条", "8条", "9条"), each = 4)

wind_cards <- rep(c("东", "南", "西", "北"), each = 4)
dragon_cards <- c(
  rep("红中", 4),
  rep("发财", 4),
  rep("白板", 4)
)
flower_cards <- c(
  c("梅", "兰", "竹", "菊"),
  c("春", "夏", "秋", "冬"),
  c("老鼠", "财神", "猫", "聚宝盆"),
  rep("百搭", 4)
)

suzhou_mahjong_deck <- c(circles, myriads, strings, wind_cards, flower_cards)
shuffled_deck <- sample(suzhou_mahjong_deck)

suzhou_mahjong_normal_cards <- c(circles, myriads, strings, wind_cards)
suzhou_mahjong_flower_cards <- c(dragon_cards, flower_cards)

emoji <- function(card) {  
  result_emoji <- switch(card,  
                         "1筒" = "🀙",  
                         "2筒" = "🀚",  
                         "3筒" = "🀛",  
                         "4筒" = "🀜",  
                         "5筒" = "🀝",  
                         "6筒" = "🀞",  
                         "7筒" = "🀟",  
                         "8筒" = "🀠",  
                         "9筒" = "🀡",  
                         "1万" = "🀇",  
                         "2万" = "🀈",  
                         "3万" = "🀉",  
                         "4万" = "🀊",  
                         "5万" = "🀋",  
                         "6万" = "🀌",  
                         "7万" = "🀍",  
                         "8万" = "🀎",  
                         "9万" = "🀏",  
                         "1条" = "🀐",  
                         "2条" = "🀑",  
                         "3条" = "🀒",  
                         "4条" = "🀓",  
                         "5条" = "🀔",  
                         "6条" = "🀕",  
                         "7条" = "🀖",  
                         "8条" = "🀗",  
                         "9条" = "🀘",  
                         "东" = "🀀",  
                         "南" = "🀁",  
                         "西" = "🀂",  
                         "北" = "🀃",  
                         "红中" = "🀄", 
                         "发财" = "🀅", 
                         "白板" = "🀆", 
                         "百搭" = "🀪", 
                         "梅" = "🀢", 
                         "兰" = "🀣", 
                         "竹" = "🀤", 
                         "菊" = "🀥", 
                         "春" = "🀦", 
                         "夏" = "🀧", 
                         "秋" = "🀨", 
                         "冬" = "🀩", 
                         "无" = "", 
                         "🀫")  
  return(result_emoji)
}

scorepad <- c(0,0,0,0)
