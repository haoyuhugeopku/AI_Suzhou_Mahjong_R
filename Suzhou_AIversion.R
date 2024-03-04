library(shiny)
library(shinyalert)

# 1 构建牌库
source("database.R") 

# 2 胡牌规则定义
source("rules.R") 

# 3 开局抓牌
source("initialization.R") 

# 4 番型
source("huTypes.R")  

# 5 打法
source("actions.R")  

# 6 AI模型
source("AI.R")  

# 7 界面

# 定义UI界面
ui <- fluidPage(
  tags$style(HTML("  
    @font-face {  
      font-family: 'MyCustomFont';  
      src: url('seguiemj.ttf') format('truetype');  
      font-weight: normal;  
      font-style: normal;  
    }  
    body {  
        font-family: 'MyCustomFont', sans-serif;  
      }  
  ")), 
  tags$audio(  
    id = "audioBackPlayer", 
    src = "music.mp3",
    controls = FALSE,  
    autoplay = TRUE,  
    loop = TRUE
  ),  
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/howler/2.2.3/howler.min.js")
  ),
  tags$script(HTML("  
    $(document).ready(function() {  
      // 获取音频元素  
      var audioElement = $('#audioBackPlayer')[0];  
        
      // 设置音量为一半（0.2）  
      if (audioElement.volume) {  
        audioElement.volume = 0.2;  
      }  
    });  
  ")),  
  tags$head(tags$script(HTML("
    // 确保在document加载完成后执行
    document.addEventListener('DOMContentLoaded', function() {
      var audioCard = {};  
      var variableName = `audio东`; 
      var variableValue = new Howl({
          src: [`东.mp3`]
        });
      audioCard[variableName] = variableValue; 
      var variableName = `audio南`; 
      var variableValue = new Howl({
          src: [`南.mp3`]
        });
      audioCard[variableName] = variableValue; 
      var variableName = `audio西`; 
      var variableValue = new Howl({
          src: [`西.mp3`]
        });
      audioCard[variableName] = variableValue; 
      var variableName = `audio北`; 
      var variableValue = new Howl({
          src: [`北.mp3`]
        });
      audioCard[variableName] = variableValue; 
      for (var i = 1; i <= 9; i++) {
        var variableName = `audio${i}筒`;
        var variableValue = new Howl({
          src: [`${i}筒.mp3`]
        });
        audioCard[variableName] = variableValue;
      }
      for (var i = 1; i <= 9; i++) {
        var variableName = `audio${i}万`;
        var variableValue = new Howl({
          src: [`${i}万.mp3`]
        });
        audioCard[variableName] = variableValue;
      }
      for (var i = 1; i <= 9; i++) {
        var variableName = `audio${i}条`;
        var variableValue = new Howl({
          src: [`${i}条.mp3`]
        });
        audioCard[variableName] = variableValue;
      }
      var audioElementcanPlay = new Howl({
          src: ['canplay.mp3']
        });
        audioElementcanPlay.volume(0.5);
      var audioElementStart = new Howl({
          src: ['start.mp3']
        });
      var audioElementHu = new Howl({
          src: ['hu.mp3']
        });
      var audioElementPong = new Howl({
          src: ['pong.mp3']
        });
      var audioElementGang = new Howl({
          src: ['gang.mp3']
        });
      var audioElementQianggang = new Howl({
          src: ['qianggang.mp3']
        });
      var audioElementZimo = new Howl({
          src: ['zimo.mp3']
        });
      var audioElementcanHu = new Howl({
          src: ['canhu.mp3']
        });
      var audioElementcanPong = new Howl({
          src: ['canpong.mp3']
        });
      var audioElementcanGang = new Howl({
          src: ['cangang.mp3']
        });
      var audioElementcanQianggang = new Howl({
          src: ['canqianggang.mp3']
        });
      var audioElementcanZimo = new Howl({
          src: ['canzimo.mp3']
        });
      var audioElementcanZigang = new Howl({
          src: ['canzigang.mp3']
        });
      var audioElementcanBugang = new Howl({
          src: ['canbugang.mp3']
        });
      var audioElementGuo = new Howl({
          src: ['guo.mp3']
        });
      var audioElementBuhua = new Howl({
          src: ['buhua.mp3']
        });
      var strings = ['东', '南', '西', '北'];
      // 添加 1筒 至 9筒
      for (var i = 1; i <= 9; i++) {
        strings.push(i + '筒');
      }
      // 添加 1万 至 9万
      for (var i = 1; i <= 9; i++) {
        strings.push(i + '万');
      }
      // 添加 1条 至 9条
      for (var i = 1; i <= 9; i++) {
        strings.push(i + '条');
      }
      for (var i = 0; i < strings.length; i++) {
        var functionName = `Sound${strings[i]}`;
        var audioX = `audio${strings[i]}`;
        var functionCode = `
          Shiny.addCustomMessageHandler('${functionName}', function(message) {
            if (message.playCondition) {
              audioCard['${audioX}'].play();
            }
          });
        `;
        eval(functionCode);
      }
      Shiny.addCustomMessageHandler('canplaySound', function(message) {
        audioElementcanPlay.play()
      });
      Shiny.addCustomMessageHandler('startSound', function(message) {
        if (message.playCondition) {
          audioElementStart.play();
        }
      });
      Shiny.addCustomMessageHandler('huSound', function(message) {
        if (message.playCondition) {
          audioElementHu.play();
        }
      });
      Shiny.addCustomMessageHandler('pongSound', function(message) {
        if (message.playCondition) {
          audioElementPong.play();
        }
      });
      Shiny.addCustomMessageHandler('gangSound', function(message) {
        if (message.playCondition) {
          audioElementGang.play();
        }
      });
      Shiny.addCustomMessageHandler('zimoSound', function(message) {
        if (message.playCondition) {
          audioElementZimo.play();
        }
      });
      Shiny.addCustomMessageHandler('qianggangSound', function(message) {
        if (message.playCondition) {
          audioElementQianggang.play();
        }
      });
      Shiny.addCustomMessageHandler('canhuSound', function(message) {
        if (message.playCondition) {
          audioElementcanHu.play();
        }
      });
      Shiny.addCustomMessageHandler('canpongSound', function(message) {
        if (message.playCondition) {
          audioElementcanPong.play();
        }
      });
      Shiny.addCustomMessageHandler('cangangSound', function(message) {
        if (message.playCondition) {
          audioElementcanGang.play();
        }
      });
      Shiny.addCustomMessageHandler('canzimoSound', function(message) {
        if (message.playCondition) {
          audioElementcanZimo.play();
        }
      });
      Shiny.addCustomMessageHandler('canqianggangSound', function(message) {
        if (message.playCondition) {
          audioElementcanQianggang.play();
        }
      });
      Shiny.addCustomMessageHandler('canzigangSound', function(message) {
        if (message.playCondition) {
          audioElementcanZigang.play();
        }
      });
      Shiny.addCustomMessageHandler('canbugangSound', function(message) {
        if (message.playCondition) {
          audioElementcanBugang.play();
        }
      });
      Shiny.addCustomMessageHandler('guoSound', function(message) {
        if (message.playCondition) {
          audioElementGuo.play();
        }
      });
      Shiny.addCustomMessageHandler('buhuaSound', function(message) {
        if (message.playCondition) {
          audioElementBuhua.play();
        }
      })
    });
  "))),

  titlePanel("智慧麻将人机对战——当前麻将规则：苏州麻将"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("startButton", "重新开始一局新游戏"),
      h4("牌桌上有4位玩家"),
      h5("您是玩家1，其余玩家为AI"),
      h5("玩家操作："),
      uiOutput("noticeOutput"),
      actionButton("selfGangButton", "自杠"),
      actionButton("buGangButton", "补杠"),
      actionButton("selfHuButton", "自摸"),
      selectInput("cardSelect", "选择要打出的牌:", choices = player_hands[[1]], selected =  player_hands[[1]][1]),
      actionButton("playButton", "打出牌"),
      actionButton("listenButton", "检查听牌"),
      h5("牌局信息："),
      verbatimTextOutput("output1"),
      h5("玩家鸣牌操作："),
      uiOutput("currentPlayedOutput"),
      actionButton("pongButton", "碰牌"),
      actionButton("gangButton", "杠牌"),
      actionButton("qiangGangButton", "抢杠"),
      actionButton("huButton", "胡牌"),
      actionButton("guoButton", "过牌（不进行任何鸣牌）")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",   
                  
                  tabPanel("玩家视角", h3("玩家手牌："),
                           uiOutput("playerHandsOutput1"),
                           h3("玩家碰牌："),
                           uiOutput("playerPongOutput1"),
                           h3("玩家杠牌："),
                           uiOutput("playerGangOutput1"),
                           h3("玩家花牌："),
                           uiOutput("playerFlowerOutput1"),
                           h3("玩家花数："),
                           verbatimTextOutput("playerFlowersOutput1"),
                           h3("牌堆剩余牌数："),
                           verbatimTextOutput("remainingDeckOutput1")),
                  
                  tabPanel("牌桌视角", 
                           h3("牌堆剩余牌数："),
                           verbatimTextOutput("remainingDeckOutput"),
                           h3("牌桌上被打出的牌："),
                           verbatimTextOutput("deskOutput"),
                           h3("玩家碰牌和杠牌："),
                           verbatimTextOutput("playerOthersDOutput"),
                           h3("玩家花牌："),
                           verbatimTextOutput("playerFlowersDOutput")),  
                  
                  tabPanel("计分板", 
                           uiOutput("gameCount"),
                           actionButton("clearScoreButton", "清空计分板"),
                           h3("玩家1："),
                           uiOutput("player1Score"),
                           h3("玩家2："),
                           uiOutput("player2Score"),
                           h3("玩家3："),
                           uiOutput("player3Score"),
                           h3("玩家4："),
                           uiOutput("player4Score"),
                           h3("本局终局展示："),
                           uiOutput("gameOverShow")) 
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  dealer <- reactiveVal(0)
  nextdealer <- reactiveVal(0)
  
  gameNum <- reactiveVal(1)

  gameFinished <- reactiveVal(0)

  activePlayer <- reactiveVal(0)
 
  shouldPlayPlayer <- reactiveVal(0)

  isHu <- reactiveVal(0)
 
  isHuangzhuang <- reactiveVal(0)

  isMultiHu <- reactiveVal(0)

  isDianpaoING <- reactiveVal(0)

  isQianggangING <- reactiveVal(0)

  notice <- reactiveVal("")

  destination <- reactiveVal("")
  
  shinyalert(title = "欢迎来到\n🎲苏州麻将🎲", text = "重新洗牌、发牌、掷骰子中......\n\n本游戏是由haoyuhugeo开发的Web程序\n请多多支持\n联系方式：haoyuhugeo@pku.edu.cn",
             type = "info", showConfirmButton = TRUE, callbackR = function(x){
               dealer(1)
               activePlayer(1)
               shouldPlayPlayer(1)
             })
  
  session$sendCustomMessage("startSound", list(playCondition = TRUE))

  updateRemainingDeck <- function() {
    output$remainingDeckOutput <- renderText({
      paste(length(remaining_deck), collapse = " ")
    })
    output$remainingDeckOutput1 <- renderText({
      paste(length(remaining_deck), collapse = " ")
    })
    
    output$gameCount <- renderUI({
      tags$h3(paste0("当前是第", gameNum(), "局，已经完成了", gameFinished(), "局有效对局"))
    })
    
    output$noticeOutput <- renderUI({
      tags$h5(notice(), style = "border: none; font-weight: bold;")
    })
    
    output$deskOutput <- renderText({
      paste(" 玩家1已打出的牌：", paste(player_played[[1]], collapse = " "), "\n",
            "玩家2已打出的牌：", paste(player_played[[2]], collapse = " "), "\n",
            "玩家3已打出的牌：", paste(player_played[[3]], collapse = " "), "\n",
            "玩家4已打出的牌：", paste(player_played[[4]], collapse = " "), "\n")
    })
    
    output$currentPlayedOutput <- renderUI({
      tags$h1(paste(emoji(current_played),emoji(current_bugang), collapse = ""), style = "border: none;")
    })
    
    output$playerHandsOutput1 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(player_hands[[1]], emoji), collapse = ""), "</pre>"))
    })
    
    output$playerHandsOutput2 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(player_hands[[2]], emoji), collapse = ""), "</pre>"))
    })
    
    output$playerHandsOutput3 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(player_hands[[3]], emoji), collapse = ""), "</pre>"))
    })
    
    output$playerHandsOutput4 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(player_hands[[4]], emoji), collapse = ""), "</pre>"))
    })
    
    output$playerFlowersOutput1 <- renderText({
      paste("硬花：", player_hard_flowers[1], "\n 花牌数（计1花）：", player_hard_flowers[1], 
            "\n软花：", player_soft_flowers_minggang[1] + player_soft_flowers_angang[1] * 2 + player_soft_flowers_fengpeng[1] + player_soft_flowers_fenganke[1] * 2 + player_soft_flowers_fenggang[1] * 2, 
            "\n 明杠（计1花）：", player_soft_flowers_minggang[1],
            "\n 暗杠（计2花）：", player_soft_flowers_angang[1],
            "\n 风碰（计1花）：", player_soft_flowers_fengpeng[1],
            "\n 风刻（计2花）：", player_soft_flowers_fenganke[1],
            "\n 风杠（额外计2花）：", player_soft_flowers_fenggang[1])
    })
    
    output$playerFlowersOutput <- renderText({
      paste(" 玩家1花牌数：", player_hard_flowers[1], "\n",
            "玩家2花牌数：", player_hard_flowers[2], "\n",
            "玩家3花牌数：", player_hard_flowers[3], "\n",
            "玩家4花牌数：", player_hard_flowers[4], "\n")
    })
    
    output$playerFlowersDOutput <- renderText({
      paste(" 玩家1花牌数：", player_hard_flowers[1], "\n",
            "玩家2花牌数：", player_hard_flowers[2], "\n",
            "玩家3花牌数：", player_hard_flowers[3], "\n",
            "玩家4花牌数：", player_hard_flowers[4], "\n")
    })
    
    output$playerPongOutput1 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_pongs[[1]], each = 3), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerPongOutput2 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_pongs[[2]], each = 3), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerPongOutput3 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_pongs[[3]], each = 3), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerPongOutput4 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_pongs[[4]], each = 3), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerGangOutput1 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_gangs[[1]], each = 4), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerGangOutput2 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_gangs[[2]], each = 4), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerGangOutput3 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_gangs[[3]], each = 4), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerGangOutput4 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_gangs[[4]], each = 4), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerFlowerOutput1 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_hards[[1]], each = 1), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerFlowerOutput2 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_hards[[2]], each = 1), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerFlowerOutput3 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_hards[[3]], each = 1), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerFlowerOutput4 <- renderUI({
      HTML(paste0("<pre style='font-size: 40px;'>", paste(lapply(rep(player_hards[[4]], each = 1), emoji), collapse = ""), "</pre>"))
    })
    
    output$playerOthersOutput <- renderText({
      paste(" 玩家1碰牌：", paste(player_pongs[[1]], collapse = " "), "\n",
            "玩家1杠牌：", paste(player_gangs[[1]], collapse = " "), "\n",
            "玩家2碰牌：", paste(player_pongs[[2]], collapse = " "), "\n",
            "玩家2杠牌：", paste(player_gangs[[2]], collapse = " "), "\n",
            "玩家3碰牌：", paste(player_pongs[[3]], collapse = " "), "\n",
            "玩家3杠牌：", paste(player_gangs[[3]], collapse = " "), "\n",
            "玩家4碰牌：", paste(player_pongs[[4]], collapse = " "), "\n",
            "玩家4杠牌：", paste(player_gangs[[4]], collapse = " "), "\n")
    })
    
    output$playerOthersDOutput <- renderText({
      paste(" 玩家1碰牌：", paste(player_pongs[[1]], collapse = " "), "\n",
            "玩家1杠牌：", paste(player_gangs[[1]], collapse = " "), "\n",
            "玩家2碰牌：", paste(player_pongs[[2]], collapse = " "), "\n",
            "玩家2杠牌：", paste(player_gangs[[2]], collapse = " "), "\n",
            "玩家3碰牌：", paste(player_pongs[[3]], collapse = " "), "\n",
            "玩家3杠牌：", paste(player_gangs[[3]], collapse = " "), "\n",
            "玩家4碰牌：", paste(player_pongs[[4]], collapse = " "), "\n",
            "玩家4杠牌：", paste(player_gangs[[4]], collapse = " "), "\n")
    })
    
    output$output1 <- renderText({
      paste(" 本局骰子点数：", paste(dice, collapse = "和"), "\n",
            "本局的庄家是： 玩家", dealer(), "\n\n",
            "刚刚，", note, "\n\n",
            "当前被打出的牌是：", paste(current_played, collapse = " "), "\n",
            "当前打出牌的玩家是： 玩家", current_played_player, "\n")
    })
    
    output$notepadOutput <- renderText({
      paste(notepad, collapse = "\n")
    })
    
    output$player1Score <- renderUI({
      HTML(paste0("<pre style='font-size: 24px;'>", paste(scorepad[1]), collapse = " "), "</pre>")
    })
    
    output$player2Score <- renderUI({
      HTML(paste0("<pre style='font-size: 24px;'>", paste(scorepad[2]), collapse = " "), "</pre>")
    })
    
    output$player3Score <- renderUI({
      HTML(paste0("<pre style='font-size: 24px;'>", paste(scorepad[3]), collapse = " "), "</pre>")
    })
    
    output$player4Score <- renderUI({
      HTML(paste0("<pre style='font-size: 24px;'>", paste(scorepad[4]), collapse = " "), "</pre>")
    })
    
    output$gameOverShow <- renderUI({
      HTML(paste0("<pre style='font-size: 24px; text-align: center;'> ", destination()), "</pre>")
    })
  }

  observeEvent(input$startButton, { 
    session$sendCustomMessage("startSound", list(playCondition = TRUE))
    new_game()
    gameNum(gameNum()+1)
    if (nextdealer() == 0) {
      dealer(sample(1:4,1))
    }
    else {
      dealer(nextdealer())
    }
    draw_available <<- c(dealer() == 1, dealer() == 2, dealer() == 3, dealer() == 4)
    shinyalert(title = "欢迎来到\n🎲苏州麻将🎲", text = paste("重新洗牌、发牌、掷骰子中......\n新的一局庄家是：玩家", dealer(), "\n\n本游戏是由haoyuhugeo开发的Web程序\n请多多支持\n联系方式：haoyuhugeo@pku.edu.cn"),
               type = "info", showConfirmButton = TRUE, callbackR = function(x){
                 activePlayer(0)
                 activePlayer(dealer())
                 shouldPlayPlayer(0)
                 shouldPlayPlayer(dealer())
               })
    updateSelectInput(session, "playerSelect", selected = 1)  
    updateTabsetPanel(session, "tabs", selected = paste0("玩家", 1))
    isHu(0)
    isHuangzhuang(0)
    destination("")
    updateRemainingDeck()
  }) 
 
  observeEvent(input$clearScoreButton, {  
    dealer(0)
    gameNum(0)
    gameFinished(0)
    scorepad[1:4] <<- 0
    once_huangzhuang <<- 0
    once_multi_hu <<- 0
    shinyalert(title = "已清空", text = "计分板和所有对局记录都已被清空\n请您重新开始一局新游戏",
               type = "success", showConfirmButton = TRUE)
  })

  observeEvent(input$playButton, {
    player <- 1
    card <- input$cardSelect
    if (play_card(player, card)) {
      isDianpaoING(0)
      isQianggangING(0)
      session$sendCustomMessage(paste0("Sound", card), list(playCondition = TRUE))
      claiming_available[1:4] <<- FALSE
      if (length(if_can_dianpao(player, card)) == 3) {
        isDianpaoING(1)
        shinyalert(title = paste(emoji(card), "打出牌成功", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但所有其他玩家都提出鸣牌要求，可能发生一炮多响"),
                   type = "warning", showConfirmButton = FALSE, timer = 1000,
                   callbackR = function(x){
                     activePlayer(if_can_dianpao(player, card)[1])
                   })
        updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, card)[1])  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, card)[1])) 
        draw_available[as.numeric(player)] <<- FALSE
        draw_available[as.numeric(next_player(player))] <<- TRUE
      }
      else if (length(if_can_dianpao(player, card)) == 2) {
        isDianpaoING(1)
        shinyalert(title = paste(emoji(card), "打出牌成功", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, card)[1], "和玩家", if_can_dianpao(player, card)[2], "都提出鸣牌请求，可能发生一炮多响"),
                   type = "warning", showConfirmButton = FALSE, timer = 1000,
                   callbackR = function(x){
                     activePlayer(if_can_dianpao(player, card)[1])
                   })
        updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, card)[1])  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, card)[1])) 
        draw_available[as.numeric(player)] <<- FALSE
        draw_available[as.numeric(next_player(player))] <<- TRUE
      }
      else if (length(if_can_dianpao(player, card)) == 1) {
        isDianpaoING(1)
        shinyalert(title = paste(emoji(card), "打出牌成功", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, card)[1], "提出鸣牌请求"),
                   type = "warning", showConfirmButton = FALSE, timer = 1000,
                   callbackR = function(x){
                     activePlayer(if_can_dianpao(player, card)[1])
                   })
        updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, card)[1])  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, card)[1])) 
        draw_available[as.numeric(player)] <<- FALSE
        draw_available[as.numeric(next_player(player))] <<- TRUE
      }
      else if (if_can_ponggang(player, card) > 0) {
        shinyalert(title = paste(emoji(card), "打出牌成功", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但玩家" , if_can_ponggang(player, card), "提出鸣牌请求"),
                   type = "warning", showConfirmButton = FALSE, timer = 1000,
                   callbackR = function(x){
                     activePlayer(if_can_ponggang(player, card))
                   })
        updateSelectInput(session, "playerSelect", selected = if_can_ponggang(player, card))  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_ponggang(player, card))) 
        draw_available[as.numeric(player)] <<- FALSE
        draw_available[as.numeric(next_player(player))] <<- TRUE
      }
      else {
        shinyalert(title = paste(emoji(card), "打出牌成功", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌"),
                   type = "info", showConfirmButton = FALSE, timer = 1000,
                   callbackR = function(x){
                     activePlayer(as.numeric(next_player(player)))
                   })
        updateSelectInput(session, "playerSelect", selected = next_player(player))  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", next_player(player))) 
        draw_available[as.numeric(player)] <<- FALSE
        draw_available[as.numeric(next_player(player))] <<- TRUE
      }
    }
    else {
      shinyalert(title = "无法打出牌！", text = paste("玩家", player, " 当前无法打出这张牌！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })
  
  observeEvent(input$listenButton, {
    player <- 1
    card_to_remove <- input$cardSelect
    cards <- player_hands[[player]] 
    
    position <- which(cards %in% card_to_remove)[1]  
 
    if (!is.na(position)) {  
      card <- cards[-position] 
    } else {  
      card <- cards  
    } 
    
    listening <- if_listen(player, card)
    if (length(listening$cards) > 0) {
      if (sum(listening$nums) > 0) {
        tempmessage <- ""
        for (i in 1:length(listening$cards)) {
          tempmessage <- paste(tempmessage, emoji(listening$cards[i]), listening$nums[i], "张\n")
        }
        shinyalert(title = paste(emoji(card_to_remove), "如果打出牌", card_to_remove, emoji(card_to_remove),"\n可以听牌", sum(listening$nums), "张，其中\n\n", tempmessage), 
                   text = "",
                   type = "info", showConfirmButton = TRUE)
      }
      else {
        shinyalert(title = "无法检查听牌！", text = paste("当前无法检查听牌！\n请先确保您手牌数量合法\n并选中一张牌再检查这张牌打出后是否听牌"),
                   type = "warning", showConfirmButton = TRUE)
      }
    }
    else {
      shinyalert(title = paste(emoji(card_to_remove),"如果打出牌", card_to_remove, emoji(card_to_remove),"\n还不能听牌"), 
                 text = "",
                 type = "info", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })
  
  observeEvent(input$pongButton, {
    player <- 1
    card <- current_played
    if (pong_card(player, card)) {
      session$sendCustomMessage(type = 'pongSound', list(playCondition = TRUE))   
      claiming_available[1:4] <<- FALSE
      shinyalert(title = paste(emoji(card),"您碰牌成功", emoji(card)), text = paste("玩家", player, " 碰了玩家 ", current_played_player, "打出的牌 ", card, "\n接下来应该由玩家" , player, "打出一张牌"),
                 type = "info", showConfirmButton = TRUE, imageUrl = "pong.jpg",
                 callbackJS = "function() { Shiny.setInputValue('alertClicked', true); setTimeout(function() { Shiny.setInputValue('alertClicked', false);}, 1000); }")
      updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
      draw_available[1:4] <<- FALSE
      draw_available[2] <<- FALSE
      draw_available[3] <<- FALSE
      draw_available[4] <<- FALSE
      shouldPlayPlayer(player)
    }
    else {
      shinyalert(title = "无法碰牌！", text = paste("玩家", player, " 当前无法碰这张牌！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$gangButton, {
    player <- 1
    card <- current_played
    if (gang_card(player, card)) {
      session$sendCustomMessage(type = 'gangSound', list(playCondition = TRUE))  
      claiming_available[1:4] <<- FALSE
      shinyalert(title = paste(emoji(card),"您杠牌成功", emoji(card)), text = paste("玩家", player, " 杠了玩家 ", current_played_player, "打出的牌 ", card, "\n接下来应该由玩家" , player, "抓牌并打出一张牌"),
                 type = "info", showConfirmButton = TRUE, imageUrl = "gang.jpg")
      updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
      draw_available[1:4] <<- FALSE
      draw_available[as.numeric(player)] <<- TRUE
      activePlayer(0)
      activePlayer(player)
    }
    else {
      shinyalert(title = "无法杠牌！", text = paste("玩家", player, " 当前无法杠这张牌！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$huButton, {
    player <- 1
    card <- current_played
    can_hu_players <- if_can_dianpao(current_played_player, card)
    if (suppressWarnings(hu_card(player, card))) {
      session$sendCustomMessage(type = 'huSound', list(playCondition = TRUE))  
      if (length(can_hu_players) == 1) {
        shinyalert(title = "恭喜您胡牌了！", text = paste("玩家", player, " 成功达成胡牌条件！", "\n", "点炮者是玩家", current_played_player),
                   type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
        score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(player)]+count_soft_flowers(player)+count_fanxing_flowers(player, final_cards))
        shinyalert(title = paste("玩家", player, "胡牌胜利结算：", score, "分",
                                 "\n由玩家", current_played_player, "支付"), 
                   text = paste("底花：", as.numeric(5), "花", 
                                "\n硬花：", player_hard_flowers[as.numeric(player)], "花",
                                "\n软花：", count_soft_flowers(player), "花",
                                text_god(is_god(player, final_cards)),
                                text_earth(is_earth(player, final_cards)),
                                text_moon(is_moon(player, final_cards)),
                                text_gang_replacement(is_gang_replacement(player, final_cards)),
                                text_big_concealed(is_big_concealed(player, final_cards)),
                                text_small_concealed(is_small_concealed(player, final_cards)),
                                text_diao(is_diao(player, final_cards)),
                                text_pure_onecolor(is_pure_onecolor(player, final_cards)),
                                text_mixed_onecolor(is_mixed_onecolor(player, final_cards)),
                                text_pongponghu(is_pongponghu(player, final_cards)),
                                text_normal(is_normal_seven_pairs(player, final_cards)),
                                text_luxury(is_luxury_seven_pairs(player, final_cards)),
                                text_super(is_super_seven_pairs(player, final_cards)),
                                text_ultra(is_ultra_seven_pairs(player, final_cards)),
                                text_longdragon(is_longdragon(player, final_cards)),
                                text_four_winds(is_four_winds(player, final_cards)),
                                text_small_four_winds(is_small_four_winds(player, final_cards)),
                                text_three_winds(is_three_winds(player, final_cards)),
                                text_small_three_winds(is_small_three_winds(player, final_cards)),
                                text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                                text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                                text_diling(diling)),
                   type = "success", showConfirmButton = TRUE,
                   imageUrl = paste0("huimages/", hutypes(player, final_cards), ".jpg"))
        scorelist <- c(0,0,0,0)
        scorelist[as.numeric(player)] <- score
        scorelist[as.numeric(current_played_player)] <- -score
        shinyalert(title = "本局麻将分数结算", 
                   text = paste("玩家1 得分：", scorelist[1], "\n", 
                                "玩家2 得分：", scorelist[2], "\n",
                                "玩家3 得分：", scorelist[3], "\n",
                                "玩家4 得分：", scorelist[4], "\n"),
                   type = "info", showConfirmButton = TRUE)
        draw_available[1:4] <<- FALSE
        scorepad[1] <<- scorepad[1] + scorelist[1]
        scorepad[2] <<- scorepad[2] + scorelist[2]
        scorepad[3] <<- scorepad[3] + scorelist[3]
        scorepad[4] <<- scorepad[4] + scorelist[4]
        isHu(1)
      }

      else {
        isMultiHu(1)
        shinyalert(title = "恭喜您胡牌了！\n一炮多响！", text = paste("多位玩家成功达成胡牌条件！", "\n", "点炮者是玩家", current_played_player),
                   type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
        scorelist <- c(0,0,0,0)
        for (i in 1: length(can_hu_players)) {
          playeri <- can_hu_players[i]
          handi <- player_hands[[playeri]]
          pongi <- c(player_pongs[[playeri]],player_pongs[[playeri]],player_pongs[[playeri]])
          gangi <- c(player_gangs[[playeri]],player_gangs[[playeri]],player_gangs[[playeri]])
          final_cards <<- c(handi,pongi,gangi,card)
          score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(playeri)]+count_soft_flowers(playeri)+count_fanxing_flowers(playeri, final_cards))
          shinyalert(title = paste("玩家", playeri, "胡牌胜利结算：", score, "分",
                                   "\n由玩家", current_played_player, "支付"), 
                     text = paste("底花：", as.numeric(5), "花", 
                                  "\n硬花：", player_hard_flowers[as.numeric(playeri)], "花",
                                  "\n软花：", count_soft_flowers(playeri), "花",
                                  text_god(is_god(playeri, final_cards)),
                                  text_earth(is_earth(playeri, final_cards)),
                                  text_moon(is_moon(playeri, final_cards)),
                                  text_gang_replacement(is_gang_replacement(playeri, final_cards)),
                                  text_big_concealed(is_big_concealed(playeri, final_cards)),
                                  text_small_concealed(is_small_concealed(playeri, final_cards)),
                                  text_diao(is_diao(playeri, final_cards)),
                                  text_pure_onecolor(is_pure_onecolor(playeri, final_cards)),
                                  text_mixed_onecolor(is_mixed_onecolor(playeri, final_cards)),
                                  text_pongponghu(is_pongponghu(playeri, final_cards)),
                                  text_normal(is_normal_seven_pairs(playeri, final_cards)),
                                  text_luxury(is_luxury_seven_pairs(playeri, final_cards)),
                                  text_super(is_super_seven_pairs(playeri, final_cards)),
                                  text_ultra(is_ultra_seven_pairs(playeri, final_cards)),
                                  text_longdragon(is_longdragon(player, final_cards)),
                                  text_four_winds(is_four_winds(player, final_cards)),
                                  text_small_four_winds(is_small_four_winds(player, final_cards)),
                                  text_three_winds(is_three_winds(player, final_cards)),
                                  text_small_three_winds(is_small_three_winds(player, final_cards)),
                                  text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                                  text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                                  text_diling(diling)),
                     type = "success", showConfirmButton = TRUE,
                     imageUrl = paste0("huimages/", hutypes(playeri, final_cards), ".jpg"))
          scorelist[as.numeric(playeri)] <- score
          scorelist[as.numeric(current_played_player)] <- scorelist[as.numeric(current_played_player)] - score
        }
        shinyalert(title = "本局麻将分数结算", 
                   text = paste("玩家1 得分：", scorelist[1], "\n", 
                                "玩家2 得分：", scorelist[2], "\n",
                                "玩家3 得分：", scorelist[3], "\n",
                                "玩家4 得分：", scorelist[4], "\n"),
                   type = "info", showConfirmButton = TRUE)
        draw_available[1:4] <<- FALSE
        scorepad[1] <<- scorepad[1] + scorelist[1]
        scorepad[2] <<- scorepad[2] + scorelist[2]
        scorepad[3] <<- scorepad[3] + scorelist[3]
        scorepad[4] <<- scorepad[4] + scorelist[4]
        isHu(1)
      }
    }
    else {
      shinyalert(title = "无法胡牌！", text = paste("玩家", player, " 当前无法胡牌！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$selfGangButton, {
    player <- 1
    if (self_gang(player)) {
      session$sendCustomMessage(type = 'gangSound', list(playCondition = TRUE))  
      claiming_available[1:4] <<- FALSE
      shinyalert(title = "您自杠成功！", text = paste("玩家", player, " 自杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌"),
                 type = "info", showConfirmButton = TRUE, imageUrl = "gang.jpg")
      updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
      draw_available[as.numeric(player)] <<- TRUE
      draw_available[as.numeric(next_player(player))] <<- FALSE
      draw_available[1:4] <<- FALSE
      draw_available[as.numeric(player)] <<- TRUE
      activePlayer(0)
      activePlayer(player)
    }
    else {
      shinyalert(title = "无法自杠！", text = paste("玩家", player, " 当前无法自杠！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$buGangButton, {
    player <- 1
    if (bu_gang(player)) {
      session$sendCustomMessage(type = 'gangSound', list(playCondition = TRUE))  
      claiming_available[1:4] <<- FALSE
      if (length(if_can_dianpao(player, current_bugang)) == 3) {
        isQianggangING(1)
        shinyalert(title = "您补杠成功！", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌", "\n但所有其他玩家都提出抢杠请求"),
                   type = "warning", showConfirmButton = TRUE, imageUrl = "gang.jpg")
        updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, current_bugang)[1])  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, current_bugang)[1])) 
        draw_available[1:4] <<- FALSE
        draw_available[as.numeric(player)] <<- TRUE
        activePlayer(if_can_dianpao(player, current_bugang)[1])
      }
      else if (length(if_can_dianpao(player, current_bugang)) == 2) {
        isQianggangING(1)
        shinyalert(title = "您补杠成功！", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, current_bugang)[1], "和玩家", if_can_dianpao(player, current_bugang)[2], "都提出抢杠请求"),
                   type = "warning", showConfirmButton = TRUE, imageUrl = "gang.jpg")
        updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, current_bugang)[1])  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, current_bugang)[1])) 
        draw_available[1:4] <<- FALSE
        draw_available[as.numeric(player)] <<- TRUE
        activePlayer(if_can_dianpao(player, current_bugang)[1])
      }
      else if (length(if_can_dianpao(player, current_bugang)) == 1) {
        isQianggangING(1)
        shinyalert(title = "您补杠成功！", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, current_bugang)[1], "提出抢杠请求"),
                   type = "warning", showConfirmButton = TRUE, imageUrl = "gang.jpg")
        updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, current_bugang)[1])  
        updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, current_bugang)[1])) 
        draw_available[1:4] <<- FALSE
        draw_available[as.numeric(player)] <<- TRUE
        activePlayer(if_can_dianpao(player, current_bugang)[1])
      }
      else {
        shinyalert(title = "您补杠成功！", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌"),
                   type = "info", showConfirmButton = TRUE, imageUrl = "gang.jpg")
        updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
        draw_available[1:4] <<- FALSE
        draw_available[as.numeric(player)] <<- TRUE
        activePlayer(0)
        activePlayer(player)
      }
    }
    else {
      shinyalert(title = "无法补杠！", text = paste("玩家", player, " 当前无法补杠！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$qiangGangButton, {
    player <- 1
    if (qiang_gang(player, current_bugang)) {
      session$sendCustomMessage(type = 'qianggangSound', list(playCondition = TRUE))  
      shinyalert(title = "恭喜您胡牌了！", text = paste("玩家", player, " 成功达成胡牌条件！", "\n", "被抢杠者是玩家", current_bugang_player),
                 type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
      score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(player)]+count_soft_flowers(player)+count_fanxing_flowers(player, final_cards))
      shinyalert(title = paste("玩家", player, "抢杠胜利结算：", 3 * score, "分",
                               "\n由玩家", current_bugang_player, "支付"), 
                 text = paste("底花：", as.numeric(5), "花",
                              "\n硬花：", player_hard_flowers[as.numeric(player)], "花",
                              "\n软花：", count_soft_flowers(player), "花",
                              text_god(is_god(player, final_cards)),
                              text_earth(is_earth(player, final_cards)),
                              text_moon(is_moon(player, final_cards)),
                              text_gang_replacement(is_gang_replacement(player, final_cards)),
                              text_big_concealed(is_big_concealed(player, final_cards)),
                              text_small_concealed(is_small_concealed(player, final_cards)),
                              text_diao(is_diao(player, final_cards)),
                              text_pure_onecolor(is_pure_onecolor(player, final_cards)),
                              text_mixed_onecolor(is_mixed_onecolor(player, final_cards)),
                              text_pongponghu(is_pongponghu(player, final_cards)),
                              text_normal(is_normal_seven_pairs(player, final_cards)),
                              text_luxury(is_luxury_seven_pairs(player, final_cards)),
                              text_super(is_super_seven_pairs(player, final_cards)),
                              text_ultra(is_ultra_seven_pairs(player, final_cards)),
                              text_longdragon(is_longdragon(player, final_cards)),
                              text_four_winds(is_four_winds(player, final_cards)),
                              text_small_four_winds(is_small_four_winds(player, final_cards)),
                              text_three_winds(is_three_winds(player, final_cards)),
                              text_small_three_winds(is_small_three_winds(player, final_cards)),
                              text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                              text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                              text_diling(diling)),
                 type = "success", showConfirmButton = TRUE)
      scorelist <- c(0,0,0,0)
      scorelist[as.numeric(player)] <- 3*score
      scorelist[as.numeric(current_bugang_player)] <- -3*score
      shinyalert(title = "本局麻将分数结算", 
                 text = paste("玩家1 得分：", scorelist[1], "\n", 
                              "玩家2 得分：", scorelist[2], "\n",
                              "玩家3 得分：", scorelist[3], "\n",
                              "玩家4 得分：", scorelist[4], "\n"),
                 type = "info", showConfirmButton = TRUE)
      draw_available[1:4] <<- FALSE
      scorepad[1] <<- scorepad[1] + scorelist[1]
      scorepad[2] <<- scorepad[2] + scorelist[2]
      scorepad[3] <<- scorepad[3] + scorelist[3]
      scorepad[4] <<- scorepad[4] + scorelist[4]
      isHu(1)
    }
    else {
      shinyalert(title = "无法抢杠！", text = paste("玩家", player, " 当前无法抢杠！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$selfHuButton, {
    player <- 1
    if (self_hu(player)) {
      session$sendCustomMessage(type = 'zimoSound', list(playCondition = TRUE))  
      shinyalert(title = "恭喜您胡牌了！", text = paste("玩家", player, " 成功达成胡牌条件！", "\n", "自摸"),
                 type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
      score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(player)]+count_soft_flowers(player)+count_fanxing_flowers(player, final_cards))
      shinyalert(title = paste("玩家", player, "自摸胜利结算：", 3 * score, "分",
                               "\n由所有其他玩家分摊支付"), 
                 text = paste("底花：", as.numeric(5), "花",
                              "\n硬花：", player_hard_flowers[as.numeric(player)], "花",
                              "\n软花：", count_soft_flowers(player), "花",
                              text_god(is_god(player, final_cards)),
                              text_earth(is_earth(player, final_cards)),
                              text_moon(is_moon(player, final_cards)),
                              text_gang_replacement(is_gang_replacement(player, final_cards)),
                              text_big_concealed(is_big_concealed(player, final_cards)),
                              text_small_concealed(is_small_concealed(player, final_cards)),
                              text_diao(is_diao(player, final_cards)),
                              text_pure_onecolor(is_pure_onecolor(player, final_cards)),
                              text_mixed_onecolor(is_mixed_onecolor(player, final_cards)),
                              text_pongponghu(is_pongponghu(player, final_cards)),
                              text_normal(is_normal_seven_pairs(player, final_cards)),
                              text_luxury(is_luxury_seven_pairs(player, final_cards)),
                              text_super(is_super_seven_pairs(player, final_cards)),
                              text_ultra(is_ultra_seven_pairs(player, final_cards)),
                              text_longdragon(is_longdragon(player, final_cards)),
                              text_four_winds(is_four_winds(player, final_cards)),
                              text_small_four_winds(is_small_four_winds(player, final_cards)),
                              text_three_winds(is_three_winds(player, final_cards)),
                              text_small_three_winds(is_small_three_winds(player, final_cards)),
                              text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                              text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                              text_diling(diling)),
                 type = "success", showConfirmButton = TRUE,
                 imageUrl = paste0("huimages/", hutypes(player, final_cards), ".jpg"))
      scorelist <- c(0,0,0,0)
      for (i in 1:4) {
        scorelist[i] <- -score
      }
      scorelist[as.numeric(player)] <- 3*score
      shinyalert(title = "本局麻将分数结算", 
                 text = paste("玩家1 得分：", scorelist[1], "\n", 
                              "玩家2 得分：", scorelist[2], "\n",
                              "玩家3 得分：", scorelist[3], "\n",
                              "玩家4 得分：", scorelist[4], "\n"),
                 type = "info", showConfirmButton = TRUE)
      draw_available[1:4] <<- FALSE
      scorepad[1] <<- scorepad[1] + scorelist[1]
      scorepad[2] <<- scorepad[2] + scorelist[2]
      scorepad[3] <<- scorepad[3] + scorelist[3]
      scorepad[4] <<- scorepad[4] + scorelist[4]
      isHu(1)
    }
    else {
      shinyalert(title = "无法自摸！", text = paste("玩家", player, " 当前无法自摸！"),
                 type = "error", showConfirmButton = TRUE)
    }
    updateRemainingDeck()
  })

  observeEvent(input$guoButton, {
    player <- 1
    if (sum(claiming_available) > 1) {
      session$sendCustomMessage(type = 'guoSound', list(playCondition = TRUE))  
      claiming_available[as.numeric(player)] <<- FALSE
      shinyalert(title = "玩家过牌！", text = paste("玩家", player, " 选择放弃鸣牌！", "\n接下来应该由玩家" , which(claiming_available)[1], "鸣牌"),
                 type = "info", showConfirmButton = FALSE, timer = 1000,
                 callbackR = function(x){
                   activePlayer(0)
                   activePlayer(as.numeric(which(claiming_available)[1]))
                 })
      updateSelectInput(session, "playerSelect", selected = which(claiming_available)[1])  
      updateTabsetPanel(session, "tabs", selected = paste0("玩家", which(claiming_available)[1])) 
    }
    else if (sum(claiming_available) == 1) {
      session$sendCustomMessage(type = 'guoSound', list(playCondition = TRUE))  
      shinyalert(title = "玩家过牌！", text = paste("玩家", player, " 选择放弃鸣牌！", "\n接下来应该由玩家" , next_player(current_played_player), "抓牌并打出一张牌"),
                 type = "info", showConfirmButton = FALSE, timer = 1000,
                 callbackR = function(x){
                   activePlayer(0)
                   activePlayer(as.numeric(next_player(current_played_player)))
                 })
      claiming_available[as.numeric(player)] <<- FALSE
      updateSelectInput(session, "playerSelect", selected = next_player(current_played_player))  
      updateTabsetPanel(session, "tabs", selected = paste0("玩家", next_player(current_played_player))) 
    }
    else if ((as.numeric(player) == as.numeric(current_pong_player)) || (as.numeric(player) == as.numeric(current_get_player) && as.numeric(player) != as.numeric(current_played_player))) {
      shinyalert(title = "无法过牌！", text = paste("玩家", player, " 当前应该打出一张牌"),
                 type = "warning", showConfirmButton = FALSE, timer = 1000)
    }
    else if (draw_available[as.numeric(player)]) {
      shinyalert(title = "无法过牌！", text = paste("玩家", player, " 当前应该抓牌并打出一张牌"),
                 type = "warning", showConfirmButton = FALSE, timer = 1000, callbackR = function(x){
                   activePlayer(0)
                   activePlayer(1)
                 })
    }
    else if (current_pong_player > 0 || current_gang_player > 0) {
      shinyalert(title = "无法过牌！", text = paste("当前不是玩家", player, " 的轮次", "\n现在是鸣牌者玩家" , max(current_pong_player, current_gang_player), "的轮次"),
                 type = "warning", showConfirmButton = FALSE, timer = 1000, callbackR = function(x){
                   activePlayer(0)
                   activePlayer(max(current_pong_player, current_gang_player))
                 })
      updateSelectInput(session, "playerSelect", selected = max(current_pong_player, current_gang_player))  
      updateTabsetPanel(session, "tabs", selected = paste0("玩家", max(current_pong_player, current_gang_player))) 
    }
    else if (current_played_player > 0) {
      shinyalert(title = "无法过牌！", text = paste("当前不是玩家", player, " 的轮次", "\n现在是玩家" , max(1,next_player(current_played_player)), "的轮次"),
                 type = "warning", showConfirmButton = FALSE, timer = 1000, callbackR = function(x){
                   activePlayer(0)
                   activePlayer(max(1,next_player(current_played_player)))
                 })
      updateSelectInput(session, "playerSelect", selected = max(1,next_player(current_played_player)))  
      updateTabsetPanel(session, "tabs", selected = paste0("玩家", max(1,next_player(current_played_player)))) 
    }
    else {
      shinyalert(title = "无法过牌！", text = paste("当前不是玩家", player, " 的轮次", "\n现在是玩家" , max(1,current_get_player), "的轮次"),
                 type = "warning", showConfirmButton = FALSE, timer = 1000, callbackR = function(x){
                   activePlayer(0)
                   activePlayer(selected = max(1,current_get_player))
                 })
      updateSelectInput(session, "playerSelect", selected = max(1,current_get_player))  
      updateTabsetPanel(session, "tabs", selected = paste0("玩家", max(1,current_get_player))) 
    }
    updateRemainingDeck()
  })
  
  observeEvent(activePlayer(), {  
    if (activePlayer() == 1) {  
      player <- 1
      original_flowers <- player_hard_flowers[player]
      if (draw_available[as.numeric(player)] && !claiming_available[as.numeric(player)]) {
        if (draw_card(player)) {
          isHuangzhuang(if_huangzhuang)
          claiming_available[1:4] <<- FALSE
          if (player_hard_flowers[player] > original_flowers) {
            session$sendCustomMessage(type = 'buhuaSound', list(playCondition = TRUE)) 
            shinyalert(title = "您补花成功", text = paste("玩家", player, " 抓到了花牌 ", "\n从剩余牌堆中抓牌进行了补花"),
                       type = "info", showConfirmButton = FALSE, timer = 1000, imageUrl = "hua.jpg")
          }
          if (if_can_zimo(player)){
            shinyalert(title = paste(emoji(current_get),"您抓牌成功", emoji(current_get)), text = paste("玩家", player, " 抓到了牌 ", current_get, "\n当前可以自摸胡牌！\n也可以不自摸，并直接打出一张牌"),
                       type = "warning", showConfirmButton = TRUE,
                       callbackJS = "function() { Shiny.setInputValue('alertCanZimo', true); setTimeout(function() { Shiny.setInputValue('alertCanZimo', false);}, 1000); }")
            updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
            notice("提示：当前可以自摸胡牌！")
            shouldPlayPlayer(player)
          }
          else if (if_can_zigang(player)) { 
            shinyalert(title = paste(emoji(current_get),"您抓牌成功", emoji(current_get)), text = paste("玩家", player, " 抓到了牌 ", current_get, "\n当前可以自杠！\n也可以不自杠，并直接打出一张牌"),
                       type = "warning", showConfirmButton = TRUE,
                       callbackJS = "function() { Shiny.setInputValue('alertCanZigang', true); setTimeout(function() { Shiny.setInputValue('alertCanZigang', false);}, 1000); }")
            updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
            notice("提示：当前可以自杠！")
            shouldPlayPlayer(player)
          }
          else if (if_can_bugang(player)) {
            shinyalert(title = paste(emoji(current_get),"您抓牌成功", emoji(current_get)), text = paste("玩家", player, " 抓到了牌 ", current_get, "\n当前可以补杠！\n也可以不补杠，并直接打出一张牌"),
                       type = "warning", showConfirmButton = TRUE,
                       callbackJS = "function() { Shiny.setInputValue('alertCanBugang', true); setTimeout(function() { Shiny.setInputValue('alertCanBugang', false);}, 1000); }")
            updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
            notice("提示：当前可以补杠！")
            shouldPlayPlayer(player)
          }
          else {
            shinyalert(title = paste(emoji(current_get),"您抓牌成功", emoji(current_get)), text = paste("玩家", player, " 抓到了牌 ", current_get, "\n接下来应该由玩家" , player, "打出一张牌"),
                       type = "warning", showConfirmButton = TRUE,
                       callbackJS = "function() { Shiny.setInputValue('alertClicked', true); setTimeout(function() { Shiny.setInputValue('alertClicked', false);}, 1000); }")
            updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
            notice("")
            shouldPlayPlayer(player)
          }
        }
        else {
          shinyalert(title = "无法抓牌！", text = paste("玩家", player, " 当前无法抓牌！"),
                     type = "error", showConfirmButton = TRUE)
          notice("")
        }
      }
      else if (!draw_available[as.numeric(player)] && claiming_available[as.numeric(player)]) {
        if (isQianggangING() > 0) {
          shinyalert(title = "您可以抢杠！", text = paste("玩家", player, " 当前可以选择鸣牌(抢)或过牌！", "\n过牌后将进入其他玩家的轮次"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanQianggang', true); setTimeout(function() { Shiny.setInputValue('alertCanQianggang', false);}, 1000); }")
        }
        else if (isDianpaoING() > 0) {
          shinyalert(title = "您可以胡牌！", text = paste("玩家", player, " 当前可以选择鸣牌(胡)或过牌！", "\n过牌后将进入其他玩家的轮次"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanHu', true); setTimeout(function() { Shiny.setInputValue('alertCanHu', false);}, 1000); }")
        }
        else if (if_you_can_gang(player, current_played)) {
          shinyalert(title = "您可以杠牌或碰牌！", text = paste("玩家", player, " 当前可以选择鸣牌(杠/碰)或过牌！", "\n过牌后将进入其他玩家的轮次"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanGang', true); setTimeout(function() { Shiny.setInputValue('alertCanGang', false);}, 1000); }")
        }
        else if (if_you_can_pong(player, current_played)) {
          shinyalert(title = "您可以碰牌！", text = paste("玩家", player, " 当前可以选择鸣牌(碰)或过牌！", "\n过牌后将进入其他玩家的轮次"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanPong', true); setTimeout(function() { Shiny.setInputValue('alertCanPong', false);}, 1000); }")
        }
      }
      else if (draw_available[as.numeric(player)] && claiming_available[as.numeric(player)]) {
        if (isQianggangING() > 0) {
          shinyalert(title = "您可以抢杠！", text = paste("玩家", player, " 当前可以选择鸣牌(抢)或过牌！", "\n过牌后将自动抓一张牌"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanQianggang', true); setTimeout(function() { Shiny.setInputValue('alertCanQianggang', false);}, 1000); }")
        }
        else if (isDianpaoING() > 0) {
          shinyalert(title = "您可以胡牌！", text = paste("玩家", player, " 当前可以选择鸣牌(胡)或过牌！", "\n过牌后将自动抓一张牌"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanHu', true); setTimeout(function() { Shiny.setInputValue('alertCanHu', false);}, 1000); }")
        }
        else if (if_you_can_gang(player, current_played)) {
          shinyalert(title = "您可以杠牌或碰牌！", text = paste("玩家", player, " 当前可以选择鸣牌(杠/碰)或过牌！", "\n过牌后将自动抓一张牌"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanGang', true); setTimeout(function() { Shiny.setInputValue('alertCanGang', false);}, 1000); }")
        }
        else if (if_you_can_pong(player, current_played)) {
          shinyalert(title = "您可以碰牌！", text = paste("玩家", player, " 当前可以选择鸣牌(碰)或过牌！", "\n过牌后将自动抓一张牌"),
                     type = "warning", showConfirmButton = TRUE,
                     callbackJS = "function() { Shiny.setInputValue('alertCanPong', true); setTimeout(function() { Shiny.setInputValue('alertCanPong', false);}, 1000); }")
        }
      }
      updateRemainingDeck()
    }
  })
  
  observeEvent(activePlayer(), {  
    if (activePlayer() %in% c(2, 3, 4)) {  
      isHuangzhuang(if_huangzhuang)
      cat("\n\n\nAI玩家开始操作\n")  
      player <- activePlayer()
      card <- current_played
      can_hu_players <- if_can_dianpao(current_played_player, card)

      if (qiang_gang(player, current_bugang)) {
        AIqianggang(player, current_bugang)
      }

      else if (suppressWarnings(hu_card(player, card))) {
        if (length(can_hu_players) == 1) {
          AIhu(player, card)
        }
        else {
          AImultihu(player, card, can_hu_players)
        }
      }
      
      else if (gang_card(player, card)) {
        session$sendCustomMessage("gangSound", list(playCondition = TRUE))
        claiming_available[1:4] <<- FALSE
        shinyalert(title = paste(emoji(card),"AI玩家杠牌", emoji(card)), text = paste("玩家", player, " 杠了玩家 ", current_played_player, "打出的牌 ", card, "\n接下来应该由玩家" , player, "抓牌并打出一张牌"),
                   type = "info", showConfirmButton = FALSE, timer = 1000, imageUrl = "gang.jpg",
                   callbackR = function(x){
                     activePlayer(0)
                     activePlayer(player)
                   })
        updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
        draw_available[1:4] <<- FALSE
        draw_available[as.numeric(player)] <<- TRUE
        updateRemainingDeck()
      }

      else if (pong_card(player, card)) {
        session$sendCustomMessage("pongSound", list(playCondition = TRUE))
        claiming_available[1:4] <<- FALSE
        shinyalert(title = paste(emoji(card),"AI玩家碰牌", emoji(card)), text = paste("玩家", player, " 碰了玩家 ", current_played_player, "打出的牌 ", card, "\n接下来应该由玩家" , player, "打出一张牌"),
                   type = "info", showConfirmButton = FALSE, timer = 1000, imageUrl = "pong.jpg",
                   callbackR = function(x){
                     shouldPlayPlayer(player)
                   })
        updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
        draw_available[1:4] <<- FALSE
        updateRemainingDeck()
      }
      
      else {
        AIdraw(player)
        updateRemainingDeck()
      }
      updateRemainingDeck()
    }  
  })  
  
  AIdraw <- function(player) {
    if (draw_available[as.numeric(player)]) {
      original_flowers <- player_hard_flowers[player]
      if (draw_card(player)) {
        isHuangzhuang(if_huangzhuang)
        if (player_hard_flowers[player] > original_flowers && length(player_played[[1]]) + length(player_played[[2]]) + length(player_played[[3]]) + length(player_played[[4]]) > 0) {
          session$sendCustomMessage(type = 'buhuaSound', list(playCondition = TRUE)) 
          shinyalert(title ="AI玩家补花", text = paste("玩家", player, " 抓到了花牌 ", "\n从剩余牌堆中抓牌进行了补花"),
                     type = "info", showConfirmButton = FALSE, timer = 1000, imageUrl = "hua.jpg",
                     callbackR = function(x){
                       AIdrawing(player)
                     })
        }
        else {
          AIdrawing(player)
        }
      }
    }
  }
  AIdrawing <- function(player) {
    claiming_available[1:4] <<- FALSE
    if (if_can_zimo(player)){
      if (self_hu(player)) {
        AIselfhu(player)
      }
      updateRemainingDeck()
    }
    else {
      updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
      
      if (self_gang(player)) {
        session$sendCustomMessage("gangSound", list(playCondition = TRUE))
        claiming_available[1:4] <<- FALSE
        shinyalert(title = "AI玩家自杠", text = paste("玩家", player, " 自杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌"),
                   type = "info", showConfirmButton = FALSE, timer = 1000, imageUrl = "gang.jpg",
                   callbackR = function(x){
                     AIdraw(player)
                   })
        updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
        draw_available[as.numeric(player)] <<- TRUE
        draw_available[as.numeric(next_player(player))] <<- FALSE
        draw_available[1:4] <<- FALSE
        draw_available[as.numeric(player)] <<- TRUE
        updateRemainingDeck()
      }
      
      else if (bu_gang(player)) {
        session$sendCustomMessage("gangSound", list(playCondition = TRUE))
        claiming_available[1:4] <<- FALSE
        if (length(if_can_dianpao(player, current_bugang)) == 3) {
          isQianggangING(1)
          shinyalert(title = "AI玩家补杠", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌", "\n但所有其他玩家都提出抢杠请求"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000, imageUrl = "gang.jpg",
                     callbackR = function(x){
                       activePlayer(if_can_dianpao(player, current_bugang)[1])
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, current_bugang)[1])  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, current_bugang)[1])) 
          draw_available[1:4] <<- FALSE
          draw_available[as.numeric(player)] <<- TRUE
          updateRemainingDeck()
        }
        else if (length(if_can_dianpao(player, current_bugang)) == 2) {
          session$sendCustomMessage("gangSound", list(playCondition = TRUE))
          isQianggangING(1)
          shinyalert(title = "AI玩家补杠", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, current_bugang)[1], "和玩家", if_can_dianpao(player, current_bugang)[2], "都提出抢杠请求"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000, imageUrl = "gang.jpg",
                     callbackR = function(x){
                       activePlayer(if_can_dianpao(player, current_bugang)[1])
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, current_bugang)[1])  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, current_bugang)[1])) 
          draw_available[1:4] <<- FALSE
          draw_available[as.numeric(player)] <<- TRUE
          updateRemainingDeck()
        }
        else if (length(if_can_dianpao(player, current_bugang)) == 1) {
          session$sendCustomMessage("gangSound", list(playCondition = TRUE))
          isQianggangING(1)
          shinyalert(title = "AI玩家补杠", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, current_bugang)[1], "提出抢杠请求"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000, imageUrl = "gang.jpg",
                     callbackR = function(x){
                       activePlayer(if_can_dianpao(player, current_bugang)[1])
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, current_bugang)[1])  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, current_bugang)[1])) 
          draw_available[1:4] <<- FALSE
          draw_available[as.numeric(player)] <<- TRUE
          updateRemainingDeck()
        }
        else {
          session$sendCustomMessage("gangSound", list(playCondition = TRUE))
          shinyalert(title = "AI玩家补杠", text = paste("玩家", player, " 补杠了！", "\n接下来应该由玩家" , player, "抓牌并打出一张牌"),
                     type = "info", showConfirmButton = FALSE, timer = 1000, imageUrl = "gang.jpg",
                     callbackR = function(x){
                       AIdraw(player)
                     })
          updateSelectInput(session, "cardSelect", choices = player_hands[[1]])
          draw_available[1:4] <<- FALSE
          draw_available[as.numeric(player)] <<- TRUE
        }
        updateRemainingDeck()
      }
      
      else {
        updateRemainingDeck()
        shouldPlayPlayer(player)
      }
    }
  }
  
  observe( {  
    if (shouldPlayPlayer() %in% c(2, 3, 4)) {  
      isHuangzhuang(if_huangzhuang)
      isDianpaoING(0)
      isQianggangING(0)
      player <- shouldPlayPlayer()
      card <- aiNewbee(player)
      if (play_card(player, card)) {
        session$sendCustomMessage(paste0("Sound", card), list(playCondition = TRUE))
        claiming_available[1:4] <<- FALSE
        if (length(if_can_dianpao(player, card)) == 3) {
          session$sendCustomMessage(paste0("Sound", card), list(playCondition = TRUE))
          isDianpaoING(1)
          shinyalert(title = paste(emoji(card), "AI打出牌", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但所有其他玩家都提出鸣牌要求，可能发生一炮多响"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000,
                     callbackR = function(x){
                       activePlayer(if_can_dianpao(player, card)[1])
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, card)[1])  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, card)[1])) 
          draw_available[as.numeric(player)] <<- FALSE
          draw_available[as.numeric(next_player(player))] <<- TRUE
        }
        else if (length(if_can_dianpao(player, card)) == 2) {
          isDianpaoING(1)
          shinyalert(title = paste(emoji(card), "AI打出牌", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, card)[1], "和玩家", if_can_dianpao(player, card)[2], "都提出鸣牌请求，可能发生一炮多响"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000,
                     callbackR = function(x){
                       activePlayer(if_can_dianpao(player, card)[1])
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, card)[1])  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, card)[1])) 
          draw_available[as.numeric(player)] <<- FALSE
          draw_available[as.numeric(next_player(player))] <<- TRUE
        }
        else if (length(if_can_dianpao(player, card)) == 1) {
          isDianpaoING(1)
          shinyalert(title = paste(emoji(card), "AI打出牌", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但玩家" , if_can_dianpao(player, card)[1], "提出鸣牌请求"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000,
                     callbackR = function(x){
                       activePlayer(if_can_dianpao(player, card)[1])
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_dianpao(player, card)[1])  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_dianpao(player, card)[1])) 
          draw_available[as.numeric(player)] <<- FALSE
          draw_available[as.numeric(next_player(player))] <<- TRUE
        }
        else if (if_can_ponggang(player, card) > 0) {
          shinyalert(title = paste(emoji(card), "AI打出牌", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌", "\n但玩家" , if_can_ponggang(player, card), "提出鸣牌请求"),
                     type = "warning", showConfirmButton = FALSE, timer = 1000,
                     callbackR = function(x){
                       activePlayer(if_can_ponggang(player, card))
                     })
          updateSelectInput(session, "playerSelect", selected = if_can_ponggang(player, card))  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", if_can_ponggang(player, card))) 
          draw_available[as.numeric(player)] <<- FALSE
          draw_available[as.numeric(next_player(player))] <<- TRUE
        }
        else {
          shinyalert(title = paste(emoji(card), "AI打出牌", emoji(card)), text = paste("玩家", player, " 打出了牌 ", card, "\n接下来应该由玩家" , next_player(player), "抓牌并打出一张牌"),
                     type = "info", showConfirmButton = FALSE, timer = 1000,
                     callbackR = function(x){
                       activePlayer(as.numeric(next_player(player)))
                       })
          updateSelectInput(session, "playerSelect", selected = next_player(player))  
          updateTabsetPanel(session, "tabs", selected = paste0("玩家", next_player(player))) 
          draw_available[as.numeric(player)] <<- FALSE
          draw_available[as.numeric(next_player(player))] <<- TRUE
        }
      }
      updateRemainingDeck()
    }  
  }) 

  AIqianggang <- function(player, current_bugang) {
    session$sendCustomMessage(type = 'qianggangSound', list(playCondition = TRUE)) 
    shinyalert(title = "AI玩家胡牌了！", text = paste("玩家", player, " 成功达成胡牌条件！", "\n", "被抢杠者是玩家", current_bugang_player),
               type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
    score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(player)]+count_soft_flowers(player)+count_fanxing_flowers(player, final_cards))
    shinyalert(title = paste("玩家", player, "抢杠胜利结算：", 3 * score, "分",
                             "\n由玩家", current_bugang_player, "支付"), 
               text = paste("底花：", as.numeric(5), "花",
                            "\n硬花：", player_hard_flowers[as.numeric(player)], "花",
                            "\n软花：", count_soft_flowers(player), "花",
                            text_god(is_god(player, final_cards)),
                            text_earth(is_earth(player, final_cards)),
                            text_moon(is_moon(player, final_cards)),
                            text_gang_replacement(is_gang_replacement(player, final_cards)),
                            text_big_concealed(is_big_concealed(player, final_cards)),
                            text_small_concealed(is_small_concealed(player, final_cards)),
                            text_diao(is_diao(player, final_cards)),
                            text_pure_onecolor(is_pure_onecolor(player, final_cards)),
                            text_mixed_onecolor(is_mixed_onecolor(player, final_cards)),
                            text_pongponghu(is_pongponghu(player, final_cards)),
                            text_normal(is_normal_seven_pairs(player, final_cards)),
                            text_luxury(is_luxury_seven_pairs(player, final_cards)),
                            text_super(is_super_seven_pairs(player, final_cards)),
                            text_ultra(is_ultra_seven_pairs(player, final_cards)),
                            text_longdragon(is_longdragon(player, final_cards)),
                            text_four_winds(is_four_winds(player, final_cards)),
                            text_small_four_winds(is_small_four_winds(player, final_cards)),
                            text_three_winds(is_three_winds(player, final_cards)),
                            text_small_three_winds(is_small_three_winds(player, final_cards)),
                            text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                            text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                            text_diling(diling)),
               type = "success", showConfirmButton = TRUE,
               imageUrl = paste0("huimages/", hutypes(player, final_cards), ".jpg"))
    scorelist <- c(0,0,0,0)
    scorelist[as.numeric(player)] <- 3*score
    scorelist[as.numeric(current_bugang_player)] <- -3*score
    shinyalert(title = "本局麻将分数结算", 
               text = paste("玩家1 得分：", scorelist[1], "\n", 
                            "玩家2 得分：", scorelist[2], "\n",
                            "玩家3 得分：", scorelist[3], "\n",
                            "玩家4 得分：", scorelist[4], "\n"),
               type = "info", showConfirmButton = TRUE)
    draw_available[1:4] <<- FALSE
    scorepad[1] <<- scorepad[1] + scorelist[1]
    scorepad[2] <<- scorepad[2] + scorelist[2]
    scorepad[3] <<- scorepad[3] + scorelist[3]
    scorepad[4] <<- scorepad[4] + scorelist[4]
    isHu(1)
    nextdealer(as.numeric(player))
  }
  
  AIhu <- function(player, card) {
    session$sendCustomMessage(type = 'huSound', list(playCondition = TRUE)) 
    shinyalert(title = "AI玩家胡牌了！", text = paste("玩家", player, " 成功达成胡牌条件！", "\n", "点炮者是玩家", current_played_player),
               type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
    score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(player)]+count_soft_flowers(player)+count_fanxing_flowers(player, final_cards))
    shinyalert(title = paste("玩家", player, "胡牌胜利结算：", score, "分",
                             "\n由玩家", current_played_player, "支付"), 
               text = paste("底花：", as.numeric(5), "花", 
                            "\n硬花：", player_hard_flowers[as.numeric(player)], "花",
                            "\n软花：", count_soft_flowers(player), "花",
                            text_god(is_god(player, final_cards)),
                            text_earth(is_earth(player, final_cards)),
                            text_moon(is_moon(player, final_cards)),
                            text_gang_replacement(is_gang_replacement(player, final_cards)),
                            text_big_concealed(is_big_concealed(player, final_cards)),
                            text_small_concealed(is_small_concealed(player, final_cards)),
                            text_diao(is_diao(player, final_cards)),
                            text_pure_onecolor(is_pure_onecolor(player, final_cards)),
                            text_mixed_onecolor(is_mixed_onecolor(player, final_cards)),
                            text_pongponghu(is_pongponghu(player, final_cards)),
                            text_normal(is_normal_seven_pairs(player, final_cards)),
                            text_luxury(is_luxury_seven_pairs(player, final_cards)),
                            text_super(is_super_seven_pairs(player, final_cards)),
                            text_ultra(is_ultra_seven_pairs(player, final_cards)),
                            text_longdragon(is_longdragon(player, final_cards)),
                            text_four_winds(is_four_winds(player, final_cards)),
                            text_small_four_winds(is_small_four_winds(player, final_cards)),
                            text_three_winds(is_three_winds(player, final_cards)),
                            text_small_three_winds(is_small_three_winds(player, final_cards)),
                            text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                            text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                            text_diling(diling)),
               type = "success", showConfirmButton = TRUE,
               imageUrl = paste0("huimages/", hutypes(player, final_cards), ".jpg"))
    scorelist <- c(0,0,0,0)
    scorelist[as.numeric(player)] <- score
    scorelist[as.numeric(current_played_player)] <- -score
    shinyalert(title = "本局麻将分数结算", 
               text = paste("玩家1 得分：", scorelist[1], "\n", 
                            "玩家2 得分：", scorelist[2], "\n",
                            "玩家3 得分：", scorelist[3], "\n",
                            "玩家4 得分：", scorelist[4], "\n"),
               type = "info", showConfirmButton = TRUE)
    draw_available[1:4] <<- FALSE
    scorepad[1] <<- scorepad[1] + scorelist[1]
    scorepad[2] <<- scorepad[2] + scorelist[2]
    scorepad[3] <<- scorepad[3] + scorelist[3]
    scorepad[4] <<- scorepad[4] + scorelist[4]
    isHu(1)
    nextdealer(as.numeric(player))
  }
  
  AImultihu <- function(player, card, can_hu_players) {
    session$sendCustomMessage(type = 'huSound', list(playCondition = TRUE)) 
    isMultiHu(1)
    shinyalert(title = "AI玩家胡牌了！\n一炮多响！", text = paste("多位玩家成功达成胡牌条件！", "\n", "点炮者是玩家", current_played_player),
               type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
    scorelist <- c(0,0,0,0)
    for (i in 1: length(can_hu_players)) {
      playeri <- can_hu_players[i]
      handi <- player_hands[[playeri]]
      pongi <- c(player_pongs[[playeri]],player_pongs[[playeri]],player_pongs[[playeri]])
      gangi <- c(player_gangs[[playeri]],player_gangs[[playeri]],player_gangs[[playeri]])
      final_cards <<- c(handi,pongi,gangi,card)
      score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(playeri)]+count_soft_flowers(playeri)+count_fanxing_flowers(playeri, final_cards))
      shinyalert(title = paste("玩家", playeri, "胡牌胜利结算：", score, "分",
                               "\n由玩家", current_played_player, "支付"), 
                 text = paste("底花：", as.numeric(5), "花", 
                              "\n硬花：", player_hard_flowers[as.numeric(playeri)], "花",
                              "\n软花：", count_soft_flowers(playeri), "花",
                              text_god(is_god(playeri, final_cards)),
                              text_earth(is_earth(playeri, final_cards)),
                              text_moon(is_moon(playeri, final_cards)),
                              text_gang_replacement(is_gang_replacement(playeri, final_cards)),
                              text_big_concealed(is_big_concealed(playeri, final_cards)),
                              text_small_concealed(is_small_concealed(playeri, final_cards)),
                              text_diao(is_diao(playeri, final_cards)),
                              text_pure_onecolor(is_pure_onecolor(playeri, final_cards)),
                              text_mixed_onecolor(is_mixed_onecolor(playeri, final_cards)),
                              text_pongponghu(is_pongponghu(playeri, final_cards)),
                              text_normal(is_normal_seven_pairs(playeri, final_cards)),
                              text_luxury(is_luxury_seven_pairs(playeri, final_cards)),
                              text_super(is_super_seven_pairs(playeri, final_cards)),
                              text_ultra(is_ultra_seven_pairs(playeri, final_cards)),
                              text_longdragon(is_longdragon(player, final_cards)),
                              text_four_winds(is_four_winds(player, final_cards)),
                              text_small_four_winds(is_small_four_winds(player, final_cards)),
                              text_three_winds(is_three_winds(player, final_cards)),
                              text_small_three_winds(is_small_three_winds(player, final_cards)),
                              text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                              text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                              text_diling(diling)),
                 type = "success", showConfirmButton = TRUE,
                 imageUrl = paste0("huimages/", hutypes(playeri, final_cards), ".jpg"))
      scorelist[as.numeric(playeri)] <- score
      scorelist[as.numeric(current_played_player)] <- scorelist[as.numeric(current_played_player)] - score
    }
    shinyalert(title = "本局麻将分数结算", 
               text = paste("玩家1 得分：", scorelist[1], "\n", 
                            "玩家2 得分：", scorelist[2], "\n",
                            "玩家3 得分：", scorelist[3], "\n",
                            "玩家4 得分：", scorelist[4], "\n"),
               type = "info", showConfirmButton = TRUE)
    draw_available[1:4] <<- FALSE
    scorepad[1] <<- scorepad[1] + scorelist[1]
    scorepad[2] <<- scorepad[2] + scorelist[2]
    scorepad[3] <<- scorepad[3] + scorelist[3]
    scorepad[4] <<- scorepad[4] + scorelist[4]
    isHu(1)
    nextdealer(0)
  }
  
  AIselfhu <- function(player) {
    session$sendCustomMessage(type = 'zimoSound', list(playCondition = TRUE)) 
    shinyalert(title = "AI玩家胡牌了！", text = paste("玩家", player, " 成功达成胡牌条件！", "\n", "自摸"),
               type = "success", showConfirmButton = TRUE, imageUrl = "hu.jpg")
    score <- diling_calculation(diling)*(5+player_hard_flowers[as.numeric(player)]+count_soft_flowers(player)+count_fanxing_flowers(player, final_cards))
    shinyalert(title = paste("玩家", player, "自摸胜利结算：", 3 * score, "分",
                             "\n由所有其他玩家分摊支付"), 
               text = paste("底花：", as.numeric(5), "花",
                            "\n硬花：", player_hard_flowers[as.numeric(player)], "花",
                            "\n软花：", count_soft_flowers(player), "花",
                            text_god(is_god(player, final_cards)),
                            text_earth(is_earth(player, final_cards)),
                            text_moon(is_moon(player, final_cards)),
                            text_gang_replacement(is_gang_replacement(player, final_cards)),
                            text_big_concealed(is_big_concealed(player, final_cards)),
                            text_small_concealed(is_small_concealed(player, final_cards)),
                            text_diao(is_diao(player, final_cards)),
                            text_pure_onecolor(is_pure_onecolor(player, final_cards)),
                            text_mixed_onecolor(is_mixed_onecolor(player, final_cards)),
                            text_pongponghu(is_pongponghu(player, final_cards)),
                            text_normal(is_normal_seven_pairs(player, final_cards)),
                            text_luxury(is_luxury_seven_pairs(player, final_cards)),
                            text_super(is_super_seven_pairs(player, final_cards)),
                            text_ultra(is_ultra_seven_pairs(player, final_cards)),
                            text_longdragon(is_longdragon(player, final_cards)),
                            text_four_winds(is_four_winds(player, final_cards)),
                            text_small_four_winds(is_small_four_winds(player, final_cards)),
                            text_three_winds(is_three_winds(player, final_cards)),
                            text_small_three_winds(is_small_three_winds(player, final_cards)),
                            text_flower_three_dragons(is_flower_three_dragons(player, final_cards)),
                            text_flower_small_three_dragons(is_flower_small_three_dragons(player, final_cards)),
                            text_diling(diling)),
               type = "success", showConfirmButton = TRUE,
               imageUrl = paste0("huimages/", hutypes(player, final_cards), ".jpg"))
    scorelist <- c(0,0,0,0)
    for (i in 1:4) {
      scorelist[i] <- -score
    }
    scorelist[as.numeric(player)] <- 3*score
    shinyalert(title = "本局麻将分数结算", 
               text = paste("玩家1 得分：", scorelist[1], "\n", 
                            "玩家2 得分：", scorelist[2], "\n",
                            "玩家3 得分：", scorelist[3], "\n",
                            "玩家4 得分：", scorelist[4], "\n"),
               type = "info", showConfirmButton = TRUE)
    draw_available[1:4] <<- FALSE
    scorepad[1] <<- scorepad[1] + scorelist[1]
    scorepad[2] <<- scorepad[2] + scorelist[2]
    scorepad[3] <<- scorepad[3] + scorelist[3]
    scorepad[4] <<- scorepad[4] + scorelist[4]
    isHu(1)
    nextdealer(as.numeric(player))
  }
  
  observeEvent(isHu(), {  
    if (isHu() > 0) {
      gameFinished(gameFinished()+1)
      once_huangzhuang <<- 0
      once_multi_hu <<- isMultiHu()
      gameover <- paste("玩家1\n", paste((lapply(rep(player_gangs[[1]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[1]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[1]], emoji), collapse = ""), "\n\n",
                        "玩家2\n", paste((lapply(rep(player_gangs[[2]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[2]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[2]], emoji), collapse = ""), "\n\n",
                        "玩家3\n", paste((lapply(rep(player_gangs[[3]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[3]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[3]], emoji), collapse = ""), "\n\n",
                        "玩家4\n", paste((lapply(rep(player_gangs[[4]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[4]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[4]], emoji), collapse = ""))
      destination(gameover)
      shinyalert(title = "所有玩家终局展示\n", 
                 text = gameover, type = "success", showConfirmButton = TRUE, showCancelButton = TRUE,
                 confirmButtonText = "开始下一局", cancelButtonText = "查看本局", 
                 closeOnClickOutside = FALSE,
                 callbackR = function(x) { 
                     if (x != FALSE)  {
                       new_game()
                       gameNum(gameNum()+1)
                       if (nextdealer() == 0) {
                         dealer(sample(1:4,1))
                       }
                       else {
                         dealer(nextdealer())
                       }
                       draw_available <<- c(dealer() == 1, dealer() == 2, dealer() == 3, dealer() == 4)
                       updateSelectInput(session, "playerSelect", selected = 1)  
                       updateTabsetPanel(session, "tabs", selected = paste0("玩家", 1))
                       activePlayer(0)
                       activePlayer(dealer())
                       shouldPlayPlayer(0)
                       shouldPlayPlayer(dealer())
                       isHu(0)
                       isHuangzhuang(0)
                       isMultiHu(0)
                       destination("")
                       session$sendCustomMessage("startSound", list(playCondition = TRUE))
                       updateRemainingDeck()
                     } 
                   })
    }
  })
  observeEvent(isHuangzhuang(), {  
    if (isHuangzhuang() > 0) {
      nextdealer(0)
      gameFinished(gameFinished()+1)
      once_huangzhuang <<- 1
      once_multi_hu <<- isMultiHu()
      shinyalert(title = "荒庄", text = paste("所有牌已经被抓完了", "\n但没有玩家胡牌"),
                 type = "warning", showConfirmButton = TRUE)
      gameover <- paste("玩家1\n", paste((lapply(rep(player_gangs[[1]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[1]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[1]], emoji), collapse = ""), "\n\n",
                        "玩家2\n", paste((lapply(rep(player_gangs[[2]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[2]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[2]], emoji), collapse = ""), "\n\n",
                        "玩家3\n", paste((lapply(rep(player_gangs[[3]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[3]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[3]], emoji), collapse = ""), "\n\n",
                        "玩家4\n", paste((lapply(rep(player_gangs[[4]], each = 4), emoji)), collapse = ""), paste((lapply(rep(player_pongs[[4]], each = 3), emoji)), collapse = ""), paste(lapply(player_hands[[4]], emoji), collapse = ""))
      destination(gameover)
      shinyalert(title = "所有玩家终局展示\n", 
                 text = gameover, type = "success", showConfirmButton = TRUE, showCancelButton = TRUE,
                 confirmButtonText = "开始下一局", cancelButtonText = "查看本局", 
                 closeOnClickOutside = FALSE,
                 callbackR = function(x) { 
                   if (x != FALSE)  {
                     new_game()
                     gameNum(gameNum()+1)
                     if (nextdealer() == 0) {
                       dealer(sample(1:4,1))
                     }
                     else {
                       dealer(nextdealer())
                     }
                     draw_available <<- c(dealer() == 1, dealer() == 2, dealer() == 3, dealer() == 4)
                     updateSelectInput(session, "playerSelect", selected = 1)  
                     updateTabsetPanel(session, "tabs", selected = paste0("玩家", 1))
                     activePlayer(0)
                     activePlayer(dealer())
                     shouldPlayPlayer(0)
                     shouldPlayPlayer(dealer())
                     isHu(0)
                     isHuangzhuang(0)
                     isMultiHu(0)
                     destination("")
                     session$sendCustomMessage("startSound", list(playCondition = TRUE))
                     updateRemainingDeck()
                   } 
                 })
    }
  })
  
  observeEvent(input$alertClicked, {
    if (input$alertClicked) {
      session$sendCustomMessage("canplaySound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanZimo, {
    if (input$alertCanZimo) {
      session$sendCustomMessage("canzimoSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanZigang, {
    if (input$alertCanZigang) {
      session$sendCustomMessage("canzigangSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanBugang, {
    if (input$alertCanBugang) {
      session$sendCustomMessage("canbugangSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanGang, {
    if (input$alertCanGang) {
      session$sendCustomMessage("cangangSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanPong, {
    if (input$alertCanPong) {
      session$sendCustomMessage("canpongSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanQianggang, {
    if (input$alertCanQianggang) {
      session$sendCustomMessage("canqianggangSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCanHu, {
    if (input$alertCanHu) {
      session$sendCustomMessage("canhuSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertGang, {
    if (input$alertGang) {
      session$sendCustomMessage("gangSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertPong, {
    if (input$alertPong) {
      session$sendCustomMessage("pongSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertHu, {
    if (input$alertHu) {
      session$sendCustomMessage("huSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertQianggang, {
    if (input$alertQianggang) {
      session$sendCustomMessage("qianggangSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertZimo, {
    if (input$alertZimo) {
      session$sendCustomMessage("zimoSound", list(playCondition = TRUE))
    }
  })
  observeEvent(input$alertCard, {
    if (input$alertCard) {
      session$sendCustomMessage(paste0("Sound", input$readCard), list(playCondition = TRUE))
    }
  })
  
  updateRemainingDeck()
}


# 运行Shiny应用程序
shinyApp(ui = ui, server = server)



