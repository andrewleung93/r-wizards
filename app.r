# to run
library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
ui <- fluidPage(
  
  titlePanel(
    "Wizards Score Calculator"
  ),
  sidebarLayout(
    sidebarPanel(numericInput("numplayer", 
                              "Number of Players",
                              min = 2,
                              max = 6,
                              value = 4),
                 flowLayout(
                   textInput("name1", "Player 1",value = "Player1"),
                   textInput("name2", "Player 2",value = "Player2"),
                   conditionalPanel(
                     condition = "input.numplayer >= 3",
                     textInput("name3", "Player 3",value = "Player3")
                   ),
                   conditionalPanel(
                     condition = "input.numplayer >= 4",
                     textInput("name4", "Player 4",value = "Player4")
                   ),
                   conditionalPanel(
                     condition = "input.numplayer >= 5",
                     textInput("name5", "Player 5",value = "Player5")
                   ),
                   conditionalPanel(
                     condition = "input.numplayer >= 6",
                     textInput("name6", "Player 6",value = "Player6")
                   ),
                   actionButton("btn_start", "Start New Game")
                 )
        ),
    mainPanel(
      
      textOutput("currentround"),
      textOutput("currentdealer"),
      textOutput("invalidchoices"),
      
      fluidRow(column( 4,
                      
                        h3("Guesses"),
                        uiOutput("guesses")
                      
                      ),
               column( 4,
                       
                       h3("Tricks Won"),
                       uiOutput("tricks")
                       
               )
               ),
      
      
      uiOutput("nextbutton"),
      
      tabsetPanel(
        tabPanel("Score",
                 DT::dataTableOutput("tb_score")),
        tabPanel("Progress Graph",
                 plotOutput("progplot")),
        tabPanel("Progress Table",
                 DT::dataTableOutput("tb_prog")),
        tabPanel("Guess",
                 DT::dataTableOutput("tb_guess")),
        tabPanel("Trick",
                 DT::dataTableOutput("tb_trick"))
      )
      
      
      
    )
      
    )
    
    
  )
  

server <- function(input,output){
  #declare max round count
  vals <- reactiveValues(
                         maxround = c(1,20,20,15,12,10),
                         invalid = -1
                         )
  #Click Start
  observeEvent(input$btn_start,{
    
    #Set all Game Variables
    vals$names <- c(input$name1,
                    input$name2,
                    input$name3,
                    input$name4,
                    input$name5,
                    input$name6)
    vals$nplayers <- input$numplayer
    vals$currentnames <- vals$names[1:vals$nplayers]
    vals$round <-  1
    
    #initialize dataframes for guess and actual
    
    df_guess <- data.frame(matrix(ncol = vals$nplayers, nrow=0))
    colnames(df_guess) <- vals$names[1:vals$nplayers]
    vals$df_guess <- df_guess
    
    df_trick <- data.frame(matrix(ncol = vals$nplayers, nrow=0))
    colnames(df_trick) <- vals$names[1:vals$nplayers]
    vals$df_trick <- df_trick
    
    df_score <- data.frame(matrix(data = 0, ncol = vals$nplayers, nrow=1))
    colnames(df_score) <- vals$names[1:vals$nplayers]
    vals$df_score <- df_score
    
    df_prog <- data.frame(matrix(data = 0, ncol = vals$nplayers, nrow=1))
    colnames(df_prog) <- vals$names[1:vals$nplayers]
    vals$df_prog <- df_prog
    
    print(vals$df_guess)
    print(vals$df_trick)
    print(vals$df_score)
    print(vals$currentnames)
    print(vals$nplayers)
    
    output$currentround <- renderText({paste0("The Current Round is ", vals$round)})
    output$currentdealer  <-  renderText({paste0("The Current Dealer is ", vals$names[(vals$round-1) %% vals$nplayers +1])})
    
    valid <- 0:vals$round
    #guesses input
    output$guesses <- renderUI({
      lapply(1:vals$nplayers, function(i) {
        selectInput(paste0("guess_", vals$currentnames[i]), vals$currentnames[i], 0:vals$round,0)
      })
    })
    
    #tricks input
    output$tricks <- renderUI({
      lapply(1:vals$nplayers, function(i) {
        selectInput(paste0("tricks_", vals$currentnames[i]), vals$currentnames[i], 0:vals$round,0)
      })
    })
    

    output$nextbutton <-  renderUI({actionButton("btn_nextround", "Go to Next Round")})
    
    #guess values
    output$tb_guess <- DT::renderDataTable({vals$df_guess},options = list(sDom  = '<"top">lrt<"bottom">ip'))
    #actual values
    output$tb_trick <- DT::renderDataTable({vals$df_trick},options = list(sDom  = '<"top">lrt<"bottom">ip'))
    output$tb_score <- DT::renderDataTable({vals$df_score},options = list(sDom  = '<"top">lrt<"bottom">ip'))
    output$tb_prog <- DT::renderDataTable({vals$df_prog},options = list(sDom  = '<"top">lrt<"bottom">ip'))
  }
  )
  
  #Click Next Round
  observeEvent(input$btn_nextround,{
    #advance Round
    vals$round <-  vals$round + 1
    
    #save results
    guessinput <- unlist(lapply(1:vals$nplayers, function(i) {
      input[[paste0("guess_", vals$currentnames[i])]]
    }))

    
    tricksinput <- unlist(lapply(1:vals$nplayers, function(i) {
      input[[paste0("tricks_", vals$currentnames[i])]]
    }))

    
    #calculate scores
    scores <- rep(0,vals$nplayers)
    for(i in 1:vals$nplayers){
      print(tricksinput[i])
      print(guessinput[i])
      if(guessinput[i] == tricksinput[i]){
        scores[i] <- 20 + 10*as.numeric(guessinput[i])
      } else {
        scores[i] <- -abs(as.numeric(guessinput[i])-as.numeric(tricksinput[i]))*10
      }
    }
    
    guessinput <- data.frame(matrix(data = guessinput, ncol = vals$nplayers, nrow=1))
    colnames(guessinput) <- vals$names[1:vals$nplayers]
    vals$df_guess <- rbind(vals$df_guess, guessinput)
    tricksinput <- data.frame(matrix(data = tricksinput, ncol = vals$nplayers, nrow=1))
    colnames(tricksinput) <- vals$names[1:vals$nplayers]
    vals$df_trick <- rbind(vals$df_trick, tricksinput)
    scores <- data.frame(matrix(data = scores, ncol = vals$nplayers, nrow=1))
    colnames(scores) <- vals$names[1:vals$nplayers]
    vals$df_score <- rbind(vals$df_score, scores)
    
    prog <- cumsum(vals$df_score[vals$currentnames[1]])
    print(prog)
    for(i in 2:vals$nplayers){
      prog <- cbind(prog, cumsum(vals$df_score[vals$currentnames[i]]))
      print(prog)
    }
    
    vals$df_prog <- cbind(round = 0:(vals$round-1), prog)
    #vals$df_prog <- prog
    pmelt <- melt(vals$df_prog, id.vars="round", value.name = "score", variable.name = "players")
    output$progplot <- renderPlot({
      ggplot(pmelt, aes(x=round, y=score, color = players)) + geom_line(size = 2) + theme(axis.text=element_text(size=14))
    })
    
    if(vals$round == (vals$maxround[vals$nplayers] - 1)){
      output$nextbutton = renderUI({actionButton("btn_nextround", "Go to Final Round!")})
    }
    if(vals$round == (vals$maxround[vals$nplayers])){
      output$nextbutton = renderUI({actionButton("btn_nextround", "Finish Game")})
    }
    if(vals$round > (vals$maxround[vals$nplayers])){
      output$nextbutton = renderUI({actionButton("btn_nextround", "", width = 0)})
    }
    
    print(round)
  })

observeEvent(input$btn_end,{
  output$nextbutton <- renderUI({actionButton("btn_end", "")})
})
}

shinyApp(ui, server)

