# to run
library(shiny)
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
                   textInput("name1", "Player 1"),
                   textInput("name2", "Player 2"),
                   conditionalPanel(
                     condition = "input.numplayer >= 3",
                     textInput("name3", "Player 3")
                   ),
                   conditionalPanel(
                     condition = "input.numplayer >= 4",
                     textInput("name4", "Player 4")
                   ),
                   conditionalPanel(
                     condition = "input.numplayer >= 5",
                     textInput("name5", "Player 5")
                   ),
                   conditionalPanel(
                     condition = "input.numplayer >= 6",
                     textInput("name6", "Player 6")
                   ),
                   actionButton("btn_start", "Start New Game")
                 )
        ),
    mainPanel(
      textOutput("currentround"),
      textOutput("currentdealer"),
      uiOutput("nextbutton")
    
      
      
      )
      
    )
    
    
  )
  

server <- function(input,output){
  vals <- reactiveValues(
                         maxround = c(1,20,20,15,12,10)
                         )
  
  observeEvent(input$btn_start,{
    vals$names <- c(input$name1,
                    input$name2,
                    input$name3,
                    input$name4,
                    input$name5,
                    input$name6)
    vals$nplayers <- input$numplayer
    vals$round <- 1
    output$final <- renderText({1})
    #initialize dataframes for guess and actual
    
    vals$df_guess <- data.frame(matrix(ncol = vals$nplayers, nrow=0))
    colnames(vals$df_guess) <- vals$names[1:vals$nplayers]
    
    print(rbind(vals$df_guess, c(0,0,0,0)))
    print(vals$names[1:vals$nplayers])
    print(vals$nplayers)
    
    output$currentround = renderText({paste0("The Current Round is ", vals$round)})
    output$currentdealer = renderText({paste0("The Current Dealer is ", vals$names[(vals$round-1) %% vals$nplayers +1])})
    #guess values
    #actual values
    output$nextbutton = renderUI({actionButton("btn_nextround", "Go to Next Round")})
    
    #plots
    #scores table
  }
  )
  observeEvent(input$btn_nextround,{

    vals$round = vals$round + 1
    if(vals$round == (vals$maxround[vals$nplayers] - 1)){
      output$nextbutton = renderUI({actionButton("btn_lastround", "Go to Final Round!")})
    }
    print(round)
  })
  
  observeEvent(input$btn_lastround,{
    
    vals$round = vals$round + 1
    output$nextbutton = renderUI({actionButton("btn_end", "Finish Game")})
  })

observeEvent(input$btn_end,{
   output$final <- renderText({0})
})
}
shinyApp(ui, server)

