# ------------------------------
#  BART TASK
#  Originally developed by Lejuez et al. (2002). Evaluation of a behavioral measure of Risk taking...
#  Implimented in Shiny by Nathaniel Phillips, http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com
# ------------------------------

# --------------------------
# Load Libraries
# --------------------------
library(shiny)
library(rdrop2)

# --------------------------
# Dropbox Parameters
# --------------------------

# You must have this file saved in your working directory
EPtoken <- readRDS("EP_droptoken.rds")          # Reads in authentication for EP dropbox
outputDir <- "nphillips/BART_A/data"            # Determine dropbox output folder

# --------------------------
# Game Parameters
# --------------------------
pop.max <- 50
pop.min <- 1
balloons.n <- 10
pop.v <- sample(pop.min:pop.max, size = balloons.n, replace = TRUE)

# --------------------------
# User Interface
# --------------------------

ui <- fixedPage(
  
  title = "The Balloon Game",
  uiOutput("MainAction"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # Prevents gray screen during Sys.sleep()
  
)

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {
  

# --------
# Set up reactive values
# These are objects that will change over the course of the game
# ---------
  
# CurrentValues stores scalers representing the latest game outcomes
CurrentValues <- reactiveValues(page = "welcome",
                                balloon = 1,
                                pumps = 0,
                                pop = 0,
                                saveballoon = 0,
                                points.cum = 0)

# GameValues stores vectors of histories
GameData <- reactiveValues(balloon = c(),          
                           time = c(),
                           pumps = c(),
                           action = c(),
                           pop = c())
  
# --------
# PageLayouts
# ---------

# Send dynamic UI to ui - DON'T CHANGE!
output$MainAction <- renderUI( {
  PageLayouts()
})

PageLayouts <- reactive({
    
    # 1) WELCOME PAGE
    if (CurrentValues$page == "welcome") {
      
      return(
        list(
          h1("Study Description"),
          p("This study will take place in two phases and should take between 5 and 10 minutes to complete."),
          p("In Phase 1, you will play a game called 'The Balloon Game' and try to earn points while making risky decisions."),
          p("In Phase 2, you will complete a few short questionnnaires about the game and how you make decisions in general."),
          p("There are no health risks or personal identifying information associated with your participation."),
          p("This study is founded by the chair of the department of Economic Psychology at the University of Basel."),
          p("Your responses will be anonymous and in a group and your individual responses will not be published."),
          p("If you consent to participating in this study, please enter a unique ID that no one else would use and click Continue."),
          textInput(inputId = "workerid", 
                    label = "Please enter a unique ID that no one else would use", 
                    value = "", 
                    placeholder = "e.g.; Cat57Door"),
          # This displays the action putton Next.
          actionButton(inputId = "gt_inst1", 
                       label = "Continue to Phase 1") 
        )
      )}
  
  
  # 1) WELCOME PAGE
  if (CurrentValues$page == "inst1") {
    
    return(
      list(
        h1("The Balloon Game"),
        p("In Phase 1 of this study, you will play The Balloon Game."),
        p("Here's how The Balloon Game works. You will be presented with several deflated balloons. When you see a balloon, you can pump it by pressing a 'Pump' button. If you save a balloon before it pops, you will earn points. If you pops, you earn no points!"),
        p("For example, here is one deflated balloon. At the top of the screen you can see that this is balloon #1 and that you haven't pumped it."),
        plotOutput(outputId = "ss0np"),
        p("You can pump balloons by clicking the 'Pump' button below each balloon. Each time you pump a balloon, it will get a bit larger and has the potential to give you more points."),
        p("To earn points, you need to SAVE a balloon before it pops! You can save a balloon at any time by clicking the 'Save' button. If you save a balloon, you will earn points equal to the number of pumps you put into it. You will then move on to the next balloon"),
        p("However -- if you pump a balloon too much it might pop! If a balloon pops, you cannot save it and will not earn any points for that balloon. You will then move on to the next balloon."),
        p(paste("You will have the opportunity to pump", balloons.n, "balloons. Different balloons have different popping points and you never know exactly when each balloon will pop. However, you know for sure that the maximum possible number of pumps for a balloon is 100.")),
        h2("Examples"),
        p("For example, someone might pump a balloon 10 times. The balloon does not pop, and the player decides to 'Save'. Because the person Saved the balloon before it popped the person would earn 10 points for that balloon."),
        plotOutput(outputId = "ss10np"),
        p("However, someone else might pump the balloon 40 times, and then see that it pops on the 40th pump. Because the balloon popped, the person would not earn any points for this balloon"),
        plotOutput(outputId = "ss40p"),
        p("As you can see, the more you pump a balloon the more risk you take. On the one hand, you might earn more points if the balloon does not pop. On the other hand, if the balloon does pop, you won't be ablew to save it and will not earn any points for that balloon."),
        h2(paste("There will be", balloons.n, "balloons")),
        p(paste("You will play a total of", balloons.n, "balloons in this game. Again, different balloons have different popping points. You can never know for sure what the popping point for a balloon is unless it pops.")),
        p(paste("You can never lose points that you've earned after saving a balloon. Your goal is to earn as many points as you can across all", balloons.n, "balloons")),
        p("When you are ready to start, click Continue to start the first balloon!"),
        actionButton(inputId = "gt_game", 
                     label = "Continue") 
      )
    )}
  
    
    # 3) BALLOON PAGE
    if (CurrentValues$page == "game") {
      
      if(CurrentValues$pop == 0 & CurrentValues$saveballoon == 0) {
        
        return(
          list(
            # Main Display: Contains both a pump and a save button
            fixedRow(
              column(12, 
                     plotOutput('BalloonDisplay'),
                     # Buttons
                     fixedRow(
                       column(5,
                              actionButton("pump", label = "Pump The Balloon")),
                       column(8,
                              actionButton("saveballoon", label = "Save!"))
                     )))
          )
        )
        
      }
      
      if(CurrentValues$pop == 1) {
        
        return(
          list(
            # Pop Display: Only contains a "Start next balloon" button
            fixedRow(
              column(12,
                     plotOutput('PopDisplay'),
                     # Buttons
                     fixedRow(
                       column(1, offset = 5,
                              actionButton("nextballoon", 
                                           label = "Start Next Balloon"))
                     )))
          )
        )
      }
      
      if(CurrentValues$pop == 0 & CurrentValues$saveballoon == 1) {
        
        return(
          list(
            # Pop Display: Only contains a "Start next balloon" button
            fixedRow(
              column(12,
                     plotOutput('SaveDisplay'),
                     # Buttons
                     fixedRow(
                       column(1, offset = 5,
                              actionButton("nextballoon", 
                                           label = "Start Next Balloon"))
                     )))
          )
        )
      }
      
      
    }
    
    # End of game
    if (CurrentValues$page == "gameend") {
      
      return(list(h3("You finished the game!"),
                  p(paste("You earned", CurrentValues$points, "points in the game.")),
                  p("Please click continue to complete a short survey"),
                  actionButton(inputId = "gt_surveyA", 
                               label = "Continue")))
    }
    
    # Post-game Survey A
    if (CurrentValues$page == "surveyA") {
      
      return(list(
        
        h3("Survey (1 of 1)"),
        
        radioButtons("sex",
                     label = "What is your sex?",
                     choices = list("Male" = 1, "Female" = 2, "Other" = 3),
                     selected = 1),
        
        numericInput("age", 
                     label = "What is your age?",
                     value = 0),
        
        radioButtons("interesting", 
                     label = "How interesting did you find the Balloon Game?",
                     choices = c("1 - Not at all" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - Very Much" = 5
                     )),
        
        textAreaInput(inputId = "strategy",
                      label = "What was your strategy in the Balloon Game?",
                      placeholder = "I tried to...", resize = "both"),
        
        radioButtons("playedbefore", 
                     label = "Have you played the balloon game before?",
                     choices = c("No, I have not played it." =  1,
                                 "Yes, I have played it." = 2,
                                 "I don't know" = 3
                     )),
        
        radioButtons("dontuse",
                     label = "Is there any reason why we should NOT use your data for scientific research? For example, were you not paying attention or were you intoxicated?",
                     choices = c("No. My data should be valid for scientific research" =  "0",
                                 "Yes. There is a good reason why you should NOT use my data for scientific research" = 1
                     )),
        
        textAreaInput("comments",
                      label = "If you have any additional comments, please enter them below",
                      resize = "both"),
        
        actionButton(inputId = "gt_goodbye",
                     label = "Save Data and End Study"))
      )
    }
    
    # 6) Goodbye
    if (CurrentValues$page == "goodbye") {
      
      # CALCULATE COMPLETION CODE  
      completion.code <- paste0("EP-BANDITA-", sample(100:999, size = 1), "-", sample(100:999, size = 1), "-", sample(100:999, size = 1))
      
      return(list(
        h3("Thank you for your participation!"),
        p("Here is your study completion code. Please write it down as a confirmation of your participation"),
        h2(completion.code),
        h3("What was this research about?"),
        p("The purpose of this research is to try and understand how people learn and make decisions about risky options. The game you played is known as the Balloon Analogue Risk Task (BART). It is designed to measure how much risk people are willing to take when making decisions."),
        p("If you have any questions about this study, you may contact us at EconPsychBasel@gmail.com. If you do contact us, please include your study completion code."),
        p("You may close this window :)"),
        tags$img(src = "thankyou.jpg", width = "300px")
      ))
    }
    
  })
  

# -------------------
# Bart game display
# -------------------

bart.display <- function(balloon, 
                         pumps, 
                         points.cum = 0,
                         pop = FALSE,
                         saveballoon = FALSE,
                         balloons.n = 100,
                         max.pumps = 100,
                         show.max = TRUE,
                         balloon.col = gray(.5, .5),
                         pop.col = "indianred1",
                         saveballoon.col = "green3") {
  
  # Plotting space
  par(mar = c(0, 0, 0, 0))
  layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), heights = c(4, 7), widths = c(9))
  
  # Title row
  plot.new()
  text(c(.1, .5, .9), c(.7, .7, .7), labels = c("Balloon", "Pumps", "Points"), cex = 2, font = 3)
  text(c(.1, .5, .9), c(.4, .4, .4), labels = c(paste(balloon, "of", balloons.n), pumps, points.cum), cex = 3)
  
  # text(c(.1, .5, .9), c(.5, .5, .5), labels = c(paste(balloon, "of", balloons.n), pumps, points.cum), cex = 3)

  
  par(mar = c(0, 0, 1, 0))
  # Balloon display
  plot(1, xlim = c(0, 1), ylim = c(0, 1), 
       type = "n", xaxt = "n", yaxt = "n", 
       bty = "n", xlab = "", ylab = "")
  
  if(show.max) {
    
    points(x = .5, y = .5, 
           cex = (max.pumps + 1) / 2.5, 
           pch = 1, col = gray(.3))
  }
  
  if(pop == FALSE) {
  
  points(x = .5, 
         y = .5, 
         cex = (pumps + 1) / 2.5, 
         pch = 21, 
         bg = balloon.col, 
         col = "black")
    
  }
  
  if(pop == TRUE) {
    
    points(x = .5, 
           y = .5, 
           cex = (pumps + 1) / 2.5, 
           pch = 21, 
           bg = pop.col, 
           col = "black")
    
    mtext(text = "POP!", 
         cex = 5, side = 3, font = 3)
    
    # segments(.65, .40, .85, .4)
    # segments(.65, .5, .85, .5)
    
    text(.8, .6, labels = paste("OUTCOME\nThe balloon popped at pump", pumps, "\nYou earned 0 points for this balloon"), cex = 1.5, font = 3)
    
  }
  
  if(saveballoon == TRUE) {
    
    points(x = .5, 
           y = .5, 
           cex = (pumps + 1) / 2.5, 
           pch = 21, 
           bg = saveballoon.col, 
           col = "black")
    
    
    text(.8, .6, labels = paste("OUTCOME\nYou saved after", pumps, "pumps\nYou earned", pumps, "points for this balloon"), cex = 1.5, font = 3)
    
    
  }
  
}


output$ss0np <- renderPlot({
  
  bart.display(balloon = 1,
               balloons.n = balloons.n,
               pumps = 0,
               points.cum = 0,
               max.pumps = pop.max,
               pop = FALSE)
  
  text(.7, .85, labels = "Deflated\nBalloon\n(0 Pumps)", cex = 1.5, font = 3)
  arrows(.65, .85, .51, .51, lty = 1, lwd = 1.5, col = gray(.2))
  
})

output$ss10np <- renderPlot({
  
  bart.display(balloon = 1,
               balloons.n = balloons.n,
               pumps = 10,
               points.cum = 0,
               max.pumps = pop.max,
               pop = FALSE,
               saveballoon = TRUE)
  
  
  #text(.40, .9, labels = "This person saveballoonped after 10 pumps", cex = 1.5)
#  arrows(.65, .85, .52, .55, lty = 1, lwd = 1.5, col = gray(.2))
  
})

output$ss40p <- renderPlot({
  
  bart.display(balloon = 5,
               balloons.n = balloons.n,
               points.cum = 0,
               max.pumps = pop.max,
               pumps = 40,
               pop = TRUE)
  
 # text(.40, .9, labels = "This person popped the\nBalloon at 40 Pumps", cex = 1.5)
 # arrows(.65, .85, .6, .7, lty = 1, lwd = 1.5, col = gray(.2))
  
})

# -------------------
# Define buttons 
# -------------------
  
# Starting game display
observeEvent({input$gt_game}, {
  
  output$BalloonDisplay <- renderPlot({
    
    bart.display(balloon = CurrentValues$balloon,
                 pumps = CurrentValues$pumps,
                 balloons.n = balloons.n,
                 max.pumps = pop.max,
                 points.cum = CurrentValues$points.cum,
                 pop = FALSE)
    
  })
})

# Pump button
observeEvent({input$pump}, {
  
  # Balloon does NOT Pop
  if(CurrentValues$pumps < pop.v[CurrentValues$balloon]) {
    
    CurrentValues$pumps <- CurrentValues$pumps + 1
    
    GameData$balloon <- c(GameData$balloon, CurrentValues$balloon)
    GameData$time <- c(GameData$time, proc.time()[3])
    GameData$pumps <- c(GameData$pumps, CurrentValues$pumps)
    GameData$action <- c(GameData$action, 1)
    GameData$pop <- c(GameData$pop, 0)
    
    # No pop display
    output$BalloonDisplay <- renderPlot({
      
      bart.display(balloon = CurrentValues$balloon,
                   pump = CurrentValues$pumps,
                   balloons.n = balloons.n,
                   max.pumps = pop.max,
                   points.cum = CurrentValues$points.cum,
                   pop = FALSE,
                   saveballoon = FALSE)
      
    })
    
  }
  
  # Balloon DOES Pop
  if(CurrentValues$pumps >= pop.v[CurrentValues$balloon]) {
    
    # Pop Display
    output$PopDisplay <- renderPlot({
      
      bart.display(balloon = CurrentValues$balloon,
                   pump = CurrentValues$pumps,
                   points.cum = CurrentValues$points.cum,
                   balloons.n = balloons.n,
                   max.pumps = pop.max,
                   pop = TRUE,
                   saveballoon = FALSE)
      
    })
    
    CurrentValues$pop <- 1
    
    GameData$balloon <- c(GameData$balloon, CurrentValues$balloon)
    GameData$time <- c(GameData$time, proc.time()[3])
    GameData$pumps <- c(GameData$pumps, CurrentValues$pumps)
    GameData$action <- c(GameData$action, 1)
    GameData$pop <- c(GameData$pop, 1)
    
  } 
  
  
})

# saveballoon button
observeEvent(input$saveballoon, {
  
  # Pop Display
  output$SaveDisplay <- renderPlot({
    
    bart.display(balloon = CurrentValues$balloon,
                 pump = CurrentValues$pumps,
                 balloons.n = balloons.n,
                 max.pumps = pop.max,
                 points.cum = CurrentValues$points.cum,
                 pop = FALSE,
                 saveballoon = TRUE)
    
  })
  
  GameData$balloon <- c(GameData$balloon, CurrentValues$balloon)
  GameData$time <- c(GameData$time, proc.time()[3])
  GameData$pumps <- c(GameData$pumps, NA)
  GameData$action <- c(GameData$action, 0)
  GameData$pop <- c(GameData$pop, 0)
  
  # Add points for current balloon to point total
  CurrentValues$points.cum <- CurrentValues$points.cum + CurrentValues$pumps
  CurrentValues$saveballoon <- 1

})

# After a pop, start next balloon
observeEvent({input$nextballoon}, {
    
    CurrentValues$pumps <- 0
    CurrentValues$pop <- 0
    CurrentValues$balloon <- CurrentValues$balloon + 1
    CurrentValues$saveballoon <- 0
    
  })

# Look for final balloon -> Go to gameend
observeEvent({CurrentValues$balloon}, {
  
  if(CurrentValues$balloon > balloons.n) {
    
    CurrentValues$page <- "gameend"
    
  }
  
})
  
# Page navigation buttons
observeEvent(input$gt_inst1, {CurrentValues$page <- "inst1"})
observeEvent(input$gt_game, {CurrentValues$page <- "game"})
observeEvent(input$gt_surveyA, {CurrentValues$page <- "surveyA"})
observeEvent(input$gt_goodbye, {CurrentValues$page <- "goodbye"})
  
# Saving data
observeEvent(input$gt_goodbye, {
  
  # Create progress message   
  withProgress(message = "Saving data...", 
               value = 0, {
                 
                 incProgress(.25)
                 
                 # Write GameData to a dataframe
                 GameData.i <- data.frame("balloon" = GameData$balloon,
                                          "time" = GameData$time,
                                          "action" = GameData$action, 
                                          "pop" = GameData$pop)
                 
                 
                 incProgress(.5)
                 
                 GameDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(GameData.i), "_g.csv")
                 GameDatafilePath <- file.path(tempdir(), GameDatafileName)
                 write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
                 rdrop2::drop_upload(GameDatafilePath, 
                                     dest = outputDir, 
                                     dtoken = EPtoken)
                 
                 # # Write survey data 
                 # SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_s.csv")
                 # SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                 # write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                 # rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = EPtoken)
                 # 
                 incProgress(.40)
                 
                 # Some interesting plots (not necessary)
                 
                 output$GameData.tbl <- renderTable(GameData.i)         
                 output$earnings <- renderPlot({
                   
                   plot(x = GameData.i$trial, 
                        y = cumsum(GameData.i$outcome), 
                        type = "b", 
                        xlab = "Trial", 
                        ylab = "Points")
                   
                 }) 
                 
                 CurrentValues$page <- "goodbye"
                 Sys.sleep(.25)
                 incProgress(1)
                 
               })
  
})
  
}

# Create app!
shinyApp(ui = ui, server = server)