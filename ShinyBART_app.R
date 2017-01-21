# ------------------------------
#  ShinyBART
#  Originally developed by Lejuez et al. (2002). Evaluation of a behavioral measure of Risk taking...
#  Implimented in Shiny by Nathaniel Phillips, http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com
#
#   CODE SECTIONS
#
#   0: Load libraries
#   A: Setup Game
#     A1: Game parameters
#     A2: Data saving
#   B: Overall layout
#   C: Reactive values
#   D: Page layouts
#   E: Game display
#     E1: Example game displays (for instructions)
#   F: Event (button) actions
#     F1: Page navigation buttons
#     F2: Event tracking
#   G: Save data
# ------------------

# --------------------------
# Section 0: Load libraries
# --------------------------
library(shiny)
library(rdrop2)
library(dplyr)
library(yarrr)


# --------------------------
# Section A: Setup game
# --------------------------

# Section A1: GAME PARAMETERS

pop.max <- 20
pop.min <- 1
balloons.n <- 10
pop.v <- sample(pop.min:pop.max, size = balloons.n, replace = TRUE)

# Section A2: DATA SAVING

saveDataLocation <- "dropbox"           # Either dropbox, email, or local
outputDir <- "nphillips/BART_A/data"  # Directory to save data

# Dropbox
if(saveDataLocation == "dropbox") {
  
  droptoken <- readRDS("droptoken.rds")        # Reads in authentication for dropbox
  
}



# --------------------------------
# Section B: Define overall layout
# -------------------------------

ui <- fixedPage(
  
  title = "ShinyBART",
  uiOutput("MainAction"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # Prevents gray screen during Sys.sleep()
  
)


server <- function(input, output, session) {
  

# --------------------------------
# Section C: Define Reactive Values
#   These store the main values in the game
# --------------------------------
  
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
  
# --------------------------------
# Section D: Page Layouts
# --------------------------------

# Send dynamic UI to ui - DON'T CHANGE!
output$MainAction <- renderUI( {
  PageLayouts()
})

PageLayouts <- reactive({
    
    # 1) WELCOME PAGE
    if (CurrentValues$page == "welcome") {
      
      return(
        list(
          h1("ShinyBART"),
          p("This is a version of the Balloon Analogue Risk Task (BART) implimented in Shiny. The task was developed by Lejeuz et al. (2002) as a behavioral measure of risk taking."),
          p("This experiment has two phases:"),
          p("In Phase 1, you will play a game called 'The Balloon Game' and try to earn points while making risky decisions."),
          p("In Phase 2, you will complete a short questionnnaire about the game and how you make decisions in general."),
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
        p("Here's how The Balloon Game works. You will be presented with several deflated balloons. When you see a balloon, you can pump it by pressing a 'Pump' button. If you save a balloon before it pops, you will earn points for that balloon. If you pops, you earn no points for that balloon!"),
        p("For example, here is one deflated balloon. At the top of the screen you can see that this is balloon #1 and that you haven't pumped it."),
        plotOutput(outputId = "ss0np"),
        p("You can pump balloons by clicking the 'Pump' button below each balloon. Each time you pump a balloon, it will get a bit larger and has the potential to give you more points."),
        p("To earn points, you need to SAVE a balloon before it pops! You can save a balloon at any time by clicking the 'Save' button. If you save a balloon, you will earn points equal to the number of pumps you put into it. You will then move on to the next balloon"),
        p("However -- if you pump a balloon too much it might pop! If a balloon pops, you cannot save it and will not earn any points for that balloon. You will then move on to the next balloon."),
        p(paste("You will have the opportunity to pump", balloons.n, "balloons. Different balloons have different popping points and you never know exactly when each balloon will pop. However, you know for sure that the maximum possible number of pumps for a balloon is", pop.max)),
        h3("Examples"),
        p("For example, someone might pump a balloon 10 times. The balloon does not pop, and the player decides to 'Save'. Because the person Saved the balloon before it popped the person would earn 10 points for that balloon."),
        plotOutput(outputId = "ss10np"),
        p("However, someone else might pump the balloon 18 times, and then see that it pops on the 18th pump. Because the balloon popped, the person would not earn any points for this balloon"),
        plotOutput(outputId = "ss18p"),
        p("As you can see, the more you pump a balloon the more risk you take. On the one hand, you might earn more points if the balloon does not pop. On the other hand, if the balloon does pop, you won't be ablew to save it and will not earn any points for that balloon."),
        h3(paste("Maximum of", pop.max, "pumps")),
        p(paste0("The maximum number of pumps for any balloon is ", pop.max, ". If you try to pump a balloon ", pop.max, " times, then it will surely pop!")),
        h3(paste("There will be", balloons.n, "balloons")),
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
                     selected = 99),
        
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
                     ), selected = 99),
        
        textAreaInput(inputId = "strategy",
                      label = "What was your strategy in the Balloon Game?",
                      placeholder = "I tried to...", resize = "both"),
        
        radioButtons("playedbefore", 
                     label = "Have you played the balloon game before?",
                     choices = c("No, I have not played it." =  1,
                                 "Yes, I have played it." = 2,
                                 "I don't know" = 3
                     ), selected = 99),
        
        radioButtons("dontuse",
                     label = "Is there any reason why we should NOT use your data for scientific research? For example, were you not paying attention or were you intoxicated?",
                     choices = c("No. My data should be valid for scientific research" =  "0",
                                 "Yes. There is a good reason why you should NOT use my data for scientific research" = 1
                     ), selected = 99),
        
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
        p("Here is your randomly generated study completion code. Please write it down as a confirmation of your participation"),
        h3(completion.code),
        h3("What was this study about?"),
        p("The purpose of this study is measure how much risk people are willing to take when making decisions. In this game, the more pumps you take, the more risk you are willing to take. In the plot below, you can see the outcomes of your game"),
        plotOutput(outputId = "EarningsPlot"),
        h3("ShinyPsych"),
        p("This task was written as part of ShinyPsych, a collection of psychology experiments programmed in Shiny. To learn more about ShinyPsych, go to https://ndphillips.github.io/ShinyPsych.html which contains links to the source code."),
        tags$img(src = "thankyou.jpg", width = "300px")
      ))
    }
    
  })
  

# --------------------------------
# Section E: Game Display
# --------------------------------

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
           cex = (max.pumps + 1), 
           pch = 1, col = gray(.3))
  }
  
  if(pop == FALSE) {
  
  points(x = .5, 
         y = .5, 
         cex = (pumps + 1), 
         pch = 21, 
         bg = balloon.col, 
         col = "black")
    
  }
  
  if(pop == TRUE) {
    
    points(x = .5, 
           y = .5, 
           cex = (pumps + 1), 
           pch = 21, 
           bg = pop.col, 
           col = "black")
    
    text(.5, .9, labels = paste("POPPED at pump", pumps), 
         cex = 2, font = 3)

    
     text(.7, .5, labels = paste("No Points"), cex = 2, font = 3)
    

    
  }
  
  if(saveballoon == TRUE) {
    
    points(x = .5, 
           y = .5, 
           cex = (pumps + 1), 
           pch = 21, 
           bg = saveballoon.col, 
           col = "black")
    
    text(.5, .9, labels = paste("Saved at pump", pumps), 
          cex = 2, font = 3)
    
    text(.7, .5, labels = paste("+", pumps, "Points"), cex = 2, font = 3)
    
    
  }
  
}

# Section E1: Example game displays

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

output$ss18p <- renderPlot({
  
  bart.display(balloon = 5,
               balloons.n = balloons.n,
               points.cum = 0,
               max.pumps = pop.max,
               pumps = 18,
               pop = TRUE)
  
 # text(.40, .9, labels = "This person popped the\nBalloon at 40 Pumps", cex = 1.5)
 # arrows(.65, .85, .6, .7, lty = 1, lwd = 1.5, col = gray(.2))
  
})

# --------------------------------
# Section F: Event (e.g.; button) actions
# --------------------------------
  
# Section F1: Page Navigation Buttons
observeEvent(input$gt_inst1, {CurrentValues$page <- "inst1"})
observeEvent(input$gt_game, {CurrentValues$page <- "game"})
observeEvent(input$gt_surveyA, {CurrentValues$page <- "surveyA"})
observeEvent(input$gt_goodbye, {CurrentValues$page <- "goodbye"})
  
# Section F2: Event tracking buttons

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

# --------------------------------
# Section G: Save data
# --------------------------------
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
                                     dtoken = droptoken)
                 
                 # # Write survey data 
                 # SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_s.csv")
                 # SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                 # write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                 # rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = EPtoken)
                 # 
                 incProgress(.40)
                 
    # Some interesting plots (not necessary)
  
     output$EarningsPlot <- renderPlot({
       
       balloon.agg <- GameData.i %>% group_by(balloon) %>%
         summarise(
           pop = max(pop),
           pumps = sum(action == 1)
         )
       
       # balloon.agg <- data.frame(balloon = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
       #                           pop = sample(c(0, 1), size = 10, replace = TRUE),
       #                           pumps = runif(10, 1, 50))
       
       my.cols <- yarrr::piratepal("xmen", trans = .3, length.out = balloons.n)
       
       plot(1, xlim = c(1, balloons.n), ylim = c(0, pop.max), 
            xlab = "Balloon", ylab = "Pumps", main = "Your Balloons", 
            yaxt = "n",
            type = "n", xaxt = "n")
       
       grid()
       
       axis(1, at = 1:balloons.n)
       axis(2, at = seq(0, pop.max, 5), las = 1, lwd = 0)

       
       # Add strings
       segments(x0 = balloon.agg$balloon[balloon.agg$pop == 0] + runif(balloons.n[balloon.agg$pop == 0], -.09, .09), 
                y0 = balloon.agg$pumps[balloon.agg$pop == 0] - 3, 
                x1 = balloon.agg$balloon[balloon.agg$pop == 0], 
                y1 = balloon.agg$pumps[balloon.agg$pop == 0] - 1,
                lwd = 1)
       
       with(subset(balloon.agg, pop == 1), points(x = balloon, y = pumps, col = "black", pch = 8, cex = pumps / 2))
       with(subset(balloon.agg, pop == 0), points(x = balloon, y = pumps, bg = my.cols, col = "white", pch = 21, cex = pumps / 2, lwd = 2))
       with(subset(balloon.agg, pop == 0), text(x = balloon, y = pumps + 2, "Saved!", pos = 3))
       
     }) 
                 
                 CurrentValues$page <- "goodbye"
                 Sys.sleep(.25)
                 incProgress(1)
                 
               })
  
})
  
}

# Create app!
shinyApp(ui = ui, server = server)