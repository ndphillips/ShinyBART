      # ------------------------------
      #  ShinyBART
      #  Originally developed by Lejuez et al. (2002). Evaluation of a behavioral measure of Risk taking...
      #  Implimented in Shiny by Nathaniel Phillips, http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com,
      #  with help from Kevin Trutmann
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
      #   E: Game Display / Plotting Function
      #   F: Event (button) actions
      #     F1: Page navigation buttons
      #     F2: Event tracking
      #   G: Save data
      # ------------------
      
      # --------------------------
      # Section 0: Load libraries ----
      # --------------------------
      library(shiny)
      library(rdrop2)
      library(dplyr)
      library(yarrr)
      
      
      # --------------------------
      # Section A: Setup game     -----
      # --------------------------
      
      # Section A1: GAME PARAMETERS
      game.type = "lobby" # either "direct" if the opponents are chosen by ID, or "lobby" if the opponent should be found automatically.
      pop.max <- 20
      pop.min <- 1
      balloonsPerBlock <- 5
      blocks.n <- 3
      balloons.n <- balloonsPerBlock*blocks.n
      pop.v <- sample(pop.min:pop.max, size = balloons.n, replace = TRUE)
      RDrawBoundary <- TRUE # Wether to show the maximum size of the ballons
      
      # Setting up a "Database" of current players (This is only done once per app, and thus globally):
      playerDB <- data.frame(ID = character(), block = integer(), earned.hist = I(list()), connected.to = character(), stringsAsFactors = FALSE)
      
        
      # Section A2: DATA SAVING
      
      saveDataLocation <- "dropbox" # Either dropbox, email, or local
      outputDir <- "nphillips/ShinyBart/data"  # Directory to save data
      
      # Dropbox
      if(saveDataLocation == "dropbox") {
        droptoken <- readRDS("droptoken.rds")       # Reads in authentication for dropbox
      }
      
      
      # --------------------------------
      # Section B: The user Interface and its JavaScript logic to run the game (in a seperate file)----
      # -------------------------------
      ui <- fixedPage(
        
        title = "ShinyBART",
        uiOutput("MainAction"),
        tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),   # Prevents gray screen during Sys.sleep()
        
        includeScript("BalloonGame.js")
      )
      
      server <- function(input, output, session) {

        
        # --------------------------------
        # Section C: Define Reactive Values ----
        #   These store the main values in the game
        # --------------------------------
        
        # CurrentValues stores scalers representing the latest game outcomes and values
        CurrentValues <- reactiveValues(opponentid = character(1),
                                        page = "welcome",
                                        training = TRUE,
                                        errors = "none",
                                        lastClick = Sys.time(),
                                        balloon = 1,
                                        block = 0,
                                        pumps = 0,
                                        pop = 0,
                                        saveballoon = 0,
                                        points.lastblock = 0,
                                        points.cum = 0)
        
        # GameValues stores vectors of histories
        GameData <- reactiveValues(balloon = c(),
                                   balloonGlobal = c(),
                                   block = c(),          
                                   time = c(),
                                   pumps = c(),
                                   action = c(),
                                   pop = c())

        # Create a local, reactive version of the global player database and update it regularly       
        playerDB.local <- reactivePoll(2000, session, function(){playerDB}, function(){playerDB})
        
        # Send the maximum pumpvalue to the Javascript part, so it knows how big to draw the balloon:
        session$sendCustomMessage(type='maxPopHandler', c(pop.max, RDrawBoundary))
        
        # --------------------------------
        # Section D: Page Layouts ----
        # --------------------------------
        
        # Send dynamic UI to ui - DON'T CHANGE!
        output$MainAction <- renderUI( {
          PageLayouts()
        })
        
        PageLayouts <- reactive({
          
          # 1) WELCOME PAGE
          if (CurrentValues$page == "welcome") {
            
            if(CurrentValues$errors == "Name_Taken"){
              inputLabel <- p(style = "color:Red", "Error: This ID is already being used.")
              
            } else if (CurrentValues$errors == "Blank_Name") {
              inputLabel <- p(style = "color:Red", "Please enter an ID!")
            } else {
              inputLabel <- p("Please enter the ID")
            }
            
            # Show one or two inputs, depending on game.type:
            if (game.type == "lobby") {
              inputFields <- list(
                p("If you consent to participating in this study, please enter a unique ID that noone else would use."),
                textInput(inputId = "workerid", 
                                       label = inputLabel,
                                       placeholder = "e.g.; Cat57Door"))
            } else {
              inputFields <- list(
                p("If you consent to participating in this study, please enter your given ID, as well as the ID of your opponent."),
                textInput(inputId = "workerid", 
                          label = inputLabel,
                          placeholder = "Your ID here"),
                textInput(inputId = "opponentid", 
                          label = inputLabel,
                          placeholder = "Your opponents ID here")
                )
            }
            
            return(
              list(
                h1("ShinyBART"),
                p("This is a version of the Balloon Analogue Risk Task (BART) implimented in Shiny. The task was developed by Lejeuz et al. (2002) as a behavioral measure of risk taking."),
                p("This experiment has two phases:"),
                p("In Phase 1, you will play a game called 'The Balloon Game' and try to earn more points than your opponent while making risky decisions."),
                p("In Phase 2, you will complete a short questionnnaire about the game and how you make decisions in general."),
                p("Your responses will be anonymous and in a group and your individual responses will not be published."),
                inputFields,
                # This displays the action putton Next.
                actionButton(inputId = "gt_waiting",
                             label = "Connect!")
              )
            )}
  
          # First waiting Page:
          if (CurrentValues$page == "waiting_start") {
            
            return(
              list(
                h1("Looking for an opponent..."),
                tags$img(src = "spinner.gif"),
                h3("You entered the following ID:"),
                p("Your ID: ", input$workerid),
                HTML("<br>"),
                p("If you want to change it, click here:"),
                actionButton(inputId = "back_to_welcome", 
                             label = "Go Back!")
              )
            )}
          
          # 1) WELCOME PAGE
          if (CurrentValues$page == "inst1") {
            
            return(
                list(
                  h3("Opponent found!"),
                  h1("The Balloon Game"),
                  p("In Phase 1 of this study, you will play The Balloon Game."),
                  p("Here's how The Balloon Game works. You will be presented with several deflated balloons. When you see a balloon, you can pump it by pressing a 'Pump' button. If you save a balloon before it pops, you will earn points for that balloon. If you pops, you earn no points for that balloon!"),
                  p("For example, here is one deflated balloon. At the top of the screen you can see that this is balloon #1 and that you haven't pumped it."),
                  fixedRow(column(12, align ="center", tags$img(src = "Inst_Pic_start.PNG"))),
                  p("You can pump balloons by clicking the 'Pump' button below each balloon. Each time you pump a balloon, it will get a bit larger and has the potential to give you more points."),
                  p("To earn points, you need to SAVE a balloon before it pops! You can save a balloon at any time by clicking the 'Save' button. If you save a balloon, you will earn points equal to the number of pumps you put into it. You will then move on to the next balloon"),
                  p("However -- if you pump a balloon too much it might pop! If a balloon pops, you cannot save it and will not earn any points for that balloon. You will then move on to the next balloon."),
                  p(paste("You will have the opportunity to pump", balloons.n, "balloons. Different balloons have different popping points and you never know exactly when each balloon will pop. However, you know for sure that the maximum possible number of pumps for a balloon is", pop.max)),
                  h3("Examples"),
                  p("For example, someone might pump a balloon 10 times. The balloon does not pop, and the player decides to 'Save'. Because the person Saved the balloon before it popped the person would earn 10 points for that balloon."),
                  fixedRow(column(12, align ="center", tags$img(src = "Inst_Pic_saved.PNG"))),
                  p("However, someone else might pump the balloon 18 times, and then see that it pops on the 18th pump. Because the balloon popped, the person would not earn any points for this balloon"),
                  fixedRow(column(12, align ="center", tags$img(src = "Inst_Pic_popped.PNG"))),
                  p("As you can see, the more you pump a balloon the more risk you take. On the one hand, you might earn more points if the balloon does not pop. On the other hand, if the balloon does pop, you won't be able to save it and will not earn any points for that balloon."),
                  h3(paste("Maximum of", pop.max, "pumps")),
                  p(paste0("The maximum number of pumps for any balloon is ", pop.max, ". If you try to pump a balloon ", pop.max, " times, then it will surely pop!")),
                  h3(paste("There will be", balloons.n, "balloons in", blocks.n, "blocks")),
                  p(paste("You will play a total of", balloons.n, "balloons in this game. They will be presented in ", blocks.n, "groups of ", balloonsPerBlock, "balloons. You will receive feedback about your and your opponents earnings after each block.")),
                  p("Feedback will be given in the form of numbers, as well as a lineplot (an empty one is shown below) to show your progress."),
                  renderPlot({
                    plot(10, xlim = c(1, blocks.n), col = "white",
                         xlab = "Block", ylab = "Points", xaxt = "n")
                    axis(1, at = 1:blocks.n, labels = c(1:(blocks.n-1), "Final"))
                  }),
                  p(paste("Again, different balloons have different popping points. You can never know for sure what the popping point for a balloon is unless it pops.")),
                  p(paste("You can never lose points that you've earned after saving a balloon. Your goal is to earn more points than your opponent across all", balloons.n, "balloons")),
                  p("When you are ready to start, click Continue to start with the training round! After that, the real game will begin."),
                  actionButton(inputId = "gt_training",
                               label = "Start the Training Round"),            
                HTML("<br><br><br>")
              )
            )}
          
          # 3) BALLOON PAGE
          if (CurrentValues$page == "game") {
            
            # Declare the Parts that stay the same for all three game situations:
            
            # The block counter should shot "training" during the training round
            if (CurrentValues$training == F) {
              blockCounter <- column(3, align="center", p(style = "font-size:36px", paste0(CurrentValues$block, " of ", blocks.n)))
            } else {
              blockCounter <- column(3, align="center", p(style = "font-size:36px", "Training"))
            }
            
            # The names of the counters on top:
            counterHeader <- fixedRow(
              column(3, align="center", h3(tags$i("Block"))),
              column(3, align="center", h3(tags$i("Balloon"))),
              column(3, align="center", h3(tags$i("Pumps"))),
              column(3, align="center", h3(tags$i("Points")))
            )
            
            # The content of the counters on top:
            counterCounters <- fixedRow(
              blockCounter,
              column(3, align="center", p(style = "font-size:36px", paste0(CurrentValues$balloon, " of ", balloonsPerBlock))),
              column(3, align="center", p(style = "font-size:36px", id = "pumpCounter", "0")), # This is updated via JavaScript
              column(3, align="center", p(style = "font-size:36px", paste(CurrentValues$points.cum)))
            )
            
            # The canvas for the balloon:
            balloonCanvas <- fixedRow(column(12, align="center", tags$canvas(id ="balloonCanvas", width = "500", height = "300")))

                        
              # Main display while the game is in progress
              if (CurrentValues$pop == 0 & CurrentValues$saveballoon == 0) { 
    
                # Send the maximum pumps for this balloon to the JavaScript
                session$sendCustomMessage(type='maxPumpHandler', pop.v[CurrentValues$balloon])

                return(
                    list(
                      fixedRow(
                         column(12,
                                counterHeader,
                                counterCounters,
                                balloonCanvas,
                                tags$script('var jsCanvas = document.getElementById("balloonCanvas");
                                            var ctx = jsCanvas.getContext("2d");
                                            var t = new Date().getTime();
                                            jsRedrawBalloon("gray");
                                          '),
                               # Main Display: Contains both a pump and a save button
                               fixedRow(
                                 column(12, align="center", actionButton("pump", label = "Pump The Balloon", onclick = "jsPump()")),
                                 column(12, align="center", actionButton("saveballoon", label = "Save!", onclick = "jsSaveBalloon()"))

                               )
                          )
                      )
                    )
                  )
              }
              
              # Main display when the balloon popped
              else if (CurrentValues$pop == 1) {
                return(
                  list(
                    fixedRow(
                      column(12,
                             counterHeader,
                             counterCounters,
                             balloonCanvas,
                             tags$script('var c = document.getElementById("balloonCanvas");
                                         var ctx = c.getContext("2d");
                                         jsRedrawBalloon("red");
                                         jsDelayButton();
                                        '),
                             
                             fixedRow(
                               column(12, align="center", actionButton("nextballoon", label = "Go to the Next Balloon!", onclick = "jsNewBalloon()"))
                             )))
                    )
                  )
            }
              
              # Main display when the balloon is saved
              else if (CurrentValues$saveballoon == 1) {
                
                return(
                  list(
                    fixedRow(
                      column(12,
                             counterHeader,
                             counterCounters,
                             balloonCanvas,
                             tags$script('var c = document.getElementById("balloonCanvas");
                                         var ctx = c.getContext("2d");
                                         jsRedrawBalloon("green");
                                         '),
                             
                             fixedRow(
                               column(12, align="center", actionButton("nextballoon", label = "Go to the Next Balloon!", onclick = "jsNewBalloon()"))
                             )))
                    )
                  )
              }
            
          }

          # Waiting for the opponent:
          if (CurrentValues$page == "waiting_block") {
            
            if (!CurrentValues$training) {
              waitMsg      <- h3("Block ", CurrentValues$block, " of ", blocks.n, " finished!")
              feedbackMsg1 <- p("Overall you have earned ", CurrentValues$points.cum, " points.")
              feedbackMsg2 <- p("In this block you have earned ", (CurrentValues$points.cum - CurrentValues$points.lastblock), " points.") 
              
              # If its the first round, ommit the first message:
              if (CurrentValues$block == 1) {
                feedbackMsg1 <- p("")
              }
            
            # Waiting during Training:
            } else {
              waitMsg      <- h3("Training round finished!")  
              feedbackMsg1 <- p("You have earned ", CurrentValues$points.cum, " points in the training round.")
              feedbackMsg2 <- p("")
            }
            
            
            return(
              list(
                waitMsg,
                p("Waiting for your opponent to finish..."),
                tags$img(src = "spinner.gif"),
                HTML("<br><br>"),
                feedbackMsg1,
                feedbackMsg2
              )
            )}
          
          # If the opponent has disconnected, skip to the Survey:
          if (CurrentValues$page == "disconnected") {
            
            return(
              list(
                h3("Disconnected!"),
                p("It seems your opponent has disconnected from the game."),
                p("Please proceed to phase two:"),
                actionButton(inputId = "gt_surveyA", 
                             label = "Skip to Survey!")
              )
            )}

          # Feedback during Training
          if (CurrentValues$page == "feedback" && CurrentValues$training) {
            
            
            return(list(h3("Training round finished!"),
                        p("You have earned ", CurrentValues$points.cum, " points in the training."),
                        p("Your opponent has earned ", sum(playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]]), " points in the training."),
                        HTML("<br>"),
                        p("Here is a plot of your progerss so far:"),
                        feedbackPlot(),
                        p("You are now ready to start the game!"),
                        actionButton(inputId = "gt_game",
                                     label = "Start the game!",
                                     onclick = "jsNewBalloon()")
            ))
          }
          
          # Feedback during game:
          if (CurrentValues$page == "feedback" && !CurrentValues$training) {

            finMsg <- h3("Block ", CurrentValues$block, " of ", blocks.n, " finished!")
            feedbackMsg1 <- list(p("Overall you have earned ", CurrentValues$points.cum, " points."),
                                 p("Your opponent has earned ", playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][CurrentValues$block], " points."),
                                 HTML("<br>"))
            feedbackMsg2 <- list(p("In this block you have earned ", (CurrentValues$points.cum - CurrentValues$points.lastblock), " points."),
                                 p("In this block your opponent has earned ", playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][CurrentValues$block] -
                                                                              playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][CurrentValues$block - 1], " points."))
            
            # Ommit the first message if its the first block:
            if (CurrentValues$block == 1) {
              feedbackMsg1 <- p("")
            }
           
            return(list(finMsg,
                        feedbackMsg1,
                        feedbackMsg2,
                        HTML("<br>"),
                        p("Here is a plot of your progerss so far:"),
                        feedbackPlot(),
                        actionButton(inputId = "nextBlock",
                                     label = "Start the next Block!",
                                     onclick = "jsNewBalloon()")
                        ))
          }
          
          # End of game
          if (CurrentValues$page == "gameend") {
            
            if (CurrentValues$points.cum > playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][blocks.n]) {
              return(list(h3("You finished the game!"),
                        p(paste("You earned", CurrentValues$points.cum, "points in the game.")),
                        p("Your opponent earned ", playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][CurrentValues$block], " points."),
                        p("That means that you gathered more points than your opponent. Congratulations!"),
                        p("Please click continue to complete a short survey"),
                        actionButton(inputId = "gt_surveyA", 
                                     label = "Continue")
                     ))
            } else if (CurrentValues$points.cum < playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][blocks.n]) {
              return(list(h3("You finished the game!"),
                          p(paste("You earned", CurrentValues$points.cum, "points in the game.")),
                          p("Your opponent earned ", playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][CurrentValues$block], " points."),
                          p("That means that your opponent gathered more points than you."),
                          p("Please click continue to complete a short survey"),
                          actionButton(inputId = "gt_surveyA", 
                                       label = "Continue")
                     ))
            } else {
              return(list(h3("You finished the game!"),
                          p(paste("You earned", CurrentValues$points.cum, "points in the game.")),
                          p("Your opponent earned ", playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]][CurrentValues$block], " points."),
                          p("That means that your opponent and you gathered exactly the same amount of points!"),
                          p("Please click continue to complete a short survey"),
                          actionButton(inputId = "gt_surveyA", 
                                       label = "Continue")
              ))

            }
          }
          
          # Post-game Survey A
          if (CurrentValues$page == "surveyA") {
            
            # Throw an error if not all question have been answered.
            if (CurrentValues$errors == "answerQuestions") {
              answerQuestions <- p(style = "color:Red", "Please answer all required Questions!")
            } else {
              answerQuestions <- ""
            }
            
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
              
              p(answerQuestions),
              
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
        # Section E: Game Display / Plotting Function ----
        # --------------------------------
        
                
        # The function that renders the plot during the feedback
        feedbackPlot <- function() {
            # Figure out how high the plot should be:
            plotLim <- c(0, max(c(CurrentValues$points.cum, playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]])) + 1)
            
            fixedRow(column(6,{
              renderPlot({
                # Plot during the training:
                if(CurrentValues$training){
                  # Plotting the lines:
                  plot(playerDB$earned.hist[playerDB$ID == input$workerid][[1]],
                       xlim = c(1, 1), ylim = plotLim, pch = 16, col = "red",
                       xlab = "Block", ylab = "Points", xaxt = "n")
                  
                  # Plotting the points on top of the lines:
                  points(playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]], col = "blue", pch = 16)
                  # Adding a custom x-axis:
                  axis(1, at = 1, labels = "Training")
                  
                  # Plot during the game:
                } else {
                  # Plotting the lines:
                  plot(playerDB$earned.hist[playerDB$ID == input$workerid][[1]],
                       xlim = c(1, blocks.n), ylim = plotLim, type = 'l', col = "red", lwd = 2,
                       xlab = "Block", ylab = "Points", xaxt = "n")
                
                  points(playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]], type = 'l', col = "blue")
                  # Plotting the points on top of the lines:
                  points(playerDB$earned.hist[playerDB$ID == input$workerid][[1]], col = "red", pch = 16)
                  points(playerDB$earned.hist[playerDB$ID == CurrentValues$opponentid][[1]], col = "blue", pch = 16)
                  # Adding a custom x-axis:
                  axis(1, at = 1:blocks.n, labels = c(1:(blocks.n-1), "Final"))
                }
                
                # Add a legend:
                legend("bottomright", legend = c("You", "Opponent"), col = c("red", "blue"), lty = 1)
              })
            }))
        } 
      
        # --------------------------------
        # Section F: Event (e.g.; button) actions ----
        # --------------------------------
        
        # Section F1: Simple Page Navigation Buttons
        observeEvent(input$gt_training, CurrentValues$page <- "game")
        observeEvent(input$gt_surveyA, CurrentValues$page <- "surveyA")
                
        # Section F2: Event tracking
                
        # At the start, add the player to the database
        observeEvent(input$gt_waiting, {
                                        if(Sys.time() - CurrentValues$lastClick > .5){
                                          CurrentValues$lastClick <- Sys.time()
                                          
                                          #First, check wether that ID is not already in use or no ID has been given:
                                          if (input$workerid %in% playerDB$ID) {
                                            CurrentValues$errors <- "Name_Taken"
                                          } else if (input$workerid == ""){
                                            CurrentValues$errors <- "Blank_Name"
                                          } else {
                                            CurrentValues$opponentid <- input$opponentid
                                            playerDB <<- rbind(playerDB, data.frame(ID = input$workerid, block = -1, earned.hist = I(list(c())), connected.to = "waiting_for_opponent", stringsAsFactors = F))
                                            CurrentValues$page <- "waiting_start"
                                          }
                                        }
                                      })
  
        # The button to go back to the welcome page and change the IDs:
        observeEvent(input$back_to_welcome, {playerDB <<- playerDB[!playerDB$ID == input$workerid,]
                                             CurrentValues$page <- "welcome"
        })
        
        # If the opponent is found, continue to the instructions:
        # /if the opponent disconnects, display the disconnected page:
        observeEvent(playerDB.local(), {if(CurrentValues$page == "waiting_start") {
          
                                                      if (game.type == "lobby") {
                                                        # What to do if another clent has added a connection:          
                                                        if (input$workerid %in% playerDB$connected.to){
                                                          CurrentValues$opponentid <- playerDB$ID[playerDB$connected.to == input$workerid]
                                                          
                                                          CurrentValues$page <- "inst1"
                                                        # What to do if another client is found:
                                                        } else if ("waiting_for_opponent" %in% playerDB$connected.to[playerDB$ID != input$workerid]){
                                                          # First, write found opponent to own entry, then write your ID to the opponents enrty.
                                                          CurrentValues$opponentid <- playerDB$ID[playerDB$ID != input$workerid &
                                                                                                  playerDB$connected.to == "waiting_for_opponent"][1]
                                                          playerDB$connected.to[playerDB$ID == input$workerid] <<- CurrentValues$opponentid
                                                          playerDB$connected.to[playerDB$ID == CurrentValues$opponentid] <<- input$workerid
                                                          
                                                          CurrentValues$page <- "inst1"
                                                        }
                                                      } else {
                                                        
                                                        if (input$opponentid %in% playerDB$ID){
                                                          CurrentValues$page <- "inst1"
                                                        } 
                                                      }
                                                    }
                                                      
                                                      # What to do if the other person disconnected
                                                      if (CurrentValues$page %in% c("game", "waiting_block") && !CurrentValues$opponentid %in% playerDB.local()$ID) {
                                                          CurrentValues$page <- "disconnected"      
                                                      }
                                                      
        })
        
        # Reset all values after the training:
        observeEvent(input$gt_game, { CurrentValues$balloon <- 1
                                      CurrentValues$points.cum <- 0
                                      CurrentValues$block <- 1
                                      CurrentValues$training <- FALSE
                                      CurrentValues$page <- "game"
                                      
                                      GameData$balloon = c()
                                      GameData$balloonGlobal = c()
                                      GameData$block = c()        
                                      GameData$time = c()
                                      GameData$pumps = c()
                                      GameData$action = c()
                                      GameData$pop = c()
                                      
                                      playerDB$earned.hist[playerDB$ID == input$workerid] <<- I(list(c()))
        })
        

        # Game updates:
        
        # After a pop or save, start next balloon
        observeEvent(input$nextballoon, {
          if(Sys.time() - CurrentValues$lastClick > .5){
            CurrentValues$lastClick <- Sys.time()
              
              CurrentValues$pumps <- 0
              CurrentValues$pop <- 0
              CurrentValues$balloon <- CurrentValues$balloon + 1
              CurrentValues$saveballoon <- 0
          }
        })
        
        # Starting the next block after a feedback
        observeEvent(input$nextBlock, {
          if(Sys.time() - CurrentValues$lastClick > .5){
            CurrentValues$lastClick <- Sys.time()
              
            # Go to the end screen if it was the last block:
            if(CurrentValues$block >= blocks.n) {
              CurrentValues$page <- "gameend"
            } else {
              CurrentValues$pumps <- 0
              CurrentValues$pop <- 0
              CurrentValues$balloon <- 1
              CurrentValues$block <- CurrentValues$block + 1
              CurrentValues$saveballoon <- 0
              CurrentValues$points.lastblock <- CurrentValues$points.cum
              
              CurrentValues$page <- "game"
            }
          }
        })
        
        # What to do if the balloon popped:
        observeEvent(input$popped, {
       
            if (input$popped == 1) {
              
              CurrentValues$pop <- 1
              CurrentValues$pumps <- input$pumps
              
              GameData$balloon <- c(GameData$balloon, rep(CurrentValues$balloon, CurrentValues$pumps))
              GameData$balloonGlobal <- c(GameData$balloonGlobal, rep((CurrentValues$block-1)*balloonsPerBlock + CurrentValues$balloon, CurrentValues$pumps))
              GameData$block <- c(GameData$block, rep(CurrentValues$block, CurrentValues$pumps))
              GameData$time <- c(GameData$time, input$actionTimes)
              GameData$pumps <- c(GameData$pumps, 1:CurrentValues$pumps)
              GameData$action <- c(GameData$action, rep(1, CurrentValues$pumps))
              GameData$pop <- c(GameData$pop, rep(0, CurrentValues$pumps-1), 1)
              
            }
          })
    
        # saveballoon button
        observeEvent(input$saveballoon, {
          if(Sys.time() - CurrentValues$lastClick > .5){
            CurrentValues$lastClick <- Sys.time()
            
            CurrentValues$pumps <- input$pumps
            
            GameData$balloon <- c(GameData$balloon, rep(CurrentValues$balloon, CurrentValues$pumps+1))
            GameData$balloonGlobal <- c(GameData$balloonGlobal, rep((CurrentValues$block-1)*balloonsPerBlock+CurrentValues$balloon, CurrentValues$pumps+1))
            GameData$block <- c(GameData$block, rep(CurrentValues$block, CurrentValues$pumps+1))
            GameData$time <- c(GameData$time, input$actionTimes)
            GameData$pumps <- c(GameData$pumps, 1:CurrentValues$pumps, NA)
            GameData$action <- c(GameData$action, rep(1, CurrentValues$pumps), 0)
            GameData$pop <- c(GameData$pop, rep(0, CurrentValues$pumps+1))
            
            # Add points for current balloon to point total
            CurrentValues$points.cum <- CurrentValues$points.cum + CurrentValues$pumps
            CurrentValues$saveballoon <- 1
          }
        })
        
        # Look for the last balloon in a Block -> update Database and wait for opponent
        observeEvent(CurrentValues$balloon, {
          
          if(CurrentValues$balloon > balloonsPerBlock) {
            playerDB$block[playerDB$ID == input$workerid] <<- CurrentValues$block
            playerDB$earned.hist[playerDB$ID == input$workerid][[1]] <<- c(playerDB$earned.hist[playerDB$ID == input$workerid][[1]], CurrentValues$points.cum)

            CurrentValues$page <- "waiting_block"
          }
          
        })
        
        # Check wether the opponent is done with the block:
        observeEvent(playerDB.local()$block, {
          
          if (CurrentValues$page == "waiting_block" &&
              playerDB$block[playerDB$ID == CurrentValues$opponentid] == CurrentValues$block) {
              CurrentValues$page <- "feedback"
          }
          
        })        
              
    
        # --------------------------------
        # Section G: Save data ----
        # --------------------------------
          observeEvent(input$gt_goodbye, {
            
            # First, check wether all questions have been answered:
            if (any(input$sex == 99, input$age == 0, input$interesting == 99,
                input$strategy == "", input$playedbefore == 99, input$dontuse == 99)) {
              
              CurrentValues$errors <- "answerQuestions"
              
            } else {
            
              # Create progress message
              withProgress(message = "Saving data...",
                           value = 0, {
         
                             incProgress(.25)
  
                             # Write game data to a dataframe
                             GameData.i <- data.frame("workerid" = rep(input$workerid, length(GameData$balloon)),
                                                      "opponentid" = rep(CurrentValues$opponentid, length(GameData$balloon)),
                                                      "balloonGlobal" = GameData$balloonGlobal,
                                                      "balloon" = GameData$balloon,
                                                      "block" = GameData$block,
                                                      "time" = GameData$time,
                                                      "action" = GameData$action,
                                                      "pop" = GameData$pop)
  
                             # Write survey data to a dataframe
                             SurveyData.i <- data.frame("workerid" = input$workerid,
                                                        "opponentid" = CurrentValues$opponentid,
                                                        "sex" = input$sex,
                                                        "age" = input$age,
                                                        "interesting" = input$interesting,
                                                        "strategy" = input$strategy,
                                                        "playedbefore" = input$playedbefore,
                                                        "dontuse" = input$dontuse,
                                                        "comments" = input$comments)
  
  
                             incProgress(.5)
  
                             GameDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(GameData.i), "_g.csv")
                             SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_s.csv")
  
                             # Create The Filepath and save the data depending on the method chosen:
                             if (saveDataLocation == "dropbox") {
                                GameDatafilePath <- file.path(tempdir(), GameDatafileName)
                                write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
                                rdrop2::drop_upload(GameDatafilePath, dest = outputDir, dtoken = droptoken)
  
                                SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                                write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                                rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = droptoken)
  
                             } else if (saveDataLocation == "local") {
                                GameDatafilePath <- file.path(outputDir, GameDatafileName)
                                write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
  
                                SurveyDatafilePath <- file.path(outputDir, SurveyDatafileName)
                                write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                             }
  
  
                             incProgress(.40)
  
                              # Some interesting plots (not necessary)
  
                             output$EarningsPlot <- renderPlot({
  
                               balloon.agg <- GameData.i %>% group_by(balloonGlobal) %>%
                                 summarise(
                                   pop = max(pop),
                                   pumps = sum(action == 1)
                                 )
  
                               my.cols <- yarrr::piratepal("xmen", trans = .3, length.out = balloons.n)
  
                               plot(1, xlim = c(1, balloons.n), ylim = c(0, pop.max),
                                    xlab = "Balloon", ylab = "Pumps", main = "Your Balloons",
                                    yaxt = "n",
                                    type = "n", xaxt = "n")
  
                               grid()
  
                               axis(1, at = 1:balloons.n)
                               axis(2, at = seq(0, pop.max, 5), las = 1, lwd = 0)
  
  
                               # Add strings
                               segments(x0 = balloon.agg$balloonGlobal[balloon.agg$pop == 0] + runif(balloons.n[balloon.agg$pop == 0], -.09, .09),
                                        y0 = balloon.agg$pumps[balloon.agg$pop == 0] - 3,
                                        x1 = balloon.agg$balloonGlobal[balloon.agg$pop == 0],
                                        y1 = balloon.agg$pumps[balloon.agg$pop == 0] - 1,
                                        lwd = 1)
  
                               with(subset(balloon.agg, pop == 1), points(x = balloonGlobal, y = pumps, col = "black", pch = 8, cex = pumps / 2))
                               with(subset(balloon.agg, pop == 0), points(x = balloonGlobal, y = pumps, bg = my.cols, col = "white", pch = 21, cex = pumps / 2, lwd = 2))
                               with(subset(balloon.agg, pop == 0), text(x = balloonGlobal, y = pumps + 2, "Saved!", pos = 3))
  
                              })
                             
                             CurrentValues$page <- "goodbye"
                             Sys.sleep(.25)
                             incProgress(1)
                  })
              }
           })
    
        # Delete the player from the database when their session ends.
        session$onSessionEnded(function() {isolate(playerDB <<- playerDB[!playerDB$ID == input$workerid,])})
      }
      
    # Create app!
    shinyApp(ui = ui, server = server)

      