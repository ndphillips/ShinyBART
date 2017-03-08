    # ------------------------------
    #  ShinyBART
    #  Originally developed by Lejuez et al. (2002). Evaluation of a behavioral measure of Risk taking...
    #  Implimented in Shiny by Nathaniel Phillips, http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com
    #  JavaScript added by Kevin Trutmann, k.trutmann@unibas.ch
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
    #   E: Event (button) actions
    #     F1: Page navigation buttons
    #     F2: Event tracking
    #   F: Save data
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
    
    pop.max <- 20
    pop.min <- 1
    balloons.n <- 10
    pop.v <- sample(pop.min:pop.max, size = balloons.n, replace = TRUE)
    RDrawBoundary <- TRUE # Wether to show the maximum size of the ballons (written in lowercase for JavaScript)
    
    # Section A2: DATA SAVING
    
    saveDataLocation <- "local"           # Either dropbox, email, or local
    outputDir <- getwd()  # Directory to save data
    
    # Dropbox
    if(saveDataLocation == "dropbox") {
      
      droptoken <- readRDS("droptoken.rds")        # Reads in authentication for dropbox
    }
    
    
    # --------------------------------
    # Section B: The user Interface and its JavaScript logic to run the game----
    # -------------------------------
    
    ui <- fixedPage(
      
      title = "ShinyBART",
      uiOutput("MainAction"),
      tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),   # Prevents gray screen during Sys.sleep()
    
      tags$script(HTML('
            var pumps = 0;
            var maxPumps;
            var popMax;
            var actionTimes = [];
  
            //Receive the maximum amount of pumps for the whole game, and wether the boundary should be drawn
            Shiny.addCustomMessageHandler("maxPopHandler",     
              function(temp) {
                popMax = temp[0];
                drawBoundary = temp[1];
              }
            );

            //Receives the new max-pump values for this balloon
            Shiny.addCustomMessageHandler("maxPumpHandler",     
              function(newMaxPumps) {
                maxPumps = newMaxPumps;
              }
            );
    

            function jsPump(){
             actionTimes.push(new Date().getTime() - t);
             pumps = pumps + 1;
             if (pumps <= maxPumps) {
                document.getElementById("pumpCounter").innerHTML = pumps;
                jsRedrawBalloon("grey");
              } else {
                Shiny.onInputChange("popped", 1);
                Shiny.onInputChange("pumps", pumps);
                Shiny.onInputChange("actionTimes", actionTimes);
              }
            }
  
  
            function jsSaveBalloon(){
              actionTimes.push(new Date().getTime() - t);
              Shiny.onInputChange("actionTimes", actionTimes);
              Shiny.onInputChange("pumps", pumps);
            }
 
 
            function jsNewBalloon(){
              pumps = 0;
              Shiny.onInputChange("popped", 0);
              actionTimes = [];
            }

  
            function jsRedrawBalloon(balloonColor) {
              balloonSize = pumps/popMax * 95 + 5
              ctx.clearRect(0, 0, jsCanvas.width, jsCanvas.height);
              ctx.beginPath();
              ctx.arc(250, 160, balloonSize, 0, 2 * Math.PI);
              ctx.stroke();
              ctx.fillStyle = balloonColor;
              ctx.fill();

              // Write the pop/save messages to the canvas:
              ctx.fillStyle = "black";
              ctx.font = "24px Arial";
              if (balloonColor == "red") {
                  ctx.fillText("Popped at pump " + pumps, 160, 40);
                  ctx.fillText("No points", 360, 170);
              } else if (balloonColor == "green") {
                  ctx.fillText("Saved at pump " + pumps, 160, 40);
                  ctx.fillText("+ " + pumps + " points", 360, 170);
              }

              // Draw the maxmum boundary if desired:
              if (drawBoundary) {
                ctx.beginPath();
                ctx.arc(250, 160, 100, 0, 2 * Math.PI);
                ctx.stroke();
                       
              }
            }


            // Wait a second before showing the nextballoon button to avoid misclicks
            function jsDelayButton() { 
               document.getElementById("nextballoon").style.visibility = \'hidden\';
               setTimeout(function(){
               document.getElementById("nextballoon").style.visibility = \'visible\';
               }, 1000);
            }
                       
            '))
    )
    
    
    server <- function(input, output, session) {
      
      
      # --------------------------------
      # Section C: Define Reactive Values ----
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
      # Section D: Page Layouts ----
      # --------------------------------
      
      # Send the maximum pumpvalue to the Javascript part, so it knows how big to draw the balloon:
      session$sendCustomMessage(type='maxPopHandler', c(pop.max, RDrawBoundary))      
      
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
                p("As you can see, the more you pump a balloon the more risk you take. On the one hand, you might earn more points if the balloon does not pop. On the other hand, if the balloon does pop, you won't be ablew to save it and will not earn any points for that balloon."),
                h3(paste("Maximum of", pop.max, "pumps")),
                p(paste0("The maximum number of pumps for any balloon is ", pop.max, ". If you try to pump a balloon ", pop.max, " times, then it will surely pop!")),
                h3(paste("There will be", balloons.n, "balloons")),
                p(paste("You will play a total of", balloons.n, "balloons in this game. Again, different balloons have different popping points. You can never know for sure what the popping point for a balloon is unless it pops.")),
                p(paste("You can never lose points that you've earned after saving a balloon. Your goal is to earn as many points as you can across all", balloons.n, "balloons")),
                p("When you are ready to start, click Continue to start the first balloon!"),
                actionButton(inputId = "gt_game", 
                             label = "Continue"),            
              HTML("<br><br><br>")
            )
          )}
        
        
        # 3) BALLOON PAGE
        if (CurrentValues$page == "game") {
          
            # Main display while the game is in progress
            if (CurrentValues$pop == 0 & CurrentValues$saveballoon == 0) { 
  
              # Send the maximum pumps for this balloon to the JavaScript
              session$sendCustomMessage(type='maxPumpHandler', pop.v[CurrentValues$balloon])   
              
              return(
                  list(
                    # Main Display: Contains both a pump and a save button
                    fixedRow(
                       column(12,
                             fixedRow(
                                column(3, align="center", h3(tags$i("Balloon"))),
                                column(6, align="center", h3(tags$i("Pumps"))),
                                column(3, align="center", h3(tags$i("Points")))
                             ),
                             fixedRow(
                               column(3, align="center", p(style = "font-size:36px", paste0(CurrentValues$balloon, " of ", balloons.n))),
                               column(6, align="center", p(style = "font-size:36px", id = "pumpCounter", "0")), # This is updated via JavaScript
                               column(3, align="center", p(style = "font-size:36px", paste(CurrentValues$points.cum)))
                             ),
                             fixedRow(
                               column(12, align="center", tags$canvas(id ="balloonCanvas", width = "500", height = "300"))),
                         
                             # The Balloon is also later updated via JavaScript
                             tags$script('var jsCanvas = document.getElementById("balloonCanvas");
                                          var ctx = jsCanvas.getContext("2d");
                                          var t = new Date().getTime();
                                          jsRedrawBalloon("gray");
                                        '),
              
                             fixedRow(
                               column(12, align="center", actionButton("pump", label = "Pump The Balloon", onclick = "jsPump()")),
                               column(12, align="center", actionButton("saveballoon", label = "Save!", onclick = "jsSaveBalloon()"))
                             )))
                  )
                )
          }
            
            # Main display when the balloon popped
            else if (CurrentValues$pop == 1) {
              return(
                list(
                  # Main Display: Contains both a pump and a save button
                  fixedRow(
                    column(12,
                           fixedRow(
                             column(3, align="center", h3(tags$i("Balloon"))),
                             column(6, align="center", h3(tags$i("Pumps"))),
                             column(3, align="center", h3(tags$i("Points")))
                           ),
                           fixedRow(
                             column(3, align="center", p(style = "font-size:36px", paste0(CurrentValues$balloon, " of ", balloons.n))),
                             column(6, align="center", p(style = "font-size:36px", id = "pumpCounter", CurrentValues$pumps)),
                             column(3, align="center", p(style = "font-size:36px", paste(CurrentValues$points.cum)))
                           ),
                           fixedRow(
                             column(12, align="center", tags$canvas(id ="balloonCanvas", width = "500", height = "300"))),
                           
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
                           fixedRow(
                             column(3, align="center", h3(tags$i("Balloon"))),
                             column(6, align="center", h3(tags$i("Pumps"))),
                             column(3, align="center", h3(tags$i("Points")))
                           ),
                           fixedRow(
                             column(3, align="center", p(style = "font-size:36px", paste0(CurrentValues$balloon, " of ", balloons.n))),
                             column(6, align="center", p(style = "font-size:36px", id = "pumpCounter", CurrentValues$pumps)),
                             column(3, align="center", p(style = "font-size:36px", paste(CurrentValues$points.cum)))
                           ),
                           fixedRow(
                             column(12, align="center", tags$canvas(id ="balloonCanvas", width = "500", height = "300"))),
                           
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
      # Section E: Event (e.g.; button) actions ----
      # --------------------------------
      
      # Section F1: Page Navigation Buttons
      observeEvent(input$gt_inst1, {CurrentValues$page <- "inst1"})
      observeEvent(input$gt_game, {CurrentValues$page <- "game"})
      observeEvent(input$gt_surveyA, {CurrentValues$page <- "surveyA"})
      observeEvent(input$gt_goodbye, {CurrentValues$page <- "goodbye"})
      
      # Section F2: Event tracking buttons
      
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
      
    
      # What to do if the balloon popped:
      observeEvent(input$popped, {
     
          if (input$popped == 1) {
            
            CurrentValues$pop <- 1
            CurrentValues$pumps <- input$pumps
            
            GameData$balloon <- c(GameData$balloon, rep(CurrentValues$balloon, CurrentValues$pumps))
            GameData$time <- c(GameData$time, input$actionTimes)
            GameData$pumps <- c(GameData$pumps, 1:CurrentValues$pumps)
            GameData$action <- c(GameData$action, rep(1, CurrentValues$pumps))
            GameData$pop <- c(GameData$pop, rep(0, CurrentValues$pumps-1), 1)
            
          }
        }) 
  
      
      # saveballoon button
      observeEvent(input$saveballoon, {
        
        CurrentValues$pumps <- input$pumps
        
        GameData$balloon <- c(GameData$balloon, rep(CurrentValues$balloon, CurrentValues$pumps+1))
        GameData$time <- c(GameData$time, input$actionTimes)
        GameData$pumps <- c(GameData$pumps, 1:CurrentValues$pumps, NA)
        GameData$action <- c(GameData$action, rep(1, CurrentValues$pumps), 0)
        GameData$pop <- c(GameData$pop, rep(0, CurrentValues$pumps+1))
        
        # Add points for current balloon to point total
        CurrentValues$points.cum <- CurrentValues$points.cum + CurrentValues$pumps
        CurrentValues$saveballoon <- 1

      })
      
  
      # --------------------------------
      # Section F: Save data ---- Commented out for now
      # --------------------------------
        observeEvent(input$gt_goodbye, {
     
          # Create progress message
          withProgress(message = "Saving data...",
                       value = 0, {
     
                         incProgress(.25)
     
                         # Write game data to a dataframe
                         GameData.i <- data.frame("balloon" = GameData$balloon,
                                                  "time" = GameData$time,
                                                  "action" = GameData$action,
                                                  "pop" = GameData$pop)
                         
                         # Write survey data to a dataframe
                         SurveyData.i <- data.frame("sex" = input$sex,
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
                         if (saveDataLocation == "dorpbox") {
                            GameDatafilePath <- file.path(tempdir(), GameDatafileName)
                            rdrop2::drop_upload(GameDatafilePath, dest = outputDir, dtoken = droptoken)
                            
                            SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                            rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = EPtoken)
                            
                         } else if (saveDataLocation == "local") {
                            GameDatafilePath <- file.path(outputDir, GameDatafileName)
                            write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
                            
                            SurveyDatafilePath <- file.path(outputDir, SurveyDatafileName)
                            write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                         }
     
                          
                         incProgress(.40)
     
                          # Some interesting plots (not necessary)
     
                         output$EarningsPlot <- renderPlot({
     
                           balloon.agg <- GameData.i %>% group_by(balloon) %>%
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
    
