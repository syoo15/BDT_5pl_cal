############## BDT Five Parameter Logistic Regression Program ##############
############## Server                                         ##############
############## Author: SeokChan Yoo                           ##############
############## Version. Beta                                  ##############

# MIT License
# 
# Copyright (c) [2019/01/08] [Seokchan Yoo]
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


##### Load dependencies ####
library("xlsx")
library("shiny")
library("minpack.lm")
library("ggplot2")
library("DT")
library("rhandsontable")


M.5pl <- function(x, lowx, highx, inflec, hill, asym) {
  f <- highx + ((lowx - highx) / (((
    1 + (x / inflec) ^ hill
  )) ^ asym))
  return(f)
}

# Inverse 5PL Function

# inv_5pl <- function(y, lowx, highx, inflec,hill,asym){
#   f <- inflec*(((highx - lowx) / (y - lowx))^(1/asym) - 1)^(1 / hill)
#   return(f)
# }

inv_5pl <- function(y, lowx, highx, inflec,hill,asym){
  f <- inflec*(((lowx - highx) / (y - highx))^(1/asym) - 1)^(1 /hill)
  return(f)
}

## Server Function ##

server <- function(input, output, session) {
  
  X <- c(0.477,1.765,2.367,2.627,3.047,3.269,3.445,3.570,3.667, 3.998)
  Y <- c(1.81, 5.60, 12.6, 13.6, 17.4,19.2,21.1,22.8,23.9, 25.2)
  df <- data.frame(X, Y)
  
  datavalues <- reactiveValues(data=df)
  
  # Display the data using renderRHandsontable() function
  # Creates Excel like editable cells from reactiveValues()
  
  output$tbl <- renderRHandsontable({
    rhandsontable(datavalues$data, stretchH="all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>%
      hot_rows(rowHeights=50)
  }) 
  
  
  # Observe Event for any changes made to cells
  # Update these modified values as R dataframe for internal computation
  
  observeEvent(
    input$tbl$changes$changes,  # observe any changes
    {    
      
      xi=input$tbl$changes$changes[[1]][[1]]   # Capture the row changed
      datavalues$data <- hot_to_r(input$tbl)   # Convert table into R dataframe
      
      # Add columns that show calculated concentration based on Response values... (using inverse 5PL function)
      # nunununu
    }
  )
  
  ## Calculated Concentration Tab ## 
  
  #Sample_name <- c("Sample 1", "Sample 2", "Sample 3", "Sample 4","Sample 5", "Sample 6", "Sample 7", "Sample 8", "Sample 9", "Sample 10")
  Response <- rep(0,10)
  Calculated <- rep(0,10)
  df2 <- data.frame(Response,Calculated)
  
  datavalues2 <- reactiveValues(data2=df2)
  
  # Display the data using renderRHandsontable() function
  # Creates Excel like editable cells from reactiveValues()
  
  output$tbl2 <- renderRHandsontable({
    rhandsontable(datavalues2$data2, stretchH="all") %>%
      hot_context_menu(allowRowEdit = TRUE) %>%
      hot_rows(rowHeights=50)
  }) 
  
  
  # Observe Event for any changes made to cells
  # Update these modified values as R dataframe for internal computation
  
  observeEvent(
    input$tbl2$changes$changes,  # observe any changes
    {    
      
      xii=input$tbl2$changes$changes[[1]][[1]]   # Capture the row changed
      datavalues2$data2 <- hot_to_r(input$tbl2)   # Convert table into R dataframe
      
      # Add columns that show calculated concentration based on Response values... (using inverse 5PL function)
      
      datavalues2$data2[xii+1,2] = inv_5pl(datavalues2$data2[xii+1,1],e_lowx,e_highx,e_inf,e_hill,e_asym)
    }
  )
  
  # On Button click, retrieve user input for start estimates
  # Saved as variables
  # Run nlsLM function
  observeEvent(input$submit, {
    
    # Return NULL if user input is NULL
    if(is.null(input$min) || is.null(input$max) ||is.null(input$inflec) || is.null(input$slope) || is.null(input$asym))
      return(NULL)
    
    # Assign user input as variables for start estimates
    lowx <- input$min
    highx <- input$max
    inflec <- input$inflec
    hill <- input$slope
    asym <- input$asym
    start_param <- c(lowx,highx,inflec,hill,asym)
    
    # Render Text on app (for test use only)
    output$estimate <- renderText({
      paste("Start estimates are: ", lowx, highx, inflec,hill,asym)
    })
    
    # Run 5PL regression based on user input of start estimate
    lm5pl <-nlsLM(Y ~ M.5pl(X, lowx, highx, inflec, hill, asym), data=datavalues$data, start=start_parm, trace=FALSE, control=nls.lm.control(maxiter=1000))
    
    # # Text output of Regression Summary
    # output$reg_summary <- renderText({
    #   paste(summary(lm5pl))
    # })
    
    
    # Save the current parameters into variables
    e_lowx <- coef(lm5pl)[1]
    e_highx <- coef(lm5pl)[2]
    e_inf <- coef(lm5pl)[3]
    e_hill <- coef(lm5pl)[4]
    e_asym <- coef(lm5pl)[5]
    
    # Predict Y value using 5PL model
    pred_y <- predict(lm5pl,datavalues$data$Std)
    
    # Render graph based on the data
    
    output$p1 <- renderPlot({
      
      reg_plot <- ggplot(data=datavalues$data, aes(x=X, y=Y)) + geom_point() + theme_bw() + xlab("Concentration") + ylab("Response")
      reg_plot <- reg_plot + geom_smooth(method="loess",aes(x=X,y=pred_y))
      return(reg_plot)
    }, res=150)
    
    # Mouse-hover info. for plot
    output$info <- renderPrint({
      nearPoints(df, input$plot_hover, xvar="X",yvar="Y")
      
    })
    
    # Print parameters on App
    output$params <- renderText({
      paste("Asymptotic Minimum: ", e_lowx, "Asymptotic Maximum: " , e_highx, "Inflection Point: ",e_inf, "Slope: ", e_hill,"Asymmetry Factor: ", e_asym, sep="\n")
    })
  },
  ignoreInit = TRUE, ignoreNULL=FALSE    # This allows observeEvent occurs everytime action button is clicked
  )
  
  observeEvent(input$reset, {
    # Set params as NA
    e_lowx <- NA
    e_highx <- NA
    e_inf <- NA
    e_hill <- NA
    e_asym <- NA

    output$params <- renderText({
      
    })
  })
  
  # # Save the output data to local hard drive
  # 
  # output$save <- downloadHandler(
  #   filename = function(){
  #     paste(input$)
  #   }
  # )
  # 
  # saveData <- function(){
  #   write.xlsx(datavalues2$data2, file="result.xlsx")
  # }
  # 
  # # Link saveData function to a Button
  # 
  # observeEvent(
  #   input$save,
  #   saveData()
  # )
}