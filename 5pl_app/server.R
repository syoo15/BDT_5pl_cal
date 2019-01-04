##### Load dependencies ####
library(xlsx)
library(shiny)
library(minpack.lm)
library(ggplot2)
library(shinysky)
library(DT)

X <- c(0.477,1.765,2.367,2.627,3.047,3.269,3.445,3.570,3.667)
Y <- c(1.81, 5.60, 12.6, 13.6, 17.4,19.2,21.1,22.8,23.9)
df <- data.frame(X,Y)


server <- function(input, output, session) {
  

  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    # Null Case = Return sample data_frame
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    a <- read.xlsx(inFile$datapath, sheetIndex = 1)
    df <- c(a$X, a$Y)
    
  })
  
  output$p1 <- renderPlot({
    # if(is.null(input$file1$datapath))
    #     return(NULL)
    
    ggplot(df, aes(x=X, y=Y)) + geom_point() + theme_bw() + xlab("Concentration") + ylab("Response")
  }, res=150)
  
  # Mouse-hober info. for plot
  output$info <- renderPrint({
    nearPoints(df, input$plot_hover, xvar="X",yvar="Y")

  })
  
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
    lm5pl <-nlsLM(Y ~ M.5pl(X, lowx, highx, inflec, hill, asym), data=df, start=start_parm, trace=FALSE, control=nls.lm.control(maxiter=1000))
    
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
    
    # Print parameters on App
    output$params <- renderText({
      paste("Asymptotic Minimum: ", e_lowx, "Asymptotic Maximum: " , e_highx, "Inflection Point: ",e_inf, "Slope: ", e_hill,"Asymmetry Factor: ", e_asym, sep="\n")
    })
    
  },
  ignoreInit = TRUE     # This allows observeEvent occurs everytime action button is clicked
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
  
  # Prediction of Concentration using Fitted eqn.
  previous <- reactive(df)
  
  MyChanges <- reactive({
    if(is.null(input$predict)){return(previous())}
    else if(!identical(previous(),input$predict)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$predict))
    }
  })
  output$predict <- renderHotable({MyChanges()}, readOnly = F)
  output$tbl = DT::renderDataTable(MyChanges())
  
  
  
  
}