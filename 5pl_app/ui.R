install_load <- function (package1, ...)  {

  # convert arguments to vector
  packages <- c(package1, ...)

  # start loop to determine if each package is installed
  for(package in packages){

    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))

    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}
install_load("xlsx")
install_load("minpack.lm")
install_load("ggplot2")
install_load("DT")
devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)

##### Load dependencies ####
library(xlsx)
library(shiny)
library(minpack.lm)
library(ggplot2)
library(DT)

############# Provide sample dataset #####################

log_conc <- c(0.477,1.765,2.367,2.627,3.047,3.269,3.445,3.570,3.667)
resp <- c(1.81, 5.60, 12.6, 13.6, 17.4,19.2,21.1,22.8,23.9)
sample_df <- data.frame(log_conc,resp)

#### Function: 5PL curve function ####
#### Unweigthed regression  ####

M.5pl <- function(x, lowx, highx, inflec, hill, asym){
  f <- highx + ((lowx - highx)/(((1+(x/inflec)^hill))^asym))
  return(f)
}

start_parm <- c(lowx=0.1, highx = 30, inflec=20, hill=1, asym=0)


if (interactive()) {
  
  ui <- fluidPage(
    

    
    # TITLE HEADER 
    headerPanel(
      div(class="col-sm-12 lato", id="pageTitle",
          tags$span('5PL Calibration'),
          tags$span(class="small", ' Beta version')
      )),
    
    sidebarLayout(position="right",
                  sidebarPanel(
                    h5(code("Non-Linear Regression using Levenberg-Marquardt algorithm")),
                    h3("File Selection"),
                    fileInput("file1", "Choose Excel File",
                              accept = c(".xlsx")),
                    
                    # Logistic Model option (not working yet)
                    h3("Model Selection"),
                    radioButtons("model", "Logistic Model: ",
                                 c("5PL" ="5PL", "4PL"="4PL"),
                                 selected="5PL"),
                    h3("Weighting Option"),
                    p("If checked, the inverse variance of the responses at that concentration is used as weight"),
                    checkboxInput("checkbox","Weighted", value=FALSE)
                    
                  ),
                  mainPanel(
                    h2("Data"),
                    tableOutput("contents")
                  )
    ),
    
    sidebarLayout(position="right",
                  
                  sidebarPanel(
                    h3("Start Estimates"),
                    p("Start estimates must be provided to run 5PL regression..."),
                    numericInput("min", "Asymptotic Minimum :",0,min=0),
                    numericInput("max", "Asymptotic Maximum :",0,min=0),
                    numericInput("inflec", "Inflection Point :",0,min=0),
                    numericInput("slope", "Hill Slope :",0),
                    numericInput("asym", "Asymmetry Factor :",0),
                    actionButton("submit", "Start Regression"),
                    actionButton("reset", "Reset")
                    
                  ),
                  
                  mainPanel(
                    h2("Regression Analysis"),
                    tabsetPanel(
                      tabPanel("Plot", 
                               plotOutput("p1", hover="plot_hover"),
                               verbatimTextOutput("info")), 
                      tabPanel("Summary", 
                               verbatimTextOutput("estimate"),
                               verbatimTextOutput("reg_summary"),
                               verbatimTextOutput("params"))
                    )
                  )
    ),
    
    mainPanel(
      h2("Predict module"),
      column(6, hotable("predict")),
      column(6,DT::dataTableOutput('tbl'))
    )
    
    
  )}
  