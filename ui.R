############## BDT Five Parameter Logistic Regression Program ##############
############## User Interface                                 ##############
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



# Library
library("xlsx")
library("shiny")
library("minpack.lm")
library("ggplot2")
library("DT")
library("rhandsontable")

# 5PL Eqn.

M.5pl <- function(x, lowx, highx, inflec, hill, asym) {
  f <- highx + ((lowx - highx) / (((
    1 + (x / inflec) ^ hill
  )) ^ asym))
  return(f)
}

# Inverse 5PL Function

inv_5pl <- function(y, highx, lowx, inflec,hill,asym){
  f <- 1/(inflec*(((highx - lowx) / 
                  (y - lowx))^(1/asym) - 1)^(1 / hill))
}

## UI Design ##

  ui <- fluidPage(
    
    # TITLE PANEL
    #titlePanel("5PL web app", windowTitle="5PL web app"),
    
    # TITLE HEADER
    headerPanel(
      div(
        class = "col-sm-12 lato",
        id = "pageTitle",
        tags$span('5PL Calibration'),
        tags$span(class = "small", ' Beta version')
      )
    ),
    
    sidebarLayout(
      position = "right",
      sidebarPanel(
        h5(
          code("Non-Linear Regression using Levenberg-Marquardt algorithm")
        ),
        
        h3("Start Estimates"),
        p("Start estimates must be provided to run 5PL regression..."),
        numericInput("min", "Asymptotic Minimum :", 0, min =
                       0),
        numericInput("max", "Asymptotic Maximum :", 0, min =
                       0),
        numericInput("inflec", "Inflection Point :", 0, min =
                       0),
        numericInput("slope", "Hill Slope :", 0),
        numericInput("asym", "Asymmetry Factor :", 0),
        h3("Weighting Option"),
        p(
          "If checked, the inverse variance of the responses at that concentration is used as weight"
        ),
        checkboxInput("checkbox", "Weighted", value = FALSE),
        actionButton("submit", "Start 5PL Regression"),
        actionButton("reset", "Clear All")
        
      ),
      mainPanel(h2("Data for Standards curve"),
                h4("Copy and paste the data for standards"),
                rHandsontableOutput("tbl"))
      ),
    
      # Another sidebar layout for graph and console output
      sidebarLayout(
        position="right",
        sidebarPanel(
          h3("Console Output"),
          verbatimTextOutput("info"),
          verbatimTextOutput("estimate"),
          verbatimTextOutput("reg_summary"),
          verbatimTextOutput("params"),
          actionButton("save", "Save Result")
        ),
        mainPanel(
          h2("Regression Analysis"),
          tabsetPanel(
            tabPanel(
              "Plot",
              plotOutput("p1", hover = "plot_hover")
            ),
            tabPanel(
              "Summary of Regression"
            ),
            tabPanel(
              "Calculated Concentration",
              rHandsontableOutput("tbl2")
            )
          ))
        ),
      
        # An empty space
    
        fluidRow(
          column(12,
                 h2(" "))
        )
        
      )

