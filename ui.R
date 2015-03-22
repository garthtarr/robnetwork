library(shiny)
require(huge)
require(networkD3)
source("covest.R")
source("graphest.R")

shinyUI(fluidPage(
  # Application title
  titlePanel("Robust precision matrix estimation"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("data", 
                  "Select data set", 
                  choices=c("Financial data","Upload csv file","Upload RData file"), 
                  selected = "Financial data", 
                  multiple = FALSE,
                  selectize = FALSE, width = NULL, size = NULL),
      conditionalPanel(
        condition = "input.data == 'Upload csv file'",
        fileInput('csvfile', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ',')),
      conditionalPanel(
        condition = "input.data == 'Upload RData file'",
        fileInput('rdatafile', 'Choose RData File')),
      selectInput("cov.mtd", 
                  "How to estiamte the covariance matrix?", 
                  choices=c("Classical covariance","Qn pairwise","Pn pairwise","Gaussian Rank"), 
                  selected = "Classic", 
                  multiple = FALSE,
                  selectize = FALSE, width = NULL, size = NULL),
      sliderInput("lambda",
                  "Regularisation parameter value:",
                  min = 0,
                  max = 1,
                  value = 0.53,
                  step=0.01),
      radioButtons("plot_type", "Plot type:",
                   c("d3" = "d3",
                     "base" = "base")),
      radioButtons("extra_info", "Additional graph information?",
                   c("No" = "FALSE",
                     "Yes" = "TRUE")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Graph",
                 conditionalPanel(
                   condition = "input.plot_type == 'd3'",
                   forceNetworkOutput("d3net")),
                 conditionalPanel(
                   condition = "input.plot_type == 'base'",
                   plotOutput("basenet")),
                 conditionalPanel(
                   condition = "input.extra_info == 'TRUE'",
                   htmlOutput("htmltxt"),
                   verbatimTextOutput("txt2")),
                 value=1),
        tabPanel("Info",
                 includeMarkdown("info.Rmd"),
                 value=2),
        tabPanel("Data",
                 verbatimTextOutput("datastr"),
                 value=3)
      )
    )
  )
))