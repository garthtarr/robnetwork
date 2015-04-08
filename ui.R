library(shiny)
require(huge)
require(networkD3)
require(pairsD3)
require(htmlwidgets)
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
      conditionalPanel(
        condition = "input.tabs1 == 1",
        selectInput("cov.mtd", 
                    "How to estimate the covariance matrix?", 
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
        sliderInput("charge",
                    "Charge for d3 graph:",
                    min = -100,
                    max = 100,
                    value = -30,
                    step = 1),
        radioButtons("extra_info", "Additional graph information?",
                     c("No" = "FALSE",
                       "Yes" = "TRUE"))
      ),
      conditionalPanel(
        condition = "input.tabs1 == 3",
        uiOutput("varselect"),
        sliderInput("cex","Size of plotting symbol",3,min=1,max=10),
        sliderInput("opacity","Opacity of plotting symbol",0.9,min=0,max=1),
        radioButtons("theme", "Colour theme",
                     choices = c("Colour"= "colour",
                                 "Monochrome"="bw")),
        #sliderInput("fontsize","Font size",12,min=6,max=24),
        sliderInput("width","Width and height",600,min=200,max=1200),
        radioButtons("table_data_logical", label="Table of data?",
                     choices = c("No" = 0,
                                 "Yes" = 1)),
        conditionalPanel(
          condition = "input.table_data_logical == 1",
          selectInput(inputId="table_data_vars",label="Include all variables in table?",
                      choices=c("No" = 0,
                                "Yes" = 1))
        )
      )
    ),
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
                 #verbatimTextOutput("datastr"),
                 uiOutput("pairsplot"),
                 br(),br(),
                 dataTableOutput(outputId="outputTable"),
                 #pairsD3Output("pairsplot"),
                 value=3),
        id="tabs1")
    )
  )
))
