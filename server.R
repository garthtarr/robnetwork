library(shiny)
require(huge)
data(stockdata)
X = log(stockdata$data[2:1258,]/stockdata$data[1:1257,])
colnames(X) = stockdata$info[,3]
# cor.mat.classical.save = cor(X)
# cor.mat.Qn.save = cov2cor(pair.cov(X,scale.fn = Qn))
# cor.mat.Pn.save = cov2cor(pair.cov(X,scale.fn=Pn))
# cor.mat.gr.save = cov2cor(gaus.cov.Qn(X))
load("cormats.RData")
require(networkD3)
require(pairsD3)
require(htmlwidgets)
source("covest.R")
source("graphest.R")

shinyServer(function(input, output) {
  output$txt1 = renderText({
    if(input$extra_info){
      paste("Covariance estimation method:",input$cov.mtd,
            "\n",
            "Regularisation parameter:",input$lambda,
            "\n",
            "Number of edges:",
            "\n",
            "Graph structure:")
    } else {return(NULL)}
  })
  
  output$htmltxt <- renderUI({
    str1 = paste("Covariance estimation method:",input$cov.mtd)
    str2 = paste("Regularisation parameter:",input$lambda)
    str3 = paste("Number of edges:")
    str4 = paste("Graph structure:")
    HTML(paste(str1, str2, str4, sep = '<br/>'))
  })
  
  output$txt2 = renderPrint({
    if(input$extra_info){
      if(is.element(input$data, c('Upload csv file', 'Upload RData file'))){
        if(input$cov.mtd=="Classical covariance"){
          str(mk.g(cor.classical.save, lambda=input$lambda,
                   var.names = varnm(), group.labs = grpnm()))
        } else if(input$cov.mtd=="Qn pairwise"){
          str(mk.g(cor.Qn.save, lambda=input$lambda,
                   var.names = varnm(), group.labs = grpnm()))
        } else if(input$cov.mtd=="Pn pairwise"){
          str(mk.g(cor.Pn.save, lambda=input$lambda,
                   var.names = varnm(), group.labs = grpnm()))
        } else if(input$cov.mtd=="Gaussian Rank"){
          str(mk.g(cor.gr.save, lambda=input$lambda,
                   var.names = varnm(), group.labs = grpnm()))
        }
      } else { # inbuilt Financial example 
        if(input$cov.mtd=="Classical covariance"){
          str(mk.g(cor.mat.classical.save,lambda=input$lambda,
                   var.names = stockdata$info[,1],
                   group.labs = stockdata$info[,2]))
        } else if(input$cov.mtd=="Qn pairwise"){
          str(mk.g(cor.mat.Qn.save,lambda=input$lambda,
                   var.names = stockdata$info[,1],
                   group.labs = stockdata$info[,2]))
        } else if(input$cov.mtd=="Pn pairwise"){
          str(mk.g(cor.mat.Pn.save,lambda=input$lambda,
                   var.names = stockdata$info[,1],
                   group.labs = stockdata$info[,2]))
        } else if(input$cov.mtd=="Gaussian Rank"){
          str(mk.g(cor.mat.gr.save,lambda=input$lambda,
                   var.names = stockdata$info[,1],
                   group.labs = stockdata$info[,2]))
        }
      }
    } else {return(NULL)}
  })
  
  cor.classical.save = NULL
  cor.Pn.save = NULL
  cor.Qn.save = NULL
  cor.gr.save = NULL
  
  cor.mat = reactive({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating covariance matrix", value = 0)
    if(input$cov.mtd=="Classical covariance"){
      return(cor(datain()))
    } else if(input$cov.mtd=="Qn pairwise"){
      return(cov2cor(pair.cov(datain(),scale.fn = Qn)))
    } else if(input$cov.mtd=="Pn pairwise"){
      return(cov2cor(pair.cov(datain(),scale.fn=Pn)))
    } else if(input$cov.mtd=="Gaussian Rank"){
      return(cov2cor(gaus.cov.Qn(datain())))
    }
  })
  
  output$d3net = renderForceNetwork({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating precision matrix", value = 0)
    if(input$plot_type=="d3"){
      if(is.element(input$data, c('Upload csv file','Upload RData file'))){
        if(input$cov.mtd=="Classical covariance"){
          if(is.null(cor.classical.save)){
            cor.classical.save <<- cor.mat()
          }
          plot.g.d3(cor.classical.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = varnm(), group.labs = grpnm())
        } else if(input$cov.mtd=="Qn pairwise"){
          if(is.null(cor.Qn.save)){
            cor.Qn.save <<- cor.mat()
          }
          plot.g.d3(cor.Qn.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = varnm(), group.labs = grpnm())
        } else if(input$cov.mtd=="Pn pairwise"){
          if(is.null(cor.Pn.save)){
            cor.Pn.save <<- cor.mat()
          }
          plot.g.d3(cor.Pn.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = varnm(), group.labs = grpnm())
        } else if(input$cov.mtd=="Gaussian Rank"){
          if(is.null(cor.gr.save)){
            cor.gr.save <<- cor.mat()
          }
          plot.g.d3(cor.gr.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = varnm(), group.labs = grpnm())
        }
      } else { # inbuilt Financial example 
        if(input$cov.mtd=="Classical covariance"){
          plot.g.d3(cor.mat.classical.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        } else if(input$cov.mtd=="Qn pairwise"){
          plot.g.d3(cor.mat.Qn.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        } else if(input$cov.mtd=="Pn pairwise"){
          plot.g.d3(cor.mat.Pn.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        } else if(input$cov.mtd=="Gaussian Rank"){
          plot.g.d3(cor.mat.gr.save,lambda=input$lambda,
                    width=700,height=550,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        }
      } 
    } else {
      return(NULL)
    }
  })
  
  output$basenet = renderPlot({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating precision matrix", value = 0)
    if(input$plot_type=="base"){
      if(is.element(input$data, c('Upload csv file','Upload RData file'))){
        if(input$cov.mtd=="Classical covariance"){
          if(is.null(cor.classical.save)){
            cor.classical.save <<- cor.mat()
          }
          plot.g.fn(cor.classical.save,lambda=input$lambda,
                    var.names = varnm(), group.labs = grpnm())
        } else if(input$cov.mtd=="Qn pairwise"){
          if(is.null(cor.Qn.save)){
            cor.Qn.save <<- cor.mat()
          }
          plot.g.fn(cor.Qn.save,lambda=input$lambda,
                    var.names = varnm(), group.labs = grpnm())
        } else if(input$cov.mtd=="Pn pairwise"){
          if(is.null(cor.Pn.save)){
            cor.Pn.save <<- cor.mat()
          }
          plot.g.fn(cor.Pn.save,lambda=input$lambda,
                    var.names = varnm(), group.labs = grpnm())
        } else if(input$cov.mtd=="Gaussian Rank"){
          if(is.null(cor.gr.save)){
            cor.gr.save <<- cor.mat()
          }
          plot.g.fn(cor.gr.save,lambda=input$lambda,
                    var.names = varnm(), group.labs = grpnm())
        }
      } else { # inbuilt Financial example
        if(input$cov.mtd=="Classical covariance"){
          plot.g.fn(cor.mat.classical.save,lambda=input$lambda,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        } else if(input$cov.mtd=="Qn pairwise"){
          plot.g.fn(cor.mat.Qn.save,lambda=input$lambda,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        } else if(input$cov.mtd=="Pn pairwise"){
          plot.g.fn(cor.mat.Pn.save,lambda=input$lambda,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        } else if(input$cov.mtd=="Gaussian Rank"){
          plot.g.fn(cor.mat.gr.save,lambda=input$lambda,
                    var.names = stockdata$info[,1],
                    group.labs = stockdata$info[,2])
        }
      }
    } else {
      return(NULL)
    }
  })
  
  datain <- reactive({
    datain=NULL
    if(input$data == 'Upload csv file'){
      if(!is.null(input$csvfile)){
        data = read.csv(input$csvfile$datapath, header=input$header, sep=input$sep)
      }
    } else if(input$data == 'Upload RData file'){
      if(!is.null(input$rdatafile)){
        datain = load(input$rdatafile$datapath)
        if(is.null(get(datain)$data)){
          data = get(datain)
          return(data)
        } else {
          return(get(datain)$data)
        }
      }
    } else return(datain)
  })
  
  varnm <- reactive({
    if(!is.null(datain())){
      return(colnames(datain()))
    } else {
      return(names(datain()))
    }
  })
  
  grpnm = reactive({
    if(input$data == 'Upload RData file'){
      if(!is.null(input$rdatafile)){
        datain = load(input$rdatafile$datapath)
        if(is.null(get(datain)$info)){
          return(NULL)
        } else if(dim(get(datain)$info)[2]>1){
          grpnm = get(datain)$info[,2]
        }
      }
    } else return(NULL)
  })
  
  output$datastr = renderPrint({
    if(input$data != "Financial data"){
      #str(datain())
      #return(input$choose_vars)
      list(input$selectedobs,
           input$mynumber)
    } else {
      #return(str(list(data=X,info=stockdata$info)))
      #return(input$choose_vars)
      list(input$selectedobs,
           input$mynumber)
    }
  })
  
  output$pairsplot = renderPairsD3({
    if(input$data != "Financial data"){
      pairsD3(subset(datain(),select = input$choose_vars))
    } else {
      pairsD3(subset(X,select=input$choose_vars))
    }
  })
  
  choices<-reactive({
    input$choose_vars
  })
  
  output$varselect <- renderUI({
    if(input$data != "Financial data"){
      cols = colnames(datain())
    } else { cols = colnames(X)}
    selectInput("choose_vars", "Select variables to plot:",
                choices=cols, selected=cols[1:3], multiple=T)  
  })
  
  output$outputTable <- renderDataTable({
    if(input$data != "Financial data"){
      data = datain()
    } else { 
      data = X 
    }
    if(input$table_data_logical==1){
      displayDF <- as.matrix(data) # baseData$df #data sent to d3.js 
      n=dim(displayDF)[1]
      dfFilter <- input$selectedobs[1:n] # passed from the web interface
      if (is.null(dfFilter)){
        # means no selection has been made
        dfFilter = rep(TRUE,n)
      }
      displayDF <- as.data.frame(cbind(names=row.names(displayDF), 
                                       displayDF))
      dfFilter[dfFilter==''] = TRUE
      dfFilter[dfFilter=='greyed'] = FALSE
      if(input$table_data_vars==0){
        return(as.matrix(displayDF[dfFilter == TRUE,choices(),drop=FALSE]))
      } else if(input$table_data_vars==1){
        return(as.matrix(displayDF[dfFilter == TRUE,,drop=FALSE]))
      }
    } else {
      return(NULL)
    }
  }, 
  options = list(pageLength = 20,
                 lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                 searching = FALSE)
  )
  
  
})



