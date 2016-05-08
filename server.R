
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(mice)
library(sinkr)
library(reshape2)
library(ggplot2)

shinyServer(function(input, output) {
  
  
  #file upload
  
  display_dataset <- reactive({ 
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    # function that will filter that product based on the user's input
    data_set = head(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                            quote=input$quote) , 1000)
    
    data_num <- sapply(data_set, is.numeric)
    dataset <- data_set[,data_num]
    
    return(dataset = dataset)
  })
  
  #choose column names
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(display_dataset()))
      return()
    
    # Get the data set with the appropriate name
    dat <- display_dataset()
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  })
  
  
  tables <- reactive({
    if(is.null(display_dataset())){return ()}
    
    # Get the data set
    dat <- display_dataset()
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat)))
      return()
    
    # Keep the selected columns
    dat <- dat[, input$columns, drop = FALSE]
    
    data_table <- head(dat, 1000)
    
    miss_table<- as.matrix(data_table[,input$columns])
    set.seed(1)
    miss_table[sample(length(miss_table), 0.15*length(miss_table))] <- NaN
    miss_table
    
    return(list(data_table = data_table, miss_table = miss_table))
  })
  
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    tables()$data_table
  })
  
  output$missing_table <- renderTable({
    tables()$miss_table
  })
  
  interpolation <- reactive({
    data_table <- tables()$miss_table
    
    if(input$method == 'mice.pmm'){
      y <- mice(data_table, m = 5, method = "pmm")
      interp_table = complete(y)
    }
    else if(input$method == 'mice.norm'){
      y <- mice(data_table, m = 5, method = "norm")
      interp_table = complete(y)
    }
    else if(input$method == 'mice.mean'){
      y <- mice(data_table, m = 5, method = "mean")
      interp_table = complete(y)
    }
    else if(input$method == 'eof'){
      a <- eof(data_table, recursive = TRUE, method="svd")
      interp_table = eofRecon(a)
    }
    else{
      interp_table = NULL
    }
    interp_table
    
  })
  
  error_Plot <- reactive({
    if(is.null(interpolation()))
      return()
    
    data_table <- tables()$data_table
    inter_table <- interpolation()
    error_table <- abs(data_table - inter_table)
#     x <- 1:50
#     plot(x)
    for(i in 1:ncol(error_table)){
      local({
        data_col <- error_table[,i]
        plot(data_col, col = "red")

      })
    }
  })
  
  output$errorPlot <- renderUI({
    
  })
  
  #---------------Error plot starts-------------
  
  max_plots <- reactive({
    if(is.null(interpolation()))
      return()
    data <- tables()$data_table
    int <- interpolation()
    data_sub <- data - int
    max_plot <- ncol(data_sub)
    return(list(data_sub = data_sub, max_plot = max_plot))
  })
  
  output$plots <- renderUI({
    if(is.null(interpolation()))
      return()
    max_col <- max_plots()$max_plot
    
    plot_output_list <- lapply(1:max_col, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 280, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
    plot_content()
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  plot_content <- reactive({
    if(is.null(interpolation()))
      return()
    for (i in 1:max_plots()$max_plot) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          data_sub <- max_plots()$data_sub
          plot(data_sub[,my_i])
        })
      })
    }
  })
  
    
    
#     m <- data_sub$FIPS
#     data_sub <- melt(data_sub, id.vars = m)
#     
#     ggplot(data_sub, aes(x=m, y=value, fill=variable)) + geom_bar(stat='identity')
    

 
  
  
  #------------------Error plot ends ---------------
  
  output$intp_table <- renderTable({
    if(is.null(interpolation()))
      return()
    interpolation()
  })
  
  output$sub_table <- renderTable({
    if(is.null(interpolation()))
      return()
    data <- tables()$data_table
    int <- interpolation()
    data_sub <- data - int
    abs(data_sub)
  })
  
  output$downloadOriginalData <- downloadHandler(
      filename ='origianl_data.csv',
      content = function(file) {
        write.csv(tables()$data_table, file)
      }

  )
  
  output$downloadInterpolatedData <- downloadHandler(
    filename = 'interpolated_data.csv',
    content = function(file) {
      write.csv(interpolation(), file)
    }
  )
  
})
