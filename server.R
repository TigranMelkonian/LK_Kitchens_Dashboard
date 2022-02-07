source("lkHelperFunctions.R")

server <- shinyServer(function(input, output, session) {
  #############################################
  #               DATA TabPanel               #
  #############################################
  
  #######################################
  # Orders raw data table              #
  #####################################
  
  output$ordersDfRaw <- renderDataTable({
    # initialize updated data for data table output
    rawDf <- sessionDownloadDataInput()
    # set custome features for data table output
    datatable(rawDf,
      options = list(
        paging = T, scrollX = F, searching = T, bInfo = T,
        autoWidth = F,
        class = "cell-border hover compact",
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      escape = F
    )
  })


  #######################################
  # Orders summary stats data table    #
  #####################################

  output$ordersDfSummaryStats <- renderDataTable({
    # initialize updated data for data table output
    ordersSummaryDf <- sessionDownloadDataInput() %>%
      summarise(
        time_opened_start = min(time_opened),
        time_opened_end = max(time_opened),
        min_order_subtotal = min(subtotal_dollar),
        median_order_subtotal = median(subtotal_dollar),
        mean_order_subtotal = round(mean(subtotal_dollar), 2),
        max_order_subtotal = max(subtotal_dollar),
        stdin_order_subtotal = round(sd(subtotal_dollar), 2),
        total_order_subtotal = sum(subtotal_dollar)
      ) %>%
      mutate(meal_type = input$dataDropDownSelectMeal)
    # set custome features for data table output
    datatable(ordersSummaryDf,
      options = list(
        paging = T, scrollX = T, searching = T, bInfo = T,
        autoWidth = F,
        class = "cell-border hover compact",
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      escape = F
    )
  })
  
  
  ##############################################
  # Download handler - pageview summary stats #
  ############################################
  
  output$downloadOrdersSummary <- downloadHandler(
    filename = function() {
      paste0("orders_df_summary_stats_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- sessionDownloadDataInput() %>%
        summarise(
          time_opened_start = min(time_opened),
          time_opened_end = max(time_opened),
          min_order_subtotal = min(subtotal_dollar),
          median_order_subtotal = median(subtotal_dollar),
          mean_order_subtotal = round(mean(subtotal_dollar), 2),
          max_order_subtotal = max(subtotal_dollar),
          stdin_order_subtotal = round(sd(subtotal_dollar), 2),
          total_order_subtotal = sum(subtotal_dollar)
        )
      
      write.csv(data,
                file,
                row.names = F
      )
    }
  )

  
  ####################################
  # Download handler - orders raw   #
  ##################################

  output$downloadOrdersRaw <- downloadHandler(
    filename = function() {
      paste0("orders_df_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- (sessionDownloadDataInput())
      write.csv(data,
        file,
        row.names = F
      )
    }
  )
  

  ###############################################
  #               CUSTOMER TabPanel             #
  ###############################################

  output$customerOrderSummaryStats <- renderDataTable({
    data <- sessionCustomerDataInput() %>%
      filter(customer_id %in% input$customerDropDownSelectId) %>%
      group_by(customer_id) %>%
      summarise(
        first_order_date = min(time_opened),
        last_order_date = max(time_opened),
        min_order_subtotal = min(subtotal_dollar),
        median_order_subtotal = median(subtotal_dollar),
        mean_order_subtotal = round(mean(subtotal_dollar), 2),
        max_order_subtotal = max(subtotal_dollar),
        stdin_order_subtotal = round(sd(subtotal_dollar), 2),
        total_order_subtotal = sum(subtotal_dollar),
        order_count = n()
      )

    # set custome features for data table output
    datatable(data,
      options = list(
        paging = F, scrollX = T, searching = F, bInfo = F,
        autoWidth = F,
        class = "cell-border hover compact",
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      escape = F
    )
  })
  

  output$customerOrdersMealTypePieChart <- renderPlotly({
    data <- sessionCustomerDataInput() %>%
      filter(customer_id %in% input$customerDropDownSelectId)

    data <- data.frame(table(data$meal_type))
    colnames(data) <- c("meal_type", "freq")
    plot_ly(data, labels = data$meal_type, values = data$freq) %>%
      add_pie(hole = 0.6) %>%
      layout(
        title = "Number of Orders by Meal Type", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })
  

  output$customerOrdersSubtotalHist <- renderPlotly({
    # initialize updated data for trend plot
    data <- sessionCustomerDataInput() %>%
      filter(customer_id %in% input$customerDropDownSelectId)

    ggplot(data = data, aes(x = "", y = subtotal_dollar)) +
      geom_boxplot(notch = TRUE, width = 0.5, outlier.colour = "red", outlier.shape = 1) +
      ggtitle("Order Subtotal Distribution") +
      xlab(paste0("Meal Type: ", input$customerDropDownSelectMeal)) +
      ylab("Order Subtotal") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })
  
  
  ###############################################
  #               COMPANY TabPanel              #
  ###############################################
  
  output$companyCustomerCohortMatrixRawCounts <- renderDataTable({
    cohortMatrix <- setCohortAnalysisMatrix(getCohortBaseMatrix(sessionCompanyDataInput(), sessionDataAggregationLevel()))
    # Creating 19 breaks and 20 rgb color values ranging from blue to white
    breaks <- quantile(cohortMatrix[, 3:ncol(cohortMatrix)], probs = seq(.05, .95, .05), na.rm = TRUE)
    colors <- sapply(
      round(seq(155, 80, length.out = length(breaks) + 1), 0),
      function(x) {
        rgb(x, x, 155, maxColorValue = 155)
      }
    )
    
    datatable(cohortMatrix,
              class = "cell-border stripe",
              rownames = FALSE,
              options = list(
                ordering = F,
                dom = "t",
                pageLength = 12
              )
    ) %>%
      formatStyle("0",
                  backgroundColor = "lightgrey",
                  fontWeight = "bold"
      ) %>%
      formatStyle(names(cohortMatrix[c(-1, -2)]), fontWeight = "bold", color = "white", backgroundColor = styleInterval(breaks, colors))
  })
  
  
  output$companyCustomerCohortRetentionMatrix <- renderDataTable({
    cohortMatrix <- setCohortAnalysisMatrix(getCohortBaseMatrix(sessionCompanyDataInput(), sessionDataAggregationLevel()))
    retentionMatrix <- getCohortRetentionAnalysis(cohortMatrix)
    # Creating 19 breaks and 20 rgb color values ranging from blue to white
    breaks <- quantile(retentionMatrix[, 3:ncol(retentionMatrix)], probs = seq(.05, .95, .05), na.rm = TRUE)
    colors <- sapply(
      round(seq(155, 80, length.out = length(breaks) + 1), 0),
      function(x) {
        rgb(x, x, 155, maxColorValue = 155)
      }
    )
    
    datatable(retentionMatrix,
              class = "cell-border stripe",
              rownames = FALSE,
              options = list(
                ordering = F,
                dom = "t",
                pageLength = 12
              )
    ) %>%
      formatStyle("0",
                  backgroundColor = "lightgrey",
                  fontWeight = "bold"
      ) %>%
      formatStyle(names(retentionMatrix[c(-1, -2)]), fontWeight = "bold", color = "white", backgroundColor = styleInterval(breaks, colors))
  })
  
  
  output$companyTrendPlot <- renderPlotly({
    # initialize updated data for trend plot

    data <- sessionCompanyDataInput() %>%
      group_by(aggregation_start = cut(time_opened, tolower(input$companyDateAggregation))) %>%
      summarise(agregated_subtotal = sum(subtotal_dollar, na.rm = T)) %>%
      mutate(aggregation_start = as.Date(aggregation_start))


    ggplot(data = data, aes(x = aggregation_start, y = agregated_subtotal)) +
      stat_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE) +
      geom_point(size = 2) +
      ggtitle("Aggregated Revenue Over Time") +
      xlab("Date") +
      ylab("Total Revenue") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })
  

  output$companyOrdersSubtotalHist <- renderPlotly({
    # initialize updated data for trend plot
    data <- sessionCompanyDataInput()
    ggplot(data = data, aes(x = "", y = subtotal_dollar)) +
      geom_boxplot(notch = TRUE, width = 0.5, outlier.colour = "red", outlier.shape = 1) +
      ggtitle("Order Subtotal Distribution") +
      xlab(paste0("Meal Type: ", input$companyDropDownSelectMeal)) +
      ylab("Order Subtotal") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })
  

  output$companyOrdersMealTypePieChart <- renderPlotly({
    data <- sessionCompanyDataInput()
    data <- data.frame(table(data$meal_type))
    colnames(data) <- c("meal_type", "freq")
    plot_ly(data, labels = data$meal_type, values = data$freq) %>%
      add_pie(hole = 0.6) %>%
      layout(
        title = "Number of Orders by Meal Type", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })
  

  output$companyCustomerOrderFrequencyBarChart <- renderPlotly({
    # initialize updated data for bar plot output
    data <- sessionCompanyDataInput()
    data <- data.frame(table(data$customer_id))
    colnames(data) <- c("customer_id", "freq")
    data <- data[order(-data$freq), ] %>%
      top_n(10) %>%
      slice(1:10)

    ggplot(data = data, aes(x = reorder(customer_id, -freq), y = freq)) +
      geom_bar(stat = "identity", fill = "red", group = data$customer_id) +
      ggtitle("Customer Total Order Count - TOP 10") +
      xlab("Customer Id") +
      ylab("Order Count") +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.position = "none"
      ) -> P
    plotly::ggplotly(P, height = 480.2, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })

  #################################################################################################
  #                                       REACTIVE DATA UPDATE ELEMENTS                          #
  ###############################################################################################

  sessionDataAggregationLevel <- eventReactive(input$companyDateAggregation, {
    if (input$companyDateAggregation == "Quarter" | input$companyDateAggregation == "Day") {
      return("week")
    } else {
      return(tolower(input$companyDateAggregation))
    }
  })
  # Reactive expressions are smarter than regular R functions.
  # They cache their values and know when their values have become outdated.
  sessionCompanyDataInput <- eventReactive(c(input$companyDateRange, input$companyDropDownSelectMeal), {
    # initialize global server wikipedia data frame
    ORDERS <- sourceOrdersData(dateRange = as.POSIXct(input$companyDateRange, origin = "PST"), mealType = input$companyDropDownSelectMeal)
    return(ORDERS)
  })
  

  # Reactive expressions are smarter than regular R functions.
  # They cache their values and know when their values have become outdated.
  sessionCustomerDataInput <- eventReactive(c(input$customerDateRange, input$customerDropDownSelectMeal), {
    # initialize global server wikipedia data frame
    ORDERS <- sourceOrdersData(dateRange = as.POSIXct(input$customerDateRange, tz = "America/Los Angeles"), mealType = input$customerDropDownSelectMeal)
    return(ORDERS)
  })
  
  
  # Reactive expressions are smarter than regular R functions.
  # They cache their values and know when their values have become outdated.
  sessionDownloadDataInput <- eventReactive(c(input$dataDateRange, input$dataDropDownSelectMeal), {
    # initialize global server wikipedia data frame
    ORDERS <- sourceOrdersData(dateRange = input$dataDateRange, mealType = input$dataDropDownSelectMeal)
    return(ORDERS)
  })
})
