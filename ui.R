source("lkHelperFunctions.R")

ui <- navbarPage(
  #################################################################################################
  #                                       Main Navpage Body                                      #
  ###############################################################################################
  useShinydashboard(),
  theme = shinytheme("lumen"),
  inverse = F,
  title = tags$header(tags$img(src = "images/localKitchensLogo.jpg", height = 25, width = 100), "Sales Dashboard", tags$style(HTML(".navbar-header .navbar-brand:hover {color: #333333;} .navbar-header .navbar-brand{height: 100%;} .navbar-default .navbar-nav {float: right} .nav-tabs>li>a {padding-left: 12px; padding-right: 12px;} .nav>li>a {border-radius: 5px; margin-right: 2px; margin-left: 2px; margin-top: 2px;} .dropdown-menu>li>a {border-radius: 5px; margin-right: 2px; margin-left: 2px; margin-top: 2px;} #admissionstatus ~ .selectize-control .option:nth-child(1) {color:#F5F5F5;};"))),
  windowTitle = "Local Kitchens Sales Dashboard",
  collapsible = T,
  position = "static-top",
  footer = tags$div(tags$footer(style = "position:relative;width:100%;height:75px;color: grey;background-color: #f8f8f8;margin: 0 auto;border-top: 1px solid #e7e7e7;}")),

  ######################################################
  # Home - Sales Dashboard info Text  & User Briefing #
  ####################################################

  tabPanel(
    title = "Home",
    icon = icon("home", lib = "font-awesome", "fa-1x"),
    style = "min-height: 100vh",
    withTags({
      div(
        class = "header", checked = NA, style = "text-align:center;",
        b(h1("Local Kitchens Sales Analytics")),
        h1("In One Dashboard"),
        br(),
        br()
      )
    }),
    fluidRow(
      width = 12, align = "center",
      withTags({
        div(
          class = "body",
          h4("This dashboard provides a collection of widgets that will help understand LKs sales performance at both a user and company granularity."),
          br(),
          hr()
        )
      })
    ),
    fluidRow(
      width = 12,
      column(
        width = 6,
        fluidRow(
          withTags({
            div(
              h2("COMPANY", style = "text-align:center;margin-bottom:5%")
            )
          }),
          column(
            width = 6,
            withTags({
              div(
                valueBox(
                  subtitle = hr(style = "border-top: 2px solid #c6e9f0; border-radius: 7px 7px 7px 7px;"),
                  width = 12,
                  color = "blue",
                  icon = icon("list-alt", lib = "font-awesome", style = "margin-bottom:55%;"),
                  h4("Summary Statistics", style = "font-size:40px;")
                ),
                br(),
              )
            })
          ),
          column(
            width = 6,
            withTags({
              div(
                valueBox(
                  subtitle = hr(style = "border-top: 2px solid #c6e9f0; border-radius: 7px 7px 7px 7px;"),
                  width = 12,
                  color = "blue",
                  icon = icon("th", lib = "font-awesome", style = "margin-bottom:55%;"),
                  h4("Retention Analysis", style = "font-size:40px;")
                ),
                br(),
              )
            })
          )
        ),
        fluidRow(
          column(
            width = 6,
            offset = 3,
            withTags({
              div(
                valueBox(
                  subtitle = hr(style = "border-top: 2px solid #c6e9f0; border-radius: 7px 7px 7px 7px;"),
                  width = 12,
                  color = "blue",
                  icon = icon("puzzle-piece", lib = "font-awesome", style = "margin-bottom:50%;"),
                  h4("Segmented Analysis", style = "font-size:40px;")
                ),
                style(".valuBox:hover {background-color: red;}"),
                br(),
              )
            })
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          withTags({
            div(
              h2("USER", style = "text-align:center;margin-bottom:5%")
            )
          }),
          column(
            width = 6,
            withTags({
              div(
                valueBox(
                  subtitle = hr(style = "border-top: 2px solid #febbd5; border-radius: 7px 7px 7px 7px;"),
                  width = 12,
                  color = "red",
                  icon = icon("list-alt", lib = "font-awesome", style = "margin-bottom:55%;"),
                  h4("Summary Statistics", style = "font-size:40px;")
                ),
                br(),
              )
            })
          ),
          column(
            width = 6,
            withTags({
              div(
                valueBox(
                  subtitle = hr(style = "border-top: 2px solid #febbd5; border-radius: 7px 7px 7px 7px;"),
                  width = 12,
                  color = "red",
                  icon = icon("funnel-dollar", lib = "font-awesome", style = "margin-bottom:45%;"),
                  h4("Purchasing Overview", style = "font-size:40px;")
                ),
                br(),
              )
            })
          )
        ),
        fluidRow(
          column(
            width = 6,
            offset = 3,
            withTags({
              div(
                valueBox(
                  subtitle = hr(style = "border-top: 2px solid #febbd5; border-radius: 7px 7px 7px 7px;"),
                  width = 12,
                  color = "red",
                  icon = icon("user", lib = "font-awesome", style = "margin-bottom:60%;"),
                  h4("Customer Behavior", style = "font-size:40px;")
                ),
                style(".valuBox:hover {background-color: red;}"),
                br(),
              )
            })
          )
        )
      )
    )
  ),

  ################################################################################################
  # Analytics - Individualized Customer & Company Sales Data Analytic Dashboard Views           #
  ##############################################################################################
  navbarMenu(
    "Analytics",
    icon = icon("chart-line", lib = "font-awesome", "fa-1x"),
    tabPanel(
      "Company",
      style = "min-height:100vh",
      fluidRow(
        column(
          4,
          align = "center",
          dateRangeInput(
            "companyDateRange",
            "Select Date Range",
            start = as.Date(format(minOrderObservationDate, "%Y-%m-%d")),
            end = as.Date(format(maxOrderObservationDate, "%Y-%m-%d")),
            min = as.Date(format(minOrderObservationDate, "%Y-%m-%d")),
            max = as.Date(format(maxOrderObservationDate, "%Y-%m-%d"))
          ),
          tags$style(HTML(".datepicker {z-index:99999 !important;}"))
        ),
        column(
          4,
          align = "center",
          selectizeInput(
            "companyDateAggregation",
            "Select Data Aggregation",
            choices = sort(c("Day", "Week", "Month", "Quarter")),
            selected = "Week",
            options = list(create = F),
            multiple = F
          )
        ),
        column(
          4,
          align = "center",
          selectizeInput(
            "companyDropDownSelectMeal",
            "Select Meal Type",
            choices = sort(c("All", "Breakfast", "Lunch", "Dinner")),
            selected = "All",
            options = list(create = F),
            multiple = F
          )
        )
      ),
      fluidRow(
        column(
          6,
          plotlyOutput("companyTrendPlot", height = "100%"),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        ),
        column(
          6,
          plotlyOutput("companyOrdersSubtotalHist", height = "100%"),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        )
      ),
      fluidRow(
        style = "padding-top: 10%",
        column(
          6,
          plotlyOutput("companyOrdersMealTypePieChart", height = "100%"),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        ),
        column(
          6,
          plotlyOutput("companyCustomerOrderFrequencyBarChart", height = "100%"),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        )
      ),
      hr(),
      fluidRow(
        h3("Retention Analysis"),
        style = "margin-bottom: 10%;",
        column(
          6,
          h4("Customer Cohort Retention Analysis - Raw Count"),
          div(dataTableOutput("companyCustomerCohortMatrixRawCounts"),
            style = "font-size: 85%; width: 100%;"
          ), conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        ),
        column(
          6,
          h4("Customer Cohort Retention Analysis - Percentage"),
          div(dataTableOutput("companyCustomerCohortRetentionMatrix"),
            style = "font-size: 85%; width: 100%;"
          ), conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        )
      )
    ),
    tabPanel(
      "Customer",
      style = "min-height:100vh",
      fluidRow(
        column(
          4,
          align = "center",
          dateRangeInput(
            "customerDateRange",
            "Select Date Range",
            start = as.Date(format(minOrderObservationDate, "%Y-%m-%d")),
            end = as.Date(format(maxOrderObservationDate, "%Y-%m-%d")),
            min = as.Date(format(minOrderObservationDate, "%Y-%m-%d")),
            max = as.Date(format(maxOrderObservationDate, "%Y-%m-%d"))
          ),
          tags$style(HTML(".datepicker {z-index:99999 !important;}"))
        ),
        column(
          4,
          align = "center",
          selectizeInput(
            "customerDropDownSelectMeal",
            "Select Meal Type",
            choices = sort(c("All", "Breakfast", "Lunch", "Dinner")),
            selected = "All",
            options = list(create = F),
            multiple = F
          )
        ),
        column(
          4,
          align = "center",
          selectizeInput(
            "customerDropDownSelectId",
            "Select Customer ID",
            choices = sort(uniqueCustomerIds),
            selected = uniqueCustomerIds[1],
            options = list(create = T),
            multiple = T
          )
        )
      ),
      fluidRow(
        style = "margin-bottom:5%",
        column(
          12,
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center;",
              h2("Customer Orders Summary Statistics Data Frame")
            )
          }),
          div(dataTableOutput("customerOrderSummaryStats"),
            style = "font-size: 85%; width: 100%;"
          )
        )
      ),
      fluidRow(
        style = "margin-bottom: 5%",
        column(
          6,
          plotlyOutput("customerOrdersMealTypePieChart", height = "100%"),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        ),
        column(
          6,
          plotlyOutput("customerOrdersSubtotalHist", height = "100%"),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Loading Plot...", id = "loadmessage")
          )
        )
      )
    ),
    tabPanel(
      "Data",
      style = "min-height:100vh",
      fluidRow(
        column(
          6,
          align = "center",
          dateRangeInput(
            "dataDateRange",
            "Select Date Range",
            start = as.Date(format(minOrderObservationDate, "%Y-%m-%d")),
            end = as.Date(format(maxOrderObservationDate, "%Y-%m-%d")),
            min = as.Date(format(minOrderObservationDate, "%Y-%m-%d")),
            max = as.Date(format(maxOrderObservationDate, "%Y-%m-%d"))
          ),
          tags$style(HTML(".datepicker {z-index:99999 !important;}"))
        ),
        column(
          6,
          align = "center",
          selectizeInput(
            "dataDropDownSelectMeal",
            "Select Meal Type",
            choices = sort(c("All", "Breakfast", "Lunch", "Dinner")),
            selected = "All",
            options = list(create = F),
            multiple = F
          )
        )
      ),
      fluidRow(
        column(
          10,
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center;",
              h2("Orders Summary Statistics Data Frame")
            )
          }),
          div(dataTableOutput("ordersDfSummaryStats"),
            style = "font-size: 85%; width: 100%;"
          )
        ),
        column(
          2,
          fluidRow(
            style = "padding-top:61px;text-align:left;",
            downloadButton("downloadOrdersSummary", "Download")
          )
        )
      ),
      fluidRow(
        column(
          10,
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
              h2("Orders Raw Data Frame")
            )
          }),
          DT::dataTableOutput(outputId = "ordersDfRaw")
        ),
        column(
          2,
          fluidRow(
            style = "padding-top:101px;text-slign:left;",
            downloadButton("downloadOrdersRaw", "Download")
          )
        )
      )
    )
  )
)
