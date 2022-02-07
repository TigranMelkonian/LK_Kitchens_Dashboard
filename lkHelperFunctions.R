library(rsconnect)
library(shiny)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(ggplot2)
library(data.table)
library(tidyr)
library(RColorBrewer)
library(pageviews)
library(scales)
library(ggridges)
library(shinythemes)
library(httr)

#########################################################
# Expects: Integer input of order subtotal in cents
# Does: Converts subtotal value from cents to dollars
# Returns: Float value representing dollar subtotal amount
#########################################################
convertCentsToDollar <- function(cents) {
  dollarValue <- cents / 100
  return(dollarValue)
}

#########################################################
# Expects: Pandas dateTime representing order time
# Does: Categorizes order time into one of three meal type categories: Breakfast, Lunch, Dinner
# Returns: Retunrs string value of infered meal type
#########################################################
getMealType <- function(time) {
  curTime <- as.integer(format(as.POSIXct(time), format = "%H"))
  if (curTime >= 4 & curTime < 12) {
    mealType <- "Breakfast"
  }
  else if (curTime >= 12 & curTime < 17) {
    mealType <- "Lunch"
  }
  else if ((curTime >= 17 & curTime <= 24) | (curTime >= 0 & curTime < 4)) {
    mealType <- "Dinner"
  }
  else {
    mealType <- ""
  }
  return(mealType)
}


################################
# INITIALIZE GLOBAL ENVIROMENT #
################################
sourceOrdersData <- function(dateRange, mealType) {
  # load in Order data table
  ORDERS <- read.csv(paste0(getwd(), "/data/sample_orders.csv"), stringsAsFactors = F)
  # convert time_opened to datetime
  ORDERS$time_opened <- as.POSIXct(ORDERS$time_opened, format = "%Y-%m-%d %H:%M:%S", origin = "PST")
  # remove incomplete rows
  ORDERS <- ORDERS[complete.cases(ORDERS), ]
  # append augmented data features - meal_type and subtotal_dollar
  ORDERS$meal_type <- unlist(lapply(ORDERS$time_opened, FUN = getMealType))
  ORDERS$subtotal_dollar <- unlist(lapply(ORDERS$subtotal, convertCentsToDollar))
  ORDERS <- filterOrdersData(ORDERS, dateRange, mealType)
  return(ORDERS)
}

filterOrdersData <- function(rawOrdersData, dateRange, mealType) {
  if (is.null(dateRange) | is.null(mealType)) {
    return(rawOrdersData)
  } else {
    filteredOrdersData <- rawOrdersData %>% filter(
      if (mealType != "All") {
        (time_opened > (dateRange[1]) - 1 & time_opened < dateRange[2] + 1) & (meal_type == mealType)
      } else {
        (time_opened > (dateRange[1]) - 1 & time_opened < dateRange[2] + 1)
      }
    )
  }
  return(filteredOrdersData)
}

setCohortAnalysisMatrix <- function(data) {
  cohort_heatmap_df <- data %>%
    group_by(aquired_cohort, order_cohort) %>%
    summarise(
      customers = n_distinct(customer_id)
    )

  cohort_heatmap_df <- cohort_heatmap_df %>%
    mutate(period = round(difftime(strptime(order_cohort, format = "%Y-%m-%d"),
      strptime(aquired_cohort, format = "%Y-%m-%d"),
      units = "weeks"
    ), 0))


  # create base dataframe for heat map visualization
  cohort_heatmap_df <- cohort_heatmap_df %>%
    select(aquired_cohort, period, customers) %>%
    spread(period, customers)

  cohort_heatmap_df[is.na(cohort_heatmap_df)] <- 0

  return(cohort_heatmap_df)
}


getCohortBaseMatrix <- function(data, period = "week") {
  # Getting the first transaction dates for each customer
  join.date <- aggregate(time_opened ~ customer_id, ORDERS, min, na.rm = TRUE)

  # Changing the name of the column InvoiceDate to Join_Date
  # since this is the first transaction date for each customer
  colnames(join.date)[2] <- "join_date"

  # Merge the Join date data to the cohort2011 data frame
  orderCohorts <- merge(data, join.date, by.x = "customer_id", by.y = "customer_id", all.x = TRUE)

  # Creating the groups/Cohorts based on the join date week
  orderCohorts$aquired_cohort <- format(cut(orderCohorts$join_date, period), format = "%Y-%m-%d")
  orderCohorts$order_cohort <- format(cut(orderCohorts$time_opened, period), format = "%Y-%m-%d")


  return(orderCohorts)
}

getCohortRetentionAnalysis <- function(data) {
  for (i in rev(3:ncol(data))) {
    data[, i] <- round(data[, i] / data[, 2], 4)
  }
  rm(i)

  # Cloning the retention mixpanel
  retention_avgs <- data

  # When calculating the column averages, 0 won't get ignored,
  # which is a problem. Converting these 0 to NAs solves this issue.
  retention_avgs[retention_avgs == 0.0000] <- NA
  avgs_ret <- round(apply(retention_avgs[, -1], 2, mean, na.rm = TRUE), 4)

  # We use the zero because this is a numerical vector
  # Changing it after the merge can't happen due to the
  # factoring of the Cohort labels
  avgs_ret <- c(0, avgs_ret)

  # Adding the averages row to the retention mixpanel
  cohortRetentionMatrix <- rbind(data, avgs_ret)

  cohortRetentionMatrix <- cohortRetentionMatrix[, -ncol(cohortRetentionMatrix)]
  cohortRetentionMatrix$aquired_cohort[nrow(cohortRetentionMatrix)] <- "Average"
  return(cohortRetentionMatrix)
}


update_shinyio <- function() {
  rsconnect::deployApp(paste0(getwd()))
}

###########################################
# Static/Global Date Min/Max Date values #
#########################################
ORDERS <- read.csv(paste0(getwd(), "/data/sample_orders.csv"), stringsAsFactors = F)
ORDERS$time_opened <- as.POSIXct(ORDERS$time_opened, format = "%Y-%m-%d %H:%M:%S")
ORDERS <- ORDERS[complete.cases(ORDERS), ]
ORDERS <- sourceOrdersData(dateRange = NULL, mealType = NULL)
minOrderObservationDate <<- min(ORDERS$time_opened)
maxOrderObservationDate <<- max(ORDERS$time_opened)
uniqueCustomerIds <<- unique(ORDERS$customer_id)
