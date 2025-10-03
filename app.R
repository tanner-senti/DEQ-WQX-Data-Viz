# Testing ideas for a version of ADEQ WebLIS Viz that pulls directly from WQX

library(shiny)
library(dataRetrieval)
library(dplyr)
library(DBI)
library(duckdb)
library(ggplot2)

# ---- Initialize DuckDB ----
con <- dbConnect(duckdb::duckdb(), dbdir = "wqx_cache.duckdb")

# ---- Site cache ----
get_sites <- function(force_refresh = FALSE) {
  tbl_name <- "sites_cache"

  if (dbExistsTable(con, tbl_name) && !force_refresh) {
    return(dbReadTable(con, tbl_name))
  }

  sites <- whatWQPsites(
    statecode = "AR",
    organization = "ARDEQH2O_WQX",
    sampleMedia = "water",
    sampleMedia = "Water"
  )

  if (nrow(sites) == 0) {
    return(data.frame(id = character(), name = character()))
  }

  df <- data.frame(
    id = sites$MonitoringLocationIdentifier,
    name = paste(
      sites$MonitoringLocationIdentifier,
      sites$MonitoringLocationName
    )
  )

  if (dbExistsTable(con, tbl_name)) {
    dbExecute(con, paste0("DROP TABLE ", tbl_name))
  }
  dbWriteTable(con, tbl_name, df)

  return(df)
}
# ---- Parameter cache ----
get_parameters <- function(force_refresh = FALSE) {
  tbl_name <- "parameters_cache"

  if (dbExistsTable(con, tbl_name) && !force_refresh) {
    return(dbReadTable(con, tbl_name))
  }

  params <- readWQPsummary(
    statecode = "AR",
    organization = "ARDEQH2O_WQX",
    sampleMedia = "Water",
    SampleMedia = "water",
    summaryYears = "all"
  )

  if (nrow(params) == 0) {
    return(data.frame(siteid = character(), characteristic = character()))
  }

  df <- params %>%
    select(MonitoringLocationIdentifier, CharacteristicName) %>%
    rename(
      siteid = MonitoringLocationIdentifier,
      characteristic = CharacteristicName
    ) %>%
    distinct()

  if (dbExistsTable(con, tbl_name)) {
    dbExecute(con, paste0("DROP TABLE ", tbl_name))
  }
  dbWriteTable(con, tbl_name, df)

  return(df)
}

# ---- WQP data cache ----
get_wqx_data <- function(site, start, end, years_to_keep = 10) {
  message("Fetching WQX Result Narrow data for site: ", site)

  tbl_name <- "results"

  start <- as.Date(start)
  end <- as.Date(end)

  # Load cached data for this site
  cached <- if (dbExistsTable(con, tbl_name)) {
    dbReadTable(con, tbl_name) %>%
      mutate(
        Activity_StartDate = as.Date(Activity_StartDate),
        Result_Measure = as.numeric(Result_Measure)
      ) %>%
      filter(siteid == site)
  } else {
    tibble()
  }

  # Determine missing date range
  missing_start <- if (
    nrow(cached) == 0 || start < min(cached$Activity_StartDate)
  ) {
    start
  } else {
    NULL
  }
  missing_end <- if (
    nrow(cached) == 0 || end > max(cached$Activity_StartDate)
  ) {
    end
  } else {
    NULL
  }

  # Pull new data if needed
  new_data <- tibble()
  if (
    !is.null(missing_start) &&
      !is.null(missing_end) &&
      missing_start <= missing_end
  ) {
    # Data grab:
    new_data <- readWQPdata(
      service = "ResultWQX3",
      dataProfile = "narrow",
      siteid = site,
      startDateLo = format(missing_start, "%Y-%m-%d"),
      startDateHi = format(missing_end, "%Y-%m-%d"),
      sampleMedia = "Water",
      sampleMedia = "water"
    )

    # Data cleaning/column standardization (from WQX3 Result Narrow format)
    new_data <- new_data %>%
      select(
        Location_Identifier,
        Activity_StartDate,
        Result_Characteristic,
        Result_Measure,
        Result_MeasureUnit
      ) %>%
      mutate(
        Activity_StartDate = as.Date(Activity_StartDate),
        Result_Measure = as.numeric(Result_Measure),
        siteid = Location_Identifier
      ) %>%
      filter(
        !is.na(Result_Measure)
      )

    # Append new data to DuckDB
    if (nrow(new_data) > 0) {
      if (!dbExistsTable(con, tbl_name)) {
        dbWriteTable(con, tbl_name, new_data)
      } else {
        dbAppendTable(con, tbl_name, new_data)
      }
    }
  }

  # Combine cached + new
  df <- bind_rows(cached, new_data) %>%
    filter(Activity_StartDate >= start & Activity_StartDate <= end)

  # ---- Automatic pruning: keep only last N years of data ----
  if (dbExistsTable(con, tbl_name)) {
    prune_date <- Sys.Date() - (years_to_keep * 365)
    dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        tbl_name,
        " WHERE Activity_StartDate < DATE '",
        prune_date,
        "'"
      )
    )
  }

  return(df)
}


# ---- Shiny App ----
ui <- fluidPage(
  titlePanel("Arkansas WQX Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("site_ui"),
      uiOutput("param_ui"),
      dateRangeInput(
        "dates",
        "Date Range",
        start = "2020-01-01",
        end = Sys.Date()
      ) #,
      # actionButton("go", "Load Data")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # Site dropdown
  output$site_ui <- renderUI({
    sites <- get_sites()
    selectInput("site", "Choose Site", choices = setNames(sites$id, sites$name))
  })

  output$param_ui <- renderUI({
    if (is.null(input$site)) {
      return(NULL)
    }

    params_cache <- get_parameters()
    params <- params_cache %>%
      filter(siteid == input$site) %>%
      pull(characteristic) %>%
      unique() %>%
      sort()

    if (length(params) > 0) {
      selectInput(
        "param",
        "Choose Parameter",
        choices = c("All Parameters", params),
        selected = "All Parameters"
      )
    }
  })

  # Reactive data pull
  # data <- eventReactive(input$go, {
  #   req(input$site, input$dates)
  #   get_wqx_data(
  #     input$site,
  #     format(input$dates[1], "%Y-%m-%d"),
  #     format(input$dates[2], "%Y-%m-%d")
  #   )
  # })
  data <- reactive({
    req(input$site, input$dates)
    get_wqx_data(
      input$site,
      format(input$dates[1], "%Y-%m-%d"),
      format(input$dates[2], "%Y-%m-%d")
    )
  })

  # Plot
  output$plot <- renderPlot({
    df <- data()
    req(nrow(df) > 0)

    # Add this filtering:
    if (!is.null(input$param) && input$param != "All Parameters") {
      df <- df %>% filter(Result_Characteristic == input$param)
    }

    ggplot(
      df,
      aes(
        x = Activity_StartDate,
        y = Result_Measure,
        color = Result_Characteristic
      )
    ) +
      geom_point() +
      labs(x = "Date", y = "Value", title = paste("Results for", input$site)) +
      theme_minimal()
  })

  # Table
  output$table <- renderTable({
    df <- data()
    req(nrow(df) > 0)

    # Add this filtering:
    if (!is.null(input$param) && input$param != "All Parameters") {
      df <- df %>% filter(Result_Characteristic == input$param)
    }

    head(df)
  })
}

shinyApp(ui, server)
