library(dplyr)
library(dataRetrieval)

# Testing dataRetrieval grabs

# Sites:
# This takes ~1 - 8 seconds
# sites <- whatWQPsites(
#   statecode = "AR",
#   organization = "ARDEQH2O_WQX",
#   sampleMedia = "water",
#   sampleMedia = "Water"
# )
# Above provides a list of sites, name/description, coords, etc. but no parameter details

# With Params:
# Takes ~ 15 - 25 seconds
params <- readWQPsummary(
  statecode = "AR",
  organization = "ARDEQH2O_WQX",
  sampleMedia = "Water",
  SampleMedia = "water",
  summaryYears = "all"
)
# Above provides a list of sites, name/descriptions, coords, parameters, and # of samples by year

# Only need the readWQPsummary function to get all unique Site-Parameter combos
# Cleaning
clean_site_param <- params %>%
  select(
    SiteID_WQX = MonitoringLocationIdentifier,
    SiteName_WQX = MonitoringLocationName,
    Parameter = CharacteristicName
  ) %>%
  distinct(SiteID_WQX, SiteName_WQX, Parameter) %>%
  mutate(SiteID = sub(".*-", "", SiteID_WQX))

write.csv(clean_site_param, "clean_sites_params.csv", row.names = FALSE)


# WQX data pull example:
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

output$plot <- renderPlot({
  df <- get_data()
  req(nrow(df) > 0)

  ggplot(
    df,
    aes(
      x = Date,
      y = Result,
      color = Parameter
    )
  ) +
    geom_point() +
    labs(x = "Date", y = "Value", title = paste("Results for", input$site)) +
    theme_minimal()
})
