# Testing ideas for a version of ADEQ WebLIS Viz that pulls directly from WQX

library(shiny)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(bslib)
library(future)
library(promises)
library(DT)
library(purrr)
library(ggiraph)
future::plan(multisession)

# Load cached site/parameter combos:
cached_file <- "clean_sites_params.csv"
cached_site_param <- if (file.exists(cached_file)) {
  read.csv(cached_file)
} else {
  data.frame()
}

# ---- WQP data cache ----
# get_wqx_data <- function(site, start, end, years_to_keep = 10) {
#   message("Fetching WQX Result Narrow data for site: ", site)
#
#   tbl_name <- "results"
#
#   start <- as.Date(start)
#   end <- as.Date(end)
#
#   # Load cached data for this site
#   cached <- if (dbExistsTable(con, tbl_name)) {
#     dbReadTable(con, tbl_name) %>%
#       mutate(
#         Activity_StartDate = as.Date(Activity_StartDate),
#         Result_Measure = as.numeric(Result_Measure)
#       ) %>%
#       filter(siteid == site)
#   } else {
#     tibble()
#   }
#
#   # Determine missing date range
#   missing_start <- if (
#     nrow(cached) == 0 || start < min(cached$Activity_StartDate)
#   ) {
#     start
#   } else {
#     NULL
#   }
#   missing_end <- if (
#     nrow(cached) == 0 || end > max(cached$Activity_StartDate)
#   ) {
#     end
#   } else {
#     NULL
#   }
#
#   # Pull new data if needed
#   new_data <- tibble()
#   if (
#     !is.null(missing_start) &&
#       !is.null(missing_end) &&
#       missing_start <= missing_end
#   ) {
#     # Data grab:
#     new_data <- readWQPdata(
#       service = "ResultWQX3",
#       dataProfile = "narrow",
#       siteid = site,
#       startDateLo = format(missing_start, "%Y-%m-%d"),
#       startDateHi = format(missing_end, "%Y-%m-%d"),
#       sampleMedia = "Water",
#       sampleMedia = "water"
#     )
#
#     # Data cleaning/column standardization (from WQX3 Result Narrow format)
#     new_data <- new_data %>%
#       select(
#         Location_Identifier,
#         Activity_StartDate,
#         Result_Characteristic,
#         Result_Measure,
#         Result_MeasureUnit
#       ) %>%
#       mutate(
#         Activity_StartDate = as.Date(Activity_StartDate),
#         Result_Measure = as.numeric(Result_Measure),
#         siteid = Location_Identifier
#       ) %>%
#       filter(
#         !is.na(Result_Measure)
#       )
#
#     # Append new data to DuckDB
#     if (nrow(new_data) > 0) {
#       if (!dbExistsTable(con, tbl_name)) {
#         dbWriteTable(con, tbl_name, new_data)
#       } else {
#         dbAppendTable(con, tbl_name, new_data)
#       }
#     }
#   }
#
#   # Combine cached + new
#   df <- bind_rows(cached, new_data) %>%
#     filter(Activity_StartDate >= start & Activity_StartDate <= end)
#
#   # ---- Automatic pruning: keep only last N years of data ----
#   if (dbExistsTable(con, tbl_name)) {
#     prune_date <- Sys.Date() - (years_to_keep * 365)
#     dbExecute(
#       con,
#       paste0(
#         "DELETE FROM ",
#         tbl_name,
#         " WHERE Activity_StartDate < DATE '",
#         prune_date,
#         "'"
#       )
#     )
#   }
#
#   return(df)
# }

# ---- Shiny App ----
ui <- fluidPage(
  titlePanel("Arkansas WQX Data Explorer"),
  # Input Panel
  wellPanel(
    style = "max-width: 800px;",
    fluidRow(
      column(
        6,
        selectizeInput(
          "site_ui",
          "Select Sites:",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Search by SiteID...",
            maxOptions = 10000
          )
        ),
        selectizeInput(
          "param_ui",
          "Choose parameters:",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Search...",
            maxOptions = 10000
          )
        )
      ),
      column(
        6,
        selectizeInput(
          "desc_ui",
          "Search by Description:",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Search by Description...",
            maxOptions = 10000
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "text-align:center; margin-top:15px;",
          actionButton(
            "update_data",
            "Display Data",
            style = "font-size:16px; padding:10px 30px; background-color:#0080b7; color:white; border:none; border-radius:8px;",
            icon = icon("sync")
          )
        )
      )
    )
  ),

  br(),

  mainPanel(
    girafeOutput("plot"),
    plotOutput("plot2"),
    DTOutput("table")
  )
)

server <- function(input, output, session) {
  # Non-blocking ExtendedTask to update sites/parameters in the background
  # update_SiteParam_task <- ExtendedTask$new(function() {
  #   future_promise({
  #     # Query WQX
  #     # Provides a list of sites, name/descriptions, coords, parameters, and # of samples by year
  #     new_sites <- readWQPsummary(
  #       statecode = "AR",
  #       organization = "ARDEQH2O_WQX",
  #       sampleMedia = "Water",
  #       SampleMedia = "water",
  #       summaryYears = "all"
  #     )
  #
  #     # Cleaning to format like cache
  #     clean_new_sites <- new_sites %>%
  #       select(
  #         SiteID_WQX = MonitoringLocationIdentifier,
  #         Parameter = CharacteristicName
  #       ) %>%
  #       distinct(SiteID_WQX, Parameter) %>%
  #       mutate(SiteID = sub(".*-", "", SiteID_WQX))
  #
  #     # Merge with existing
  #     current <- if (file.exists(cached_file)) {
  #       read.csv(cached_file)
  #     } else {
  #       data.frame()
  #     }
  #
  #     # Keep only new site-param combos if there are any
  #     site_additions <- anti_join(
  #       clean_new_sites,
  #       current,
  #       by = c("SiteID", "Parameter")
  #     )
  #
  #     updated <- rbind(current, site_additions) %>% distinct()
  #
  #     write.csv(updated, cached_file, row.names = FALSE)
  #
  #     rm(current, site_additions, new_sites, clean_new_sites)
  #
  #     updated
  #   })
  # })
  #
  # sites_params <- reactive({
  #   # Start with cached data
  #   if (update_SiteParam_task$status() == "success") {
  #     update_SiteParam_task$result() # Use fresh data when ready
  #   } else {
  #     cached_site_param # Use cache while updating
  #   }
  # })

  # # Trigger on app start (non-blocking)
  # update_SiteParam_task$invoke()

  # # Testing, status update message:
  # output$update_status <- renderText({
  #   status <- update_SiteParam_task$status()
  #   if (status == "running") {
  #     "Updating sites list..."
  #   } else if (status == "success") {
  #     "✓ Sites updated"
  #   } else {
  #     ""
  #   }
  # })

  # Loading cache for now, can add in ExtendedTasking later
  # Keep this reactive for future updates
  sites_params <- reactive({
    cached_site_param
  })

  ################
  # Step one: populate site and description dropdowns
  ################
  observe({
    updateSelectizeInput(
      session,
      "site_ui",
      choices = sites_params()$SiteID,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "desc_ui",
      choices = sites_params()$SiteName_WQX,
      server = TRUE
    )
  })

  ##############
  # Step two: sync sites/desc
  # and dynamically update parameter options
  ###################################################################
  # --- Sync sites <-> descriptions ---
  update_selects <- reactiveVal(FALSE)

  # Here, updating the descriptions box when a site input is observed:
  observeEvent(
    input$site_ui,
    {
      if (update_selects()) {
        return()
      }

      current_sites <- sites_params() %>%
        filter(SiteName_WQX %in% input$desc_ui) %>%
        pull(SiteID)
      if (setequal(input$site_ui, current_sites)) {
        return()
      }
      update_selects(TRUE)

      sel_desc <- sapply(input$site_ui, function(site) {
        sites_params() %>%
          filter(SiteID == site) %>%
          pull(SiteName_WQX) %>%
          head(1)
      })
      updateSelectizeInput(
        session,
        "desc_ui",
        selected = unique(sel_desc)
      )
      later::later(function() update_selects(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # Here, updating the sites box when a description input is observed:
  observeEvent(
    input$desc_ui,
    {
      if (update_selects()) {
        return()
      }
      current_descs <- sites_params() %>%
        filter(SiteID %in% input$site_ui) %>%
        pull(SiteName_WQX)
      if (setequal(input$desc_ui, current_descs)) {
        return()
      }
      update_selects(TRUE)

      sel_sites <- sapply(input$desc_ui, function(desc) {
        sites_params() %>%
          filter(SiteName_WQX == desc) %>%
          pull(SiteID)
      })

      updateSelectizeInput(
        session,
        "site_ui",
        selected = unique(unlist(sel_sites))
      )
      later::later(function() update_selects(FALSE), delay = 0.05)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # --- Dynamic parameter list based on selected sites ---
  available_params <- reactive({
    req(input$site_ui)
    sites_params() %>%
      filter(SiteID %in% input$site_ui) %>%
      distinct(Parameter) %>%
      pull(Parameter)
  })

  observe({
    new_params <- sort(available_params())
    current_params <- isolate(input$param_ui)
    valid_params <- intersect(current_params, new_params)
    updateSelectizeInput(
      session,
      "param_ui",
      choices = new_params,
      selected = valid_params,
      server = TRUE
    )
  })

  #############
  # Step 3: using selected SiteIDs and Parameters,
  # grab the data from WQX 3.0 and CLEAN
  #########################################################################
  get_data <- eventReactive(input$update_data, {
    sites <- isolate(input$site_ui)
    parameters <- isolate(input$param_ui)
    req(sites, parameters)
    # Can also add a dates requirement here

    # For more complicated stuff (caching), can make this a function
    # available globally at the top.

    # For now, build the WQX pull here:

    # Grab list of parameters, and WQX style names:
    sites_wqx <- sites_params() %>%
      filter(SiteID %in% sites) %>%
      distinct(SiteID_WQX) %>%
      pull(SiteID_WQX)

    # New looped Data grab to chunk by selected site:
    new_data <- map_dfr(sites_wqx, function(site) {
      message("Fetching WQX Result Narrow data for sites: ", site)
      tryCatch(
        {
          data <- readWQPdata(
            service = "ResultWQX3",
            sampleMedia = "Water",
            sampleMedia = "water",
            dataProfile = "narrow",
            siteid = site,
            characteristicName = parameters
            # startDateLo = format(missing_start, "%Y-%m-%d"),
            # startDateHi = format(missing_end, "%Y-%m-%d"),
          )
          if (nrow(data) > 0) {
            data$Result_Measure <- as.character(data$Result_Measure)
          }
          return(data)
        },
        error = function(e) {
          message("Error fetching site ", site, ": ", e$message)
          return(NULL)
        }
      )
    })

    # Data cleaning/column standardization (from WQX3 Result Narrow format)
    new_data <- new_data %>%
      select(
        Location_Identifier,
        Date = Activity_StartDate,
        Parameter = Result_Characteristic,
        Result = Result_Measure,
        Unit = Result_MeasureUnit,
        Fraction = Result_SampleFraction,
        DetectionLimit = Result_ResultDetectionCondition,
        Qualifiers = Result_MeasureQualifierCode,
        Depth = ResultDepthHeight_Measure,
        Depth_unit = ResultDepthHeight_MeasureUnit
      ) %>%
      mutate(
        Date = as.Date(Date),
        Result = as.numeric(Result),
        SiteID = sub(".*-", "", Location_Identifier),
        `Parameter (units)` = paste0(Parameter, " (", Unit, ")"),
        `Depth (units)` = paste0(Depth, " (", Depth_unit, ")"),
        Qualifiers = if_else(
          is.na(Qualifiers) | Qualifiers == "",
          "None",
          Qualifiers
        ),
        Depth = if_else(
          is.na(trimws(Depth)) |
            trimws(Depth) == "",
          "surface",
          tolower(trimws(Depth))
        ),
        DetectionLimit = if_else(
          is.na(DetectionLimit) | DetectionLimit == "",
          "Measured Value",
          DetectionLimit
        )
      ) %>%
      mutate(
        legend_shape = case_when(
          Qualifiers != "None" ~ "Qualifier",
          TRUE ~ DetectionLimit
        )
      ) %>%
      mutate(across(c(Qualifiers, DetectionLimit), as.factor)) %>%
      filter(
        !is.na(Result)
      ) %>%
      mutate(SiteParam = paste(SiteID, Parameter, sep = " - "))

    return(new_data)
  })

  ###############
  #Step 4: Plotting and table output
  #####################################################################

  # PLOT
  make_plot <- function(selected_plot_dat) {
    use_size <- any(selected_plot_dat$RelativeDepthComments != "surface")

    # Create a uniqueID for interactive plot features:
    selected_plot_dat <- selected_plot_dat %>%
      mutate(.point_id = seq_len(n()))

    # Determine faceting
    facet_choice <- input$facet_type
    if (is.null(facet_choice)) {
      facet_choice <- "Site"
    }

    # Set facet and color variable for legend
    if (facet_choice == "Parameter") {
      color_var <- "SiteID" # Legend shows Site
      facet_formula <- ~Parameter
      color_label <- "Site"
    } else {
      color_var <- "Parameter" # Legend shows Parameter
      facet_formula <- ~SiteID
      color_label <- "Parameter"
    }

    # Compute Y-axis limits for all data (when using fixed y scale):
    y_range <- range(selected_plot_dat$Result, na.rm = TRUE)
    y_buffer <- diff(y_range) * 0.15 # Add 15% buffer
    y_range <- c(y_range[1] - y_buffer, y_range[2] + y_buffer)

    # Base aesthetics
    base_aes <- aes(
      x = Date,
      y = Result,
      tooltip = paste0(
        "Site: ",
        SiteID,
        "<br>Parameter: ",
        Parameter,
        "<br>Date: ",
        Date,
        "<br>Result: ",
        Result,
        "<br>Qualifier: ",
        Qualifiers,
        "<br>Depth: ",
        `Depth (units)`
      ),
      shape = legend_shape,
      data_id = .point_id # Can use this for interacive effects
    )

    # Color aesthetic only affects legend, actual grouping by SiteParam
    p <- ggplot(selected_plot_dat, base_aes) +
      {
        if (use_size) {
          geom_point_interactive(
            aes(
              size = Depth,
              color = .data[[color_var]],
              group = SiteParam
            ),
            alpha = 0.7
          )
        } else {
          geom_point_interactive(
            aes(color = .data[[color_var]], group = SiteParam),
            alpha = 0.7,
            size = 2.5
          )
        }
      } +
      scale_shape_manual(
        values = c(
          "Measured value" = 16,
          # "<DL" = 25,
          # ">DL" = 24,
          "Qualifier" = 5
        )
      ) +
      {
        if (use_size) {
          scale_size_manual(
            values = c(
              "epilimnion" = 2.5,
              "hypolimnion" = 5.5,
              "thermocline" = 4,
              "mid-depth" = 4,
              "surface" = 2.5
            ),
            name = "Relative Depth",
            drop = TRUE
          )
        }
      } +
      theme_classic(base_size = 11) +
      labs(
        x = "Date",
        y = "Result",
        color = color_label,
        shape = "Values"
      ) +
      scale_color_discrete(labels = function(x) {
        stringr::str_wrap(x, width = 25)
      }) +
      guides(
        color = guide_legend(
          label.hjust = 0,
          ncol = if (length(unique(selected_plot_dat[[color_var]])) > 8) {
            2
          } else {
            1
          },
          byrow = TRUE
        ),
        shape = guide_legend(label.hjust = 0, ncol = 1, byrow = TRUE)
      ) +
      #ggtitle("Multiple Sites & Parameters") +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        strip.text = element_text(size = 9, lineheight = 1)
      ) +
      coord_cartesian(ylim = y_range) + # fixed scale without clipping
      facet_wrap(
        facet_formula,
        scales = "fixed",
        labeller = labeller(.default = label_wrap_gen(width = 20))
      )

    # --- LOESS Trend line logic (non-interactive) ---
    # if (input$loess_trend4) {
    #   loess_dat <- selected_plot_dat %>%
    #     group_by(SiteParam, SamplingPoint, WebParameter) %>%
    #     filter(n() >= 8) %>%
    #     ungroup()
    #
    #   if (nrow(loess_dat) > 0) {
    #     p <- p +
    #       geom_smooth(
    #         data = loess_dat,
    #         method = "loess",
    #         span = 0.8,
    #         se = TRUE,
    #         inherit.aes = FALSE,
    #         aes(
    #           x = DateSampled,
    #           y = FinalResult,
    #           group = SiteParam,
    #           color = .data[[color_var]]
    #         ),
    #         alpha = 0.2,
    #         linetype = "dashed"
    #       )
    #   }
    # }

    return(p)
  }

  output$plot <- renderGirafe({
    dat <- get_data()

    req(nrow(dat) > 0)

    suppressMessages(girafe(
      ggobj = make_plot(dat),
      options = list(
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    ))
  })

  output$plot2 <- renderPlot({
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

  # # Table
  output$table <- renderDT({
    df <- get_data() #%>%
    # select(
    #   SiteID,
    #   Date,
    #   Parameter,
    #   Result,
    #   Unit,
    #   Fraction,
    #   DetectionLimit,
    #   Location_Identifier
    # )
    req(nrow(df) > 0)
    return(df)
  })
}

shinyApp(ui, server)
