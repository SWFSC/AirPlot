#' AirDAS data module
#'
#' Shiny module for AirDAS data
#'
#' @name mod_airdas
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param load_state A [shiny::reactiveValues()] object that allows for a 
#' saved 'app state' to be loaded for this module. The main function
#' saves the state, using the 'to_save' output list, and then loads
#' those values into the `load_state` object when a saved app state is loaded
#' @param color_style reactive of the app color style. Specifically
#'   "1" for Color or "2" for Gray scale
#' @param map_range_config the `config` output of [CruzPlot::mod_map_range_server()]
#'
#' @details
#' This module handles the loading, processing, and plotting of AirDAS data.
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#'
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file.
#'   See [CruzPlot::cruzplot_gui()] for more info.
#' - `das_data`: reactive of the processed DAS data;
#'   the output of [swfscAirDAS::airdas_process()]
#' - `sp_codes`: reactive of a data frame of loaded species codes
#'
#' @export
mod_airdas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabPanel(
      title = "Data",
      fluidRow(
        cruz_box(
          title = "AirDAS Data", width = 12,
          helpText("For details about these parameters, see the ",
                   # actionLink(ns("das_file_help"), "click here"), "or see the official",
                   tags$a(
                     href = "https://swfsc.github.io/swfscAirDAS/reference/airdas_process.html",
                     "swfscAirDAS documentation",
                     target = "_blank"
                   ),
                   " (external link)"),
          # fluidRow(
          #   column(3, tags$h5("Number of lines to skip before reading each file")),
          #   column(3, tags$h5("days.gap argument of das_process()")),
          #   column(3, tags$h5("reset.event argument of das_process()")),
          #   column(3, tags$h5("reset.effort argument of das_process()"))
          # ),
          # fluidRow(
          #   column(3, numericInput(ns("das_file_skip"), NULL,
          #                          value = 0, min = 0)),
          #   column(3, numericInput(ns("das_file_days_gap"), NULL,
          #                          value = 20, min = 0, step = 1)),
          #   column(3, selectInput(ns("das_file_reset_event"), NULL,
          #                         choices = list("TRUE" = 1, "FALSE" = 2), selected = TRUE)),
          #   column(3, selectInput(ns("das_file_reset_effort"), NULL,
          #                         choices = list("TRUE" = 1, "FALSE" = 2), selected = TRUE))
          # ),
          helpText("To load DAS data file(s), first set the desired parameters, and then click the \"Browse...\" button and",
                   "select the file(s) you want to load. Hold the Shift key to select multiple files.",
                   "If you change the parameters, you will have to Browse and select the file(s) again.",
                   "To 'remove' a file, browse again and select only the desired DAS file(s). ",
                   "Or simply uncheck the 'plot' checkbox for sightings or effort. "),
          fileInput(ns("das_file"), tags$h5("DAS file input"), multiple = TRUE),
          textOutput(ns("das_file_load_text")),
          tags$span(textOutput(ns("das_loaded_text")), style = "color: blue;")
        ),
        cruz_box(
          title = "Species codes", width = 12,
          helpText("Processing...",
                   "AirPlot can load the default ",
                   tags$a(
                     href = "https://github.com/SWFSC/swfscAirDAS/blob/main/inst/extdata/SpCodesAirDAS.DAT",
                     "SpCodesAirDAS.DAT",
                     target = "_blank"
                   ),
                   "  from swfscDAS (external link), ",
                   "or you can also load your own species codes file.",
                   "If you load your own file, it must follow the same format ",
                   "as the default SpCodes file; see the manual for details. "),
          fluidRow(
            column(5, fileInput(ns("das_spcodes_file"), tags$h5("Load species codes file"), accept = ".dat")),
            column(
              width = 6, offset = 1,
              tags$br(), tags$br(),
              actionButton(ns("das_spcodes_default"), "Load default species codes")
            )
          ),
          textOutput(ns("spcodes_user_read_text")),
          textOutput(ns("spcodes_default_read_text")),
          tags$span(textOutput(ns("spcodes_message")), style = "color: blue;")
        )
      )
    )    
  )
}


#' @name mod_airdas
#' @export
mod_airdas_server  <- function(id, load_state, color_style, map_range_config) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state),
      is.reactive(color_style)
    )

    # Stored reactiveValues for the module
    cruz.list <- reactiveValues(
      sp.codes = NULL,      # Species code file
      sp.codes.name = NULL, # Either "default" or "personal"
      das.data = NULL       # DAS dataframe
    )

    cruz.eff.leg       <- reactiveVal(FALSE)
    cruz.eff.leg.title <- reactiveVal("")

    # Load state
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          cruz.list[[item$id]] <- item$value
        } else {
          update_widget(item, session)
        }
      }
    }, priority = 10)


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Sp Codes

    spcodes_default_read <- eventReactive(input$das_spcodes_default, {
      cruz.list$sp.codes <- NULL
      cruz.list$sp.codes.name <- NULL
      shinyjs::reset("das_spcodes_file")

      spcodes_path <- system.file("extdata", "SpCodesAirDAS.DAT", package = "swfscAirDAS")
      x <- try(airdas_spcodes_read(spcodes_path), silent = TRUE)
      validate(
        need(x, paste("Error reading the default SpCodes.dat file.",
                      "Please try reinstalling AirPlot and swfscAirDAS"))
      )

      cruz.list$sp.codes <- x
      cruz.list$sp.codes.name <- "default"

      ""
    })

    spcodes_user_read <- eventReactive(input$das_spcodes_file, {
      cruz.list$sp.codes <- NULL
      cruz.list$sp.codes.name <- NULL

      x <- try(das_spcodes_read(input$das_spcodes_file$datapath), silent = TRUE)
      validate(
        need(x, "Error reading the loaded SpCodes.dat file")
      )

      cruz.list$sp.codes <- x
      cruz.list$sp.codes.name <- input$das_spcodes_file$name

      ""
    })

    output$spcodes_user_read_text <- renderText(spcodes_user_read())
    output$spcodes_default_read_text <- renderText(spcodes_default_read())
    output$spcodes_message <- renderText({
      if (req(cruz.list$sp.codes.name) == "default") {
        "The default species code file, 'SpCodes.dat' from swfscAirDAS, is loaded"
      } else {
        paste(
          "The following user-provided species code file is loaded:",
          cruz.list$sp.codes.name
        )
      }
    })


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Loading DAS file(s)

    ### Render DAS load messages
    output$das_file_load_text <- renderText(das_file_load())

    output$das_loaded_text <- renderText({
      req(cruz.list$das.data)
      data.name <- unique(cruz.list$das.data$file_das)
      if (length(data.name) == 1) {
        paste("The following DAS file is loaded:", data.name)
      } else {
        paste("The following DAS files are loaded:",
              paste(data.name, collapse = ", "))
      }
    })

    ### Output flag indicating if DAS data has been loaded
    output$das_loaded_flag <- reactive(isTruthy(cruz.list$das.data))
    outputOptions(output, "das_loaded_flag", suspendWhenHidden = FALSE)

    ### Output flag indicating if SpCodes has been loaded
    output$das_spcodes_loaded_flag <- reactive(isTruthy(cruz.list$sp.codes))
    outputOptions(output, "das_spcodes_loaded_flag", suspendWhenHidden = FALSE)

    ### Read and process DAS file(s)
    das_file_load <- eventReactive(input$das_file, {
      # Clear reactive vals, and reset plot sightings and effort selections
      cruz.list$das.data <- NULL
      cruz.list$das.data.name <- NULL
      # updateCheckboxInput(session, "das_sightings", value = FALSE)
      # updateRadioButtons(session, "das_effort", selected = 1)

      # # Get and check additional parameters
      # skip <- input$das_file_skip
      # days.gap <- input$das_file_days_gap
      # reset.event  <- input$das_file_reset_event == 1
      # reset.effort <- input$das_file_reset_effort == 1
      # # reset.day    <- input$das_file_reset_day == 1

      # v.tmp <- "skip must be a valid, whole number greater than or equal to 0"
      # validate(need(!is.na(skip), v.tmp))
      # validate(need(isTRUE(all.equal(skip %% 1, 0)), v.tmp))
      # validate(need(skip >= 0, v.tmp))
      # rm(v.tmp)

      # v.tmp <- "days.gap must be a valid, whole number greater than or equal to 0"
      # validate(need(!is.na(days.gap), v.tmp))
      # validate(need(isTRUE(all.equal(days.gap %% 1, 0)), v.tmp))
      # validate(need(days.gap >= 0, v.tmp))
      # rm(v.tmp)

      # Process DAS file
      withProgress(message = "Processing DAS file", value = 0.6, {
        das.proc <- try(suppressWarnings(
          airdas_process(
            input$das_file$datapath
            # skip = skip, 
            # days.gap = days.gap,
            # reset.event = reset.event, 
            # reset.effort = reset.effort,
            # reset.days = TRUE
          )
        ), silent = TRUE)
      })

      validate(
        need(isTruthy(das.proc),
             paste0("Error: unable to read and process the provided DAS file(s).",
                    "\n\nThe call to das_read and das_process from the ",
                    "swfscAirDAS package returned the following error. ",
                    "Please use `swfscAirDAS::airdas_check` ",
                    "for a more informative error message.\n\n",
                    attr(das.proc, "condition")))
      )

      # Correct filename
      filename.key <- data.frame(
        tmp = basename(input$das_file$datapath),
        actual = input$das_file$name,
        stringsAsFactors = FALSE
      )

      das.proc <- das.proc  |>
        left_join(filename.key, by = c("file_das" = "tmp")) |>
        mutate(file_das = .data$actual) |>
        select(-.data$actual)

      # Save reactive values
      cruz.list$das.data <- das.proc
      # cruz.list$das.data.name <- input$das_file$name

      ""
    }, ignoreInit = TRUE)


    


    ###############################################################################
    ###############################################################################
    ###############################################################################
    ### Save values
    to_save <- reactive({
      list(
        save_widget("sp.codes", "reactive", cruz.list$sp.codes),
        save_widget("sp.codes.name", "reactive", cruz.list$sp.codes.name),
        save_widget("das.data", "reactive", cruz.list$das.data)
      )
    })

    ### Return values
    list(
      to_save = to_save,
      das_data = reactive(cruz.list$das.data),
      sp_codes = reactive(cruz.list$sp.codes)
      # effort = list(
      #   plot = reactive(isTruthy(cruz.list$das.data) && input$das_effort != "1"),
      #   data = cruzDasEffortFilter,
      #   params = cruzDasEffortParams,
      #   legend_plot = reactive(input$eff_legend),
      #   legend = cruzDasEffortLegend
      # ),
      # sight = list(
      #   plot = reactive(input$das_sightings),
      #   data = cruzDasSightSymbol,
      #   symbol = cruzDasSightSymbol,
      #   legend_plot = reactive(input$das_legend),
      #   legend = cruzDasSightLegend
      # )
    )
  })
}
