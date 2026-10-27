#' Create maps of aerial DAS (AirDAS) data
#'
#' A utility program...
#'
#' @name AirPlot-package
#' @aliases airplot AirPlot
#' @title AirPlot
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://swfsc.github.io/AirPlot/}
#'
#' @import shiny
#' @import CruzPlot
#' @importFrom shinydashboard box dashboardBody tabItems tabItem dashboardHeader
#'   dashboardPage dashboardSidebar sidebarMenu menuItem tabBox
#' @importFrom shinyjs useShinyjs extendShinyjs js
#'
"_PACKAGE"

# https://r-pkgs.org/package-within.html#echo-a-working-package
utils::globalVariables("app_state_save")
