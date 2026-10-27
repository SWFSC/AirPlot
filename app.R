# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file (if in RStudio)

pkgload::load_all(export_all = FALSE, attach_testthat = FALSE)
AirPlot::airplot_gui()
