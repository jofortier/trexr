# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "ggplot2" )
usethis::use_package( "raster" )
usethis::use_package('rasterVis')
usethis::use_package( "rglwidget" )
usethis::use_package( "shinyFiles" )
usethis::use_package('mapedit')
usethis::use_package('lidR')
usethis::use_package('sf')
usethis::use_package('plotly')

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = 'in_file')
golem::add_module( name = "panel_stats" ) # Name of the module
golem::add_module( name = "panel_stat_plot" ) # Name of the module
golem::add_module( name = "panel_leaflet" )
golem::add_module( name = "panel_3d" )
golem::add_module(name = 'panel_plot_map')
golem::add_module(name = 'panel_3d_big')
# golem::add_module(name = 'cross_section')
# golem::add_module(name = 'las_select')
# golem::add_module(name = 'panel_tree_detection_stats')
# golem::add_module(name = 'panel_tree_detection_stats_plot')
# golem::add_module(name = 'panel_tree_detection_leaflet')
# golem::add_module(name = 'panel_tree_detection_3d')
# golem::add_module(name = 'trexr_logo')

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" )
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("trexr")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

