### Welcome to sDevTools package start up!

# NOTE:The following code is intented to only be run once
library(sDevTools)

# Package metadata

fill_description(pkg_name = "sDataTable", pkg_title = "Dynamic data tables", pkg_description = "Advanced automated data manipulation.",
    author_first_name = "Theo", author_last_name = "Schrock", author_email = "<theorschrock@gmail.com>")

# Package dependencies (IMPORTS)

imports <- c("sUtils", "glue", "stringr", "checkmate", "rlang",'data.table','exprTools','zeallot','R6',"AOunits")
sDevTools::import_pkg(imports)

# Use Git and create a Github repo

sDevTools::create_github_repo()

# initialize testthat and build testing directories

initializeTestthat(test_deps = c("checkmate"))

# Initialize a shiny package if this is a shiny app

is_shiny_app = FALSE

if (is_shiny_app) initializeShinyPackage("sDataTable")

# Start developement

sDevTools::go_to_dev()
