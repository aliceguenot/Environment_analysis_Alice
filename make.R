#' environment_analysis: A Research Compendium
#' 
#' @description 
#' Analysis pipeline concerning environmental data on west and southwest coast of Reunion Island.
#' 
#' 
#' @author Baptiste Frattini \email{baptiste.frattini22@gmail.com}
#' 
#' @date 2022/12/30


renv::init()
renv::install()
renv::status()
renv::snapshot()

# make the pipeline
targets::tar_visnetwork()
targets::tar_make()
targets::tar_visnetwork()


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))
