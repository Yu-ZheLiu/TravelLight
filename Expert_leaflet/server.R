library(shiny)
library(leaflet)
source('source.R')

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
shinyServer(function(input, output, session) {
  source("tab1.R", local=TRUE)
  source("tab2.R", local=TRUE)

})