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

  #' 縣市
  Showcounty <- eventReactive(input$county, {
    which(stores$county == input$county)
  })
  #' 行政區
  Showregion <- eventReactive(input$region, {
    which(stores$region == input$region)
  })
  
  #' 縣市
  observeEvent(input$county, {
    Id1 = Showcounty()
    m1 <- leafletProxy('mymap',session) %>% clearMarkers()
    for( i in 1:length(input$county) )
    {
      subid = which(stores$county == input$county[i])
      iconF = stores$county[subid[1]]
      iconF = paste('i_view.png', sep='')
      
      LeafIcon <- makeIcon(
        iconUrl = iconF,
        iconWidth = 18, iconHeight = 18,
        iconAnchorX = 18, iconAnchorY = 18)
      
      lng.path = stores$lan[subid]
      lat.path = stores$lat[subid]
      m1 <- addMarkers(m1, lng=lng.path,lat=lat.path, icon=LeafIcon)
    }
  })
  
  #' 行政區
  observeEvent(input$region, {
    Id2 = Showregion()
    m2 <- leafletProxy('mymap',session) %>% clearMarkers()
    for( i in 1:length(input$region) )
    {
      subid = which(stores$region == input$region[i])
      iconF = stores$region[subid[1]]
      iconF = paste('i_view.png', sep='')
      
      LeafIcon <- makeIcon(
        iconUrl = iconF,
        iconWidth = 18, iconHeight = 18,
        iconAnchorX = 18, iconAnchorY = 18)
      
      lng.path = stores$lan[subid]
      lat.path = stores$lat[subid]
      m2 <- addMarkers(m2, lng=lng.path,lat=lat.path, icon=LeafIcon)
    }
  })

  output$mymap <- renderLeaflet({
    markers711 <- leaflet() %>% 
      addTiles() %>%
      setView(121.5467, 25.05248, zoom = 13)
    markers711
  })

})