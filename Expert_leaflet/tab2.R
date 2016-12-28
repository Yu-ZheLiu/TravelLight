# tab 2 行政區別#
#' 行政區
Showregion <- eventReactive(input$region, {
  which(stores$region == input$region)
})
#' 行政區
observeEvent(input$region, {
  Id2 = Showregion()
  m2 <- leafletProxy('mymap2',session) %>% clearMarkers()
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

output$mymap2 <- renderLeaflet({
  markers711 <- leaflet() %>% 
    addTiles() %>%
    setView(121.5467, 25.05248, zoom = 13)
  markers711
})