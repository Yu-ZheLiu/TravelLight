# tab 2 行政區別#
#' 臺北市行政區
Showregion <- eventReactive(input$TaipeiRegion, {
  which(stores$region == input$TaipeiRegion)
})
#' 新北市行政區
Showregion <- eventReactive(input$NewtaipeiRegion, {
  which(stores$region == input$NewtaipeiRegion)
})

#' 臺北市行政區
observeEvent(input$TaipeiRegion, {
  Id2 = Showregion()
  m2 <- leafletProxy('mymap2',session)
  for( i in 1:length(input$TaipeiRegion) )
  {
    subid = which(stores$region == input$TaipeiRegion[i])
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
#' 新北市行政區
observeEvent(input$NewtaipeiRegion, {
  Id3 = Showregion()
  m3 <- leafletProxy('mymap2',session)
  for( i in 1:length(input$NewtaipeiRegion) )
  {
    subid = which(stores$region == input$NewtaipeiRegion[i])
    iconF = stores$region[subid[1]]
    iconF = paste('i_view.png', sep='')
    
    LeafIcon <- makeIcon(
      iconUrl = iconF,
      iconWidth = 18, iconHeight = 18,
      iconAnchorX = 18, iconAnchorY = 18)
    
    lng.path = stores$lan[subid]
    lat.path = stores$lat[subid]
    m3 <- addMarkers(m3, lng=lng.path,lat=lat.path, icon=LeafIcon)
  }
})

output$mymap2 <- renderLeaflet({
  markers711 <- leaflet() %>% 
    addTiles() %>%
    setView(121.5467, 25.05248, zoom = 13)
  markers711
})