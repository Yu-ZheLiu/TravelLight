# tab 1 縣市別#
#' 縣市
Showcounty <- eventReactive(input$county, {
  which(stores$county == input$county)
})
#' 縣市
observeEvent(input$county, {
  Id1 = Showcounty()
  m1 <- leafletProxy('mymap1',session) %>% clearMarkers()
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

output$mymap1 <- renderLeaflet({
  county1 <- leaflet() %>% 
    addTiles() %>%
    setView(121.5467, 25.05248, zoom = 13)
  county1
})