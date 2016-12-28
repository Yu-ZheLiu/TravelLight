library(shiny)
library(leaflet)

shinyUI(
  navbarPage("Tour Expert",
    tabPanel("County",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("county", 
                             label = h3("縣市"), 
                             choices = list("臺北市" = "TaipeiCity", 
                                            "新北市" = "NewTaipei"))
        ),
          mainPanel(
            leafletOutput("mymap1", height="800px"))
      )),
    tabPanel("Region",
      fluidPage(
        fluidRow(
          column(12,
            fluidRow(
              column(4,
                    h3("行政區"),
                    fluidRow(
                      column(6, checkboxGroupInput("TaipeiRegion", 
                                h3("臺北市"),
                                choices = list("中正區" = "Zhongzheng", 
                                              "大同區" = "Datong",
                                              "中山區" = "Zhongshan",
                                              "松山區" = "Songshan",
                                              "大安區" = "Daan",
                                              "萬華區" = "Wanhua",
                                              "信義區" = "Xinyi",
                                              "士林區" = "Shilin",
                                              "北投區" = "Beitou",
                                              "內湖區" = "Neihu",
                                              "南港區" = "Nangang",
                                              "文山區" = "Wenshan"))),
                      column(6, checkboxGroupInput("NewtaipeiRegion", 
                                h3("新北市"),
                                choices = list("八里區" = "Bali",
                                              "三芝區" = "Sanzhi",
                                              "三重區" = "Sanchong",
                                              "三峽區" = "Sanxia",
                                              "土城區" = "Tucheng",
                                              "中和區" = "Zhonghe",
                                              "五股區" = "Wugu",
                                              "平溪區" = "Pingxi",
                                              "永和區" = "Yonghe",
                                              "石門區" = "Shimen",
                                              "石碇區" = "Shiding",
                                              "汐止區" = "Xizhi",
                                              "坪林區" = "Pinglin",
                                              "林口區" = "Linkou",
                                              "板橋區" = "Banqiao",
                                              "金山區" = "Jinshan",
                                              "泰山區" = "Taishan",
                                              "烏來區" = "Wulai",
                                              "貢寮區" = "Gongliao",
                                              "淡水區" = "Tamsui",
                                              "深坑區" = "Shenkeng",
                                              "新店區" = "Xindian",
                                              "新莊區" = "Xinzhuang",
                                              "瑞芳區" = "Ruifang",
                                              "萬里區" = "Wanli",
                                              "樹林區" = "Shulin",
                                              "雙溪區" = "Shuangxi",
                                              "蘆洲區" = "Luzhou",
                                              "鶯歌區" = "Yingge")))
                    )
              ),
              column(7, offset=1,leafletOutput("mymap2", height="800px"))
            )
          )
        )
      )
    )
  )
)