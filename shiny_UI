library(shiny)

shinyUI(fluidPage(
  list(tags$head(tags$style("body {background-color: lightblue; }"))),
  # Application title
  headerPanel(tags$img(src="http://s29.postimg.org/ffy8qllwj/Ply_Media_Logo.png"), 
              list(tags$head(tags$style("body {background-color: lightblue; }")), "NR Calculations")) ,

  fluidRow(
    column(1,offset=0, selectizeInput("mygeo"    , "country:"               , choices = c("BR","CA","EG","ES","FR","GB","IN","RU","SA","US")                         , options = list(maxItems = 2) ,multiple = TRUE , selected = "US"      ,width='400px')),
    column(1,offset=1, selectizeInput("mycs"     , "creative_size:"         , choices = c("300x250","728x90","NoC_S","468x60","160x600","120x600","320x50","800x440")     , options = list(maxItems = 2) ,multiple = TRUE , selected = "300x250" ,width='400px')),
    column(1,offset=1, selectizeInput("myra"     , "requested_appearance:"  , choices = c("1300","1303","1304")                                                      , options = list(maxItems = 2) ,multiple = TRUE , selected = "1300"    ,width='400px')),
    column(1,offset=1, selectizeInput("myos"     , "os:"                    , choices = c("NoOS","Android","BlackBerry","iOS","Linux","OSX","Symbian","Windows","WindowsPhone")  , options = list(maxItems = 2) ,multiple = TRUE  , selected = "Windows" ,width='400px')),
    column(1,offset=1, selectizeInput("mydevice" , "device_type:"           , choices = c("NoDevice","MediaCenter","PC","Phone","Tablet" )                           , options = list(maxItems = 2) ,multiple = TRUE   , selected = "PC"      ,width='400px'))),
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("NR",        plotOutput("myPlot",         clickId="ClickID", height = 700, width = 1800 )), #,style = "background-color: grey;")),
      tabPanel("tables",    tableOutput("mytable" )), #,style = "background-color: grey;")),
      tabPanel("top25",     tableOutput("top25" ))
    )
  )
)
) 

#selectizeInput(..., options = list(maxItems = 2))
