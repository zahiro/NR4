setwd("~/")

shinyUI(fluidPage(
  list(tags$head(tags$style("body {background-color: lightblue; }"))),
    # Application title
    headerPanel(tags$img(src="http://s29.postimg.org/ffy8qllwj/Ply_Media_Logo.png"), 
      list(tags$head(tags$style("body {background-color: lightblue; }")), "NR Calculations")) ,
                        
#    sidebarLayout(position="left",
#                  sidebarPanel(
                    fluidRow(
                      column(8, sliderInput("gid"     , "select the grouping__id:"           ,     min=0, max=31, value=0 , width = 1200)),
                      column(1, selectInput("mygeo"   , "country :" , choices = geo.list    , selected = "US"  , width = '200px')),
                      column(1, selectInput("mydevice", "device :"  , choices = device.list , selected = "PC"  , width = '200px')),
                      column(1, checkboxInput("show_figures", "Show Figures?", FALSE))),# height = 10, width = 10)),
                  mainPanel(
                    
                    tabsetPanel(
                      
                      tabPanel("NR",        plotOutput("myPlotNR",         clickId="ClickID", height = 700, width = 1800 )), #,style = "background-color: grey;")),
                      tabPanel("NRFall",    plotOutput("myPlotNRfall",     clickId="ClickID", height = 700, width = 1800 )), #,style = "background-color: red;")), #,style = "background-color: grey;")),
                      tabPanel("delta",     plotOutput("myPlotdelta",      clickId="ClickID", height = 700, width = 1800 )), #,style = "background-color: grey;")),
                      tabPanel("deltaPerc", plotOutput("myPlotdeltaPerc",  clickId="ClickID", height = 700, width = 1800 )), #,style = "background-color: grey;")),
                      tabPanel("MLI",       plotOutput("myPlotMLI",        clickId="ClickID", height = 700, width = 1800 )), #,style = "background-color: grey;")),
                      tabPanel("Table", tableOutput("table"))
                    )
                  )
    )
  )
 # )

###  This information will be accessible on the input object using input$clickId. The value will be a named list or vector with x and y elements indicating the mouse position in user units.

### ip="192.168.178.10" # change this!  
### runApp("../xxxxx",host=ip) # change xxxx to the name of your shiny package/app
