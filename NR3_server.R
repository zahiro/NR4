

library(shiny)
library("ggplot2", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.1")

options("scipen"=100, "digits"=4)
#########################################################################################
########              s e r v e r    c o d e                         ####################
#########################################################################################

#########################################################################################
########          needed files @home directory                       ####################
#########################################################################################
#environment = environment()
#detach
imps     =read.csv("~/full_cube_NR.csv")
fallback =read.csv("~/fallbacks_cube.csv")
#########################################################################################
########          data manipulation phase - merge                    ####################
#########################################################################################
mydata=merge(imps, fallback, by=c("grouping__id","day","country","device_type","requested_appearance","creative_size"), all.x=TRUE)
names(mydata)[names(mydata)=="impression.x" ] = "impression"
names(mydata)[names(mydata)=="impression.y" ] = "fallback"
mydata$fallback[is.na(mydata$fallback)] = 0
#########################################################################################
########          change nulls to aggregative level                  ####################
#########################################################################################
levels(mydata$day)                      [levels(mydata$day)                 =="NULL"    ] <- "AllDAYS"
levels(mydata$country)                  [levels(mydata$country)             =="NULL"    ] <- "AllGEOS"
levels(mydata$device_type)              [levels(mydata$device_type)         =="NULL"    ] <- "AllDevices"
levels(mydata$requested_appearance)     [levels(mydata$requested_appearance)=="NULL"    ] <- "AllR_A"
levels(mydata$creative_size)            [levels(mydata$creative_size)       =="NULL"    ] <- "AllSIZES"
#########################################################################################
########          KPIs formulation                                   ####################
#########################################################################################
mydata$NR            =  mydata$impression                   /  (1000*mydata$countlineitems)
mydata$NRfall        = (mydata$impression+mydata$fallback)  /  (1000*mydata$countlineitems)
mydata$delta         =  mydata$NRfall - mydata$NR    ##100*(mydata$NRfall/mydata$NR-1)
mydata$deltaPerc     =  100 * mydata$delta/mydata$NR
mydata$MLI           =  mydata$fallback/(1000*mydata$NR)
mydata$grouping__id  =  factor(mydata$grouping__id)
mydata$country       =  factor(mydata$country)

#geo.list = cbind(seq(1:length(levels(mydata$country))),levels(mydata$country))
#environment = environment()
pztheme =  theme(panel.background = element_rect(fill = 'lightblue', colour = 'black'),plot.background = element_rect(fill='lightblue'),
                legend.title=element_text(size=0, face="bold", hjust=0),legend.background=element_rect(fill="lightblue", color='black'),legend.position="top")  
range.breaks = c( 0,1000,10000,100000,1000000,10000000,+Inf)
range.labels = c("0-1000","1000-10K","10K-100K","100K-1M","1M-10M","10M+")

geo.list <<- levels(mydata$country)
device.list <<- levels(mydata$device_type)

# Define server logic required to plot various variables as tabs
  shinyServer(function(input, output) {
    options("scipen"=100, "digits"=4)  ### don't use sci notation. should be @global env.
    #output$geolist = renderPrint({data.frame(geo.list)})
    ### NR tab  ### NR tab  ### NR tab  ### NR tab  ### NR tab  ### NR tab  ### NR tab  ### NR tab  ### NR tab  
    output$myPlotNR        <- renderPlot({
      p = ggplot(mydata[(mydata$country==input$mygeo | mydata$country =="AllGEOS") & (mydata$device_type==input$mydevice | mydata$device_type =="AllDevices" ) &  mydata$grouping__id==input$gid ,],   
                 aes(x=day, y=NR,        stat="identity", color='black', fill= cut(impression,range.breaks,labels=range.labels)) ,   environment = environment()     )   
      p = p +   geom_bar(stat="identity",position = "identity", color = "black") +
        geom_text(aes(label = NR),        size=(8*input$show_figures), parse = TRUE, color = "black", na.rm=TRUE) +
        pztheme +
        scale_fill_brewer()  + 
        #scale_color_brewer() +
        facet_grid(device_type+requested_appearance ~ country+creative_size)
      print(p)                   })
    
    ### NRfall tab  ### NRfall tab  ### NRfall tab  ### NRfall tab  ### NRfall tab  ### NRfall tab  ### NRfall tab  
    output$myPlotNRfall    <- renderPlot({
      p = ggplot(mydata[(mydata$country==input$mygeo | mydata$country =="AllGEOS") & (mydata$device_type==input$mydevice | mydata$device_type =="AllDevices" ) &  mydata$grouping__id==input$gid ,],                    
                 aes(x=day, y=NRfall,    stat="identity", color='black', fill= cut(impression,range.breaks,labels=range.labels)) ,   environment = environment()     )
      p = p +   geom_bar(stat="identity",position = "identity", color = "black") +
        geom_text(aes(label = NRfall),    size=(8*input$show_figures), parse = TRUE, color = "black", na.rm=TRUE) +
        pztheme +
        scale_fill_brewer()  + 
        facet_grid(device_type+requested_appearance ~ country+creative_size)
      print(p)                   })
    
    ### delta tab  ### delta tab  ### delta tab  ### delta tab  ### delta tab  ### delta tab  ### delta tab  
    output$myPlotdelta     <- renderPlot({
      p = ggplot(mydata[(mydata$country==input$mygeo | mydata$country =="AllGEOS") & (mydata$device_type==input$mydevice | mydata$device_type =="AllDevices" ) &  mydata$grouping__id==input$gid ,],                    
                 aes(x=day, y=delta,     stat="identity", color='black', fill= cut(impression,range.breaks,labels=range.labels)) ,   environment = environment()     )  
      p = p +   geom_bar(stat="identity",position = "identity", color = "black") +
        geom_text(aes(label = delta),     size=(8*input$show_figures), parse = TRUE, color = "black", na.rm=TRUE) +
        pztheme +
        scale_fill_brewer()  + 
        facet_grid(device_type+requested_appearance ~ country+creative_size)
      print(p)                   })
    
    ### deltaPerc tab  ### deltaPerc tab  ### deltaPerc tab  ### deltaPerc tab  ### deltaPerc tab  ### deltaPerc tab  
    output$myPlotdeltaPerc <- renderPlot({
      p = ggplot(mydata[(mydata$country==input$mygeo | mydata$country =="AllGEOS") & (mydata$device_type==input$mydevice | mydata$device_type =="AllDevices" ) &  mydata$grouping__id==input$gid ,],
                 aes(x=day, y=deltaPerc, stat="identity", color='black', fill= cut(impression,range.breaks,labels=range.labels)) ,   environment = environment()     )
      p = p   +  geom_bar(stat="identity",position = "identity", color = "black") + 
        geom_text(aes(label = deltaPerc), size=(8*input$show_figures), parse = TRUE, color = "black", na.rm=TRUE) +
        pztheme + 
        scale_fill_brewer()  + 
        facet_grid(device_type+requested_appearance ~ country+creative_size)
      print(p)                   })
    
    ### MLI tab  ### MLI tab  ### MLI tab  ### MLI tab  ### MLI tab  ### MLI tab  ### MLI tab  ### MLI tab  ### MLI tab  
    output$myPlotMLI       <- renderPlot({
      p = ggplot(mydata[(mydata$country==input$mygeo | mydata$country =="AllGEOS") & (mydata$device_type==input$mydevice | mydata$device_type =="AllDevices" ) &  mydata$grouping__id==input$gid ,],                    
                 aes(x=day, y=MLI,       stat="identity", color='black', fill= cut(impression,range.breaks,labels=range.labels)) ,   environment = environment()     )
      p = p +   geom_bar(stat="identity",position = "identity", color = "black") +
        geom_text(aes(label = MLI),       size=(8*input$show_figures), parse = TRUE, color = "black", na.rm=TRUE) +
        pztheme +
        scale_fill_brewer()  + 
        facet_grid(device_type+requested_appearance ~ country+creative_size)
      print(p)  
      print (input$clickId)})
    
    ### table tab  ### table tab  ### table tab  ### table tab  ### table tab  ### table tab  ### table tab  ### table tab  
    output$table <- 
      renderTable(mydata[(mydata$country==input$mygeo| mydata$country =="AllGEOS") & (mydata$device_type==input$mydevice | mydata$device_type =="AllDevices" ) &  mydata$grouping__id==input$gid ,]
                  , options = NULL, searchDelay = 500)#, callback = "function(oTable) {}", escape = TRUE, env = parent.frame(), quoted = FALSE})
  })
