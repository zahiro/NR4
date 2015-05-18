###################################################
library(shiny)
library("ggplot2",      lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.1")             ;
library("doBy",         lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.1")             ;
library("plyr",         lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.1")             ;
library("reldist",      lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.1")             ;
library("latticeExtra", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.1")             ;
### functions definitions:
count.levels  <- function(dataf) {for (i in 1:length(dataf)) { writeLines(paste(length(levels(dataf[,i])),"for",names(dataf)[i]))}  } ;
relsd         <- function(dataf) {sd(dataf)/mean(dataf)}                                                                              ;
geoList       <- c("BR","CA","EG","ES","FR","GB","IN","RU","SA","US")                                                                 ;
browserList   <- c("Chrome","Firefox","MSIE")                                                                                         ;
csList        <- c("300x250","728x90","NoC_S","468x60","160x600","120x600","320x50","800x440")                                       ;
raList        <- c("1300","1303","1304")                                                                                              ;
datums        <- seq(as.Date("2015-04-28"),as.Date("2015-05-04"),"day")                                                               ;
NM <- c("device_type"
        ,"os"
        ,"browser"
        ,"country"
        ,"requested_appearance"
        ,"line_item_id"
        ,"creative_size"
        ,"field_type"
        ,"impressions"
        ,"clicks"
        ,"conversions"
        ,"advertiser_network_revenue"
        ,"publisher_network_revenue"
        ,"date") 
ptt <- proc.time()
NR3 <- function (filename) {
  ## load file
  filepath <- "~/NRs/NR3/" 
  pt <- proc.time()
  setwd(filepath) 
  #filename = "20150505083042_x_nina_ratio 2015-04-28.gz"
  #if (length(paste0("results",gsub("-","_",substr(filename,29,38))) )>0 ) {print("exit") ; return() }
  raw1 <- read.delim(paste0(filepath,filename), header=FALSE) 
  colnames(raw1) <- NM 
  ### filtering results 
  raw <- droplevels(raw1   [ raw1$country %in% geoList &
                             raw1$browser %in% browserList &
                             raw1$creative_size %in% csList &
                             raw1$requested_appearance %in% raList
                           ,]) 
  ### changing RA and LI_ID to factors
  raw$requested_appearance                                                                                           <- factor(raw$requested_appearance) 
  raw$line_item_id                                                                                                   <- factor(raw$line_item_id) 
  
  ### dealing with NULL factors by level names
  levels(raw$device_type)[levels(raw$device_type)==""]                                                               <- 'NoDevice'
  levels(raw$os         )[levels(raw$os)         ==""]                                                               <- 'NoOS'
  levels(raw$browser    )[levels(raw$broswer)    ==""]                                                               <- 'NoBrowser'
  levels(raw$country    )[levels(raw$country)    ==""]                                                               <- 'NoGEO'
  levels(raw$requested_appearance)[levels(raw$requested_appearance)  =="" | is.na(levels(raw$requested_appearance))] <- 'NoR_A'
  levels(raw$line_item_id        )[levels(raw$line_item_id)          =="" | is.na(levels(raw$line_item_id))]         <- 'NoLIID'
  levels(raw$creative_size       )[levels(raw$creative_size)         =="" | is.na(levels(raw$creative_size))]        <- 'NoC_S'

  ### dealing with NULL measures
  raw$clicks                       [is.na(raw$clicks)                    ]                                           <- 0
  raw$conversions                  [is.na(raw$conversions)               ]                                           <- 0
  raw$advertiser_network_revenue   [is.na(raw$advertiser_network_revenue)]                                           <- 0
  raw$publisher_network_revenue    [is.na(raw$publisher_network_revenue) ]                                           <- 0
  
  ### distributing types. imps will be aggregated and remerged with fallbacks and noadds
  fallbacks <-raw[raw$field_type=="f",c(1,2,3,4,5,7,9)]
  noadds    <-raw[raw$field_type=="n",c(1,2,3,4,5,7,9)]
  imps      <-raw[raw$field_type=="i",]
  #####      summaryBy(a ~ b, data = transform(df, b = cut(b, c(-100, -1, 1, 100)))) 
  ### aggregating imps
  
  cdata <- summaryBy(impressions+ clicks + conversions + advertiser_network_revenue ~ country+device_type+os+browser+requested_appearance+creative_size+field_type, data=imps, FUN=c(sum,mean,sd,length,relsd,gini))
  #cdata2 <- summaryBy(impressions ~ country+device_type+os+browser+requested_appearance+creative_size+field_type, data=imps, FUN=length)
  #cdata2 <- summaryBy(impressions  ~ b , data=transform(imps, b = cut(b, c(0, 1000, 10000, 100000, +Inf)))) #, FUN=c(sum,mean,sd,length,relsd,gini))
  #lapplyBy(impressions+ clicks + conversions + advertiser_network_revenue ~ country+device_type+os+browser+requested_appearance+creative_size+field_type, data = imps, cut(0,1000,10000), keep.groupid = FALSE)
  names(fallbacks)[7] <- "fallbacks"
  names(noadds)[7]    <- "noadds"
  
  ### m e r g i n g 
  df <- merge(cdata,fallbacks,by= intersect(names(cdata), names(fallbacks)),all.x=TRUE)
  df <- merge(df   ,noadds   ,by= intersect(names(df   ), names(noadds   )),all.x=TRUE)
  df$fallbacks[is.na(df$fallbacks)] <- 0
  df$noadds   [is.na(df$noadds)   ] <- 0
  names(df)[20]                     <- "countLineItems"
  
  ### add KPIs
  df$NR            <-  df$impressions.sum                   /  (1000*df$countLineItems)
  df$NRfall        <- (df$impressions.sum + df$fallbacks)   /  (1000*df$countLineItems)
  df$delta         <-  df$NRfall                            -  df$NR   
  df$deltaPerc     <-  100 * df$delta                       /  df$NR
  df$MLI           <-  df$fallbacks                         /  (1000*df$NR)
  df               <-  df[,c(-7,-21,-22,-23)]
  df$date          <-  as.Date(substr(filename,29,38))
  print(paste("worked on ",filename))
  print( proc.time() -ptt)
  print( proc.time() -pt)
  assign(paste0("results",gsub("-","_",substr(filename,29,38))),df,envir = .GlobalEnv) ## remove.... DEBUG only
}       ### end of main function NR3

### calling the function on list.files()
filepath <- "~/NRs/NR3/"
setwd(filepath)
flist <- list.files()
flist <- flist[substr(flist,1,4)=="2015"]
a=llply(flist,NR3)                                                            ### TODO:: change names of a and b 
b=rbind(a[[1]],a[[2]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]])
write.csv(b, file = "NR4_data.csv")
#b=rbind(paste0("results",gsub("-","_",substr(flist,29,38))))
#  b=rbind(results2015_04_28,results2015_04_29,results2015_04_30,results2015_05_01,results2015_05_02,results2015_05_03,results2015_05_04)
  
shinyServer(function(input, output) {
  
  output$myPlot <- renderPlot({
    #par(mfrow=c(2,1))
    p=ggplot(data=b[b$os %in% input$myos & b$device_type %in% input$mydevice & b$creative_size %in% input$mycs & b$requested_appearance %in%input$myra  & b$country %in% input$mygeo  ,]
    ### for debugging purposes :
    ### p=ggplot(data=b[b$os=="Windows" & b$device_type=="PC" & b$creative_size=="300x250" & b$requested_appearance=="1300"  & (b$country=="US" | b$country=="US") ,]
             ,aes(date,fallbacks/impressions.sum))  
    p=p + geom_line(aes(color=browser )) #,size=cut(impressions.sum,breaks=c(0,10^3,10^4,10^5,10^6,+Inf)) ))  #+ scale_size_continuous(range=c(4,10))
    p=p + geom_text (aes(color=browser,label = paste("#LI=",countLineItems,"NR=",round(NR,1)," on ",impressions.sum/1000,"Kimps")),hjust=1, vjust=0)#,size=cut(impressions.sum,breaks=c(0,10^3,10^4,10^5,10^6,+Inf)),label = paste(countLineItems,round(NR,1))),hjust=1, vjust=0) 
    p=p + geom_text (aes(color=browser,label = paste("GINI=",round(impressions.gini,2))),hjust=1, vjust=1)#p=p + scale_size_continuous(range=c(4,10))   #aes(color=browser,group=browser,alpha=factor(date),size=factor(cut(impressions.sum,breaks=c(0,10^3,10^4,10^5,10^6,+Inf)) ))
    p=p + facet_grid(creative_size+os ~ country+requested_appearance+device_type, margins = FALSE, scales = "fixed", space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE, drop = TRUE)# + geom_smooth(method="lm",group=browser) # +facet_grid(date~creative_size)+ geom_smooth(method="lm")
    print(p)
     })
  output$mytable <- renderTable({
      data=b[b$os %in% input$myos & b$device_type %in% input$mydevice & b$creative_size %in% input$mycs & b$requested_appearance %in%input$myra  & b$country %in% input$mygeo  ,]
     })
  output$top25 <- renderTable ({
    tempo <- b[b$date==max(b$date),]
    data <- tempo[order(-tempo$impressions.sum),][1:25,]
    
  })
  
})
