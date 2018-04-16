#load required libraries
library(googlesheets)
library(plotly)
library(tidyverse)
library(dygraphs)

#get data
gs_ls()

brent<-gs_read(ss = gs_title("Monthly Brent Price"), ws = "data") #brent crude

wti<-gs_read(ss = gs_title("Monthly WTI Price"), ws = "data") #wti crude oil 

Brent.price<-brent$`Europe Brent Spot Price FOB (Dollars per Barrel)` 

WTI.price<-wti$`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`

#combine the bent and wti crude oil prices
Combined_Oil_Price<-as.data.frame(cbind(Brent.price,WTI.price)) 

Combined_Oil_Price["Price Difference"]<-Brent.price- WTI.price

#convert the data into a time series object
Combined_Oil_Price<-ts(Combined_Oil_Price,frequency = 12,start = c(1987,5))

#simple plot of the data
plot(Combined_Oil_Price)

dygraph(Combined_Oil_Price,main = "Monthly Crude Oil Prices")%>% 
  dyRangeSelector()%>%
  dySeries("Brent.price", label = "Brent") %>%
  dySeries("WTI.price", label = "WTI") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

Oil<-as.data.frame(Combined_Oil_Price)%>%
  gather(index,price)#%>%
  mutate(time=time(Combined_Oil_Price))

Time<-Oil$time
PRICE<-Oil$price
INDEX<-Oil$index
plot_ly(Oil,x=time(Combined_Oil_Price),y=PRICE,color = INDEX,type = "scatter",mode="lines")%>%
  layout(Oil,title="Brent and WTI Crude Oil Prices",
         xaxis=list(title="Time"),
         yaxis=list(title="Price in dolars/barrel"))
