library(rjson)
library(httr)
library(DBI)
base_A=httr::GET("https://paymium.com/api/v1/data/eur/trades?since=1389094259&fbclid=IwAR2sBSmR4C9HOMrucCvfi-T3jTcC4ucraL3Q0dZqEocCJ3gXFSypIruDoy8")
r2<-base_A$content
r3<-rawToChar(r2)
listeA<-rjson::fromJSON(r3)

bid_ask_A=httr::GET("https://paymium.com/api/v1/data/eur/ticker")
r2<-bid_ask_A$content
r3<-rawToChar(r2)
listebidA<-rjson::fromJSON(r3)

base_B=httr::GET("https://api.kraken.com/0/public/AssetPairs?fbclid=IwAR1zWiPqxKcLpjQF7olxYoCUTzaeumvx4M0_dI8La9Dh-89nhvum1KUP7ng")
r2<-base_B$content
r3<-rawToChar(r2)
listeB<-rjson::fromJSON(r3)

bid_ask_B=httr::GET("https://api.kraken.com/0/public/Ticker?pair=xbteur")
r2<-bid_ask_B$content
r3<-rawToChar(r2)
listebidB<-rjson::fromJSON(r3)

ask_A<-listebidA$ask
ask_B<-as.integer(listebidB$result$XXBTZEUR$a[1])
bid_A<-listebidA$bid
bid_B<-as.integer(listebidB$result$XXBTZEUR$b[1])

CompareBidAsk = function(link1,link2)
{
  bid_ask_A=httr::GET(link1)
  r2<-bid_ask_A$content
  r3<-rawToChar(r2)
  listebidA<-rjson::fromJSON(r3)
  
  base_B=httr::GET(link2)
  r2<-base_B$content
  r3<-rawToChar(r2)
  listeB<-rjson::fromJSON(r3)
  
  ask_A<-listebidA$ask
  ask_B<-as.integer(listebidB$result$XXBTZEUR$a[1])
  bid_A<-listebidA$bid
  bid_B<-as.integer(listebidB$result$XXBTZEUR$b[1])
  
  ab = bid_A/ask_B
  ba = bid_B/ask_A
  
  ab2= as.character(ab)
  ba2= as.character(ba)
  
  retour<-paste("bidA/askB = ",ab2)
  if (ab > ba)
  {
    retour<-paste("bidB/askA = ",ba2)
  }
  return (retour)
}

comp<-CompareBidAsk("https://paymium.com/api/v1/data/eur/ticker","https://api.kraken.com/0/public/Ticker?pair=xbteur")
comp

exA=httr::GET("https://api.kraken.com/0/public/OHLC?pair=ADACAD&interval=5")
r2<-exA$content
r3<-rawToChar(r2)
ex5_inter5<-rjson::fromJSON(r3)

con<- dbConnect(RMySQL::MySQL(),
                dbname="blockchain",
                host="localhost",
                port=3306,
                user="root",
                password="root")


data<-as.matrix(ex5_inter5$result$ADACAD)
data2<-data.frame(matrix(unlist(data),nrow=373,byrow=T))
dbWriteTable(conn=con,name='Test1', value= as.data.frame(data2))



XSMA = function(data,period)
{
  add = 0
  for (i in c(1:period))
  {
    add<- add + as.numeric(data[i,5])
  }
  result<-add/period
  return(result)
}

data3<-matrix(unlist(data),nrow=373,byrow=T)

SMA20<-XSMA(data3,20)

YEMA = function(data,period)
{
  multiplier = (2/(period+1))
  prev_ema = 0
  for (i in c(1:period))
  {
    prev_ema<- ((as.numeric(data[i,5])-prev_ema)*multiplier)+prev_ema
  }
  return(prev_ema)
}

EMA50<-YEMA(data3,50)
EMA50

EMA12<--YEMA(data3,12)
EMA26<--YEMA(data3,26)
MACD<-EMA12-EMA26

