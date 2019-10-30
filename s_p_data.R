#setwd("~/Google Drive/Medallion Fund/data_grabber")

# This is my first attempt to grab all of the data for the current S&P 500 companies 
# In order to do so we will use "quantmod package" - and we will store the data in .csv files

library(quantmod)
library(rvest)
library(data.table)
library(finreportr)


### grab get_stock_data function from finviz
source("finviz.R")

### need to get all of the companies currently listed in the S&P first
# used this thread - https://stackoverflow.com/questions/44818212/how-do-i-get-all-sp500-corp-code-list-using-r


url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
SP500 <- SP500[[1]]


symbol_list = SP500$Symbol

sp500 = new.env()

### convert "." to "-"
symbol_list <-unlist(lapply(symbol_list, function(x) gsub("[.]","-",x)))

for (i in symbol_list){

getSymbols(i, env = sp500, from = as.Date("1970-01-01"))
  
}

### merge all of the individual data into one data.table 

## use "MMM" dates for first column in sp_dt

getSymbols("MMM",from = as.Date("1970-01-01"))

MMM <-as.data.table(MMM)
sp_dt = data.table(MMM$index)
colnames(sp_dt)<-"Date"
setkey(sp_dt, Date)

for (i in symbol_list){
  temp = data.frame(mget(i, envir = sp500))
  temp_v2 = as.data.table(data.frame(Date = row.names(temp), coredata(temp)))
  temp_v2$Date<-as.Date(temp_v2$Date)
  setkey(temp_v2, Date)
  
  sp_dt<-merge(sp_dt,temp_v2, by = "Date", all.x = T)
}

#saveRDS(sp_dt, "sp_dt")


### turn into list of lists, titled by the ticker name
sp_list = list()

for (i in 0:(((length(sp_dt)-1)/6)-1)){
  temp = sp_dt[, c(2+6*i):(7+6*i)]
  temp = cbind.data.frame(sp_dt[,1],temp)
  list_name = substr(colnames(temp)[2],1, gregexpr(pattern = "[.]",colnames(temp)[2])[[1]][1][1]-1)
  colnames(temp)<-c("Date","Open","High","Low","Close","Volume","Adjusted")
  header = SP500[which(SP500$Symbol==list_name),]
  list_df<-list(header = header, ts_data = temp)
  
  sp_list[[list_name]][[1]]<- header
  sp_list[[list_name]][[2]]<- temp
  
  }

saveRDS(sp_list, "sp_list")

### get all of the stock info in 1 large dataframe
stock_db = data.frame()
for (i in symbol_list){
  temp = get_stock_data(i)
  stock_db=rbind.data.frame(stock_db, temp)
}


row_n <- row.names(stock_db)

stock_db = data.frame(lapply(stock_db, as.character), stringsAsFactors = F)
row.names(stock_db)<-row_n

### Change Column Names "Market.Cap" and "Avg.Volume" to "Market Cap" and "Avg Volume", respectively
colnames(stock_db)<-gsub("Market.Cap", "Market Cap", colnames(stock_db))
colnames(stock_db)<-gsub("Avg.Volume", "Avg Volume", colnames(stock_db))

### Change Column Names "Book.sh" and "P.E" to "Book/sh" and "P/E", respectively
colnames(stock_db)<-gsub("Book.sh", "Book/sh", colnames(stock_db))
colnames(stock_db)<-gsub("P.E", "P/E", colnames(stock_db))

saveRDS(stock_db,"stock_db")

 # stock_db_list=list()
# stock_db_list[[as.character(Sys.Date())]]=stock_db



