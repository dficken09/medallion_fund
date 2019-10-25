
# list_of_tickers=c("FB","GOOG","AMZN","AAPL")
# 
# column_name = "Market Cap"

table_converter<-function(list_of_tickers,column_name){
  tick_idx = which(row.names(stock_db) %in% list_of_tickers)
  compare_info = stock_db[tick_idx,]
  Ticker=row.names(compare_info)
  compare_info<-cbind(Ticker,compare_info)
  column_names = c("Ticker",column_name)
  col_idx = which(colnames(compare_info) %in% column_names)
  compare_info=compare_info[,col_idx]
  # compare_info = compare_info[-which(compare_info[,2]=='-'),]
  compare_info=compare_info[!grepl("-",compare_info[,2]),]
  compare_info[,2] = sapply(gsubfn('\\D',list(B='*1e9', M='*1e6',K='1e3'), compare_info[,2]), function(x) eval(parse(text=x)))
  ggplot(data=compare_info, aes(x=Ticker,y=compare_info[,2],fill=Ticker), environment=environment()) + geom_bar(stat = "identity") + 
  ylab(column_name)
}


