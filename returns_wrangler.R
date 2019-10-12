# list_of_tickers=c("FB","GOOG","AMZN","AAPL")
# date_range = c(temp[[1]][[2]]$Date[1], temp[[1]][[2]]$Date[nrow(temp[[1]][[2]])])
# plot_val = "PctRet"

returns_wrangler = function(list_of_tickers, date_range,plot_val){
  temp = sp_list[list_of_tickers]
  ts_data = data.frame()
  
  for (i in 1:length(temp)){
    ts_temp = temp[[i]][[2]]
    ts_temp = na.omit(ts_temp)
    ts_temp = ts_temp[ts_temp$Date>=date_range[1],]
    ts_temp = ts_temp[ts_temp$Date<=date_range[2],]
    start_val = ts_temp$Adjusted[1]
    PctRet = (ts_temp$Adjusted - start_val)/start_val
    Ticker = list_of_tickers[i]
    n = nrow(ts_temp)
    
    Returns = c(0,((ts_temp$Adjusted[2:n] - ts_temp$Adjusted[1:(n-1)])/ts_temp$Adjusted[1:(n-1)]))
    ts_temp=cbind.data.frame(Ticker,ts_temp,Returns,PctRet)
    ts_data= rbind.data.frame(ts_data,ts_temp)
    
    
  }
  
  ggplot(data = ts_data, aes_string(x="Date", y=plot_val, colour="Ticker")) + geom_line()

}  

