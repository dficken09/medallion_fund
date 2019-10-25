# Get historical market cap
library(XML)
get_stock_data<-function(ticker){
  url <- paste0("http://finviz.com/quote.ashx?t=", ticker)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")
  
  # ASSIGN TO STOCK NAMED DFS
  table = readHTMLTable(tableNodes[[9]], stringsAsFactors = FALSE, 
                      header= c("data1", "data2", "data3", "data4", "data5", "data6",
                                "data7", "data8", "data9", "data10", "data11", "data12"))
  
  stock_info_labels=c(c(table$data1),c(table$data3),c(table$data5),c(table$data7),c(table$data9),c(table$data11))
  
  stock_values = c(c(table$data2),c(table$data4),c(table$data6),c(table$data8),c(table$data10),c(table$data12))
  
  stock_info=data.frame(t(stock_values))
  
  colnames(stock_info)<-stock_info_labels
  
  rownames(stock_info)<-ticker
  
  stock_info
}