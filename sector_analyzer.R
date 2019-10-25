#### This script is used to organize stocks by sector
library(gsubfn)

sp_list = readRDS("sp_list")
stock_db = readRDS("stock_db")


sector_db=data.frame()

for (i in 1:length(sp_list)){
  temp = cbind.data.frame(sp_list[[i]][[1]]$Symbol,sp_list[[i]][[1]]$`GICS Sector`,sp_list[[i]][[1]]$`GICS Sub Industry`)
  colnames(temp)=c("Symbol","Sector","SubSector")
  sector_db = rbind.data.frame(sector_db,temp)
}

#### separate the stock_db by dataframes of sectors

sector_list = list()

for (i in unique(sector_db$Sector)){
  list_tickers = as.character(sector_db$Symbol[which(sector_db$Sector==i)])
  sector_df = stock_db[which(row.names(stock_db) %in% list_tickers),]
  Ticker = as.character(row.names(sector_df))
  sector_df = cbind(Ticker, sector_df)
  rownames(sector_df)<-c()
  sector_df$Ticker<-as.character(sector_df$Ticker)
  sector_list[[i]]<-sector_df
}

sector_sums = data.frame()

for (i in names(sector_list)){
  Sector = i
  
  ##### Sum ####
  sum_temp = sector_list[[i]]
  MktCap = sum(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='1e3'), sum_temp$`Market Cap`[!grepl("^-$",sum_temp[,"Market Cap"],)]), function(x) eval(parse(text=x))))
  Income = sum(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='1e3'), sum_temp$Income[!grepl("^-$",sum_temp[,"Income"],)]), function(x) eval(parse(text=x))))
  Sales = sum(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='1e3'), sum_temp$Sales[!grepl("^-$",sum_temp[,"Sales"],)]), function(x) eval(parse(text=x))))
  Employees = sum(as.numeric(sum_temp$Employees[!grepl("^-$",sum_temp[,"Employees"],)]))
  AvgVol = sum(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='1e3'), sum_temp[,"Avg Volume"]), function(x) eval(parse(text=x))))
  
  sector_temp = cbind.data.frame(Sector,MktCap,Income,Sales,Employees,AvgVol)
  sector_sums = rbind.data.frame(sector_sums,sector_temp)
  
}


sector_avgs = data.frame()
for (i in names(sector_list)){
  Sector = i
  ##### Avg ####
  mean_temp = sector_list[[i]]
  MktCap = mean(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='*1e3'), mean_temp$`Market Cap`[!grepl("^-$",mean_temp[,"Market Cap"],)]), function(x) eval(parse(text=x))))
  Income = mean(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='*1e3'), mean_temp$Income[!grepl("^-$",mean_temp[,"Income"],)]), function(x) eval(parse(text=x))))
  Sales = mean(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='*1e3'), mean_temp$Sales[!grepl("^-$",mean_temp[,"Sales"],)]), function(x) eval(parse(text=x))))
  Employees = mean(as.numeric(mean_temp$Employees[!grepl("^-$",mean_temp[,"Employees"],)]))
  AvgVol = mean(sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='*1e3'), mean_temp[,"Avg Volume"]), function(x) eval(parse(text=x))))
  
  sector_temp = cbind.data.frame(Sector,MktCap,Income,Sales,Employees,AvgVol)
  sector_avgs = rbind.data.frame(sector_avgs,sector_temp)
}

transform_BMK = function(df, col_name){
  sapply(gsubfn('\\D', list(B='*1e9', M='*1e6',K='*1e3'), df[,col_name][!grepl("^-$",df[,col_name],)]), function(x) eval(parse(text=x)))
}

transform_weighted_BMK = function(df, col_name){
  df = df[,c("Market Cap", col_name)]
  df = df[!grepl("^-$",df[,"Market Cap"]),]
  df = df[!grepl("^-$",df[,col_name]),]
  
  col_2 = sapply(gsubfn('\\D', list(B='*1e9', M = "*1e6", K = "*1e3"), df[,col_name]), function(x) eval(parse(text=x)))
  col_1 = sapply(gsubfn('\\D', list(B='*1e9', M = "*1e6", K = "*1e3"), df[,"Market Cap"]), function(x) eval(parse(text=x)))
  out_v1 = na.omit(cbind.data.frame(col_1,col_2))
  row.names(out_v1)<-c()
  mkt_cap = sum(out_v1$col_1)
  out_v2 = col_1 * col_2
  names(out_v2) = c()
  out_v3 = sum(out_v2)/mkt_cap
  out_v3
}

sector_wtd_avgs = data.frame()
for (i in names(sector_list)){
  Sector = i
  ##### Wtd Avg ####
  wtd_avg_temp = sector_list[[i]]
  MktCap = mean(transform_BMK(wtd_avg_temp,"Market Cap"))
  Income = transform_weighted_BMK(wtd_avg_temp,"Income")
  Sales =  transform_weighted_BMK(wtd_avg_temp,"Sales")
  Employees = transform_weighted_BMK(wtd_avg_temp,"Employees")
  AvgVol = transform_weighted_BMK(wtd_avg_temp,"Avg Volume")

  sector_temp = cbind.data.frame(Sector,MktCap,Income,Sales,Employees,AvgVol)
  sector_wtd_avgs = rbind.data.frame(sector_wtd_avgs,sector_temp)

}



saveRDS(sector_sums, "sector_sums")
saveRDS(sector_avgs,"sector_avgs")
saveRDS(sector_wtd_avgs, "sector_wtd_avgs")


sector_display <- function(display_type, col_name) { 
  if(display_type == "Sum"){
    pie_chart = pie(sector_sums[,col_name], labels = paste0(sector_sums$Sector,"-",
                    round(sector_sums[,col_name]/sum(sector_sums[,col_name])*100),"%"),
                    col = rainbow(length(sector_sums$Sector)), main = paste0(col_name," - ",display_type))
  }else if(display_type == "Average"){
    pie_chart = pie(sector_avgs[,col_name], labels = paste0(sector_avgs$Sector,"-",
                    round(sector_avgs[,col_name]/sum(sector_avgs[,col_name])*100),"%"),
                    col = rainbow(length(sector_avgs$Sector)), main = paste0(col_name," - ",display_type))
  }else{
    pie_chart = pie(sector_wtd_avgs[,col_name], labels = paste0(sector_wtd_avgs$Sector,"-",
                    round(sector_wtd_avgs[,col_name]/sum(sector_wtd_avgs[,col_name])*100),"%"),
                    col = rainbow(length(sector_wtd_avgs$Sector)), main = paste0(col_name," - ",display_type))
  }


pie_chart
}










