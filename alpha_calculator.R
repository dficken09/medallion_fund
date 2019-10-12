#months =  100
col_skip = 6

source("sector_analyzer.R")

library(downloader)
file <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_TXT.zip"
download(file, dest="F-F_Research_Data_5_Factors_2x3_TXT.zip")
unzip(zipfile="F-F_Research_Data_5_Factors_2x3_TXT.zip")
french_data_start <- read.table("F-F_Research_Data_5_Factors_2x3.txt",
                   skip=3, blank.lines.skip = T, header=TRUE, fill = T)


french_data = french_data_start[c(1:(which(row.names(french_data_start)=="Annual")-1)),]

latest_month = row.names(french_data)[nrow(french_data)]



# display = "Alpha"
# #display = "Mkt Beta"
# # model =  "Five Factor"
# #model = "Three Factor"
# model = "CAPM"
# months = 40
# industry = "Materials"

### Testing ###



create_heatmap = function(industry, model, display, months){
  
  all_data = french_data[c((nrow(french_data)-months):nrow(french_data)),]
  
  all_data$Mkt.RF<-as.numeric(as.character(all_data$Mkt.RF))
  all_data$SMB<-as.numeric(as.character(all_data$SMB))
  all_data$HML<-as.numeric(as.character(all_data$HML))
  all_data$RMW<-as.numeric(as.character(all_data$RMW))
  all_data$CMA<-as.numeric(as.character(all_data$CMA))
  all_data$RF<-as.numeric(as.character(all_data$RF))

  ### match months to individual stock returns
  
  #### Create an alpha and beta for each stock using 3 factor model. Display within heat map and Predicted vs. Actual
  
  sector_data = sector_list[[industry]]
  
  tickers  = sector_data$Ticker
  
  subset_start = rownames(all_data)[1]
  subset_finish = rownames(all_data)[nrow(all_data)]
  subset_total = paste0(substr(subset_start,1,4),"-",substr(subset_start,5,6),"::",substr(subset_finish,1,4),"-",substr(subset_finish,5,6))
  
  for (i in tickers){
    tester = 100*periodReturn(sp_list[[i]][[2]], period = "monthly", subset = subset_total,type = "arithmetic")
    temp_col = data.frame(i=tester)
    colnames(temp_col) = i
    rownames(temp_col) = c()
    if(nrow(temp_col)==nrow(all_data)){
      all_data = cbind.data.frame(all_data,temp_col)
    }
  }
  
  ret_rf = data.frame(row.names=row.names(all_data))
  for (i in (col_skip+1):length(all_data)){
    ret_rf_temp = all_data[,i]-all_data$RF
    ret_rf = cbind.data.frame(ret_rf,ret_rf_temp)
  }
  
  colnames(ret_rf)<-colnames(all_data[(col_skip+1):length(all_data)])
  
  final_df = cbind.data.frame(all_data[,c("Mkt.RF","SMB","HML","RMW","CMA","RF")],ret_rf)
  
  ##### Get averages #####
  
  averages_df <-sapply(final_df, mean)
  
  ##### CAPM model ######
  ##### Three-factor model #####
  ##### Five-factor model #####
  
  capm_temp = NULL
  three_fact_temp = NULL
  five_fact_temp = NULL
  for (i in 1:(length(final_df)-col_skip)){
    capm_temp[i]<-lm(final_df[,(col_skip+i)]~final_df$Mkt.RF)
    three_fact_temp[i]<-lm(final_df[,(col_skip+i)]~final_df$Mkt.RF+final_df$SMB+final_df$HML)
    five_fact_temp[i]<-lm(final_df[,(col_skip+i)]~final_df$Mkt.RF + final_df$SMB + final_df$HML + final_df$RMW + final_df$CMA)
  }
  
  alpha_list=c()
  beta_list=c()
  for (i in 1:(length(all_data)-col_skip)){
    alpha_temp<-as.numeric(capm_temp[i][[1]][1])
    beta_temp<-as.numeric(capm_temp[i][[1]][2])
    
    alpha_list<-c(alpha_list,alpha_temp)
    beta_list<-c(beta_list,beta_temp)
  }
  
  capm_df = rbind.data.frame(alpha_list,beta_list)
  colnames(capm_df)<-colnames(ret_rf)
  row.names(capm_df)<-c("Alpha","Mkt Beta")
  
  act_ret_list = c()
  pred_ret_list = c()
  for(i in names(capm_df)){
    actual_ret_temp = as.numeric(averages_df[[i]])
    pred_ret_temp = capm_df[[i]][2]*as.numeric(averages_df["Mkt.RF"])
    
    act_ret_list = c(act_ret_list,actual_ret_temp)
    pred_ret_list = c(pred_ret_list, pred_ret_temp)
    
  }
  
  capm_rets = rbind.data.frame(act_ret_list,pred_ret_list)
  row.names(capm_rets)<-c("Actual Returns","Predicted Returns")
  colnames(capm_rets)<-colnames(capm_df)
  
  capm_df<-rbind.data.frame(capm_df, capm_rets)
  
  saveRDS(capm_df,"capm_df")
  
  new_alpha_list=c() 
  mkt_beta_list=c()
  smb_beta_list=c()
  hml_beta_list=c()
  for (i in 1:(length(all_data)-col_skip)){
    new_alpha_temp<-as.numeric(three_fact_temp[i][[1]][1])
    mkt_beta_temp<-as.numeric(three_fact_temp[i][[1]][2])
    smb_beta_temp<-as.numeric(three_fact_temp[i][[1]][3])
    hml_beta_temp<-as.numeric(three_fact_temp[i][[1]][4])
    
    new_alpha_list<-c(new_alpha_list,new_alpha_temp)
    mkt_beta_list<-c(mkt_beta_list,mkt_beta_temp)
    smb_beta_list<-c(smb_beta_list,smb_beta_temp)
    hml_beta_list<-c(hml_beta_list,hml_beta_temp)
  }
  
  three_fact_df = rbind.data.frame(new_alpha_list,mkt_beta_list,smb_beta_list, hml_beta_list)
  colnames(three_fact_df)<-colnames(ret_rf)
  row.names(three_fact_df)<-c("Alpha","Mkt Beta","SMB Beta","HML Beta")
  
  
  ##### Returns #####
  
  act_ret_list = c()
  pred_ret_list = c()
  for(i in names(three_fact_df)){
    actual_ret_temp = as.numeric(averages_df[[i]])
    pred_ret_temp = three_fact_df[[i]][2]*as.numeric(averages_df["Mkt.RF"]) + 
      three_fact_df[[i]][3]*as.numeric(averages_df["SMB"])+three_fact_df[[i]][4]*as.numeric(averages_df["HML"])
    
    act_ret_list = c(act_ret_list,actual_ret_temp)
    pred_ret_list = c(pred_ret_list, pred_ret_temp)
    
  }
  
  three_fact_rets = rbind.data.frame(act_ret_list,pred_ret_list)
  row.names(three_fact_rets)<-c("Actual Returns","Predicted Returns")
  colnames(three_fact_rets)<-colnames(three_fact_df)
  
  three_fact_df<-rbind.data.frame(three_fact_df, three_fact_rets)
  
  saveRDS(three_fact_df,"three_fact_df")
  
  
  five_alpha_list = c()
  five_beta_list = c()
  five_smb_list = c()
  five_hml_list = c()
  five_rmw_list = c()
  five_cma_list = c()
  
  for (i in 1:(length(all_data)-col_skip)){
    five_alpha_temp<-as.numeric(five_fact_temp[i][[1]][1])
    five_beta_temp<-as.numeric(five_fact_temp[i][[1]][2])
    five_smb_temp<-as.numeric(five_fact_temp[i][[1]][3])
    five_hml_temp<-as.numeric(five_fact_temp[i][[1]][4])
    five_rmw_temp<-as.numeric(five_fact_temp[i][[1]][5])
    five_cma_temp<-as.numeric(five_fact_temp[i][[1]][6])
    
    five_alpha_list<-c(five_alpha_list,five_alpha_temp)
    five_beta_list<-c(five_beta_list,five_beta_temp)
    five_smb_list<-c(five_smb_list,five_smb_temp)
    five_hml_list<-c(five_hml_list,five_hml_temp)
    five_rmw_list<-c(five_rmw_list,five_rmw_temp)
    five_cma_list<-c(five_cma_list,five_cma_temp)
  }
  
  five_fact_df = rbind.data.frame(five_alpha_list, five_beta_list, five_smb_list, five_hml_list, five_rmw_list, five_cma_list)
  colnames(five_fact_df)<-colnames(ret_rf)
  row.names(five_fact_df)<-c("Alpha","Mkt Beta","SMB Beta","HML Beta","RMW Beta","CMA Beta")
  
  ##### Returns #####
  
  act_ret_list = c()
  pred_ret_list = c()
  for(i in names(five_fact_df)){
    actual_ret_temp = as.numeric(averages_df[[i]])
    pred_ret_temp = five_fact_df[[i]][2]*as.numeric(averages_df["Mkt.RF"]) + 
      five_fact_df[[i]][3]*as.numeric(averages_df["SMB"])+five_fact_df[[i]][4]*as.numeric(averages_df["HML"]) +
      five_fact_df[[i]][5]*as.numeric(averages_df["RMW"])+five_fact_df[[i]][6]*as.numeric(averages_df["CMA"])
    
    act_ret_list = c(act_ret_list,actual_ret_temp)
    pred_ret_list = c(pred_ret_list, pred_ret_temp)
    
  }
  
  five_fact_rets = rbind.data.frame(act_ret_list,pred_ret_list)
  row.names(five_fact_rets)<-c("Actual Returns","Predicted Returns")
  colnames(five_fact_rets)<-colnames(five_fact_df)
  
  five_fact_df<-rbind.data.frame(five_fact_df, five_fact_rets)
  
  saveRDS(five_fact_df,"five_fact_df")
  
  #### turn market cap and book/sh into usable list
  
  ranker = stock_db[,c("Market Cap","Book/sh")]
  ranker_v1 = cbind.data.frame(row.names(stock_db),ranker)
  colnames(ranker_v1)<-c("Ticker","Market Cap","Book/sh")
  row.names(ranker_v1) = c()
  ranker_v2 = ranker_v1[!grepl("^-$",ranker_v1$`Market Cap`),]
  ranker_v3 = ranker_v2[!grepl("^-$",ranker_v1$`Book/sh`),]
  ranker_final = na.omit(ranker_v3)
  ranker_final$Ticker<-as.character(ranker_final$Ticker)
  
  ranker_final$`Market Cap`<-transform_BMK(ranker_final,"Market Cap")
  ranker_final$`Book/sh`<-as.numeric(ranker_final$`Book/sh`)
  row.names(ranker_final)<-c()
  
  #### match up tickers to market cap and book/sh
  
  # ranker_final
  
  tickers = names(ret_rf)
  
  #which(tickers %in% ranker_final$Ticker)
  
  
  side = 0
  cut = 0
  for(i in 1:20){
    if(i^2 <= length(tickers[which(tickers %in% ranker_final$Ticker)])){
      side = i
    }
    cut = length(tickers[which(tickers %in% ranker_final$Ticker)]) - side^2
  }

  #### find cut tickers #### 
  #### Could do some refactoring here ####
  
  ranker = ranker_final[which(ranker_final$Ticker %in% tickers),]
  row.names(ranker)=c()
  
  ord_rank = ranker[c(order(ranker$`Market Cap`)),]
  remove_tickers = head(ord_rank$Ticker,cut)
  
  if (cut!=0){
    ranker_subset = ranker[-which(ranker$Ticker %in% remove_tickers),]
  }else{
    ranker_subset = ranker
  }

  
  ranker_ordered = cbind.data.frame(ranker_subset, tile = ntile(ranker_subset$`Market Cap`,side))
  
  ranker_finished = ranker_ordered[order(ranker_ordered$tile,ranker_ordered$`Book/sh`),]
  
  ticker_ordered = as.character(ranker_finished$Ticker)
  
  #### reorder columns by ticker
  if(model=="CAPM"){
    start = capm_df
  }else if(model =="Five Factor"){
    start = five_fact_df
  }else{
    start = three_fact_df
  }
  
  finish = start[,c(ticker_ordered)]

  data = as.numeric(finish[display,])
  heat_matrix = t(matrix(data,nrow = side, ncol = side))
  
  labels =paste(colnames(finish),round(data,3),sep = "\n")
  
  label_matrix = t(matrix(labels, nrow = side, ncol = side))
  
  lwid = c(0.5,4.5)
  lhei = c(1,4)
  lmat = rbind(4:3,2:1)
  
  colnames(heat_matrix)<-paste0("BM ",1:side)
  rownames(heat_matrix)<-paste0("Size ",1:side)
  
  heatmap.2(heat_matrix,
            cellnote = label_matrix,  # same data set for cell labels
            main = paste0(industry," - ",display," - ", model),
            notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            dendrogram="none",     # only draw a row dendrogram
            key = FALSE,
            Colv="NA",      # turn off column clustering
            Rowv = "NA",     # turn off row clustering
            lwid = lwid,
            lhei = lhei,
            lmat = lmat,
            margins = c(8,8))            
  
}

##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################

display = "Alpha"
#display = "Mkt Beta"
# model =  "Five Factor"
#model = "Three Factor"
model = "CAPM"
months = 40
industry = "Materials"

create_graph = function(industry, model, display, months){
  
  
  capm_df<- readRDS("capm_df")
  three_fact_df<- readRDS("three_fact_df")
  five_fact_df<- readRDS("five_fact_df")
  
  #### reorder columns by ticker
  if(model=="CAPM"){
    start_df = capm_df
  }else if(model =="Five Factor"){
    start_df = five_fact_df
  }else{
    start_df = three_fact_df
  }
  
  finish_df = data.frame(t(start_df))
  
  plot(x=finish_df$Predicted.Returns, y=finish_df$Actual.Returns, main = paste0(model, " Model: Predicted vs. Actual Returns"),
       xlab="Predicted from Betas", ylab="Actual Average Excess Returns")
  text(x=finish_df$Predicted.Returns, y=finish_df$Actual.Returns, labels=row.names(finish_df), pos=3,col = "red")
  abline(a=0,b=1,col="blue")
}

