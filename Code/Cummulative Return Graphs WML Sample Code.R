# Clear Memory 
rm(list = ls()) 

# Set Digit Parameters
options("scipen"=0,"digits"=7) # Default

cumret_graphs <- function(J,S,K){ 
  
  crsp.data <- read.table("/Users/jonathansteinberg/Desktop/R Group Project/CRSP_WITH_ESG_DATASET.csv",header=TRUE,sep=",") # INPUT YOUR OWN FILE PATH HERE
  crsp.data <- na.omit(crsp.data)
  
  #####################################
  ##       GLOBAL PARAMETERS          ##
  ######################################
  
  J<- J
  S<- S
  K<- K
  
  unique_months<- sort(unique(crsp.data$yearmonth))
  index<- 1:length(unique_months)
  
  startdate<- unique_months[J+S+1]
  enddate<- 201910
  
  ################################################################################
  ##       PART II: SORT STOCKS INTO 10 MOMENTUM BASED PORTFOLIOS        ##
  ################################################################################
  
  ret_month_esg<- array(NA,c(0))
  ret_month_prc<- array(NA,c(0))
  number_stocks<- array(NA,c(0))
  
  for (i in seq(index[which(unique_months==startdate)],(length(unique_months)-K),K)){
    
    data_i_minus_js1 <- crsp.data[which(crsp.data$yearmonth==unique_months[i-(J+S)]),] # Data from first month of formation period
    for (m in c((J+S-1):1)){ # For loop to get data from all months in formation period 
      data_i_minus_m<- crsp.data[which(crsp.data$yearmonth==unique_months[i-m]),]
      data_i_temp = merge(data_i_minus_js1, data_i_minus_m, by="CUSIP") 
      data_i_minus_js1 <- data_i_temp}
    data_i <- data_i_temp # Data from all months in formation period
    
    esg_columns <- array(0,c(0))
    for (n in 1:length(colnames(data_i))){
      if (grepl("ESG",colnames(data_i)[n],fixed=TRUE)==TRUE) {esg_columns <- rbind(esg_columns,n)}}
    data_i$momentum <- rowMeans(data_i[,c(esg_columns)]) # Average ESG decile in formation period
    decile = seq(0,1.0,by = 0.1) # Seperate stcoks into deciles based on average ESG decile in formation period 
    
    port10<- data_i          
    port_decile<- cut(port10$momentum,breaks=decile,labels=F,include.lowest=TRUE)
    number_stocks_yearmonth<- array(0,c(0))
    value_wgt<- array(NA,c(nrow(port10),1))
    for (k in 1:10){
      value_wgt[port_decile==k]<- (port10$momentum)[port_decile==k]/sum((port10$momentum)[port_decile==k])
      number_stocks_yearmonth<- cbind(number_stocks_yearmonth,length(value_wgt[port_decile==k]))}
    
    number_stocks<- rbind(number_stocks,number_stocks_yearmonth)
    port10<- cbind(port10,port_decile,value_wgt)
    
    for (z in 1:K){
      data_i_holding <- crsp.data[which(crsp.data$yearmonth==unique_months[i-1+z]),]##### Should there be a rnage of K here????
      ret<- array(0,c(0))
      for (l in 1:10){
        port10_l<- port10[port_decile==l,]
        data_j_l = merge(data_i_holding, port10_l, by="CUSIP")
        fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0,as.numeric(as.character(x)), 0) 
        data_j_l[,8] = sapply(data_j_l[,8], fn)
        rets = data_j_l$value_wgt * data_j_l[,8]
        ret_l = sum(rets)
        ret<- cbind(ret,ret_l)}
      ret_month_esg<- rbind(ret_month_esg,ret)}}
  
  for (i in seq(index[which(unique_months==startdate)],(length(unique_months)-K),K)){
    
    data_i_minus_2<- crsp.data[which(crsp.data$yearmonth==unique_months[i-1]),]
    data_i_minus_12<- crsp.data[which(crsp.data$yearmonth==unique_months[i-(J+S)]),]
    data_i = merge(data_i_minus_2, data_i_minus_12, by="CUSIP")
    data_i$momentum<- abs(data_i$PRC.x)/abs(data_i$PRC.y) - 1
    decile = quantile(data_i$momentum,seq(0,1.0,by=0.1))
    
    port10<- data_i               
    port_decile<- cut(port10$momentum,breaks=decile,labels=F,include.lowest=TRUE)
    number_stocks_yearmonth<- array(0,c(0))
    value_wgt<- array(NA,c(nrow(port10),1))
    for (k in 1:10){
      value_wgt[port_decile==k]<- (port10$ME.x)[port_decile==k]/sum((port10$ME.x)[port_decile==k])
      number_stocks_yearmonth<- cbind(number_stocks_yearmonth,length(value_wgt[port_decile==k]))}
    number_stocks<- rbind(number_stocks,number_stocks_yearmonth)
    port10<- cbind(port10,port_decile,value_wgt)
    
    for (z in 1:K){
      data_i_holding <- crsp.data[which(crsp.data$yearmonth==unique_months[i-1+z]),]##### Should there be a rnage of K here????
      ret<- array(0,c(0))
      for (l in 1:10){
        port10_l<- port10[port_decile==l,]
        data_j_l = merge(data_i_holding, port10_l, by="CUSIP")
        fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0,as.numeric(as.character(x)), 0)
        data_j_l$RET = sapply(data_j_l$RET, fn)
        rets = data_j_l$value_wgt * data_j_l$RET
        ret_l = sum(rets)
        ret<- cbind(ret,ret_l)}
      ret_month_prc<- rbind(ret_month_prc,ret)}}
  
  FF_factors <- read.table("/Users/jonathansteinberg/Downloads/FF_Factors_New.csv",header=TRUE,sep=",")
  FF_factors$dateff <- substr(as.character(FF_factors$dateff), start=1, stop=6)
  FF_factors <- FF_factors[FF_factors[,1]>=startdate & FF_factors[,1]<=unique_months[nrow(ret_month_esg)+J],]

  port_10_esg_return <- ret_month_esg[,10]-ret_month_esg[,1]
  port_10_prc_return <- ret_month_prc[,10]-ret_month_prc[,1]
  market_return <- FF_factors[,2]+FF_factors[,ncol(FF_factors)]
  
  cum_ret_port_10_esg <- array(1,c(length(port_10_esg_return)+1)) 
  for (i in 2:(length(port_10_esg_return)+1)){
    cum_ret_port_10_esg[i]<- cum_ret_port_10_esg[i-1]*(port_10_esg_return[i-1]+1)}
  
  cum_ret_port_10_prc <- array(1,c(length(port_10_prc_return)+1)) 
  for (i in 2:(length(port_10_prc_return)+1)){
    cum_ret_port_10_prc[i]<- cum_ret_port_10_prc[i-1]*(port_10_prc_return[i-1]+1)}
  
  cum_ret_market <- array(1,c(length(market_return)+1)) 
  for (i in 2:(length(market_return)+1)){
    cum_ret_market[i]<- cum_ret_market[i-1]*(market_return[i-1]+1)}
  
  plot(ts(cum_ret_port_10_esg,start=as.numeric(substr(as.character(startdate), start=1, stop=4))+as.numeric(substr(as.character(startdate), start=5, stop=6))/12,frequency=12),xlab=c("Year"),ylab=c("Cumulative Return"),main=c(paste(c(J,0,K))), xlim=c(min(2009+3/12),max(2019+10/12)),ylim=c(0,6.5))
  
  lines(ts(cum_ret_market,start=as.numeric(substr(as.character(startdate), start=1, stop=4))+as.numeric(substr(as.character(startdate), start=5, stop=6))/12,frequency=12), col="black", lty=1,lwd=2)
  lines(ts(cum_ret_port_10_prc,start=as.numeric(substr(as.character(startdate), start=1, stop=4))+as.numeric(substr(as.character(startdate), start=5, stop=6))/12,frequency=12), col="red", lty=1,lwd=2)
  lines(ts(cum_ret_port_10_esg,start=as.numeric(substr(as.character(startdate), start=1, stop=4))+as.numeric(substr(as.character(startdate), start=5, stop=6))/12,frequency=12), col="blue", lty=1,lwd=2)
  legend("topleft",c("Market","Value","ESG"),col=c("black","red","blue"),lty=c(1,1),lwd=2)}

#par(mfrow=c(1,1))
#cumret_graphs(3,0,9)

par(mfrow=c(4,5))
for (i in c(3,6,9,12)){
  for (j in c(1,3,6,9,12)){
    cumret_graphs(i,0,j)}}




