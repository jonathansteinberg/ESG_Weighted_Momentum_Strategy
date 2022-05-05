# Clear Memory            
rm(list = ls()) 

# Set Digit Parameters
options("scipen"=0,"digits"=7) # Default

prc_momentum_construction <- function(J,S,K){
  
  crsp.data <- read.csv(file="/Users/jonathansteinberg/Desktop/R Group Project/CRSP_WITH_ESG_DATASET.csv") # INPUT YOUR OWN FILE PATH HERE
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
  
  ################################################################################
  ##       PART II: SORT STOCKS INTO 10 MOMENTUM BASED PORTFOLIOS        ##
  ################################################################################
  
  ret_month<- array(NA,c(0))
  number_stocks<- array(NA,c(0))
  
  
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
      data_i_holding <- crsp.data[which(crsp.data$yearmonth==unique_months[i-1+z]),]
      ret<- array(0,c(0))
      for (l in 1:10){
        port10_l<- port10[port_decile==l,]
        data_j_l = merge(data_i_holding, port10_l, by="CUSIP")
        fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0,as.numeric(as.character(x)), 0)
        data_j_l$RET = sapply(data_j_l$RET, fn)
        rets = data_j_l$value_wgt * data_j_l$RET
        ret_l = sum(rets)
        ret<- cbind(ret,ret_l)}
      ret_month<- rbind(ret_month,ret)}}
  
  #############################################################################################
  
  FF_factors <- read.table("/Users/jonathansteinberg/Downloads/FF_Factors_New.csv",header=TRUE,sep=",")
  FF_factors$dateff <- substr(as.character(FF_factors$dateff), start=1, stop=6)
  FF_factors <- FF_factors[FF_factors[,1]>=startdate & FF_factors[,1]<=unique_months[nrow(ret_month)+J],]
  #nrow(FF_factors)
  data <- array(0,c(0))
  
  for (i in 1:10){
    
    monthly_returns <- ret_month[,i] 
    
    ##### Descriptive Data ####
    
    mean <- mean(monthly_returns)*12
    vol <- sqrt(var(monthly_returns))*sqrt(12)
    sharpe <- (mean-mean(FF_factors[,ncol(FF_factors)]))/vol
    skew <- mean((monthly_returns-mean(monthly_returns))^3)/(sqrt(var(monthly_returns))^3)
    min <- min(monthly_returns)*12
    first_quartile <- quantile(monthly_returns)[2]*12
    median <- median(monthly_returns)*12
    third_quartile <- quantile(monthly_returns)[4]*12
    max <- max(monthly_returns)*12
    pos <- array(0,c(0))
    neg <- array(0,c(0))
    for (i in monthly_returns){
      if (i>0) {pos <- rbind(pos,i)}
      else {neg <- rbind(neg,i)}}
    num_pos <- length(pos)
    num_neg <- length(neg)
    
    ###### Regressions #####
    
    # CAPM 
    dependent_variable_CAPM <- monthly_returns - FF_factors[,ncol(FF_factors)]
    independent_variable_CAPM <- FF_factors[,2] 
    ols_CAPM <- lm(dependent_variable_CAPM~independent_variable_CAPM)	
    intercept_CAPM <- ols_CAPM$coefficients[1]
    p_value_intercept_CAPM <- summary(ols_CAPM)$coefficients[,4][1]
    slope_CAPM <- ols_CAPM$coefficients[2]
    p_value_slope_CAPM <- summary(ols_CAPM)$coefficients[,4][2]
    fitted_CAPM <- ols_CAPM$fitted.values
    r_squared_CAPM <- var(fitted_CAPM)/var(dependent_variable_CAPM)
    
    # FF3
    dependent_variable_FF3 <- monthly_returns - FF_factors[,ncol(FF_factors)]
    independent_variable_FF3 <- cbind(FF_factors[,2],FF_factors[,3],FF_factors[,4])
    ols_FF3 <- lm(dependent_variable_FF3~independent_variable_FF3)
    intercept_FF3 <- ols_FF3$coefficients[1]
    p_value_intercept_FF3 <- summary(ols_FF3)$coefficients[,4][1]
    slope_FF3 <- ols_FF3$coefficients[2]
    p_value_slope_FF3 <- summary(ols_FF3)$coefficients[,4][2]
    fitted_FF3 <- ols_FF3$fitted.values
    r_squared_FF3 <- var(fitted_FF3)/var(dependent_variable_FF3)
    
    data <- cbind(data,rbind(mean,vol,sharpe,skew,min,first_quartile,median,third_quartile,max,num_pos,num_neg,intercept_CAPM,slope_CAPM,r_squared_CAPM,p_value_intercept_CAPM,p_value_slope_CAPM,intercept_FF3,slope_FF3,r_squared_FF3,p_value_intercept_FF3,p_value_slope_FF3))}
  
  
  ##### Winner Minus Losers ####
  
  monthly_returns <- ret_month[,10]-ret_month[,1]
  
  #### Descriptive Data ####
  
  mean <- mean(monthly_returns)*12
  vol <- sqrt(var(monthly_returns))*sqrt(12)
  sharpe <- (mean-mean(FF_factors[,ncol(FF_factors)]))/vol
  skew <- mean((monthly_returns-mean(monthly_returns))^3)/(sqrt(var(monthly_returns))^3)
  min <- min(monthly_returns)*12
  first_quartile <- quantile(monthly_returns)[2]*12
  median <- median(monthly_returns)*12
  third_quartile <- quantile(monthly_returns)[4]*12
  max <- max(monthly_returns)*12
  pos <- array(0,c(0))
  neg <- array(0,c(0))
  for (i in monthly_returns){
    if (i>0) {pos <- rbind(pos,i)}
    else {neg <- rbind(neg,i)}}
  num_pos <- length(pos)
  num_neg <- length(neg)
  
  ###### Regressions #####
  
  # CAPM 
  dependent_variable_CAPM <- monthly_returns - FF_factors[,ncol(FF_factors)]
  independent_variable_CAPM <- FF_factors[,2] 
  ols_CAPM <- lm(dependent_variable_CAPM~independent_variable_CAPM)	
  intercept_CAPM <- ols_CAPM$coefficients[1]
  slope_CAPM <- ols_CAPM$coefficients[2]
  fitted_CAPM <- ols_CAPM$fitted.values
  r_squared_CAPM <- var(fitted_CAPM)/var(dependent_variable_CAPM)
  p_value_intercept_CAPM <- summary(ols_CAPM)$coefficients[,4][1]
  p_value_slope_CAPM <- summary(ols_CAPM)$coefficients[,4][2]
  
  # FF3
  dependent_variable_FF3 <- monthly_returns - FF_factors[,ncol(FF_factors)]
  independent_variable_FF3 <- cbind(FF_factors[,2],FF_factors[,3],FF_factors[,4])
  ols_FF3 <- lm(dependent_variable_FF3~independent_variable_FF3)
  intercept_FF3 <- ols_FF3$coefficients[1]
  slope_FF3 <- ols_FF3$coefficients[2]
  fitted_FF3 <- ols_FF3$fitted.values
  r_squared_FF3 <- var(fitted_FF3)/var(dependent_variable_FF3)
  p_value_intercept_FF3 <- summary(ols_FF3)$coefficients[,4][1]
  p_value_slope_FF3 <- summary(ols_FF3)$coefficients[,4][2]
  
  data <- cbind(data,rbind(mean,vol,sharpe,skew,min,first_quartile,median,third_quartile,max,num_pos,num_neg,intercept_CAPM,slope_CAPM,r_squared_CAPM,p_value_intercept_CAPM,p_value_slope_CAPM,intercept_FF3,slope_FF3,r_squared_FF3,p_value_intercept_FF3,p_value_slope_FF3))
  
  colnames(data) <- c("port_1","port_2","port_3","port_4","port_5","port_6","port_7","port_8","port_9","port_10","WML")

  return(data)}

combos <- c(
"(3,0,1)","(3,0,3)","(3,0,6)","(3,0,9)","(3,0,12)",
"(6,0,1)","(6,0,3)","(6,0,6)","(6,0,9)","(6,0,12)",
"(9,0,1)","(9,0,3)","(9,0,6)","(9,0,9)","(9,0,12)",
"(12,0,1)","(12,0,3)","(12,0,6)","(12,0,9)","(12,0,12)")

losers <- array(0,c(0))
winners <- array(0,c(0))
WMLs <- array(0,c(0))
for (i in c(3,6,9,12)){
	for (j in c(1,3,6,9,12)){
		losers <- cbind(losers,prc_momentum_construction(i,0,j)[,1])
		winners <- cbind(winners,prc_momentum_construction(i,0,j)[,10])
		WMLs <- cbind(WMLs,prc_momentum_construction(i,0,j)[,11])}}
colnames(losers) <- combos
colnames(winners) <- combos
colnames(WMLs) <- combos
print(losers)
print(winners)
print(WMLs)
write.csv(losers,"/Users/jonathansteinberg/Desktop/R Group Project/PRC_LOSERS.csv", row.names = TRUE)
write.csv(winners,"/Users/jonathansteinberg/Desktop/R Group Project/PRC_WINNERS.csv", row.names = TRUE)
write.csv(WMLs,"/Users/jonathansteinberg/Desktop/R Group Project/PRC_WMLs.csv", row.names = TRUE)














