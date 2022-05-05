# Clear Memory
rm(list = ls()) 

# Load CRSP and ESG Data

crsp.data <- read.table("/Users/jonathansteinberg/Downloads/CRSP_Data.csv",header=TRUE,sep=",") # INPUT YOUR OWN FILE PATH HERE
esg_data <- read.table("/Users/jonathansteinberg/Desktop/R Group Project/esg_deciles_data.csv",header=TRUE,sep=",") # INPUT YOUR OWN FILE PATH HERE


crsp.data$year <- substr(as.character(crsp.data$date), start=1, stop=4)
crsp.data$yearmonth <- substr(as.character(crsp.data$date), start=1, stop=6)
crsp.data$month <- substr(as.character(crsp.data$date), start=5, stop=6)
crsp.data$ME <- crsp.data$SHROUT*crsp.data$PRC

crsp.data <- crsp.data[, c(2,10,11,12,3,4,5,7,9,13,6)]


########## Identify which tickers the ESG dataset and the CRSP dataset have in common ##########

# Define ticker list for each dataset
crsp_tickers <- crsp.data$TICKER # list of all tickers in crsp
esg_tickers <- colnames(esg_data) # list of all tickers in esg
esg_tickers <- esg_tickers[2:length(esg_tickers)] # date column haeder

# Set both groups of tickers as characters
typeof(crsp_tickers)
typeof(esg_tickers)
crsp_tickers <- as.character(crsp_tickers)

# Identify counts of each ticker in each ticker list (each ticker should only appear once in each list)
table(crsp_tickers) # multiple counts per ticker
table(esg_tickers) # one count per ticker
crsp_tickers <- unique(crsp_tickers)
table(crsp_tickers) # now has one count per ticker

# Identify common tickers 
common_tickers <- array(0,c(0)) # tickers that are in both crsp and esg
uncommon_tickers <- array(0,c(0)) # tickers that in crsp but not in esg 
for (ticker in esg_tickers){
	if (ticker %in% crsp_tickers) {common_tickers <- rbind(common_tickers,ticker)} else {uncommon_tickers <- rbind(uncommon_tickers,ticker)}}
	
length(common_tickers)
length(uncommon_tickers)

percent_common <- length(common_tickers)*100/ncol(esg_data)
print(percent_common)

########## Make a common dataframe ##########

crsp_dates <- sort(unique(crsp.data$yearmonth))
crsp_start_date <- crsp_dates[1]
crsp_end_date <- crsp_dates[length(crsp_dates)]

esg_dates <- esg_data[,1]
esg_start_date <- esg_dates[1]
esg_end_date <- esg_dates[length(esg_dates)]

new_data <- array (0, c(0))
for (i in 4:131){
	data <- crsp.data[which(crsp.data$yearmonth==crsp_dates[i]),]
	new_data <- rbind(new_data,data)}
	
new_data$TICKER <- as.character(new_data$TICKER)

ult_data <- array(0,c(0))
esg_tickers_new <- colnames(esg_data)
for (i in 1:length(unique(new_data$yearmonth))){
	for (j in 1:nrow(common_tickers)){
		yearmonth <- unique(new_data$yearmonth)[i]
		print(yearmonth)
		ticker <- common_tickers[j,1]
		#data <- new_data[which(new_data$yearmonth==yearmonth) & which(new_data$TICKER==ticker), ]
		data <- new_data[which(new_data$yearmonth==yearmonth),]
		data <- data[which(data$TICKER==ticker),]
		ESG <- esg_data[i,which(esg_tickers_new==ticker)]
		if (nrow(data)==0) {next}
		data <- cbind(data,ESG)
		ult_data <- rbind(ult_data,data)
	}
}

write.csv(ult_data,"/Users/jonathansteinberg/Desktop/R Group Project/CRSP_WITH_ESG_DATASET.csv", row.names = FALSE)




	













