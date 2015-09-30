export PATH="/software/R-3.1.2/bin:$PATH"		#for the compatible version of R

#valid symbols
symbols<- c('MYL','ZTS','AKRX','ALXN','AMGN','ARIA','ARWR','BIIB','BMRN','CELG','DYAX','EXAS','GILD','HALO','ILMN','INCY','KERX','MDVN','MACK','NLNK','VRTX','SGEN','BMY','JNJ','LLY','ABBV','VRX','ALKS','MRK','NVS','PFE','AGN','TEVA','NVO','AGN','ENDP','ISIS','AET','ANTM','HUM','CI','UNH','CVS','SYK','BDX','BAX','LH','TMO','DGX','A','OCRX','DVA','TSLA','GM','ALSN','AXL','DLPH','BWA','JCI','GNTX','TAP','BUD','PEP','KO','DPS','MNST','CCE','STZ-B','PAY','PBI','HSY','ECL','AAPL','BG','ADM','KHC','FBHS','LEG','TSN','BRFS','BLL','BMS','IP','OI','AVP','FBR','BERY','SEE','KMB','NUS','PG','GPRO','CAG','CPB','GMCR','K','GIS','MJN','BC','KORS','HOG','KATE','LULU','UA','VFC','FL','NKE','HAS','PCAR')
symbols<- c('MYL','ZTS','AKRX','ALXN','AMGN','ARIA','ARWR','BIIB','BMRN','CELG','DYAX','EXAS','GILD','HALO','ILMN','INCY','KERX','MDVN','MACK','NLNK','VRTX','SGEN','BMY','JNJ','LLY','ABBV','VRX','ALKS','MRK','NVS','PFE','AGN','TEVA','NVO','AGN','ENDP','ISIS','AET','ANTM','HUM','CI','UNH','CVS','SYK','BDX','BAX','LH','TMO','DGX','A','OCRX','DVA','TSLA','GM','ALSN','AXL','DLPH','BWA','JCI','GNTX','TAP','BUD','PEP','KO','DPS','MNST','CCE','PAY','PBI','HSY','ECL','AAPL','BG','ADM','KHC','FBHS','LEG','TSN','BRFS','BLL','BMS','IP','OI','AVP','FBR','BERY','SEE','KMB','NUS','PG','GPRO','CAG','CPB','GMCR','K','GIS','MJN','BC','KORS','HOG','KATE','LULU','UA','VFC','FL','NKE','HAS','PCAR')


#######Get bullish stocks
library('quantmod')
#check if a stock is bullish, symbol returned if so; NULL returned otherwise
getBullishStock<-function(symbol){		
	
	#To use this function, you have to do `null-checking` on the returned value
	#Notes:If TODAY is Monday, then YESTERDAY was Sunday (-no data on Sunday), so we set the YESTERDAY to 3 days ago(Friday)

	if (format(Sys.Date(),"%a") == "Mon"){
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-3, to = Sys.Date())
	}
	else {
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-1, to = Sys.Date())
	}
	
	sym<-get(sym_object)
	if (	abs((sym[,4][[1]] - sym[,1][[1]])/(sym[,2][[1]] - sym[,3][[1]])) < 0.4 && abs((sym[,2][[1]] - sym[,4][[1]])/(sym[,1][[1]] - sym[,3][[1]])) < 0.4	) {
		return(symbol)
	}
}
#####################################################################################

getBearishStock<-function(symbol){		
	
	#To use this function, you have to do `null-checking` on the returned value
	#Notes:If TODAY is Monday, then YESTERDAY was Sunday (-no data on Sunday), so we set the YESTERDAY to 3 days ago(Friday)

	if (format(Sys.Date(),"%a") == "Mon"){
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-3, to = Sys.Date())
	}
	else {
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-1, to = Sys.Date())
	}
	
	sym<-get(sym_object)
	if (	abs((sym[,4][[1]] - sym[,1][[1]])/(sym[,2][[1]] - sym[,3][[1]])) < 0.4 && abs((sym[,3][[1]] - sym[,4][[1]])/(sym[,2][[1]] - sym[,1][[1]])) < 0.4	) {
		return(symbol)
	}
}
#####################################################################################





getStockDirection<-function(symbol, direction='direction'){		
	
	bearish<-FALSE
	bullish<-FALSE
	
	#To use this function, you have to do `null-checking` on the returned value
	#Notes:If TODAY is Monday, then YESTERDAY was Sunday (-no data on Sunday), so we set the YESTERDAY to 3 days ago(Friday)

	if (format(Sys.Date(),"%a") == "Mon"){
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-3, to = Sys.Date())
	}
	else {
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-1, to = Sys.Date())
	}
	
	sym<-get(sym_object)
	if (	abs((sym[,4][[1]] - sym[,1][[1]])/(sym[,2][[1]] - sym[,3][[1]])) < 0.4 && abs((sym[,3][[1]] - sym[,4][[1]])/(sym[,2][[1]] - sym[,1][[1]])) < 0.4	) {
		bearish<-TRUE
	}
	if (	abs((sym[,4][[1]] - sym[,1][[1]])/(sym[,2][[1]] - sym[,3][[1]])) < 0.4 && abs((sym[,2][[1]] - sym[,4][[1]])/(sym[,1][[1]] - sym[,3][[1]])) < 0.4	) {
		bullish<-TRUE
	}
	if (bullish && direction=='bullish') TRUE else if (bearish && direction=='bearish') TRUE 
}






#####################################################################################
#install.packages('XML')
library('XML')

getLowVolContracts<-function(symbol, contract_type) {
	# getting contracts with low volatility (<40%) 
	# For symbol, returns contracts with Vol < 40%
	# pass either "C" or "P" as contract_type

	#***function** to check validity of symbol, contract type C or P

	options_url="http://finance.yahoo.com/q/op?s=%s+Options"
	symbol_options_url=sprintf(options_url, symbol)
	options_table=readHTMLTable(symbol_options_url, stringAsFactors=FALSE)
	contracts_table<-if(contract_type=="C") options_table[[2]] else if (contract_type=="P") options_table[[3]]

	sym_table<-NULL; 
	contracts<-NULL; 
	strikes<-NULL; 

	for (vol in contracts_table[[10]]) { 
		pat<-"([0-9]{1,2}\\.[0-9]{2}).*"; 
		volatility<- sub(pat,"\\1", vol); 
		if (as.numeric(volatility) <= 60.0 ){
			posn<-match(c(vol), contracts_table[[10]]); 
			contract<-as.character(contracts_table[[2]])[posn]; 
			strike<-as.character(contracts_table[[1]])[posn]; 
			contracts<-c(contracts,contract); strikes<-c(strikes,strike); 
		}
	}; 
	sym_table<-cbind(contracts,strikes)
	sym_table
}

#################################################################################

#these symbols are the selected 21
symbols<-c("AGN","CI","TSLA","GPRO","UHN","BIIB","GMCR","MYL","MNST","HUM","UA","BMRN","NUS","MJN","VRX","ALXN","TEVA","AAPL","CELG","ISIS","BG");
#pool A
AGN_s<-c(312.0, 297.8, 321.0, 339.0)
CI_s<-c(149.5, 142.2, 156.5)
TSLA_s<-c(268.6,253.2,258.6,236.5,285)
GPRO_s<-c(48.7, 52.5,40.5, 36.5, 58.5)
UHN_s<-c(116.7, 112.7, 111.0, 121.7, 126.0)
BIIB_s<-c(309.1, 318.5, 407.0)
GMCR_s<-c(56.9,55.2,50.6,69.1)
#pool B
MYL_s<-c(49.0, 52.0, 57.0)
MNST_s<-c(140.0, 150.4, 155.3)
HUM_s<-c(186.5,192.5)
UA_s<-c(97.0, 100, 101.5)
BMRN_s<-c(140.0, 147.0)
NUS_s<-c(46.4, 44.0, 41.7, 38.4)
MJN_s<-c(74.0, 78.4, 83.8)
#pool C
VRX_s<-c(260.00,235.00,250.00,220.00)
ALXN_s<-c(180.00,190.5,204.00,165.00)
TEVA_s<-c(65.38, 63.36,61.74, 70.00,71.6)
AAPL_s<-c(127,107,117,112)
CELG_s<-c(131.00, 123.00,120.5)
ISIS_s<-c(57.00,55.00,49.00,63.00)
BG_s<-c(88.00,69.5,76.5,72.5,92.0)


getPriceLevels<-function(symbol){
	price_levels_name<-paste(symbol,"s",sep="_")
	price_levels<-get(price_levels_name)
	return(price_levels)
}

####################################################################################

getNextResistance<-function(levels_vector, current_price){
	all_prices<-c(levels_vector,current_price)					#'mix' the current_price within the prices vector	
	all_prices_sorted=sort(unlist(all_prices), decreasing=FALSE)	# order the prices
	all_prices_sorted=append(all_prices_sorted, 10000.0, after=all_prices_sorted[length(all_prices_sorted)]) #create theoretical highest possible resistance
	current_price_posn<-match(c(current_price),all_prices_sorted)	# position of the current_price
	next_resistance<-all_prices_sorted[as.numeric(current_price_posn)+1]  # next resistance price to hit 
	return(next_resistance)
}


getNextSupport<-function(levels_vector, current_price){
	all_prices<-c(levels_vector,current_price)					#'mix' the current_price within the prices vector
	all_prices_sorted=sort(unlist(all_prices), decreasing=FALSE)	# order the prices
	all_prices_sorted=append(all_prices_sorted, 0.0, after=0)		#create theoretical lowest possible support
	current_price_posn<-match(c(current_price),all_prices_sorted)	# position of the current_price
	next_support<-all_prices_sorted[as.numeric(current_price_posn)-1]  # next support price to hit 
	return(next_support)
}

####################################################################################

#Step 1: get bullish/bearish stocks
#Step 2: get low volatility contracts of those stocks
#Step 3: find contracts with strikes that are below the next resistance

get_good_contracts<-function() {	
	for (symbol in symbols) {
		bullish<-getStockDirection(symbol, direction='bullish')			#check if stock is bullish
		bearish<-getStockDirection(symbol, direction='bearish')			#check if stock is bearish

		levels<-getPriceLevels(symbol)	

		#whatever the direction, get the stock low_vol contracts
		lowVol_contracts<-if (!is.null(bullish)) getLowVolContracts(symbol,"C") else if(!is.null(bearish)) getLowVolContracts(symbol,"P")		
		for (contract in lowVol_contracts[,1]) {
		
			#get the strikes of the selected contracts (using indexing)
			strike<-lowVol_contracts[,2][match(c(contract),lowVol_contracts[,1])]

			#establish the current(last) stock-price; current price is needed to know its location relative to the next level (above or below)
			current_price<-tail(get(symbol), n=1)[,6][[1]]

			#When looking for the next level that is to be hit, for bullish stocks, only handle contracts whose strikes that are above the current price (the strikes u want to hit)
			#for bearish stocks, only handle contracts whose strikes that are below the current price (the strikes u want to hit)
			next_level=if (!is.null(bullish) && as.numeric(strike) > current_price) getNextResistance(levels,current_price) else if( !is.null(bearish) && as.numeric(strike) < current_price) getNextSupport(levels,current_price)	

			#if the next level is above the current price, then it is a resistance; the converse is true
			#we null-check because contracts that dont meet the above condition return NULL
			if (!is.null(next_level)){
				if (next_level > current_price  &&   as.numeric(strike) < next_level) {				
					print(contract)
				}			
				else if (next_level < current_price  && as.numeric(strike) > next_level) {
					print(contract)
				}
			}
		}
	}
}

