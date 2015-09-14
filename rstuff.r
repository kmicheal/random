export PATH="/software/R-3.1.2/bin:$PATH"		#for the compatible version of R


#######Get bullish stocks
library('quantmod')
#check if a stock is bullish, symbol returned if so; NULL returned otherwise
getBullishStock<-function(symbol){		#you have to do null-checking on the returned value

	sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = Sys.Date()-1, to = Sys.Date())
	sym<-get(sym_object)
	if (	abs((sym[,4][[1]] - sym[,1][[1]])/(sym[,2][[1]] - sym[,3][[1]])) < 0.4 && abs((sym[,2][[1]] - sym[,4][[1]])/(sym[,1][[1]] - sym[,3][[1]])) < 0.4	) {
		return(symbol)
	}
}
#####################################################################################




######## getting contracts with low volatility (<40%) 
######## Given a symbol, it returns the contracts with the lowest volatility
#install.packages('XML')
library('XML')

get_prelim_strikes<-function(symbol) {
	options_url="http://finance.yahoo.com/q/op?s=%s+Options"
	symbol_options_url=sprintf(options_url, symbol)
	options_table=readHTMLTable(symbol_options_url, stringAsFactors=FALSE)
	calls_table=options_table[[2]]

	sym_table<-NULL; 
	contracts<-NULL; 
	strikes<-NULL; 

	for (vol in calls_table[[10]]) { 
		pat<-"([0-9]{1,2}\\.[0-9]{2}).*"; 
		volatility<- sub(pat,"\\1", vol); 
		if (as.numeric(volatility) <= 40.0 ){
			posn<-match(c(vol), calls_table[[10]]); 
			contract<-as.character(calls_table[[2]])[posn]; 
			strike<-as.character(calls_table[[1]])[posn]; 
			contracts<-c(contracts,contract); strikes<-c(strikes,strike); 
		}
	};
	sym_table<-cbind(contracts,strikes)
	sym_table
}

#################################################################################

#Step 1: get bullish stocks
#Step 2: get low volatility contracts of those stocks
#Step 3: find contracts with strikes that are below the next resistance


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

get_good_calls<-function() {
for (symbol in symbols) {
	bullish<-getBullishStock(symbol)				#check if stock is bullish
	if (!is.null(bullish)) {											#if a stock is bullish, get its low_vol contracts
		good_contracts<-get_prelim_strikes(symbol)	
		for (contract in good_contracts[,1]) {
			contract_label<- NULL
			strike_posn<-match(c(contract),good_contracts[,1])
			strike<-good_contracts[,2][strike_posn]				#get the strikes of the good contracts
			
			resistances_name<-paste(symbol,"s",sep="_")
			resistances<-get(resistances_name)
			
			#if the stock is going up, u need to establish its location relative to the next resistance
			current_price<-tail(get(symbol), n=1)[,6][[1]]
			all_prices<-c(resistances,current_price)					#put the current_price with the the 	
			all_prices_sorted=sort(unlist(all_prices), decreasing=FALSE)		# resistances to establish the next
			current_price_posn<-match(c(current_price),all_prices_sorted)	#resistance
			next_resistance<-all_prices_sorted[as.numeric(current_price_posn)+1]
			if (as.numeric(strike) < next_resistance) {
				print(contract)
			}
		}
	}
}
}
