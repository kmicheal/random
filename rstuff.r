export PATH="/software/R-3.1.2/bin:$PATH"		#for the compatible version of R
symbols<- c('ACT','MYL','ZTS','AKRX','ALXN','AMGN','ARIA','ARWR','BIIB','BMRN','CELG','DYAX','EXAS','GILD','HALO','ILMN','INCY','KERX','MDVN','MACK','NEKTR','NLNK','VRTX','SGEN','BMY','JNJ','LLY','ABBV','VRX','ALKS','MRK','NVS','PFE','AGN','TEVA','NVO','AGN','ENDP','ISIS','AET','ANTM','HUM','CI','UNH','CVS','SYK','ZMH','BDX','BAX','LH','TMO','DGX','A','OCR','DVA','TSLA','GM','ALSN','AXL','DLPH','BWA','JCI','GNTX','TAP','BUD','PEP','KO','DPS','MNST','CCE','STZ-B','PAY','PBI','HSY','ECL','AAPL','BG','ADM','KHC','FBHS','LEG','TSN','BRFS','BLL','BMS','IP','OI','AVP','FBR','BERY','SEE','RKT','KMB','NUS','PG','GPRO','CAG','CBP','GMCR','K','GIS','MJN','BC','KORS','HOG','KATE','LULU','UA','VFC','FL','NKE','HAS','PCAR')
symbols<- c('MYL','ZTS','AKRX','ALXN','AMGN','ARIA','ARWR','BIIB','BMRN','CELG','DYAX','EXAS','GILD','HALO','ILMN','INCY','KERX','MDVN','MACK','NLNK','VRTX','SGEN','BMY','JNJ','LLY','ABBV','VRX','ALKS','MRK','NVS','PFE','AGN','TEVA','NVO','AGN','ENDP','ISIS','AET','ANTM','HUM','CI','UNH','CVS','SYK','BDX','BAX','LH','TMO','DGX','A','OCRX','DVA','TSLA','GM','ALSN','AXL','DLPH','BWA','JCI','GNTX','TAP','BUD','PEP','KO','DPS','MNST','CCE','STZ-B','PAY','PBI','HSY','ECL','AAPL','BG','ADM','KHC','FBHS','LEG','TSN','BRFS','BLL','BMS','IP','OI','AVP','FBR','BERY','SEE','KMB','NUS','PG','GPRO','CAG','CPB','GMCR','K','GIS','MJN','BC','KORS','HOG','KATE','LULU','UA','VFC','FL','NKE','HAS','PCAR')

*****binning; if 'too many server requests' would be a problem****
#bins of 10 symbols while getting daily data
n_bins<-floor(length(symbols)/10)
remainder<-length(symbols)%%10
for (i in 0:(n_bins-1)){
	in_focus<-symbols[((i*10)+1):((i+1)*10)]
	print(in_focus)
	for (symbol in in_focus){
		getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = "2015-08-24", to = Sys.Date())
		Sys.sleep(3)
	}
	Sys.sleep(10)
}
last_ones<-tail(symbols,remainder)



*****below works****
library('quantmod')
#first get bullish stocks
getBullishStocks<-function(){
	bullish<-NULL
	for (symbol in symbols) {
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = "2015-08-24", to = Sys.Date())
		sym<-get(sym_object)
		sym<-get(symbol)
		if (	abs((sym[,4][[1]] - sym[,1][[1]])/(sym[,2][[1]] - sym[,3][[1]])) < 0.4 && abs((sym[,2][[1]] - sym[,4][[1]])/(sym[,1][[1]] - sym[,3][[1]])) < 0.4	) {
			bullish<-c(bullish,symbol)
		}
	}
	bullish
}

getBullishStocks<-function(){
	bullish<-NULL
	for (symbol in symbols){
		
		print(symbol)
		sym_object<-getSymbols.yahoo(symbol,.GlobalEnv, return.class ='xts', index.class  = 'Date', from = "2015-08-21", to = Sys.Date())
		sym<-get(symbol)
		print(sym)
		tt<-sym[,4][[1]] - sym[,1][[1]]
		print(tt)
		#ss<-sym[1,]$symbol.High[[1]] - sym[1,]$symbol.Low[[1]]
		#vv<-sym[1,]$symbol.High[[1]] - sym[1,]$symbol.Close[[1]]
		#yy<-sym[1,]$symbol.Open[[1]] - sym[1,]$symbol.Low[[1]]
		#if (tt/ss<0.4) {
		#&& abs(vv/yy)<0.4) 
		#	print('ok')
		#}
		Sys.sleep(5)
	}
}


#install.packages('XML')
library('XML')

### getting contracts with low volatility (<10%) 
get_prelim_strikes<-function(symbol) {
	finale<-NULL
	
	options_url="http://finance.yahoo.com/q/op?s=%s+Options"
	symbol_options_url=sprintf(options_url, symbol)
	options_table=readHTMLTable(symbol_options_url, stringAsFactors=FALSE)
	calls_table=options_table[[2]]
	
	for (vol in calls_table[[10]]) { 
		final<-NULL
		contracts<-NULL
		strikes<-NULL
		pat<-"([0-9]{1,2}\\.[0-9]{2}).*"
		volatility<- sub(pat,"\\1", vol)
		if (as.numeric(volatility) <= 60.0 ){
			posn<-match(c(vol), calls_table[[10]])
			#posns<-which(as.character(calls_table[[10]]) %in% c(volatility))
			contract<-as.character(calls_table[[2]])[posn]
			strike<-as.character(calls_table[[1]])[posn]
			contracts<-c(contracts,contract)
			strikes<-c(strikes,strike)
				
		}
		final<-cbind(contracts,strikes)
		finale<-c(finale,final)
	}
	finale
}



getSymbols(Symbols ='MYL', env = parent.frame(),reload.Symbols = FALSE,verbose = FALSE, warnings = TRUE,src = "yahoo", symbol.lookup = TRUE, auto.assign = getOption('getSymbols.auto.assign',TRUE))
last_close=tail(MYL$MYL.Adjusted)[[6]]

getSymbols.yahoo(c('MYL'),.GlobalEnv, return.class ='xts', index.class  = 'Date',from = "2015-08-01",to = Sys.Date())


#### get price action trend over for the previous day(upward pin)
####   CLOSE-OPEN/HIGH-LOW < 40% && HIGH-CLOSE/OPEN-LOW < 40
for (i in 1:dim(MYL)[1]){		#for productn, only the last day's data should b considered	
	if (	abs(((MYL[1,]$MYL.Close[[1]] - MYL[1,]$MYL.Open[[1]])/(MYL[1,]$MYL.High[[1]] - MYL[1,]$MYL.Low[[1]]))) < 0.4	&& abs(((MYL[1,]$MYL.High[[1]] - MYL[1,]$MYL.Close[[1]])/(MYL[1,]$MYL.Open[[1]] - MYL[1,]$MYL.Low[[1]]))) < 0.4	) {
		print(MYL[1,])
	}
}

##### get trend over the past three days(if uptrend)
for (i in c(1,3){
		gradient<-(MYL$MYL.Close[[i]] - MYL$MYL.Close[[1]])/(3-1)
		if (gradient > 3) {
			trend=uptrend
		}
		elif (gradient <3) {
			trend=downtrend
		}
}

good_contracts<- NULL
strikes<-myl[,2]
for (strike in strikes){
	for (level in (sort(MYL_s, decreasing=FALSE))[1]) {
		if (level > strike && (level-as.numeric(strike))>1.00) {
			contract<-(myl[myl[,2] == strike])[1]
			good_contracts<-c(good_contracts, contract )		
		}
	}
}


MYL_s<-c(73,71.5,67,57,54.3)
AKRX<-c(48,46.5,44.5)
ALXN<-c(186,190,198,203)
AMGN<-c(154,157,161,164,170,173,177)
ARIA<-c(8.85,8.6,8,7.8,7.5)
ARWR<-c(7.25,61,6.75,6.4,6.0)
BIIB<-c(311,317,333,382,400,408,418,425)
BMRN<-c(138,139,137,126,123,145,147)
CELG<-c(120,117,131,133,135,138)
DYAX<-c(23.5,25,26,27,28)
EXAS<-c(22.8,24.5,24.0,27.0,28.5,)
GILD<-c(115,120,110,113,122)
HALO<-c(19.3,20.5,22.5,24.5,17.5,16,15)
ILMN<-c(207,214,218.5,223,212.5)
INCY<-c(102,110,112,106,97)
KERX<-c(10,9.5,8,6.5,6)
MACK<-c(10.5,11,9.8,9.4,11,12.3,13)
MDVN<-c(100,95.8,104,110,114,119)
NKTR<-c(11,10.7,11.4,12,12.5)
NLNK<-c(45.8,50,55,53,47.5)
SGEN<-c(45.5,47,48,44,42,40,43.5,46)
VRTX<-c(137,134,130,140)
ALKS<-c(69.5,67.5,68)
ENDP<-c(84.7,88,82.5,80,78.5)
ISIS<-c(48,49.4,52.5,56,58)
TSLA<-c(250,245.5)

library('XML')

trend=""
contract_score=0	#max score is 5; each factor contributes a certain value to the score
			#score is 4+ qualifies a contract as good
options_url="http://finance.yahoo.com/q/op?s=%s+Options"
stock_url="http://finance.yahoo.com/q?s=%s&ql=1"
for symbol in symbols {


	######
	# ascertain trend
	######
	#get trend over the past two weeks
	#using data from time series; daily closing prices
	two_weeks_data=[][][][]
	closing prices=two_weeks_data[][][][]
	log(count i.e 0:10), log(closing_prices)
	y=c(log(closing_prices[-1]),log(closing_prices[0]))
	x=c(log 10, log1)
	gradient=diff(y)/diff(x)
	if (gradient > 0.25) {
		trend=uptrend
	}
	elif (gradient < -0.25) {
		trend=downtrend
	}
	else {
		trend=ranging
	}

	#if there is an uptrend, find the strike price nearest to the current price
	# then find how far that strike price is from the nearest resistance level
	#  
	if (trend=uptrend) {
		current_price=closing_prices[-1]
		next_strikes<-NULL
		for (strike in call_strikes) {
			if strike > current_price
				next_strikes<-c(next_strikes, strike)
			}
		}
		nearest_strike<-sort(next_strikes, decreasing=FALSE)[1]
		for level in levels$symbol {
			if (level > nearest_strike) {
								#get the contract name whose 
				contract_score+=1
	}





	symbol_options_url=sprintf(options_url, symbol)
	options_table=readHTMLTable(symbol_options_url, stringAsFactors=FALSE)
	calls_table=options_table[[2]]
	puts_table=options_table[[3]] 

	call_strikes<-calls_table[]
	for strike in call_strikes {
	###### 
	# checking proximity to support levels
	######
	## u need to establish whether the level is higher or lower than the srike price
		for level in support$symbol {
			if strike == level || s


	}

	######
	# checking next time news is coming out
	######
	symbol_stock_url=sprintf(stock_url, symbol)
	stock_info_table=readHTMLTable(symbol_stock_url, stringAsFactors=FALSE)
	next_news_date=stock_info_table[[****]]
	date_today=date +%Y%M%d
	if ((next_news_date - date_today) < [1month]){
		skip the symbol
	}


		






#Program loads the option chains of all the 30 symbols upon opening
#Program given a symbol, supposed to return a contract in the next 1 month that would be good to buy/write
#Logic:
#	how far from the  next price-shakeup(earnings/product-release/event)
#	Chart technical analysis - Resistance support levels, Moving average
	is the strike price good?...	
#	What's been in the news; follow RSS feeds; Twitter feeds

calls_table=options_table[[2]]
puts_table=options_table[[3]] 
#it is a good idea to construct the tables properly here --- attributes, col/row names, factors etc



