export PATH="/software/R-3.1.2/bin:$PATH"		#for the compatible version of R

#valid symbols
symbols<- c('MYL','ZTS','AKRX','ALXN','AMGN','ARIA','ARWR','BIIB','BMRN','CELG','DYAX','EXAS','GILD','HALO','ILMN','INCY','KERX','MDVN','MACK','NLNK','VRTX','SGEN','BMY','JNJ','LLY','ABBV','VRX','ALKS','MRK','NVS','PFE','AGN','TEVA','NVO','AGN','ENDP','ISIS','AET','ANTM','HUM','CI','UNH','CVS','SYK','BDX','BAX','LH','TMO','DGX','A','OCRX','DVA','TSLA','GM','ALSN','AXL','DLPH','BWA','JCI','GNTX','TAP','BUD','PEP','KO','DPS','MNST','CCE','STZ-B','PAY','PBI','HSY','ECL','AAPL','BG','ADM','KHC','FBHS','LEG','TSN','BRFS','BLL','BMS','IP','OI','AVP','FBR','BERY','SEE','KMB','NUS','PG','GPRO','CAG','CPB','GMCR','K','GIS','MJN','BC','KORS','HOG','KATE','LULU','UA','VFC','FL','NKE','HAS','PCAR')



##Get bullish stocks
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


