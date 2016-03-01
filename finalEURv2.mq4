//+------------------------------------------------------------------+
//|                                                        daily.mq4 |
//|                                 Copyright 2016, Slivercrest Inc. |
//|                                                 www.medallia.com |
//+------------------------------------------------------------------+
#property copyright "Copyright 2016, Slivercrest Inc."
#property link      "www.medallia.com"
#property version   "1.00"
//#property strict
//+------------------------------------------------------------------+
//| Expert initialization function                                   |
//+------------------------------------------------------------------+
int OnInit()
  {
//---
   
//---
   return(INIT_SUCCEEDED);
  }
//+------------------------------------------------------------------+
//| Expert deinitialization function                                 |
//+------------------------------------------------------------------+
void OnDeinit(const int reason)
  {
//---
   
  }
//LOGIC
/*
Pin constitutes -/+ 0.5 (body size/HiLo) & [for bearish ([Hi-O]/[Lo-C])] [for bullish ([Lo-O]/[Hi-C])]  > 0.6
MathAbs((Open[1]-Close[1])/(High[1]-Low[1])) < 0.5 //small body size
OrderOpenTime is 12pm for JPY,and 00 for EUR
*/

/*
COMPOUNDING vs SIMPLE AGGREGATION
Compounding profit&losses results in a higher aggregate than simple aggregation
So if the baseline(simple) aggregate is positive as in for this robot, compounding 
individual profits/losses should results in a higher positive -- kinda like Compound vs Simple Interest  

*/



//+------------------------------------------------------------------+
//| Expert tick function                                             |
//+------------------------------------------------------------------+

string symb=Symbol();
float SL=500;
float TP=1500;
float lots;





void OnTick()
  {
    if (TimeHour(TimeLocal()) ==  12) {        
         //there should only be one trade at a time
        //if (OrdersTotal() == 0) {
        //the count of open orders of the symbol in focus
        int count=0;
        for (int i=1; i<=OrdersTotal(); i++) {
            if (OrderSelect(i-1, SELECT_BY_POS, MODE_TRADES)==true) {
                if (OrderSymbol()==Symbol()) {
                    count++;
                }
            }
        }
        if (count == 0) {
        
            /* RISK:
            Because we have three robots operating on the same account at the same time,
            We only want to risk small % of our free margin (3%) for each order just in 
            case there is a long stretch of consecutive losses - (unlikely)10 straight losses==30%
            That means the lot size changes accordingly for each order.
            This section calculates the lot_size to use.
            Formula: lots*one_lot_cost=3% of free_margin;      
            */
            RefreshRates();
            float Min_Lot=MarketInfo(symb, MODE_MINLOT);		//min number of lots allowed
            float Free=AccountFreeMargin();				//amount of free margin in account
            float One_lot=MarketInfo(symb, MODE_MARGINREQUIRED);	//cost of one lot
            lots=((3.0/100)*Free)/One_lot;
            if (lots < Min_Lot) {lots=Min_Lot;}
           
                
        
            if ( MathAbs((Open[1]-Close[1])/(High[1]-Low[1])) < 0.5 && MathAbs((High[1]-Open[1])/(Low[1]-Close[1])) > 0.6 ) { //longer top wick than bottom one; Bearish pin
                int sell_ticket=OrderSend(symb, OP_SELL, lots, Bid, 1, Bid+(500*Point), Bid-(1500*Point));
            }
            else if ( MathAbs((Open[1]-Close[1])/(High[1]-Low[1])) < 0.5 && MathAbs((Low[1]-Open[1])/(High[1]-Close[1])) > 0.6 ) {//shorter top wick than bottom one; Billish pin
                int buy_ticket=OrderSend(symb, OP_BUY, lots, Ask, 1, Ask-(500*Point), Ask+(1500*Point));
            }
        }
    }

}

