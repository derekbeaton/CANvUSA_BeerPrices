
rm(list=ls())
gc()

source('../cor.eig.reg.plot.R')
library(ExPosition)
library(ez)
library(psych)

#http://lamages.blogspot.com/2013/04/how-to-change-alpha-value-of-colours-in.html
add.alpha <- function(col, alpha=0.65){
	apply(
		sapply(col, col2rgb)/255, 
		2, 
		function(x){
			rgb(x[1], x[2], x[3], alpha=alpha)
		}
	)  
}

beer.data.in <- read.csv('BEERS_WE_LIKE_AVAILABLE_AT_LCBO.csv',header=T)
rownames(beer.data.in) <- beer.data.in[,"BEER"]


## (1) compute total ml for each beer:
CAN_total.ml <- beer.data.in[,c("PACKAGE.SIZE..LCBO.")] * beer.data.in[,c("UNIT.SIZE..ml..LCBO.")]
USA_total.ml <- beer.data.in[,c("PACKAGE.SIZE..Total.Wine.")] * beer.data.in[,c("UNIT.SIZE..ml..Total.Wine.")]
	###	 add these columns to the data
	beer.data.in <- cbind(beer.data.in, CAN_total.ml, USA_total.ml)

## (2) compute exchange rate (put it all in CAD)
USD_to_CAD <- beer.data.in[,c("USD..Total.Wine..Sacramento..CA.")] * beer.data.in[,c("USD.CAD..SEE.DATE.")]
	###	 add these columns to the data
	beer.data.in <- cbind(beer.data.in, USD_to_CAD)

## (3) CAD per 355ml serving (i.e., 1 NORMAL SIZED beer).
LCBO_CAD.per.serving <- ((beer.data.in[,c("CAD..LCBO.")] / beer.data.in[,c("CAN_total.ml")]) * 355)
TW_CAD.per.serving <- ((beer.data.in[,c("USD_to_CAD")] / beer.data.in[,c("USA_total.ml")]) * 355)
LCBO_TW.cost.ratio <- LCBO_CAD.per.serving / TW_CAD.per.serving
	###	 add these columns to the data
	beer.data.in <- cbind(beer.data.in, LCBO_CAD.per.serving, TW_CAD.per.serving, LCBO_TW.cost.ratio)

## (4) USA (USA! USA!) only data
USA.beer.data.in <- beer.data.in[beer.data.in$REGION=="USA",]


## (5) Get the good stuff: price per serving, price ratios, and ratings.
	### (a) USA beers available at LCBO with CAD price at LCBO and Total Wine (Sacramento, CA)
	LCBO.TW.CAD <- beer.data.in[beer.data.in$REGION=="USA",c("LCBO_CAD.per.serving","TW_CAD.per.serving","LCBO_TW.cost.ratio","BEER.ADVOCATE.RATING..COMMUNITY.")]
	colnames(LCBO.TW.CAD) <- c("LCBO_CAD_serving","TW_CAD_serving","LCBO.v.TW_CostRatio","BA_CommunityRatings")

		## though we will see it, there are two considerable outliers: Sam Adams Utopia and Cascade Blackcap. In general, these are very and kind of expensive beers (respectively). So we need drop them.
			
	LCBO.TW.CAD_drop <- LCBO.TW.CAD[which(!(rownames(LCBO.TW.CAD) %in% c("Samuel Adams Utopia*"))),]

