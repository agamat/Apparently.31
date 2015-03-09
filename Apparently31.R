# loading libraries-----------------------------------------------------
library(XLConnectJars)
library(XLConnect)
library(rJava)
library(gdata)
library(xlsx)
library(xlsxjars)
library(xtable)
library(PerformanceAnalytics)
library(packrat)
library(tseries)
library(vars)
library(tikzDevice)
library(qpcR)
library(plyr)
library(stargazer)
library(psych)
# importing data of European commision sentiment index, FTSE100 prices and FTSE-Allshares sector portfolios----
ukci<- read.csv('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/uksi.csv') 
FTSE100<-read.csv("/Users/Ahmed/Google Drive/Thesis/Data/FTSE/FTSE100d.csv")
m_p<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "i_p", colNames=FALSE)  # import the prices of the sector ::
c_p<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "c_p", colNames=FALSE)  
r_p<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "r_p", colNames=FALSE)  
s_p<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "s_p", colNames=FALSE)  
f_p<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "f_p", colNames=FALSE)  
sf_p<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "sf_p", colNames=FALSE)
m_v<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "i_v", colNames=FALSE)  # import the market value of the sector ::
c_v<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "c_v", colNames=FALSE)  
r_v<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "r_v", colNames=FALSE)  
s_v<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "s_v", colNames=FALSE)
f_v<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "f_v", colNames=FALSE)
sf_v<- openxlsx::read.xlsx('/Users/Ahmed/Google Drive/Thesis/First\ Paper\ analysis/First_paper/ports.xlsx', sheet = "sf_v", colNames=FALSE)
# set dates that will be used in time series class objects ---------------------
Date<-as.Date(FTSE100[,1], format="%d/%m/%Y")
datem<-seq(as.Date('1985-01-01'),as.Date('2014-10-01'),by = "month")
datem<-as.yearmon(datem)
#datem<-strftime(datem, format="%Y-%m") #return character class
# clean sentiment data ----------------------------------------------------
ukcc<-ukci[,-1] #removing date column
colnames(ukcc)<-c("mnfact","serv", "consu","retail","const", "esi")
ukcc<-ukcc[c(5,2,1,4,3,6)] # rearranging ukcc
# clean FTSE100 and calculate monthly data ---------------------------------
FTSE100.pp<-FTSE100[,2]
FTSE100.pp<-log(FTSE100.pp)
FTSE100.p<-zooreg(FTSE100.pp, order.by = Date)
#FTSE100.ts<-as.ts(FTSE100.p)
FTSE100.p <- tapply(FTSE100.p, format(time(FTSE100.p), "%Y-%m"),
                    function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
#esi.ts<-zooreg(esi, order.by = Date)
# clean sectors portfolios and calculate monthly prices ------------------------
prices<-mget(ls(pattern= "_p"))
market.values<-mget(ls(pattern= "_v"))
prices<-lapply(prices, function(a){a<-a[-(1:3),]})
market.values<-lapply(market.values, function(a){a<-a[-(1:3),]})
replaceP<-function(a){a[2,-(1)]<-gsub("(P)","",a[2,-(1)], fixed=TRUE)}
replaceMV<-function(a){a[2,-(1)]<-gsub("(MV)","",a[2,-(1)], fixed=TRUE)}
for (i in 1:6){
  prices[[i]][2,-(1)]<-replaceP(prices[[i]])
}
for (i in 1:6){
  market.values[[i]][2,-(1)]<-replaceMV(market.values[[i]])
}
for (i in 1:6){
  colnames(prices[[i]])<-prices[[i]][2,]
}
for (i in 1:6){
  colnames(market.values[[i]])<-market.values[[i]][2,]
}
companies<-list()
for (i in 1:6){
companies[[i]]<-list(prices[[i]][1:2,])
}
names(companies)<-names(prices)
prices<-lapply(prices, function(a){a<-a[-(1:23),]})
market.values<-lapply(market.values, function(a){a<-a[-(1:23),]})
for (i in 1:6){
  prices[[i]][,1]<-as.Date(as.numeric(prices[[i]][,1]), origin="1899-12-30")
}
for (i in 1:6){
  market.values[[i]][,1]<-as.Date(as.numeric(market.values[[i]][,1]), origin="1899-12-30")
}
Dated<-as.Date(as.matrix(prices[[1]][1]), format="%Y-%m-%d")
names.prices<-ls(prices)
names.marketvalues<-ls(market.values)
for (i in 1:6){
  colnames(prices[[i]])[1]<-names.prices[[i]]
}
for (i in 1:6){
  colnames(market.values[[i]])[1]<-names.marketvalues[[i]]
}
prices<-lapply(prices, function(a){a<-a[,-1]})
market.values<-lapply(market.values, function(a){a<-a[,-1]})
prices<-lapply(prices, data.matrix)
market.values<-lapply(market.values, data.matrix)
mv.prcnt<-lapply(market.values, function(a){a<-a/rowSums(a, na.rm=TRUE)})
prices.zoo<-lapply(prices, zooreg, order.by = Dated)
marketvalues.zoo<-lapply(mv.prcnt, zooreg, order.by = Dated)
c_wp<-prices.zoo[[1]]*marketvalues.zoo[[1]]
f_wp<-prices.zoo[[2]]*marketvalues.zoo[[2]]
m_wp<-prices.zoo[[3]]*marketvalues.zoo[[3]]
r_wp<-prices.zoo[[4]]*marketvalues.zoo[[4]]
s_wp<-prices.zoo[[5]]*marketvalues.zoo[[5]]
sf_wp<-prices.zoo[[6]]*marketvalues.zoo[[6]]
weighted.prices<-mget(ls(pattern= "_wp"))
for(i in 1:6){
  weighted.prices[[i]]<-as.xts(rowSums(weighted.prices[[i]], na.rm=TRUE),order.by = Dated, "%Y/%m/%d")
}
c_mon<-tapply(weighted.prices$c_wp, format(time(weighted.prices$c_wp), "%Y-%m"),
              function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
f_mon<-tapply(weighted.prices$f_wp, format(time(weighted.prices$f_wp), "%Y-%m"),
              function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
m_mon<-tapply(weighted.prices$m_wp, format(time(weighted.prices$m_wp), "%Y-%m"),
              function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
r_mon<-tapply(weighted.prices$r_wp, format(time(weighted.prices$r_wp), "%Y-%m"),
              function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
s_mon<-tapply(weighted.prices$s_wp, format(time(weighted.prices$s_wp), "%Y-%m"),
              function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
sf_mon<-tapply(weighted.prices$sf_wp, format(time(weighted.prices$sf_wp), "%Y-%m"),
               function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))
log.prices<-lapply(mget(ls(pattern= "_mon")), log)
log.prices<-lapply(log.prices, function(a){a<-a[-1]})
log.prices$sfs_mon<-c(log.prices$sf_mon[145:255],log.prices$s_mon[256:358])
sfs_monF<-c(rep(NA,times = 144),log.prices$sfs_mon)
# constructing pairs of prices and sentiment index ------------------------
market<-cbind(FTSE100.p, ukcc$esi)
const<-cbind(log.prices$c_mon, ukcc$const)
serv<-cbind(log.prices$sfs_mon, ukcc$serv[145:358])
manfact<-cbind(log.prices$m_mon, ukcc$mnfact)
retail<-cbind(log.prices$r_mon, ukcc$retail)
glist<-list(market=market, const=const, serv=serv,manfact=manfact, retail=retail)
for(i in 1:5){
  colnames(glist[[i]])<-c("prices","sentiment")
}




# ADF test and Granger Causality -----------------------------------------------
adf.testprices<-list()
for(i in 1:5){
  adf.testprices[[i]]<-list(adf.test(glist[[i]][,1], alternative = c("stationary", "explosive"),k = trunc((length(glist[[i]][,1])-1)^(1/3))))
}
names(adf.testprices)<-c("market.pr", "const.pr", "serv.pr", "manfact.pr", "retail.pr")
adf.testsentiment<-list()
for(i in 1:5){
  adf.testsentiment[[i]]<-list(adf.test(glist[[i]][,2], alternative = c("stationary", "explosive"),k = trunc((length(glist[[i]][,2])-1)^(1/3))))
}
names(adf.testsentiment)<-c("market.s", "const.s", "serv.s", "manfact.s", "retail.s")
diff<-lapply(glist, diff)
VARs<-lapply(diff, VAR)
g.causalities<-lapply(VARs, causality, cause="sentiment")

# Descriptives ------------------------------------------------------------
general<-cbind(ukcc, FTSE100=FTSE100.p, const.p=log.prices[[1]],finservserv.p=sfs_monF,manfact.p=log.prices[[3]],retail.p=log.prices[[4]],serv.p=log.prices[[5]],servfinan.p=log.prices[[6]], finan.p=log.prices[[2]] )
row.names(general)<-NULL
general<-data.matrix(general)






# plotting ----------------------------------------------------------------
glist.ts<-lapply(glist[-3], function(a){a<-ts(a, start=as.yearmon(rownames(market)[1]), frequency=12)})
serv.ts<-ts(glist$serv, start=as.yearmon(rownames(serv)[1]), frequency=12)

#tikz("/Users/Ahmed/Documents/was/UK.ESI.tex",width=4.5,height=3.2)
market.plot<-plot.ts(glist.ts$market, type="l",col="darkblue", main="UK Economic Sentiment Indicator", xlab="Time", ylab="UK ESI")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ukcc.i.tex",width=4.5,height=3.5)
manfact.plot<-plot(glist.ts$manfact, main="Manufacturing",col="darkblue", xlab=NULL,cex.axis=0.5)
#dev.off()


#tikz("/Users/Ahmed/Documents/was/ukcc.c.tex",width=4.5,height=3.5)
const.plot<-plot(glist.ts$const, main="Construction",col="darkblue", xlab=NULL,cex.axis=0.5)
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ukcc.r.tex",width=4.5,height=3.5)
retail.plot<-plot(glist.ts$retail, main="Retail",col="darkblue", xlab=NULL,cex.axis=0.5)
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ukcc.sfs.tex",width=4.5,height=3.5)
serv.plot<-plot(serv.ts, main="Services",col="darkblue", xlab=NULL,cex.axis=0.5)
#dev.off()

glist.ccf<-lapply(glist.ts, function(a){a<-ccf(a[,2],a[,1], type="correlation", 20)})
serv.ccf<-ccf(serv.ts[,2],serv.ts[,1], type="correlation", 20)

#tikz("/Users/Ahmed/Documents/was/market.ac.tex",width=4.5,height=3.5)
marketplot.ccf<-plot(glist.ccf$market, main="Market-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/cs.ac.tex",width=4.5,height=3.5)
constplot.ccf<-plot(glist.ccf$const, main="Construction-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()


#tikz("/Users/Ahmed/Documents/was/rs.ac.tex",width=4.5,height=3.5)
retailplot.ccf<-plot(glist.ccf$retail, main="Retail-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ms.ac.tex",width=4.5,height=3.5)
manfactplot.ccf<-plot(glist.ccf$manfact, main="Manfacturing-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ss.ac.tex",width=4.5,height=3.5)
servplot.ccf<-plot(serv.ccf, main="Service-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()


diff.ts<-lapply(diff, function(a){a<-ts(a, start=as.yearmon(rownames(market)[1]), frequency=12)})
diff.ccf<-lapply(diff.ts, function(a){a<-ccf(a[,2],a[,1], type="correlation", 20)})

#tikz("/Users/Ahmed/Documents/was/market.ac.tex",width=4.5,height=3.5)
marketdplot.ccf<-plot(diff.ccf$market, main="Market-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/cs.ac.tex",width=4.5,height=3.5)
constdplot.ccf<-plot(diff.ccf$const, main="Construction-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()


#tikz("/Users/Ahmed/Documents/was/rs.ac.tex",width=4.5,height=3.5)
retaildplot.ccf<-plot(diff.ccf$retail, main="Retail-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ms.ac.tex",width=4.5,height=3.5)
manfactdplot.ccf<-plot(diff.ccf$manfact, main="Manfacturing-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

#tikz("/Users/Ahmed/Documents/was/ss.ac.tex",width=4.5,height=3.5)
servdplot.ccf<-plot(diff.ccf$serv, main="Service-Sentiment monthly Cross-correlation", ylab="cross-correlation")
#dev.off()

mukcc<-ukcc
colnames(mukcc)<- c("Const.", "Serv.", "Mnfact.", "R.Trade", "Consu", "ESI")
mukcc<-mukcc[,c(3,1,2,4,6)]
mukcc.zoo<-zooreg(mukcc, order.by = datem)
tikz("/Users/Ahmed/Documents/Apps/mmukcc.tex",width=4.5,height=3.2)
mukcc.plot<-plot(mukcc.zoo, type="l",col="darkblue", main="", xlab="Time")
dev.off




