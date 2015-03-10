# Data Cleaning and Generating --------------------------------------------
ci<-ukci
colnames(ci)<-c("Date", "Manufacturing", "Services","Consumer","Retail", "Construction", "Market")
ci<-ci[,c(1,7,2,6,5,3,4)]
ci[,1]<-as.Date(datem)

sec_prices<-data.frame(log.prices[-c(7,8)])
sec_prices<-sec_prices[,-c(5,6)]
sfs_sec_prices<-log.prices$sfs_mon
sec_prices[,5]<-"NA"
sec_prices[145:358,5]<-sfs_sec_prices
sec_prices[,6]<-FTSE100.p
colnames(sec_prices)<-c("Construction", "Financials", "Manufacturing", "Retail", "Services", "Market")
attributes(sec_prices$Services)<-attributes(sec_prices$Market)
attributes(sec_prices$Financials)<-attributes(sec_prices$Market)
sec_prices$Date<-as.Date(datem)
sec_prices<-sec_prices[,c(7,6,3,1,4,5,2)]
row.names(sec_prices)<-NULL
for(i in 2:6){
  attributes(sec_prices[[i]])<-NULL
}
class(sec_prices$Services)<-"numeric"

sec_return<-returns_mon
for(i in 1:8){
  attributes(sec_return[[i]])<-NULL
}

sec_returns<-data.frame(sec_return)








# Winsorizing -------------------------------------------------------------
ci_win<-ci
ci_win[,-1]<-winsor(ci_win[,-1])

sec_prices_win<-sec_prices
sec_prices_win[,-1]<-winsor(sec_prices[,-1])

sec_returns_win<-sec_returns
sec_returns_win<-winsor(sec_returns)


# Descriptives ------------------------------------------------------------
ci_des<-describe(ci[,-1])
ci_des<-ci_des[,-c(1,5,7,10,12,13)]
ci_win_des<-describe(ci_win[,-1])
ci_win_des<-ci_win_des[,-c(1,5,7,10,12,13)]

write.xlsx(ci_des,file = "/Users/Ahmed/Google Drive/Thesis/First Paper analysis/Apparently.31/output.xls",sheetName = "ci_des")
write.xlsx(ci_win_des,file = "/Users/Ahmed/Google Drive/Thesis/First Paper analysis/Apparently.31/output.xls",sheetName = "ci_win_des")

sec_prices_des<-describe(sec_prices[,-1])
sec_prices_des<-sec_prices_des[,-c(1,5,7,10,12,13)]
sec_prices_win_des<-describe(sec_prices_win[,-1])
sec_prices_win_des<-sec_prices_win_des[,-c(1,5,7,10,12,13)]

write.xlsx(ci_des,file = "/Users/Ahmed/Google Drive/Thesis/First Paper analysis/Apparently.31/output.xls",sheetName = "sec_prices_des")
write.xlsx(ci_win_des,file = "/Users/Ahmed/Google Drive/Thesis/First Paper analysis/Apparently.31/output.xls",sheetName = "sec_prices_win_des")

sec_returns_des<-describe(sec_returns)
sec_returns_des<-sec_returns_des[,-c(1,5,7,10,12,13)]
sec_returns_win_des<-describe(sec_returns_win[,-1])
sec_returns_win_des<-sec_returns_win_des[,-c(1,5,7,10,12,13)]

write.xlsx(ci_des,file = "/Users/Ahmed/Google Drive/Thesis/First Paper analysis/Apparently.31/output.xls",sheetName = "sec_returns_des")
write.xlsx(ci_win_des,file = "/Users/Ahmed/Google Drive/Thesis/First Paper analysis/Apparently.31/output.xls",sheetName = "sec_returns_win_des")

# Plotting ----------------------------------------------------------------
ccplot<-ggplot(dat = melt(ci, id.var="Date"), aes(x=Date, y=value)) + 
  geom_line(aes(colour=variable, group=variable))+
  ggtitle("Confidence Indicators")

cc_winplot<-ggplot(dat = melt(ci_win, id.var="Date"), aes(x=Date, y=value)) + 
  geom_line(aes(colour=variable, group=variable))+
  ggtitle("Winsorized Confidence Indicators")


sec_pricesplot<-ggplot(dat = melt(sec_prices, id.var="Date"), aes(x=Date, y=value)) + 
  geom_line(aes(colour=variable, group=variable))+
  ggtitle("Prices (log)")


sec_priceswinplot<-ggplot(dat = melt(sec_prices_win, id.var="Date"), aes(x=Date, y=value)) + 
  geom_line(aes(colour=variable, group=variable))+
  ggtitle("Winsorized Prices (log)")
