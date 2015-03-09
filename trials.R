sfs_monF<-data.frame(datem, NA)
sfs_monF<-zooreg(full.na[,2],order.by = full.na[,1])
index(sfs_monF)<-format(time(sfs_monF), "%Y-%m")
sfs_monF[145:358]<-log.prices$sfs_mon


general<-cbind(ukcc, FTSE100=FTSE100.p, const.p=log.prices[[1]],finservserv.p=sfs_monF,manfact.p=log.prices[[3]],retail.p=log.prices[[4]],serv.p=log.prices[[5]],servfinan.p=log.prices[[6]], finan.p=log.prices[[2]] )
row.names(general)<-NULL


corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}



#Copy from this point: 
"granger" <-function(d, L, k = 1)  
{ 
  #d is a bivariate time-series:  regress d[,k] on L lags of d[,1] and d[,2]. 
  #This is a modified version for R, in which the command ts.matrix was substituted by ts.intersect. 
  names.d <- dimnames(d)[[2]] 
  D <- d 
  for(i in 1:L)  
  { 
    D <-ts.intersect(D, lag(d,  - i)) 
    #don't you find -1 bizarre here and it annoying to need the loop 
  } 
  dimnames(D)[[2]] <- paste(rep(names.d, L + 1), "_", rep(0:L, times = rep(2, L + 1)), sep = "") 
  y  <- D[, k] 
  n  <- length(y) 
  x1 <- D[,  - (1:2)] 
  x0 <- x1[, ((1:L) * 2) - (k %% 2)] 
  z1 <- lm(y ~ x1) 
  z0 <- lm(y ~ x0) 
  S1 <- sum(z1$resid^2) 
  S0 <- sum(z0$resid^2) 
  ftest <- ((S0 - S1)/L)/(S1/(n - 2 * L - 1)) 
  list(ftest = ftest, p.val = 1 - pf(ftest, L, n - 2 * L - 1), R2 = summary(z1)$r.squared) 
} 
#To this point.





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



xtable(summary(lm(dgeneral$FTSE100~lag(dgeneral$esi))), caption = "esi2market")
xtable(summary(lm(dgeneral$FTSE100~lag(dgeneral$const))), caption = "const2market")
xtable(summary(lm(dgeneral$FTSE100~lag(dgeneral$mnfact))), caption = "mnfact2market")
xtable(summary(lm(dgeneral$FTSE100~lag(dgeneral$serv))), caption = "serv2market")
xtable(summary(lm(dgeneral$FTSE100~lag(dgeneral$retail))), caption = "retail2market")
xtable(summary(lm(dgeneral$FTSE100~lag(dgeneral$consu))), caption = "consumer2market")

xtable(summary(lm(dgeneral$const.p~lag(dgeneral$const))), caption = "const2const")
xtable(summary(lm(dgeneral$manfact.p~lag(dgeneral$mnfact))), caption = "mnfact2mnfact")
xtable(summary(lm(dgeneral$finservserv.p~lag(dgeneral$serv))), caption = "serv2serv")
xtable(summary(lm(dgeneral$retail.p~lag(dgeneral$retail))), caption = "retail2retail")



xtable(summary(lm(dgeneral$const.p~lag(dgeneral$consu))), caption = "consumer2const")
xtable(summary(lm(dgeneral$manfact.p~lag(dgeneral$consu))), caption = "consumer2manfact")
xtable(summary(lm(dgeneral$finservserv.p~lag(dgeneral$consu))), caption = "consumer2finservservserv")
xtable(summary(lm(dgeneral$serv.p~lag(dgeneral$consu))), caption = "consumer2serv")
xtable(summary(lm(dgeneral$finan.p~lag(dgeneral$consu))), caption = "consumer2finan")
xtable(summary(lm(dgeneral$retail~lag(dgeneral$consu))), caption = "consumer2retail")

summary(lm(dgeneral$FTSE100~lag(dgeneral$const)))


FTSE100ulog<-FTSE100[,2]
FTSE100ulog<-zooreg(FTSE100ulog, order.by = Date)
#FTSE100.ts<-as.ts(FTSE100.p)
FTSE100ulog <- tapply(FTSE100ulog, format(time(FTSE100ulog), "%Y-%m"),
                    function(a) mean(a[as.integer(format(time(a), "%d")) <=31], na.rm = TRUE))




summary(lm(x~x1+x2+x3+x4))

x<-dgeneral$FTSE100*(dgeneral$consu)
x1<-dgeneral$manfact.p*dgeneral$mnfact
x2<-dgeneral$retail.p*dgeneral$retail
x3<-dgeneral$const*dgeneral$const.p
x4<-dgeneral$serv*dgeneral$finservserv.p






causality(VAR(cbind(dgeneral$FTSE100,dgeneral$consu)))
causality(VAR(cbind(dgeneral$const.p,dgeneral$consu)))
causality(VAR(cbind(dgeneral$manfact.p,dgeneral$consu)))
causality(VAR(cbind(dgeneral$retail.p,dgeneral$consu)))
causality(VAR(cbind(dgeneral$serv.p,dgeneral$consu)))
causality(VAR(cbind(dgeneral$finan.p,dgeneral$consu)))
