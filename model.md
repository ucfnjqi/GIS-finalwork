#OLS model
library(readr)
data<-read_csv('finaldata.csv')
fit<-lm("seekinghelpnum~stationnum+hospitalnum+oldpeonum",data)
print(fit)

#GWR model
install.packages("spgwr")
library(spgwr)
bw<-gwr.sel(seekinghelpnum~stationnum+hospitalnum+oldpeonum, data=data, coords=cbind(x=data$top, y=data$left), gweight = gwr.Gauss, verbose = TRUE, method = "aic")
gwr_result <- gwr(seekinghelpnum~stationnum+hospitalnum+oldpeonum,coords=cbind(x=data$top, y=data$left), data = data, bandwidth = bw, gweight = gwr.Gauss, hatmatrix = TRUE)
print(gwr_result)