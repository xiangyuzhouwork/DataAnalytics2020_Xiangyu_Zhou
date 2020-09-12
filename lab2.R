data<-c(40,38,36,37,39,39,42,42,40,37,36,38,40,42,43,42,39,40,42,62,40,38,37,41,44,43,39,39,39,37,39,31,38,40,41,42,38,39,65)
data = ts(data, start = c(2017,01), frequency = 12)
plot(data)

install.packages("forecast")
library("forecast")
abc=auto.arima(data)
summary(abc)
pred3 = forecast(abc,h=6)
summary(pred3)

fir=ets(data)
pred2 = forecast(fir,h=6,level=95)
plot(pred2)
summary(pred2)

