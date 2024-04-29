library(fpp2)
library(tseries)

Mortality_South_Korea = read.csv2("https://raw.githubusercontent.com/UC3M-student/Proyecto_Series_Temporales/main/Mortality_South_Korea.csv", skip = 1)
Mortality_South_Korea

years_45 = as.numeric(Mortality_South_Korea$X45)
years_46 = as.numeric(Mortality_South_Korea$X46)
years_47 = as.numeric(Mortality_South_Korea$X47)
years_48 = as.numeric(Mortality_South_Korea$X48)
years_49 = as.numeric(Mortality_South_Korea$X49)
years_50 = as.numeric(Mortality_South_Korea$X50)
years_51 = as.numeric(Mortality_South_Korea$X51)
years_52 = as.numeric(Mortality_South_Korea$X52)
years_53 = as.numeric(Mortality_South_Korea$X53)

y_45<-ts(years_45)
y_46<-ts(years_46)
y_47<-ts(years_47)
y_48<-ts(years_48)
y_49<-ts(years_49)
y_50<-ts(years_50)
y_51<-ts(years_51)
y_52<-ts(years_52)
y_53<-ts(years_53)


autoplot(y)
ggAcf(y) #FAS
ggPacf(y) #FAC 

y_dif<-diff(y, differences = 2) 
autoplot(y_dif)

adf.test(y) #H1 = Estacionario 
kpss.test(y,null = "Level") #H1: NO ESTACIONARO
pp.test(y) # H1: estacionario

adf.test(y_dif) #H1 = Estacionario 
kpss.test(y_dif,null = "Level") #H1: NO ESTACIONARO
pp.test(y_dif) # H1: estacionario
ggAcf(y_dif) #FAS
ggPacf(y_dif)

auto.arima(y)

#FSDFSFDDS

#flskjdfsdjfsdfsd asdasdas


#dasdasdas



