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


autoplot(y_53)
ggAcf(y_46) #FAS
ggPacf(y_46) #FAC 

y_dif_45<-diff(y_49, differences = 2) 
autoplot(y_dif)

adf.test(y_49) #H1 = Estacionario 
kpss.test(y_49,null = "Level") #H1: NO ESTACIONARO
pp.test(y_49) # H1: estacionario

adf.test(y_dif) #H1 = Estacionario 
kpss.test(y_dif,null = "Level") #H1: NO ESTACIONARO
pp.test(y_dif) # H1: estacionario
ggAcf(y_dif) #FAS
ggPacf(y_dif)

auto.arima(y_49)

ts.plot(y_45,y_46,y_47,y_48,y_49,y_50,y_51,y_52,y_53,gpars = list(col = rainbow(9)))

y_dif_45<-diff(y_45, differences = 2) 
y_dif_46<-diff(y_46, differences = 2) 
y_dif_47<-diff(y_47, differences = 2) 
y_dif_48<-diff(y_48, differences = 2) 
y_dif_49<-diff(y_49, differences = 2) 
y_dif_50<-diff(y_50, differences = 2) 
y_dif_51<-diff(y_51, differences = 2) 
y_dif_52<-diff(y_52, differences = 2) 
y_dif_53<-diff(y_53, differences = 2) 


ts.plot(y_45,y_46,y_47,y_48,y_49,y_50,y_51,y_52,y_53,gpars = list(col = rainbow(9)))

ts.plot(y_dif_45,y_dif_46,y_dif_47,y_dif_48,y_dif_49,y_dif_50,y_dif_51,y_dif_52,y_dif_53,gpars = list(col = rainbow(9)))


