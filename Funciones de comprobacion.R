

##### ESTACIONARIEDAD EN MEDIA ###

mean_stationarity = function(time_serie_data) {
  
  options(warn=-1) #Suppress warnings
  
  library(tseries)
  
  for (i in 1:30) {
    
    kpss_ = kpss.test(time_serie_data,null = "Level")
    pp_ =  pp.test(time_serie_data)
    adf_ = adf.test(time_serie_data)
    
    
    if (kpss_$p.value > 0.05 & pp_$p.value < 0.05) {
      
      print(paste("MEDIA ESTACIONARIA; DIFERIMIENTO", i - 1))
      print(paste("P- VALUES -> PP = ", pp_$p.value, "; KPSS = ", kpss_$p.value))
      
      options(warn=0) #Undo suppress warnings
      stop()
      
    }
    
    else if (adf_$p.value < 0.05 & kpss_$p.value > 0.05) {
      
      print("########## POSIBLE SOLUCION #######")
      print(paste("POSIBLE MEDIA ESTACIONARIA; DIFERIMIENTO", i - 1))
      print(paste("P- VALUES -> ADF = ", adf_$p.value, "; KPSS = ", kpss_$p.value))
      print("######################################")
      
    }
    
    else if (adf_$p.value < 0.05 & pp_$p.value < 0.05) {
      
      print("########## POSIBLE SOLUCION #######")
      print(paste("POSIBLE MEDIA ESTACIONARIA; DIFERIMIENTO", i - 1))
      print(paste("P- VALUES -> ADF = ", adf_$p.value, "; PP = ", pp_$p.value))
      print("######################################")
      
      
    }
    
      time_serie_data = diff(time_serie_data, differences = i)
    
  }
  
} 


mean_stationarity(y)
