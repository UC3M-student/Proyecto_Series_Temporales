
library(fpp2)
library(tseries)
library(quantmod)
library(stats)
library("goftest")

##### MEAN STATIONARY

mean_stationarity = function(time_serie_data) {
  
  options(warn=-1) #Suppress warnings
  
  library(tseries)
  
  differences = 0
  
  for (i in 1:30) {
    
    kpss_ = kpss.test(time_serie_data,null = "Level")
    pp_ =  pp.test(time_serie_data)
    adf_ = adf.test(time_serie_data)
    
    if (kpss_$p.value > 0.05 & pp_$p.value < 0.05) {
      
      
      options(warn=0) #Undo suppress warnings
      
      return(differences)
      
      #stop()
      
    }
    
    
    time_serie_data = diff(time_serie_data, differences = i)
    
    differences = differences + 1
    
  }
  
} 

##### ANALISIS DE DIAGNOSIS

diagnosys_phase = function(modelo,time_serie_data) {
  
  problems = 0 #Numero de errores en el diagnostico
  
  # COEFICIENTES NO SIGNIFICATIVOS
  
  coef_no_significativos = 0
  
  tryCatch( {
    
    for (i in 1:length(modelo$coef)) {
      
      if (qnorm(c(0.025,0.975),0,(modelo$var.coef^0.5)[i,i])[1] < modelo$coef[i] &  qnorm(c(0.025,0.975),0,(modelo$var.coef^0.5)[i,i])[2] > modelo$coef[i] ) {
        
        problems = problems + 1 
        
        coef_no_significativos = coef_no_significativos + 1
        
      }
      
    }
    
  }, error = function(e) {
    
    problems = 1000
    return(problems)
    
  }
  
  )
  
  # RESIDUOS INCORRELACIONADOS
  
  residuos_incorrelacionados = 0
  
  res <- residuals(modelo) #Residuals
  ggacf_tsd = ggAcf(time_serie_data)
  
  incorrelacion = Box.test(res, lag = max(ggacf_tsd$data$lag), type = c("Ljung-Box"))
  
  if (incorrelacion$p.value < 0.05) {
    
    problems = problems + 1
    
    residuos_incorrelacionados = residuos_incorrelacionados + 1
    
  }
  
  # NORMALIDAD EN LOS RESIDUOS
  
  normalidad_residuos = 0
  
  CvM_normal = cvm.test(res,"pnorm",0,sd(res),estimated = TRUE)
  ad_normal = ad.test(res,"pnorm",0,sd(res),estimated = TRUE)
  jb = jarque.bera.test(res)
  
  if (jb$p.value < 0.05) {
    
    problems = problems + 1
    normalidad_residuos = normalidad_residuos + 1
    
  }
  
  # Media marginal igual a cero - Esperanza nula
  
  media_marginal_cero = 0
  
  if (qnorm(c(0.025,0.975),0,sd(res)/sqrt(length(res)))[1] > mean(res) | qnorm(c(0.025,0.975),0,sd(res)/sqrt(length(res)))[2] < mean(res)) {
    
    problems = problems + 1
    media_marginal_cero = media_marginal_cero + 1
    
  }
  
  return(c(problems, coef_no_significativos,residuos_incorrelacionados, normalidad_residuos,media_marginal_cero))
  
}

##### TABLA RESUMEN ARIMA

ARIMA_Summary <- function(time_serie_data) {
  
  count_problems_vector <- c()
  ar_vector <- c()
  difference_vector <- c()
  ma_vector <- c()
  BIC_vector <- c()
  AIC_vector <- c()
  coef_no_significativos <- c()
  residuos_incorrelacionados <- c()
  normalidad_residuos <- c()
  media_marginal_cero <- c()
  contador <- 1
  
  for (i in 0:5) { # Ar
    for (j in 0:5) { # Ma
      for (k in 0:4) { # difference
        
        if (j == 0 & i == 0) {
          next
        } else {
          tryCatch({
            fit_ <- Arima(time_serie_data, c(i, k, j), include.constant = FALSE)
            count_problems_vector[contador] <- diagnosys_phase(fit_, time_serie_data)[1]
            ar_vector[contador] <- i
            ma_vector[contador] <- j
            difference_vector[contador] <- k
            BIC_vector[contador] <- fit_$bic
            AIC_vector[contador] <- fit_$aic
            coef_no_significativos[contador] <- diagnosys_phase(fit_, time_serie_data)[2]
            residuos_incorrelacionados[contador] <- diagnosys_phase(fit_, time_serie_data)[3]
            normalidad_residuos[contador] <- diagnosys_phase(fit_, time_serie_data)[4]
            media_marginal_cero[contador] <- diagnosys_phase(fit_, time_serie_data)[5]
            contador <- contador + 1
          }, error = function(e) {
            # Manejar el error aquí
            #next
            #cat("Error ajustando modelo ARIMA con parámetros:", i, k, j, "\n")
          })
        }
      }
    }
  }
  
  possible_solutions_table <- matrix(
    c(ar_vector, difference_vector, ma_vector, AIC_vector, BIC_vector,
      count_problems_vector, coef_no_significativos, residuos_incorrelacionados,
      normalidad_residuos, media_marginal_cero),
    ncol = 10
  )
  colnames(possible_solutions_table) <- c(
    "p", "d", "q", "AIC", "BIC", "Diagnosys_Problems", "Coef_no_signi",
    "Incorrelacion", "Normalidad", "Media_marginal_cero"
  )
  
  return(possible_solutions_table)
}


ARIMA_table = ARIMA_Summary(y)

