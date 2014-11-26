## fitfunc Function takes parameters retention (vector of actual retention metrics), 
## ARPU (average revenue per user), and days_trailing (the number of days of retention
## data that the estimates are to be derived with).

fitfunc <- function(retention,arpu,days_trailing) {
  # Benchmarks for retention data
  benchmarks <- c(1,2,3,4,5,6,7,14,28,30)
  n <- match(days_trailing,benchmarks)
  days <- benchmarks[1:n]
  
  lndf <- log(data.frame(days,retention))
  coefs <- data.frame(day=numeric(n-1),a=numeric(n-1),b=numeric(n-1))
  
  #Populates coefs data frame with coefficients of linear regression models for each benchmark
  for(i in 1:(n-1)) {
    coefs$day[i] <- days[(i+1)]
    coefs$a[i] <- exp(lm(retention ~ days, data=lndf,subset=1:(i+1))$coef[1])
    coefs$b[i] <- lm(retention ~ days, data=lndf,subset=1:(i+1))$coef[2]
  }

  a <- coefs$a[match(days_trailing,coefs$day)]
  b <- coefs$b[match(days_trailing,coefs$day)]
  
  predictedRet <- data.frame(day=numeric(360),predicted_reten=numeric(360))
  
  # Uses linear regression model of the logs of retention to calculate predicted retention
  for(i in 1:360){
    predictedRet$day[i] = i 
    predictedRet$retention[i] = a * (i^b)
  }
  
  cat("Generalized Model: y = ",a,"* x ^",b,"\n")
  
  #Plot predicted retention
  plot(predictedRet$day,predictedRet$retention,type="l",main="Predicted Retention",
       xlim = c(0,30),ylab="Retention", xlab = "Day")

  # Sums the predicted retention to estimate duration
  ltv <- data.frame(day=c(30,90,180,360), duration = numeric(4), LTV = numeric(4))
  ltv$duration[1] <- sum(predictedRet$retention[1:30])
  ltv$duration[2] <- sum(predictedRet$retention[1:90])
  ltv$duration[3] <- sum(predictedRet$retention[1:180]) 
  ltv$duration[4] <- sum(predictedRet$retention[1:360]) 
  
  # Multiply duration by ARPU to estimate LTV
  ltv$LTV[1] <- ltv$duration[1] * arpu
  ltv$LTV[2] <- ltv$duration[2] * arpu
  ltv$LTV[3] <- ltv$duration[3] * arpu
  ltv$LTV[4] <- ltv$duration[4] * arpu
  ltv[,2:3] <- round(ltv[,2:3],2)
  print(ltv)
 
}
 
