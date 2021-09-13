transformation <- function (data.model,r,reggt)
{  
#this function apply four data transformations and find the best fitting model for each transformation

# No transformation -------------------------------------------------------
data <- data.model
st <- adf.test(data)
diff1 <-0

if (r==1) 
{
  xregd1 <- reggt
  
  xregd1 <- na.remove(xregd1)
  sample.model1 <- auto.arima (data, xreg=xregd1, allowdrift = FALSE,approximation = FALSE,allowmean = FALSE)
  sample.model1
  sig1 <- (1-pnorm(abs(sample.model1$coef)/sqrt(diag(sample.model1$var.coef))))*2
} 

if (r==0) 
{
  sample.model1 <- auto.arima (data, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE)
  #print(sample.model1)
  forecast(sample.model1,h=10)
  sig1 <- (1-pnorm(abs(sample.model1$coef)/sqrt(diag(sample.model1$var.coef))))*2
}


# Ratio Transformation ----------------------------------------------------

data <- ts(data.model, start=c(1,1))
data <- data[-1]/data[-length(data)]
data <- ts(data, start=c(1,1))
st <- adf.test(data)
diff2 <-0

#plot(data)
if (r==1)
{
  xregd2 <- reggt [-1]
  
  xregd2 <- na.remove(xregd2)
  sample.model2 <- auto.arima (data, xreg=xregd2, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE)
  sample.model2
  sig2 <- (1-pnorm(abs(sample.model2$coef)/sqrt(diag(sample.model2$var.coef))))*2
}

if (r==0)
{
  sample.model2 <- auto.arima (data, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE)
  #print(sample.model2)
  sig2 <- (1-pnorm(abs(sample.model2$coef)/sqrt(diag(sample.model2$var.coef))))*2
}


# Power Transformation ----------------------------------------------------

data <- ts(data.model, start=c(1,1))

tk_index(data)
data <- data^(1/tk_index(data))
data <- ts(data, start=c(1,1))
st <- adf.test(data)
diff3 <-0

#plot(data)
if (r==1)
{
  xregd3 <- reggt

  xregd3 <- na.remove(xregd3)
  sample.model3 <- auto.arima (data, xreg=xregd3, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE)
  sample.model3
  sig3 <- (1-pnorm(abs(sample.model3$coef)/sqrt(diag(sample.model3$var.coef))))*2
}

if (r==0) 
{
  sample.model3 <- auto.arima (data, allowdrift = FALSE, allowmean = FALSE,approximation = FALSE)
  #print(sample.model3)
  forecast(sample.model3,h=10)
  sig3 <- (1-pnorm(abs(sample.model3$coef)/sqrt(diag(sample.model3$var.coef))))*2
}


# logaritmic Transformation -----------------------------------------------


data <- ts(data.model, start=c(1,1))
data <- log(data)
data <- ts(data, start=c(1,1))
st <- adf.test(data)
diff4 <-0

#plot(data)
if (r==1)
{
  xregd4 <- reggt
 
  xregd4 <- na.remove(xregd4)
  sample.model4 <- auto.arima (data, xreg=xregd4, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE)
  sample.model4
  sig4 <- (1-pnorm(abs(sample.model4$coef)/sqrt(diag(sample.model4$var.coef))))*2
}

if (r==0) 
{
  sample.model4 <- auto.arima (data, allowdrift = FALSE, allowmean = FALSE,approximation = FALSE)
  #print(sample.model4)
  forecast(sample.model4,h=10)
  sig4 <- (1-pnorm(abs(sample.model4$coef)/sqrt(diag(sample.model4$var.coef))))*2
}

#output
output <- list()
output$model1 <- sample.model1
output$model2 <- sample.model2
output$model3 <- sample.model3
output$model4 <- sample.model4

output$sig1 <- sig1
output$sig2 <- sig2
output$sig3 <- sig3
output$sig4 <- sig4

return (output)
}