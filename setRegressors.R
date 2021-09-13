setRegressors <- function(dates, interventions.past, interventions.future){
  
  past <- rep(0, length(dates))
  keep <- which(dates %in% interventions.past)
  past[keep] <- 1
  
  # print("Past")
  # print(dates[keep])
  
  last_day = as.Date(dates[length(dates)]%>% gsub("_","-",.))
  future_dates = seq.Date(last_day+1, last_day+10,1)%>% gsub("-","_",.)
  future <- rep(0, 10)
  keep <- which(future_dates %in% interventions.future)
  future[keep] <- 1
  
  # print("Future")
  # print(future_dates[keep])
  
  regg <- list()
  regg$reggt <- past
  regg$reggp <- future
  
  return(regg)
}