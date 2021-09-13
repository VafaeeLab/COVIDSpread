update_data_model <- function(){

  source("getJHU.R")
  source("generateModel.R")
  source("setRegressors.R")

# Update ECDC -------------------------------------------------------------

  jhu <- getJHU()
  
  
# Update Models -----------------------------------------------------------

  allModels <- list()
  keep <- c()
  pred10 <- c()
  countries <- unique(as.character(colnames(jhu)))
  
  for (i in 1:length(countries)) {
    c <- countries[i]
    output <- try(generateModel(country = c, ms = 1, ecdc = jhu, r = 0, interventions.past = c(), interventions.future = c(), day.range = c()), silent = TRUE)
    if (class(output) != "try-error") {
      allModels[[i]] <- output
      p <- output$meanp*1e-5
      pred10 <- rbind(pred10,p)
      keep <- c(keep, i)
      print(paste(c,"pass", i, sep = "  "))
    }
    else{
      print(paste(c,"fail", i))
    }
  }

# Remove Models with Errors -----------------------------------------------

  jhu <- jhu[,keep]
  allModels <- allModels[keep]
  
  
  startDay = as.Date(gsub("_", "-", rownames(jhu)[1]))+1
  endDay = startDay + 9
  future = as.character(seq(startDay, endDay, by="day"))
  pred10 <- cbind(countries[keep], pred10)
  colnames(pred10) <- c("Country", future)
  pred10 <- pred10[-1,]

  saveRDS(allModels, file = "./Data/allModels.rds")
  write.csv(jhu, file = "./Data/jhu.csv", fileEncoding = "UTF-8")
  write.csv(pred10, file = "./Data/all_prediction_next_days.csv", row.names=FALSE, fileEncoding = "UTF-8")
}

