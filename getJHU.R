getJHU <- function(){
  # install.packages("coronavirus")
  # install.packages("devtools")
  # devtools::install_github("RamiKrispin/coronavirus")
  
  
  
  c19 <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", header = T)
  df <- c19 %>% 
    filter(type == "confirmed") %>%
    select(date, country, cases) %>%
    group_by(date, country) %>%
    summarise(total_cases = sum(cases)) %>% 
    pivot_wider(names_from = country, values_from = total_cases)
   
  
  #df$date <- gsub("-", "_", df$date) 
  date <- format(as.Date(df$date), "%Y_%m_%d")
  x <- cbind(date, as.data.frame(apply(df[,-1], 2, cumsum)))
  df <- x %>% arrange(desc(date))
  rownames(df) <- df$date
  df <- df[,-1]
  colnames(df) <- gsub(" ", "_", colnames(df)) 
  return(df)
  
} 
