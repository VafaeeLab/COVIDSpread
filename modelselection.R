modelselection <- function(ms,rmse1,rmse2,rmse3,rmse4)
{
  
  if (ms==2)
  {
    rmse2 <- Inf
    rmse3 <- Inf
    rmse4 <- Inf
  }
  if (ms==3)
  {
    rmse1 <- Inf
    rmse3 <- Inf
    rmse4 <- Inf
  }
  if (ms==4)
  {
    rmse1 <- Inf
    rmse2 <- Inf
    rmse4 <- Inf
  }
  if (ms==5)
  {
    rmse1 <- Inf
    rmse2 <- Inf
    rmse3 <- Inf
  }
  
  
#output
  
  output <- list()
  output$rmse1 <- rmse1
  output$rmse2 <- rmse2
  output$rmse3 <- rmse3
  output$rmse4 <- rmse4
  

  return (output) 
}