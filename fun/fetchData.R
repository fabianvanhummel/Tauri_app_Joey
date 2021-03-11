fetchData <- function(input,url,api_key,secret){
  
  data <- list()
  par <- data.frame( r = input$servername , n = input$name)
  data <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret,"character-sheet",par = par))$response
  
  return(data)
}