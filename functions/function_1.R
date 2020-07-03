GetTauri <- function(url,apikey,secret,url2,par,...){
  require(RCurl)
  require(RJSONIO)
  require(curl)
  require(httr)
  body <- list(secret = secret, url = url2, params = par)
  
  temp <- toJSON(body)
  # Hier halen we beide vierkante haken weg met een gsub, kan niet in een pattern want dan zeurt hij over regular expressions (heel ander verhaal)
  # Daarna moeten we HU en EN weer fixen, want daar moeten juist vierkante haken omheen, facking Hongaren
  temp_2 <- gsub("HU", "[HU]", gsub("EN", "[EN]", gsub("\\[", "", gsub("\\]", "", paste(temp)))))
  # JSON encoded
  r <- POST(paste0(url, apikey), body = temp_2)

    result <- rawToChar(r$content)
  print(result)
  return(result)
}