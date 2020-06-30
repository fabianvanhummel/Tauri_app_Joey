## In dit script maken we een body voor een app die raid data van de Tauri API afhaalt
url = "http://chapi.tauri.hu/apiIndex.php?apikey="
api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
secret <- "8a80fb7c81fb718a7a80339d689582642974ce12"
url_2 <- "achievement-firsts"
params <- data.frame(r = '[HU] Tauri WoW Server')

library(RCurl)
library(RJSONIO)
library(curl)
library(httr)

body <- list(secret = secret, url = url_2, params = params)

temp <- toJSON(body)
# Hier halen we beide vierkante haken weg met een gsub, kan niet in een pattern want dan zeurt hij over regular expressions (heel ander verhaal)
# Daarna moeten we HU en EN weer fixen, want daar moeten juist vierkante haken omheen, facking Hongaren
temp_2 <- gsub("HU", "[HU]", gsub("EN", "[EN]", gsub("\\[", "", gsub("\\]", "", paste(temp)))))

# JSON encoded
r <- POST(paste0(url, api_key), body = temp_2)

result <- rawToChar(r$content)
print(result)
