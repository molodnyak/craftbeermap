#GOOGLE.Fusion.Tables.API
library(jsonlite)
library(curl)

ratingdata <- fromJSON("https://www.googleapis.com/fusiontables/v1/query?sql=SELECT+Type,Name,Address,Time,Phone,VK,FB,INST,FRSQ,UNTAPPD,Ratebeer,OpenDate,Card,Tap,Room,'Table',Seat,Street,WC,Bartender,Liquor,Waiter,WiFi,Cuisine,Menu,Ring+FROM+1ZXX8vWvi8FQI6TSPtlNHX7gvcUefTlIzBdcspfaY++WHERE+Type+IN+('Бар','Магазин-бар')+and+Address+not+equal+to+''+and+Response+>=+5+and+Response+<=+10+order+by+Name+asc&key=AIzaSyD9OWz96j4E0XgWWFwxL7T-G_ilz0o11Vk")

#DataFrame
rd <- data.frame(ratingdata$rows, stringsAsFactors = F)
colnames(rd) <- ratingdata$columns

