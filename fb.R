
library(jsonlite)
library(curl)

#GOOGLE.Fusion.Tables.API

#May17
ratingdata <- fromJSON("https://www.googleapis.com/fusiontables/v1/query?sql=SELECT+Type,Name,Address,Time,Phone,VK,FB,INST,FRSQ,UNTAPPD,OpenDate,Card,Tap,Seat,Street,WC,Bartender,Liquor,Waiter,WiFi,Cuisine+FROM+1ZXX8vWvi8FQI6TSPtlNHX7gvcUefTlIzBdcspfaY++WHERE+Type+IN+('Бар','Магазин-бар')+and+Address+not+equal+to+''+and+Response+>=+5+and+Response+<=+10+order+by+Name+asc&key=AIzaSyD9OWz96j4E0XgWWFwxL7T-G_ilz0o11Vk")

#DataFrame
rd <- data.frame(ratingdata$rows, stringsAsFactors = F)
colnames(rd) <- ratingdata$columns

#----------------------------------------------
#Facebook.ID
x <- strsplit(rd$FB, '/')
rd$FBID <- unlist(lapply(x, function(x) x[4]))

#Facebook.API
##ToGEtToken Open https://developers.facebook.com/tools/explorer
tokenfb <- "" #<-------------------------------------- past your new access Token here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
baseurlfb <- "https://graph.facebook.com/v2.8/"


#HTTP error 400 Control
rd[,"FBname"] <- NA
rd[,"FBcity"] <- NA
rd[,"FBstreet"] <- NA
rd[,"FBlatitude"] <- NA
rd[,"FBlongitude"] <- NA
rd[,"FBbirthday"] <- NA
rd[,"FBphone"] <- NA
rd[,"FBhours"] <- NA
rd[,"FBfan_count"] <- NA
rd[,"FBcheckins"] <- NA
rd[,"FBrating_count"] <- NA

#LOOP=fields
for (i in 1:nrow(rd)){
  tryCatch({
    fb <- fromJSON(paste0(baseurlfb, rd$FBID[i], "?fields=", 
                          paste("name", "location", "birthday", "phone", "hours", "fan_count", "checkins", "rating_count", sep = "%2C"), 
                          "&access_token=", tokenfb))
    if(fb[[1]] == "") {stop("ERROR: Facebook Page is unavailable ! - ", rd$FBID[i])} 
    else { 
      if(fb[[1]] != "") {
        
        rd$FBname[i] <- fb$name
        rd$FBcity[i] <- ifelse(is.null(fb$location$city) == T, "", fb$location$city)
        rd$FBstreet[i] <- ifelse(is.null(fb$location$street) == T, "", fb$location$street)
        rd$FBlatitude[i] <- ifelse(is.null(fb$location$latitude) == T, "", fb$location$latitude)
        rd$FBlongitude[i] <- ifelse(is.null(fb$location$longitude) == T, "", fb$location$longitude)
        rd$FBbirthday[i] <- ifelse(is.null(fb$birthday) == T, "", fb$birthday)
        rd$FBphone[i] <- ifelse(is.null(fb$phone) == T, "", fb$phone)
        rd$FBhours[i] <- ifelse(is.null(fb$hours$mon_1_close) == T, "", local({
          monday <- ifelse(as.numeric(as.difftime(fb$hours$mon_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$mon_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$mon_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$mon_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$mon_1_open, format = "%H:%M")))
          tuesday <- ifelse(as.numeric(as.difftime(fb$hours$tue_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$tue_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$tue_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$tue_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$tue_1_open, format = "%H:%M")))
          wednesday <- ifelse(as.numeric(as.difftime(fb$hours$wed_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$wed_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$wed_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$wed_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$wed_1_open, format = "%H:%M")))
          thursday <- ifelse(as.numeric(as.difftime(fb$hours$thu_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$thu_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$thu_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$thu_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$thu_1_open, format = "%H:%M")))
          friday <- ifelse(as.numeric(as.difftime(fb$hours$fri_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$fri_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$fri_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$fri_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$fri_1_open, format = "%H:%M")))
          saturday <- ifelse(as.numeric(as.difftime(fb$hours$sat_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$sat_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$sat_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$sat_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$sat_1_open, format = "%H:%M")))
          sunday <- ifelse(as.numeric(as.difftime(fb$hours$sun_1_close, format = "%H:%M")) < 8, as.numeric(as.difftime(fb$hours$sun_1_close, format = "%H:%M")) + 24 - as.numeric(as.difftime(fb$hours$sun_1_open, format = "%H:%M")), as.numeric(as.difftime(fb$hours$sun_1_close, format = "%H:%M")) - as.numeric(as.difftime(fb$hours$sun_1_open, format = "%H:%M")))
          as.numeric(monday+tuesday+wednesday+thursday+friday+saturday+sunday)
        }, envir = new.env()))
        rd$FBfan_count[i] <- fb$fan_count
        rd$FBcheckins[i] <- fb$checkins
        rd$FBrating_count[i] <- fb$rating_count

      }
    }
    message("Scraping Facebook page ", paste(rd$FBID[i], i, "from", nrow(rd), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

rd$FBhours <- as.numeric(rd$FBhours)

#----------------------------------------------                        
#Посты-------------------------------------
rd[,"FBPostRatio"] <- NA
rd[,"FBLastPostDays"] <- NA

#LOOP=posts
for (i in 1:nrow(rd)){
  tryCatch({
    posts <- fromJSON(paste0(baseurlfb, rd$FBID[i], "/", "posts?", "limit=100", "&access_token=", tokenfb))
    if(length(posts) == "1") {rd$FBPostRatio[i] <- "0"} 
    else { 
      if(length(posts) == "2") {
        
        rd$FBPostRatio[i] <- ifelse(as.numeric(Sys.Date() - as.Date(posts$data$created_time[1])) <= 31, 
                                    round(length(posts$data$created_time)/as.numeric(Sys.Date() - as.Date(posts$data$created_time[length(posts$data$created_time)])), digits=3), 
                                    0)
        rd$FBLastPostDays[i] <- as.numeric(Sys.Date() - as.Date(posts$data$created_time[1]))

      }
    }
    message("Scraping Facebook posts ", paste(rd$FBID[i], i, "from", nrow(rd), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}        


#FBcheckinsRatio
rd$FBcheckinsRatio <- round(rd$FBcheckins/as.numeric(Sys.Date() - as.Date(rd$OpenDate, '%d/%m/%Y')), digits=3)

#---------------------------------------------- 
#Иностранцы-------------------------------------
rd[,"FBInter"] <- NA

#LOOP=insights page_fans_country
for (i in 1:nrow(rd)){
  tryCatch({
    insights <- fromJSON(paste0(baseurlfb, rd$FBID[i], "/insights/page_fans_country/lifetime?",
                                "&access_token=", tokenfb))
    if(length(posts) == "1") {rd$FBInter[i] <- "0"} 
    else { 
      if(length(posts) == "2") {
        
        rd$FBInter[i] <- round(1-insights$data$values[[1]][[1]][3,]$RU/sum(na.omit(unlist(insights$data$values[[1]]$value[3,]))), digits=3)

      }
    }
    message("Scraping Facebook insights ", paste(rd$FBID[i], i, "from", nrow(rd), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}    

#---------------------------------------------- 
#Упоминания группы в постах-------------------------------------
rd[,"FBtagged"] <- NA
rd[,"FBtaggedRatio"] <- NA
rd[,"FBLasttaggedDays"] <- NA

#LOOP=tagged
for (i in 1:nrow(rd)){
  tryCatch({
    tagged <- fromJSON(paste0(baseurlfb, rd$FBID[i], "?fields=id%2Cname%2Cband_interests%2Cdescription%2C",
                          "tagged.include_hidden(true).",
                          "since(2013-01-01)", ".",
                          "until(now)", ".",
                          "limit(100)", 
                          "&access_token=", tokenfb))
    if(length(tagged) == "0") {rd$FBtagged[i] <- "0"} 
    else { 
      if(length(tagged) > "0") {
        
        rd$FBtagged[i] <- length(tagged$tagged$data$tagged_time)
        rd$FBLasttaggedDays[i] <- as.numeric(Sys.Date() - as.Date(tagged$tagged$data$tagged_time[1]))
        rd$FBtaggedRatio[i] <- round(length(tagged$tagged$data$tagged_time)/(as.numeric(Sys.Date() - as.Date(tagged$tagged$data$tagged_time[length(tagged$tagged$data$tagged_time)]))/7), digits=3)
        
      }
    }
    message("Scraping Facebook tagged ", paste(rd$FBID[i], i, "from", nrow(rd), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
} 
