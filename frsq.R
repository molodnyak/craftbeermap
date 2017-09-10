library(jsonlite)
library(curl)

#FOURSQUARE.API
tokenfrsq <- "YOUR_API_KEY"
baseurlfrsq <- "https://api.foursquare.com/v2/venues/"

x <- strsplit(rd$FRSQ, '/')
rd$FRSQID <- unlist(lapply(x, function(x) x[6]))

#HTTP error 400 Control
rd[,"FRSQcheckins"] <- NA
rd[,"FRSQusers"] <- NA
rd[,"FRSQtip"] <- NA
rd[,"FRSQvisits"] <- NA
rd[,"FRSQlikes"] <- NA
rd[,"FRSQrating"] <- NA
rd[,"FRSQratingSignals"] <- NA
rd[,"FRSQphotos"] <- NA

#LOOP
for (i in 1:nrow(rd)){
  tryCatch({
    frsq <- fromJSON(paste0(baseurlfrsq, rd$FRSQID[i], "?oauth_token=", tokenfrsq, "&v=20131016"))
    if(length(frsq) == "0") {stop("ERROR: FOURSQUARE Page is unavailable ! - ", rd$Name[i])}
    else { 
      if(length(frsq) != "0") {
        
        rd$FRSQcheckins[i] <- frsq$response$venue$stats$checkinsCount
        rd$FRSQusers[i] <- frsq$response$venue$stats$usersCount
        rd$FRSQtip[i] <- frsq$response$venue$stats$tipCount
        rd$FRSQvisits[i] <- frsq$response$venue$stats$visitsCount
        rd$FRSQlikes[i] <- frsq$response$venue$likes$count
        rd$FRSQrating[i] <- frsq$response$venue$rating
        rd$FRSQratingSignals[i] <- frsq$response$venue$ratingSignals
        rd$FRSQphotos[i] <- frsq$response$venue$photos$count
        
      }
    }
    message("Scraping Fousquare ", paste(rd$Name[i], i, "from", nrow(rd), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#FRSQcheckinsRatio
rd$FRSQcheckinsRatio <- round(rd$FRSQcheckins/as.numeric(Sys.Date() - as.Date(rd$OpenDate, '%d/%m/%Y')), digits=3)
#FRSQvisitsRatio
rd$FRSQvisitsRatio <- round(rd$FRSQvisits/as.numeric(Sys.Date() - as.Date(rd$OpenDate, '%d/%m/%Y')), digits=3)
#FRSQcheckinsActivity
rd$FRSQcheckinsActivity <- round(rd$FRSQcheckins/rd$FRSQusers, digits=3)
#FRSQvisitscheckins
rd$FRSQvisitscheckins <- round(rd$FRSQcheckins/rd$FRSQvisits, digits=3)

#LABELS---------------------------------------------------
###checkinsCount (total checkins ever here) 
###usersCount (total users who have ever checked in here) 
###tipCount (number of tips here)
library(Hmisc)
#FRSQcheckins
label(rd$FRSQcheckins) <- c(FRSQcheckins="Чекины")
#FRSQusers
label(rd$FRSQusers) <- c(FRSQusers="Юзеры")
#FRSQtip
label(rd$FRSQtip) <- c(FRSQtip="Отзывы")
#FRSQvisits
label(rd$FRSQvisits) <- c(FRSQvisits="Визиты")
#FRSQlikes
label(rd$FRSQlikes) <- c(FRSQlikes="Лайки")

#FRSQcheckinsRatio
label(rd$FRSQcheckinsRatio) <- c(FRSQcheckinsRatio="Ср. чекинов в день")
#FRSQvisitsRatio
label(rd$FRSQvisitsRatio) <- c(FRSQvisitsRatio="Ср. визитов в день")
#FRSQcheckinsActivity
label(rd$FRSQcheckinsActivity) <- c(FRSQcheckinsActivity="Чекины/Юзеры")
#FRSQvisitscheckins
label(rd$FRSQvisitscheckins) <- c(FRSQvisitscheckins="Чекины/Посещения")
