
#read csv data
untap <- read.csv("C:/Users/Evgenii Molodniak/Documents/Travail/Site/Rating/UNTAPPDjuin.csv", header = T, sep = ";", stringsAsFactors = F)
rd <- cbind(rd, untap[, 3:5])

#Names correction
colnames(rd)[colnames(rd) == 'Total'] <- 'UNTAPTotal'
colnames(rd)[colnames(rd) == 'Unique'] <- 'UNTAPUnique'
colnames(rd)[colnames(rd) == 'Monthly'] <- 'UNTAPMonthly'

#UNTAPRatio
rd$UNTAPRatio <- round(rd$UNTAPTotal/as.numeric(Sys.Date() - 2 - as.Date(rd$OpenDate, '%d/%m/%Y')), digits=3)
#LABELS---------------------------------------------------
library(Hmisc)
#UNTAPTotal
label(rd$UNTAPTotal) <- c(UNTAPTotal="Чекины")
#UNTAPUnique
label(rd$UNTAPUnique) <- c(UNTAPUnique="Юзеры")
#UNTAPMonthly
label(rd$UNTAPMonthly) <- c(UNTAPMonthly="Ср. чекинов в месяц")
#UNTAPRatio
label(rd$UNTAPRatio) <- c(UNTAPRatio="Ср. чекинов в день")


# --------------------------------------------------------------------------------


##UNTAPPD PARSING /////////////////// dont working /////////////////////

library(jsonlite)
library(curl)

apikeyio <- "791960314e3449749ef77d700609baa8a79762690a6902f84ad73318e56a81fe4f82f986dd107cf39a033dbd2185be74bc1fa1ae2107905b9dac69d36273f50f2e778e2af92cf667b178febd06dc7e9e"
baseurlio <- "https://extraction.import.io/query/extractor/"
extractoruntap <- "50d463cc-c5dd-4b38-b400-d06c96a6ac2e"

x <- sapply(strsplit(rd$UNTAPPD, split= "/"), function(x) x[length(x)])
rd$FRSQID <- sapply(strsplit(rd$UNTAPPD, split= "/"), function(x) x[length(x)])

#??/venue/
v <- data.frame("UNTAPPD"=rd$UNTAPPD[rd$UNTAPPD != "javascript:void(0)"], stringsAsFactors = F)


#HTTP error 400 Control
v[,"untapTotal"] <- NA
v[,"untapUnique"] <- NA
v[,"untapMonthly"] <- NA
v[,"untapLast"] <- NA

for (i in 1:nrow(v)){
  tryCatch({
    untap <- fromJSON(paste0(baseurlio, extractoruntap, "?_apikey=", apikeyio, "&url=", v$UNTAPPD[i]))
    if(length(untap) == "0") {stop("ERROR: UNTAPPD Page is unavailable ! - ", v$UNTAPPD[i])}
    else { 
      if(length(untap) != "0") {
        
        v$untapTotal[i] <- untap$extractorData$data$group[[1]][[1]][[1]][[1]][1]
        v$untapUnique[i] <- untap$extractorData$data$group[[1]][[1]][[2]][[1]][1]
        v$untapMonthly[i] <- untap$extractorData$data$group[[1]][[1]][[3]][[1]][1]
        v$untapLast[i] <- untap$extractorData$data$group[[2]][[9]][[1]][[1]]

      }
    }
    message("Parsing UNTAPPD unit ", paste(v$UNTAPPD[i], i, "from", nrow(v), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

##ERROR : HTTP error 500.

for (i in c(7,9,15,30,31,42,52,57,65)){
  tryCatch({
    untap <- fromJSON(paste0(baseurluntap, "apikey=", apikeyuntap, "&url=", v$UNTAPPD[i]))
    if(length(untap) == "0") {stop("ERROR: UNTAPPD Page is unavailable ! - ", v$UNTAPPD[i])}
    else { 
      if(length(untap) != "0") {
        
        v$untapTotal[i] <- untap$extractorData$data$group[[1]][[1]][[1]][[1]][1]
        v$untapUnique[i] <- untap$extractorData$data$group[[1]][[1]][[2]][[1]][1]
        v$untapMonthly[i] <- untap$extractorData$data$group[[1]][[1]][[3]][[1]][1]
        v$untapLast[i] <- untap$extractorData$data$group[[2]][[9]][[1]][[1]]
        
      }
    }
    message("Parsing UNTAPPD unit ", paste(v$UNTAPPD[i], i, "from", nrow(v), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


##OUT FORMAT
for (i in c(16,34,))




untap <- fromJSON("https://extraction.import.io/query/extractor/50d463cc-c5dd-4b38-b400-d06c96a6ac2e?_apikey=791960314e3449749ef77d700609baa8a79762690a6902f84ad73318e56a81fe4f82f986dd107cf39a033dbd2185be74bc1fa1ae2107905b9dac69d36273f50f2e778e2af92cf667b178febd06dc7e9e&url=https://untappd.com/venue/2180553")

#v <- v[v$UNTAPPD != "javascript:void(0)",]
