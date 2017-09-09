
#read csv data
inst <- read.csv("C:/Users/Evgenii Molodniak/Documents/Travail/Site/Rating/INSTjuin.csv", header = T, sep = ";", stringsAsFactors = F)
rd <- cbind(rd, inst[, 4:6])

#Names correction
colnames(rd)[colnames(rd) == 'Posts'] <- 'INSTPosts'
colnames(rd)[colnames(rd) == 'Followers'] <- 'INSTFollowers'
colnames(rd)[colnames(rd) == 'Following'] <- 'INSTFollowing'

#INSTRatio
rd$INSTRatio <- round(rd$INSTPosts/as.numeric(Sys.Date() - 2 - as.Date(rd$OpenDate, '%d/%m/%Y')), digits=3)
#LABELS---------------------------------------------------
library(Hmisc)
#INSTPosts
label(rd$INSTPosts) <- c(INSTPosts="Посты")
#INSTFollowers
label(rd$INSTFollowers) <- c(INSTFollowers="Подписчики")
#INSTFollowing
label(rd$INSTFollowing) <- c(INSTFollowing="Подписки")
#INSTRatio
label(rd$INSTRatio) <- c(INSTRatio="Ср. постов в день")



#-----------------------------------------------------------------------------------------------------

library(cluster)
library(fpc)
y <- data.frame(rd$INSTFollowers, rd$FBfan_count, stringsAsFactors = F)
x <- rd$FBID
x[29] <- "onemoreus2"
row.names(y) <- x
clusters <- kmeans(y, centers=2)
clusplot(y, clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


##INSTAGRAM PARSING /////////////////// dont working /////////////////////
library(jsonlite)
library(curl)
apikeyio <- "791960314e3449749ef77d700609baa8a79762690a6902f84ad73318e56a81fe4f82f986dd107cf39a033dbd2185be74bc1fa1ae2107905b9dac69d36273f50f2e778e2af92cf667b178febd06dc7e9e"
baseurlio <- "https://extraction.import.io/query/extractor/"
extractorinst <- "612970d2-35b8-472b-8f37-2314364c044b"

x <- strsplit(rd$INST, '/')
v <- unlist(lapply(x, function(x) x[4]))
rm(x)
v <- data.frame("INSTID"=v, stringsAsFactors = F)
v <- na.omit(v)
row.names(v) <- 1:nrow(v)

#HTTP error 400 Control
v[,"instposts"] <- NA
v[,"instfollowers"] <- NA

#LOOP
for (i in 1:nrow(v)){
  tryCatch({
    inst <- fromJSON(paste0(baseurlio, extractorinst, "?_apikey=", apikeyio, "&url=https://instagram.com/", v$INSTID[i]))
    if(length(inst) == "0") {stop("ERROR: INSTAGRAM Page is unavailable ! - ", v$INSTID[i])}
    else { 
      if(length(inst) != "0") {
        
        v$instposts[i] <- inst$extractorData$data$group[[2]][[1]][[1]][[1]][1]
        v$instfollowers[i] <- inst$extractorData$data$group[[2]][[1]][[2]][[1]][1]
        
      }
    }
    message("Parsing INSTAGRAM unit ", paste(v$INSTID[i], i, "from", nrow(v), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




inst <- fromJSON("https://extraction.import.io/query/extractor/612970d2-35b8-472b-8f37-2314364c044b?_apikey=791960314e3449749ef77d700609baa8a79762690a6902f84ad73318e56a81fe4f82f986dd107cf39a033dbd2185be74bc1fa1ae2107905b9dac69d36273f50f2e778e2af92cf667b178febd06dc7e9e&url=https://instagram.com/beer.happens")

#TO NEWS
View(paste(paste0("'@", inst[,1], "'"), collapse = ', '))


