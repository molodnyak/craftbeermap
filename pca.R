#PCA for Rating
#Data for PCA
### Without "FRSQvisits", "FRSQvisitsRatio" !!!!!!!!!!

rdpca <- rd[, c("LifeTime", "Tap", "Seat", "WC", "Bartender", "TapRatio", "WCRatio", "BartenderRatio", 
                 "FBhours", "FBfan_count", "FBInter", 
                 "FBPostRatio", "FBtaggedRatio", "FBlikesRatio", "FBcommentsRatio", "FBsharedpostsRatio", 
                 "VKmembers", "VKsexF", 
                 "FRSQcheckins", "FRSQusers", "FRSQcheckinsActivity", "FRSQcheckinsRatio", 
                 "INSTFollowers", "INSTRatio", "UNTAPTotal", "UNTAPUnique", "UNTAPMonthly",  
                 "Card", "Street", "Liquor", "Waiter", "WiFi", "Cuisine")]
row.names(rdpca) <- rd$FBID

#------------------------------------------

#Share bias correction !!! --> Self share by administrators
########FBID craftstationbeer and TheBestBar
plot(rd$FBlikesRatio, rd$FBsharedpostsRatio, col= "LightSeaGreen", pch = 19, cex = 1, lty = "solid", lwd = 2, 
     xlab="Facebook likes Ratio", ylab="Facebook shared posts Ratio", xlim=c(-5, 35), ylim=c(-1, 5))
text(rd$FBlikesRatio, rd$FBsharedpostsRatio, labels=rd1$Name, cex= 0.8, pos=1)

## FBsharedpostsRatio BIAS CORRECTIONS / to zero
rdpca["craftstationbeer",]$FBsharedpostsRatio <- 0
rdpca["TheBestBar",]$FBsharedpostsRatio <- 0

# INSTFollowers BIAS CORRECTIONS
########INSTID bar.good.idea and beertime_bar
plot(rd$FBfan_count, rd$INSTFollowers, col= "LightSeaGreen", pch = 19, cex = 1, lty = "solid", lwd = 2, 
     xlab="Facebook Fans", ylab="Instagram Followers", xlim=c(-500, 5000), ylim=c(-1000, 23000))
text(rd$FBfan_count, rd$INSTFollowers, labels=rd$Name, cex= 0.8, pos=1)

## to median
### rd$Name[75]
### rd$Name[47]
rdpca["BarGoodIdea",]$INSTFollowers <- median(rdpca$INSTFollowers[-c(47, 75)])
rdpca["1704849409750613",]$INSTFollowers <- median(rdpca$INSTFollowers[-c(47, 75)])


#rdpca["676662795832911",]$Street <- 0
#------------------------------------------

#Nominal variables as factors
rdpca[,28:33] <- lapply(rdpca[,28:33], as.factor)

#------------------------------------------

#PCA
library(factoextra)
library(FactoMineR)
res.pca <- PCA(rdpca, scale.unit=TRUE, quali.sup=c(28:33), graph=F)
###The amount of variation retained by each PC is called eigenvalues. 
###The first PC corresponds to the direction with the maximum amount of variation in the data set
##Percentage of variances
fviz_screeplot(res.pca, ncp=5, barfill = "LightSeaGreen", barcolor = "LightSeaGreen", linecolor = "black")

##Graph of variables
##cos2 - the squared loadings for numerical variables
fviz_pca_var(res.pca, scale.unit=TRUE, col.var="cos2", magnify = 3, cex = 3, lty=2) + 
  scale_color_gradient2(low="cyan1", mid="blue", high="red", midpoint=0.5) + 
  theme_minimal()

##Draw confidence ellipses around the categories
plotellipses(res.pca, keepvar = "quali.sup", magnify = 2, cex = 1.5, pch = 19, pch.means=2)

##Graph of individuals
#fviz_pca_ind(res.pca)
##The squared loadings for numerical variables (cos2)
View(res.pca$var$cos2)
res.pca$var$cos2[,1][order(-res.pca$var$cos2[,1])]
##The squared loadings for nominal variables (cos2)
View(res.pca$quali.sup$cos2)
###  Components with a large value of cos2 contribute a relatively large portion to
###  the total distance and therefore these components are important for that observation 
#------------------------------------------

#PCA RATING Calculate
rdJune <- rd
rd$Street[rd$FBID == "676662795832911"] <- 0

## FBsharedpostsRatio BIAS CORRECTIONS / to zero
rd$FBsharedpostsRatio[rd$FBID == "craftstationbeer"] <- 0
rd$FBsharedpostsRatio[rd$FBID == "TheBestBar"] <- 0

rd$INSTFollowers[rd$FBID == "BarGoodIdea"] <- median(rdpca$INSTFollowers[-c(47, 75)])
rd$INSTFollowers[rd$FBID == "1704849409750613"] <- median(rdpca$INSTFollowers[-c(47, 75)])

##LOADINGS TABLE
rdloadings <- data.frame("Loadings" = res.pca$var$cos2[,1], stringsAsFactors = F)
rdloadings[28:33,1] <- res.pca$quali.sup$cos2[row.names(res.pca$quali.sup$cos2) == '1',1]
row.names(rdloadings)[28:33] <- c("Card", "Street", "Liquor", "Waiter", "WiFi", "Cuisine")
rdloadings$Weighted <- round(rdloadings$Loadings/sum(rdloadings$Loadings)*100, digits = 3)
rd$Rating <- NA
rd$Rating <- (rd$LifeTime-min(rd$LifeTime))/(max(rd$LifeTime)-min(rd$LifeTime))*rdloadings["LifeTime",1] + 
  (rd$Tap-min(rd$Tap))/(max(rd$Tap)-min(rd$Tap))*rdloadings["Tap",1] + 
  (rd$Seat-min(rd$Seat))/(max(rd$Seat)-min(rd$Seat))*rdloadings["Seat",1] + 
  (rd$WC-min(rd$WC))/(max(rd$WC)-min(rd$WC))*rdloadings["WC",1] + 
  (rd$Bartender-min(rd$Bartender))/(max(rd$Bartender)-min(rd$Bartender))*rdloadings["Bartender",1] + 
  (rd$TapRatio-min(rd$TapRatio))/(max(rd$TapRatio)-min(rd$TapRatio))*rdloadings["TapRatio",1] + 
  (rd$WCRatio-min(rd$WCRatio))/(max(rd$WCRatio)-min(rd$WCRatio))*rdloadings["WCRatio",1] + 
  (rd$BartenderRatio-min(rd$BartenderRatio))/(max(rd$BartenderRatio)-min(rd$BartenderRatio))*rdloadings["BartenderRatio",1] + 
  (rd$FBhours-min(rd$FBhours))/(max(rd$FBhours)-min(rd$FBhours))*rdloadings["FBhours",1] + 
  (rd$FBfan_count-min(rd$FBfan_count))/(max(rd$FBfan_count)-min(rd$FBfan_count))*rdloadings["FBfan_count",1] + 
  (rd$FBInter-min(rd$FBInter))/(max(rd$FBInter)-min(rd$FBInter))*rdloadings["FBInter",1] + 
  (rd$FBPostRatio-min(rd$FBPostRatio))/(max(rd$FBPostRatio)-min(rd$FBPostRatio))*rdloadings["FBPostRatio",1] + 
  (rd$FBtaggedRatio-min(rd$FBtaggedRatio))/(max(rd$FBtaggedRatio)-min(rd$FBtaggedRatio))*rdloadings["FBtaggedRatio",1] + 
  (rd$FBlikesRatio-min(rd$FBlikesRatio))/(max(rd$FBlikesRatio)-min(rd$FBlikesRatio))*rdloadings["FBlikesRatio",1] + 
  (rd$FBcommentsRatio-min(rd$FBcommentsRatio))/(max(rd$FBcommentsRatio)-min(rd$FBcommentsRatio))*rdloadings["FBcommentsRatio",1] + 
  (rd$FBsharedpostsRatio-min(rd$FBsharedpostsRatio))/(max(rd$FBsharedpostsRatio)-min(rd$FBsharedpostsRatio))*rdloadings["FBsharedpostsRatio",1] + 
  (rd$VKmembers-min(rd$VKmembers))/(max(rd$VKmembers)-min(rd$VKmembers))*rdloadings["VKmembers",1] + 
  (rd$VKsexF-min(rd$VKsexF))/(max(rd$VKsexF)-min(rd$VKsexF))*rdloadings["VKsexF",1] + 
  (rd$FRSQcheckins-min(rd$FRSQcheckins))/(max(rd$FRSQcheckins)-min(rd$FRSQcheckins))*rdloadings["FRSQcheckins",1] + 
  (rd$FRSQusers-min(rd$FRSQusers))/(max(rd$FRSQusers)-min(rd$FRSQusers))*rdloadings["FRSQusers",1] + 
  (rd$FRSQcheckinsActivity-min(rd$FRSQcheckinsActivity))/(max(rd$FRSQcheckinsActivity)-min(rd$FRSQcheckinsActivity))*rdloadings["FRSQcheckinsActivity",1] + 
  (rd$FRSQcheckinsRatio-min(rd$FRSQcheckinsRatio))/(max(rd$FRSQcheckinsRatio)-min(rd$FRSQcheckinsRatio))*rdloadings["FRSQcheckinsRatio",1] + 
  (rd$INSTFollowers-min(rd$INSTFollowers))/(max(rd$INSTFollowers)-min(rd$INSTFollowers))*rdloadings["INSTFollowers",1] + 
  (rd$INSTRatio-min(rd$INSTRatio))/(max(rd$INSTRatio)-min(rd$INSTRatio))*rdloadings["INSTRatio",1] + 
  (rd$UNTAPTotal-min(rd$UNTAPTotal))/(max(rd$UNTAPTotal)-min(rd$UNTAPTotal))*rdloadings["UNTAPTotal",1] +
  (rd$UNTAPUnique-min(rd$UNTAPUnique))/(max(rd$UNTAPUnique)-min(rd$UNTAPUnique))*rdloadings["UNTAPUnique",1] + 
  (rd$UNTAPMonthly-min(rd$UNTAPMonthly))/(max(rd$UNTAPMonthly)-min(rd$UNTAPMonthly))*rdloadings["UNTAPMonthly",1] + 
  rd$Card*rdloadings["Card",1] +
  rd$Street*rdloadings["Street",1] +
  rd$Liquor*rdloadings["Liquor",1] +
  rd$Waiter*rdloadings["Waiter",1] +
  rd$WiFi*rdloadings["WiFi",1] +
  rd$Cuisine*rdloadings["Cuisine",1]

#RANK All +
rd$Rank <- NA
rd$Rank <- rank(-rd$Rating)
#LABELS
library(Hmisc)
#Rating
label(rd$Rating) <- c(Rating="�������")
#Rank
label(rd$Rank) <- c(Rank="�����")
#------------------------------------------

#Final RATING Table
row.names(rdloadings[order(-rdloadings$Weighted),])
ratingApr17 <- rating

rating <- rd[, c("Rank","Rating", "FBpic", "VKpic50", "VKpic100", "Name", "Type", "Address", "FB", 
                                row.names(rdloadings[order(-rdloadings$Weighted),]))]

#RANK
rating$Rank <- rank(-rating$Rating)

#LABELS
library(Hmisc)
#Rating
label(rating$Rating) <- c(Rating="�������")
#Rank
label(rating$Rank) <- c(Rank="�����")

#SORT
rating <- rating[order(-rating$Rating),]


#Save .csv
row.names(rating) <- 1:nrow(rating)
write.table(rating, file="ratingJuin17.csv", row.names = F, col.names = T, sep = ";", fileEncoding = "UTF-8")


## Loadings / Weighted table
###gvisTable
library(googleVis)

plot(gvisTable(data.frame("Variable"=row.names(rdloadings), rdloadings), 
               formats=list(Loadings = "0.000000", Weighted = "0.000"), 
               options=list(width=350, page='enable', height='automatic', sortColumn=2, sortAscending = F)))
###pieChart
x <- data.frame("Variable" = row.names(rdloadings[order(-rdloadings$Weighted),]), 
                "Weights" = rdloadings[order(-rdloadings$Weighted),]$Weighted, stringsAsFactors = F)
x[20,] <- c("Other15", sum(x[20:35,"Weights"]))
x <- x[1:20,]
x$Weights <- as.numeric(x$Weights)

plot(gvisPieChart(x,
                  labelvar = "Variable", numvar = "Weighted", 
                  options = list(title='Top 20 weights', width=400, height='automatic', pieHole=0.5, legend='none')))

plot(gvisBarChart(x, xvar = "Variable", yvar = c("Weighted"), 
                     options = list(width=400, height='automatic', legend='none')))

## STARS for final table
plot(rating$Rating, rating$Rank)
plot(rating$Rating, rating$Rank[order(rating$Rank)], col= "LightSeaGreen", pch = 19, cex = 1, lty = "solid", lwd = 2, 
     xlab="Rating Value", ylab="Rank", xlim=c(0, 5.7), ylim=rev(range(seq(0, 80, 10))))
abline(v = c(2,5))

## ICONS

x <- fromJSON("https://www.googleapis.com/fusiontables/v1/query?sql=SELECT+Name,Pic+FROM+1wYRtmoIk4kq7XkQG2ysFEveCwQDpLzfjNjF7Jayo&key=AIzaSyD9OWz96j4E0XgWWFwxL7T-G_ilz0o11Vk")
View(x$rows)

#-----------------------------------------------------------------------------------------------------


############################ 12.04.2017 CHANGES
plot(rdloadings$Weighted, rdloadings1$Weighted, col= "LightSeaGreen", pch = 19, cex = 1, lty = "solid", lwd = 2, 
     xlab="New Weights", ylab="Old Weights")
text(rdloadings$Weighted, rdloadings1$Weighted, labels=row.names(rdloadings), cex= 0.8, pos=1)
identify(rdloadings$Weighted, rdloadings1$Weighted, labels = row.names(rdloadings))


# + Supp
#Data for PCA
#-c(28,29,57,58) delete One more pub / Easy Pub (not craft and double)
rdpca <- rd[-c(28,29,57,58), c("LifeTime", "Tap", "Seat", "WC", "Bartender", "TapRatio", "WCRatio", "BartenderRatio", 
              "FBhours", "FBfan_count", "FBInter", 
              "FBPostRatio", "FBtaggedRatio", "FBlikesRatio", "FBcommentsRatio", "FBsharedpostsRatio", 
              "VKmembers", "VKsexF", 
              "FRSQcheckins", "FRSQusers", "FRSQvisits", "FRSQcheckinsActivity", "FRSQcheckinsRatio", "FRSQvisitsRatio", 
              "INSTFollowers", "INSTRatio", "UNTAPUnique", "UNTAPMonthly",  
              "Card", "Street", "Liquor", "Waiter", "WiFi", "Cuisine")]
x <- rd$FBID[-c(28,29,57,58)]
#x[29] <- "onemoreus2"
row.names(rdpca) <- x
rdpca[,29:34] <- lapply(rdpca[,29:34], as.factor)
res.pca <- PCA(rdpca, scale.unit=TRUE, quali.sup=c(29:34), graph=F)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca, scale.unit=TRUE, col.var="cos2") + 
  scale_color_gradient2(low="cyan", mid="blue", high="red", midpoint=0.5) + 
  theme_minimal()

fviz_pca_biplot(res.pca, col.var="cos2", addEllipses = T) + 
  scale_color_gradient2(low="gray30", mid="blue", high="red", midpoint=0.5)

res.desc <- dimdesc(res.pca, axes = c(1,2))
res.desc$Dim.1

plot.PCA(res.pca, choix="ind",invisible="ind")
plot(res.pca,choix="ind",new.plot=FALSE)
plot(res.pca,choix="var",new.plot=FALSE)
##Draw confidence ellipses around the categories
plotellipses(res.pca)

View(res.pca$quali.sup$cos2)

##Correlation circle
fviz_pca_var(res.pca)

#LOADINGS



View(data.frame(rd$Name, rd$Rating, rd$Rank))

#-----------------------------------------------------------------------------------------------------

#+++++++++++++++

#Data for MCA
rdpca1 <- rdpca
rdpca1$Card[rdpca1$Card == 1] <- 'Yes'
rdpca1$Card[rdpca1$Card == 0] <- 'No'
rdpca1$Street[rdpca1$Street == 1] <- 'Yes'
rdpca1$Street[rdpca1$Street == 0] <- 'No'
rdpca1$Liquor[rdpca1$Liquor == 1] <- 'Yes'
rdpca1$Liquor[rdpca1$Liquor == 0] <- 'No'
rdpca1$Waiter[rdpca1$Waiter == 1] <- 'Yes'
rdpca1$Waiter[rdpca1$Waiter == 0] <- 'No'
rdpca1$WiFi[rdpca1$WiFi == 1] <- 'Yes'
rdpca1$WiFi[rdpca1$WiFi == 0] <- 'No'
rdpca1$Cuisine[rdpca1$Cuisine == 1] <- 'Yes'
rdpca1$Cuisine[rdpca1$Cuisine == 0] <- 'No'
rdpca1[,30:35] <- lapply(rdpca1[,30:35], as.character)
res.pca1 <- PCA(rdpca1, scale.unit=TRUE, quali.sup=c(30:35), graph=F)
fviz_pca_var(res.pca1, scale.unit=TRUE, col.var="cos2") + 
  scale_color_gradient2(low="lightblue", mid="blue", high="red", midpoint=0.5) + 
  theme_minimal()
View(data.frame(res.pca$quali.sup$cos2[,1], res.pca1$quali.sup$cos2[,1]))
View(data.frame(res.pca$var$cos2[,1], res.pca$var$cos2[,1]))
plotellipses(res.pca1, keepvar = "quali.sup", magnify = 3, cex = 1, pch = 19, lwd = 6, pch.means=2, type = c("g","p"))



x <- rd$FBID
x[29] <- "onemoreus2"
row.names(rdpca) <- x
rdpca[,31:38] <- lapply(rdpca[,31:38], as.factor)

#PCA
library(factoextra)
library(FactoMineR)
#quanti.sup: vector of indexes of continuous supplementary variables
#quali.sup: vector of indexes of categorical supplementary variables
res.mca <- MCA(rdpca, quanti.sup=c(1:28))

plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.9)
### = plot.MCA(res.mca, invisible=c("var"), cex=0.9)
plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.9)
### = plot.MCA(res.mca, invisible=c("ind"), cex=0.9)
plot.MCA(res.mca, invisible=c("ind", "var"), cex=0.9)

View(res.mca$var$cos2)

View(res.mca$quanti.sup$coord)

##################### FACTOR ANALYSIS
rdpca1[,30:35] <- lapply(rdpca1[,30:35], as.factor)
res <- MFA(rdpca1, group=c(9,20,6), type=c("c","c","n"), ncp=5, name.group=c("oflinenum","online","oflinecat"))


fviz_pca_var(res.pca1, scale.unit=TRUE, col.var="cos2") + 
  scale_color_gradient2(low="lightblue", mid="blue", high="red", midpoint=0.5) + 
  theme_minimal()




#-----------------------------------------------------------------------------------------------------
rdpca <- rd[c("Card", "Street", "Liquor", "Waiter", "WiFi", "Cuisine", "Menu", "Ring", "FRSQcheckinsRatio")]
rdpca[,1:8] <- lapply(rdpca[,1:8], as.factor)
res.mca <- MCA(rdpca, quanti.sup=9)


## homals
library(homals)
res.homals <- homals(rdpca, level = c(rep("numerical",28),rep("nominal",6)), itermax = 1000)

#-----------------------------------------------------------------------------------------------------

###FBID correction

row.names(rdpca) <- c("1516", "Bad.Bro.Bar", "BeerHappens", "BeerMarket", "BeerMood", "Beerbox", "Beertep", 
                      "BestBar", "BoozerBar", "BorodaBar", "BottleCraft", "CansAndBeer", "CraftAndDraft", 
                      "CraftOrchestra", "CraftStation", "CraftRepublic", "DrunkPunk", "FckingCraftPub", 
                      "HLSTK", "HopHeadCraft", "HowardLovesCraft", "ILikeCraft", "Jawsspot", "Kraftwerk", 
                      "LittleCraftBar", "LoadingCraftBar", "LosBandidos", "Parka", "Pasternak", "Pivbar", 
                      "Pivbar2", "Pivbar3", "RULEtaproom", "RedCodeBar", "RightHops", "RollingBarrels")

#QUALITATIVE Variables correction
rdpca$Liquor[rdpca$Liquor == '2'] <- 1
rdpca$WiFi[rdpca$WiFi == '2'] <- 1

#-c(28,29,57,58) delete One more pub / Easy Pub (not craft and double)
rdpca <- rd[-c(28,29,57,58), c("LifeTime", "Tap", "Seat", "WC", "Bartender", "TapRatio", "WCRatio", "BartenderRatio", 
                               "FBhours", "FBfan_count", "FBInter", 
                               "FBPostRatio", "FBtaggedRatio", "FBlikesRatio", "FBcommentsRatio", "FBsharedpostsRatio", 
                               "VKmembers", "VKsexF", 
                               "FRSQcheckins", "FRSQusers", "FRSQvisits", "FRSQcheckinsActivity", "FRSQcheckinsRatio", "FRSQvisitsRatio", 
                               "INSTFollowers", "INSTRatio", "UNTAPTotal", "UNTAPUnique", "UNTAPMonthly",  
                               "Card", "Street", "Liquor", "Waiter", "WiFi", "Cuisine")]
row.names(rdpca) <- rd$FBID[-c(28,29,57,58)]

#------------------------------------------
rd1 <- rd[-c(28,29,57,58),]
#Share bias correction !!! --> Self share by administrators
########FBID craftstationbeer and TheBestBar
plot(rd1$FBlikesRatio, rd1$FBsharedpostsRatio, col= "LightSeaGreen", pch = 19, cex = 1, lty = "solid", lwd = 2, 
     xlab="Facebook likes Ratio", ylab="Facebook shared posts Ratio", xlim=c(-5, 35), ylim=c(-1, 5))
text(rd1$FBlikesRatio, rd1$FBsharedpostsRatio, labels=rd1$Name, cex= 0.8, pos=1)

## FBsharedpostsRatio BIAS CORRECTIONS / to zero
### rdpca$FBsharedpostsRatio[8] <- 0
### rdpca$FBsharedpostsRatio[15] <- 0
rdpca["craftstationbeer",]$FBsharedpostsRatio <- 0
rdpca["TheBestBar",]$FBsharedpostsRatio <- 0

# INSTFollowers BIAS CORRECTIONS
########INSTID bar.good.idea and beertime_bar
plot(rd1$FBfan_count, rd1$INSTFollowers, col= "LightSeaGreen", pch = 19, cex = 1, lty = "solid", lwd = 2, 
     xlab="Facebook Fans", ylab="Instagram Followers", xlim=c(-500, 5000), ylim=c(-1000, 13000))
text(rd1$FBfan_count, rd1$INSTFollowers, labels=rd1$Name, cex= 0.8, pos=1)

## to median
### rdpca$INSTFollowers[45] <- median(rdpca$INSTFollowers[-c(45, 69)])
### rdpca$INSTFollowers[69] <- median(rdpca$INSTFollowers[-c(45, 69)])
rdpca["BarGoodIdea",]$INSTFollowers <- median(rdpca$INSTFollowers[-c(45, 69)])
rdpca["1704849409750613",]$INSTFollowers <- median(rdpca$INSTFollowers[-c(45, 69)])

# ------------
modelINST <- lm(INSTFollowers ~ UNTAPUnique, data = rdpca[-c(47, 75)])
summary(modelINST)
rm(modelINST)


x <- data.frame("Var1" = c("Magasin-bar","Bar"), "Freq" = c(52, 15))
plot(gvisPieChart(x,
                  labelvar = "Var1", numvar = "Freq", 
                  options = list(title='', width=400, height='automatic', pieHole=0.4, legend='top', 
                                 colors="['teal', 'crimson']")))