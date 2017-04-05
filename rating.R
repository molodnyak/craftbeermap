#GOOGLE.Fusion.Tables.API
library(jsonlite)
library(curl)

ratingdata <- fromJSON("https://www.googleapis.com/fusiontables/v1/query?sql=SELECT+Type,Name,Address,Time,Phone,VK,FB,INST,FRSQ,UNTAPPD,Ratebeer,OpenDate,Card,Tap,Room,'Table',Seat,Street,WC,Bartender,Liquor,Waiter,WiFi,Cuisine,Menu,Ring+FROM+1ZXX8vWvi8FQI6TSPtlNHX7gvcUefTlIzBdcspfaY++WHERE+Type+IN+('Бар','Магазин-бар')+and+Address+not+equal+to+''+and+Response+>=+5+and+Response+<=+10+order+by+Name+asc&key=AIzaSyD9OWz96j4E0XgWWFwxL7T-G_ilz0o11Vk")

#DataFrame
rd <- data.frame(ratingdata$rows, stringsAsFactors = F)
colnames(rd) <- ratingdata$columns

#LABELS
library(Hmisc)

#OpenDate
label(rd$OpenDate) <- c(OpenDate="Дата открытия")
#+Term=DiffDate
rd$LifeTime <- round(as.numeric(difftime(Sys.Date(), as.Date(rd$OpenDate, '%d/%m/%Y'), units = c("weeks"))/52.25), digits=2)
label(rd$LifeTime) <- c(LifeTime="Время жизни в годах")
#Card
rd$Card[rd$Card == "Есть"] <- '1'
rd$Card[rd$Card == "Нет"] <- '0'
rd$Card <- as.numeric(rd$Card)
label(rd$Card) <- c(Card="Оплата картой:1-Есть,0-Нет")
#Tap
rd$Tap <- as.numeric(rd$Tap)
label(rd$Tap) <- c(Tap="Краны")
#Room
rd$Room <- as.numeric(rd$Room)
label(rd$Room) <- c(Room="Залы")
#Table
rd$Table <- as.numeric(rd$Table)
label(rd$Table) <- c(Table="Столы")
#Seat
rd$Seat <- as.numeric(rd$Seat)
label(rd$Seat) <- c(Seat="Места")
#Street
rd$Street[rd$Street == "Есть"] <- '1'
rd$Street[rd$Street == "Нет"] <- '0'
rd$Street <- as.numeric(rd$Street)
label(rd$Street) <- c(Street="Места на улице:1-Есть,0-Нет")
#WC
rd$WC <- as.numeric(rd$WC)
label(rd$WC) <- c(WC="Туалеты")
#Bartender
rd$Bartender <- as.numeric(rd$Bartender)
label(rd$Bartender) <- c(Bartender="Бармены")
#+TapRatio
rd$TapRatio <- round(rd$Tap/rd$Seat, digits=3)
label(rd$TapRatio) <- c(TapRatio="Краны/Места")
#+WCRatio
rd$WCRatio <- round(rd$WC/rd$Seat, digits=3)
label(rd$WCRatio) <- c(WCRatio="Туалеты/Места")
#+BartenderRatio
rd$BartenderRatio <- round(rd$Bartender/rd$Seat, digits=3)
label(rd$BartenderRatio) <- c(BartenderRatio="Бармены/Места")
#Liquor
rd$Liquor[rd$Liquor == "Есть вино и крепкий алкоголь"] <- '2'
rd$Liquor[rd$Liquor == "Есть крепкий алкоголь"] <- '1'
rd$Liquor[rd$Liquor == "Нет"] <- '0'
rd$Liquor <- as.numeric(rd$Liquor)
label(rd$Liquor) <- c(Liquor="2-Вино и кр.алкоголь,1-Кр.алкоголь,0-Нет")
#Waiter
rd$Waiter[rd$Waiter == "Есть"] <- '1'
rd$Waiter[rd$Waiter == "Нет"] <- '0'
rd$Waiter <- as.numeric(rd$Waiter)
label(rd$Waiter) <- c(Waiter="Места на улице:1-Есть,0-Нет")
#WiFi
rd$WiFi[rd$WiFi == "Есть без пароля"] <- '2'
rd$WiFi[rd$WiFi == "Есть с паролем"] <- '1'
rd$WiFi[rd$WiFi == "Нет"] <- '0'
rd$WiFi <- as.numeric(rd$WiFi)
label(rd$WiFi) <- c(WiFi="2-Без пароля,1-С паролем,0-Нет")
#Cuisine
rd$Cuisine[rd$Cuisine == "Есть"] <- '1'
rd$Cuisine[rd$Cuisine == "Нет"] <- '0'
rd$Cuisine <- as.numeric(rd$Cuisine)
label(rd$Cuisine) <- c(Cuisine="Кухня:1-Есть,0-Нет")
#Menu
rd$Menu[rd$Menu == "Есть онлайн"] <- '2'
rd$Menu[rd$Menu == "Нет онлайн"] <- '1'
rd$Menu[rd$Menu == "Нет"] <- '0'
rd$Menu <- as.numeric(rd$Menu)
label(rd$Menu) <- c(Menu="2-Онлайн,1-Офлайн,0-Нет")
#Ring
rd$Ring <- as.numeric(rd$Ring)
label(rd$Ring) <- c(Ring="Расположение:3-Бульварное,2-Садовое,1-Третье,0-Мкад")
