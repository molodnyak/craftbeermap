library(jsonlite)
library(curl)

#VK.API
tokenvk <- "72864e55fbc19be4559f88ddbbab6cb9d6456ecd3c299d7801d8d83a7229581a592bafc3242bfcdf84392"
baseurlvk <- "https://api.vk.com/method/"
#fields#city,country,place,description,wiki_page,members_count,counters,start_date,finish_date,can_post,
#fields#can_see_all_posts,activity,status,contacts,links,fixed_post,verified,site,ban_info

x <- strsplit(rd$VK, '/')
rd[,"VKID"] <- NA
rd$VKID <- unlist(lapply(x, function(x) x[4]))

#DROP
#rd <- rd[, !(colnames(rd) %in% c("VKmembers_count", "VKphotos"))]

#HTTP error 400 Control
rd[,"VKmembers_count"] <- NA
rd[,"VKphotos"] <- NA

#LOOP=groups.getById
for (i in 1:nrow(rd[!is.na(rd$VKID),])){
  tryCatch({
    vk <- fromJSON(paste0(baseurlvk, "groups.getById?", "group_id=", rd[!is.na(rd$VKID),]$VKID[i], "&fields=", 
                          "members_count,counters",
                          "&access_token=", tokenvk))
    if(length(vk) == "0") {stop("ERROR: VK Page is unavailable ! - ", rd[!is.na(rd$VKID),]$VKID[i])}
    else { 
      if(length(vk) != "0") {
        
        rd[!is.na(rd$VKID),]$VKmembers_count[i] <- vk$response$members_count
        rd[!is.na(rd$VKID),]$VKphotos[i] <- vk$response$counters$photos
        
      }
    }
    message("Scraping VK unit ", paste(rd[!is.na(rd$VKID),]$VKID[i], i, "from", nrow(rd[!is.na(rd$VKID),]), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#NA to zero
rd$VKmembers_count[is.na(rd$VKmembers_count)] <- 0
rd$VKphotos[is.na(rd$VKphotos)] <- 0


#LOOP=groups.getMembers
VKIDunique <- data.frame("VKID"=unique(rd[!is.na(rd$VKID),]$VKID), stringsAsFactors = F)
vkusers <- data.frame("VKID"="", "uid"="", "first_name"="", "last_name"="", 
                      "sex"="", "bdate"="", "city"="", "country"="", "relation"="", 
                      "occupation"="", "deactivated"="", stringsAsFactors = F)
vkusers <- vkusers[-1,]
for (i in as.integer(row.names(rd[is.na(rd$VKID)!=T,]))){
  tryCatch({

    if(length(vk) == "0") {stop("ERROR: VK Page is unavailable ! - ", rd$VKID[i])}
    else { 

        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)
        
        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", "&offset=", "1000", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)
        
        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", "&offset=", "2000", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)

        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", "&offset=", "3000", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)
        
        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", "&offset=", "4000", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)
        
        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", "&offset=", "5000", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)
        
        vk <- fromJSON(paste0(baseurlvk, "groups.getMembers?", "group_id=", rd$VKID[i], "&fields=", 
                              "sex,bdate,city,country,occupation,contacts,status,last_seen,common_count,relation",
                              "&sort=", "id_asc", "&offset=", "6000", 
                              "&access_token=", tokenvk))
        DF <- data.frame("VKID"=rd$VKID[i], "uid"=vk$response$users$uid, "first_name"=vk$response$users$first_name, "last_name"=vk$response$users$last_name, 
                         "sex"=vk$response$users$sex, "bdate"=vk$response$users$bdate, "city"=vk$response$users$city, 
                         "country"=vk$response$users$country, "relation"=vk$response$users$relation, 
                         "occupation"=vk$response$users$occupation$type, "deactivated"=vk$response$users$deactivated)
        vkusers <- rbind(vkusers, DF)
    }
    message("Parsing VK unit ", paste(rd$VKID[i], i, "from", nrow(rd$VKID), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Users correсtion
vkusers <- unique(vkusers)
vkuserstest <- data.frame("All"=table(vkusers$VKID), "NA"=table(vkusers$VKID[!is.na(vkusers$deactivated)]))
vkuserstest <- merge(table(vkusers$VKID), table(vkusers$VKID[!is.na(vkusers$deactivated)]), by="Var1")
names(vkuserstest) <- c("VKID", "All", "Deact")
vkuserstest$NARatio <- vkuserstest$Deact/vkuserstest$All

#HTTP error 400 Control
rd[,"VKmembers_deact"] <- NA
rd[,"VKsexF"] <- NA
rd[,"VKsexM"] <- NA
rd[,"VKoccupU"] <- NA
rd[,"VKoccupW"] <- NA
