
# finding the 10% of the sample most embedded in NZ

library(rtweet)
library(dplyr)
library(lubridate)


aotearoa <- read.csv("~/Syncplicity Folders/support_files/aotearoa.csv", 
                     colClasses = "character")

have_data_on <- list.dirs("../fsc_acc", full.names=FALSE)
have_data_on <- have_data_on[have_data_on != ""]
# 12079642/fav_12079642.csv is the form for fave
# 12079642/who_12079642.csv is the form for profile
# 12079642/frn_12079642.csv is the form for friends (those followed)
favs <- paste0("../fsc_acc/", have_data_on, "/fav_", have_data_on, ".csv")
profiles <- paste0("../fsc_acc/", have_data_on, "/who_", have_data_on, ".csv")
frns <- paste0("../fsc_acc/", have_data_on, "/frn_", have_data_on, ".csv")

read_frns <- function(x) {
    if(file.exists(x)){
        filinf <- file.info(x)
        if(filinf$size < 8){
            return(NULL)
        }
        stored_frns <- read.csv(x, colClasses = "character")
        stored_frns %>%
            mutate(friends_id = gsub("x","",user_id)) %>%
            select(user, friends_id) %>%
            return()
    } else {
        return(NULL)
    }
}

frnset <- bind_rows(lapply(frns, read_frns))


frnset$kiwi <- as.numeric(frnset$friends_id %in% aotearoa$user_id)
recalc <- frnset %>% group_by(user) %>% summarise(kw = sum(kiwi))
sum(recalc$kw > 10) # 1878

hetangata <- unique(c(aotearoa$user_id, recalc$user[recalc$kw > 10]))

frnset$kiwi <- as.numeric(frnset$friends_id %in% hetangata)
recalc <- frnset %>% group_by(user) %>% summarise(kw = sum(kiwi))
sum(recalc$kw > 10) # 2933

hetangata <- unique(c(hetangata, recalc$user[recalc$kw > 10]))
frnset$kiwi <- as.numeric(frnset$friends_id %in% hetangata)
recalc <- frnset %>% group_by(user) %>% summarise(kwness = sum(kiwi))
sum(recalc$kw > 10) # 4697

#So for around 10% of the accoutns we can call them kiwi
hetangata <- unique(c(hetangata, recalc$user[recalc$kw > 10]))

writeLines(hetangata, "~/Syncplicity Folders/support_files/hetangata.txt")