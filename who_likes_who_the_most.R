# assign NZness- based on make_hetangata, and the NZness indicated in test_NZfriend
# assign a NZ ness to accounts

library(rtweet)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)


have_data_on <- list.dirs("../fsc_acc", full.names=FALSE)
have_data_on <- have_data_on[have_data_on != ""]
# 12079642/fav_12079642.csv is the form for fave
# 12079642/who_12079642.csv is the form for profile
# 12079642/frn_12079642.csv is the form for friends (those followed)

onshore <-
    read_csv(
        "~/Syncplicity Folders/support_files/assignment.csv",
        col_types = cols(.default = col_character())
    ) %>% mutate(withx = paste0("x", user_id)) %>% filter(locat=="NZ")

have_nz <- have_data_on[have_data_on %in% onshore$user_id]

favs <- paste0("../fsc_acc/", have_nz, "/fav_", have_nz, ".csv")



read_favs<- function(x) {
    if(file.exists(x)){
        filinf <- file.info(x)
        if(filinf$size < 8){
            return(NULL)
        }
        stored_prof <- read.csv(x, colClasses = "character") %>% 
            select(user_id, favorited_by)
        return(stored_prof)
    } else {
        return(NULL)
    }
}

profset <- bind_rows(lapply(favs, read_favs)) 
profset <- profset %>% rename(liked = user_id, liker= favorited_by)
unfasc <- profset %>% group_by(liker) %>%
    mutate(bit_fasc = sum(liked %in% c("x164070785", "x313038011"))) %>% ungroup() %>%
    filter(bit_fasc == 0) %>%
    count(liker, liked) %>%
    group_by(liked) %>%
    summarise(mean_like= sum(n)/ 1354) %>% arrange(desc(mean_like))

antif <- lookup_users(gsub("x","",unfasc$liked[1:12]))

