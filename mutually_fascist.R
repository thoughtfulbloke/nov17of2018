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

thought_leaders <- read.csv("~/Syncplicity Folders/support_files/thought_leaders.csv",
                            stringsAsFactors = FALSE)
fa2 <- thought_leaders$user[thought_leaders$region == "terrible_twosome"]

profset <- bind_rows(lapply(favs, read_favs)) 
profset <- profset %>% rename(liked = user_id, liker= favorited_by)
likefasc <- profset %>% 
    filter(liked %in% fa2) %>%
    count(liker, liked) 
antif <- lookup_users(gsub("x","",unfasc$liked[1:20]))
fasclike <- profset %>% 
    filter(liker %in% fa2)
# the top twelve include the usual suspects
ls_likes <- read.csv("~/Syncplicity Folders/support_files/ls_likes.csv", colClasses = "character") %>%
    count(user_id) %>% rename(lsn=n)
sm_likes <- read.csv("~/Syncplicity Folders/support_files/sm_likes.csv", colClasses = "character") %>%
    count(user_id) %>% rename(smn=n)
fasclike <- ls_likes %>% inner_join(sm_likes) %>%
    filter(! user_id %in% fa2) %>%
    mutate(fstot = lsn + smn) %>% arrange(desc(fstot)) %>% slice(1:13)
