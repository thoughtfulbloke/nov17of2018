
library(rtweet)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)

have_data_on <- list.dirs("../fsc_acc", full.names=FALSE)
have_data_on <- have_data_on[have_data_on != ""]

favs <- paste0("../fsc_acc/", have_data_on, "/fav_", have_data_on, ".csv")

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

thought_leaders <- read.csv("~/Syncplicity Folders/support_files/thought_leaders.csv",
                            stringsAsFactors = FALSE)
int_fa <- thought_leaders$user[thought_leaders$region == "terrible_twosome" |
                                   thought_leaders$region == "int_fa" ]


o_for_offshore <- read.csv("~/Syncplicity Folders/support_files/assignment.csv", colClasses = "character") %>%
    mutate(user_id = paste0("x", user_id))

aus_opp <- profset %>% mutate(user_id = paste0("x", liker)) %>%
    inner_join(o_for_offshore, by="user_id") %>% 
    filter(locat == "AU") %>% mutate(fa = liked %in% int_fa) %>%
    group_by(liker) %>% mutate(n_fa = sum(fa)) %>% ungroup() %>%
    filter(n_fa == 0) %>% count(liked) %>% arrange(desc(n)) %>% slice(1:12)
gb_opp <- profset %>% mutate(user_id = paste0("x", liker)) %>%
    inner_join(o_for_offshore, by="user_id") %>% 
    filter(locat == "GB") %>% mutate(fa = liked %in% int_fa) %>%
    group_by(liker) %>% mutate(n_fa = sum(fa)) %>% ungroup() %>%
    filter(n_fa == 0) %>% count(liked) %>% arrange(desc(n)) %>% slice(1:12)
us_opp <- profset %>% mutate(user_id = paste0("x", liker)) %>%
    inner_join(o_for_offshore, by="user_id") %>% 
    filter(locat == "US") %>% mutate(fa = liked %in% int_fa) %>%
    group_by(liker) %>% mutate(n_fa = sum(fa)) %>% ungroup() %>%
    filter(n_fa == 0) %>% count(liked) %>% arrange(desc(n)) %>% slice(1:12)
write.csv(bind_rows(aus_opp,gb_opp,us_opp), file="~/Desktop/oppos.csv")
