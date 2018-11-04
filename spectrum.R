
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
nz_af <- thought_leaders$user[thought_leaders$region == "NZaf"]
au_af <- thought_leaders$user[thought_leaders$region == "AUaf"]
gb_af <- thought_leaders$user[thought_leaders$region == "GBaf"]
us_af <- thought_leaders$user[thought_leaders$region == "USaf"]


delta_f <- profset %>% mutate(int_fa = liked %in% int_fa, nz_af = liked %in% nz_af,
                              au_af = liked %in% au_af, gb_af = liked %in% gb_af,
                              us_af = liked %in% us_af) %>%
    group_by(liker) %>% summarise(nett_fa = sum(int_fa), nett_nz = sum(nz_af),
                                  nett_au = sum(au_af), nett_gb = sum(gb_af),
                                  nett_us = sum(au_af), allike= n()) 

delta_f %>% mutate(xliker = paste0("x", liker)) %>% select(-liker) %>% 
    write.csv(file="~/Syncplicity Folders/support_files/fa_spec.csv", row.names = FALSE)
