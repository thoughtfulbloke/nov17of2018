
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

int_fa <- c(
    "x164070785","x313038011","x599817378",
    "x274316654","x18643437","x125128723",
    "x196168350","x1500129642","x592730371",
    "x1583865109","x902200087","x19091173",
    "x44067298","x537709549","x770619360062898176")
nz_af <- c(
    "x246646809","x172261647","x45246673",
    "x1022600937276243968","x17284137","x17167972",
    "x172262369","x20419879","x107642576",
    "x2741035986","x147406561","x15770483")

delta_f <- profset %>% mutate(int_fa = liked %in% int_fa, nz_af = liked %in% nz_af) %>%
    group_by(liker) %>% summarise(nett_fa = sum(int_fa), nett_af = sum(nz_af), allike= n()) 

delta_f %>% mutate(xliker = paste0("x", liker)) %>% select(-liker) %>% 
    write.csv(file="~/Syncplicity Folders/support_files/fa_spec.csv", row.names = FALSE)
