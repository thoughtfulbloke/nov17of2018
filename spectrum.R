
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
#nz_af now includes appraently non-fascist accounts from AU, US, and GB
nz_af <- c(
    "x246646809","x172261647","x45246673",
    "x1022600937276243968","x17284137","x17167972",
    "x172262369","x20419879","x107642576",
    "x2741035986","x147406561","x15770483")
au_af <- c(
    "x18925120", 
    "x2603192304", 
    "x160856325", 
    "x16094885", 
    "x233343317", 
    "x808815304382021632", 
    "x90360929", 
    "x49835260", 
    "x870629200348274688", 
    "x29420301", 
    "x48624704", 
    "x1108565574")

gb_af <- c(
    "x2546258378", 
    "x310746442", 
    "x3088596743", 
    "x3263253388", 
    "x978978695082848257", 
    "x722242009", 
    "x18020612", 
    "x65045121", 
    "x420366322", 
    "x531383314", 
    "x152656121", 
    "x813554157097459714")

us_af <- c(
    "x25073877", 
    "x133938408", 
    "x78523300", 
    "x3060489838", 
    "x1367531", 
    "x132339474", 
    "x1876660194", 
    "x292929271", 
    "x32871086", 
    "x251918778", 
    "x19084896", 
    "x2546258378")

delta_f <- profset %>% mutate(int_fa = liked %in% int_fa, nz_af = liked %in% nz_af,
                              au_af = liked %in% au_af, gb_af = liked %in% gb_af,
                              us_af = liked %in% us_af) %>%
    group_by(liker) %>% summarise(nett_fa = sum(int_fa), nett_nz = sum(nz_af),
                                  nett_au = sum(au_af), nett_gb = sum(gb_af),
                                  nett_us = sum(au_af), allike= n()) 

delta_f %>% mutate(xliker = paste0("x", liker)) %>% select(-liker) %>% 
    write.csv(file="~/Syncplicity Folders/support_files/fa_spec.csv", row.names = FALSE)
