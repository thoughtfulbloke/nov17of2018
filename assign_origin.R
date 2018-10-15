# assign NZness- based on make_hetangata, and the NZness indicated in test_NZfriend
# assign a NZ ness to accounts

library(rtweet)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggtern)
library(tidytext)
library(tokenizers) #latest from github
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

# profiles with location set to NZ or profiles whop have type macrons

read_profile <- function(x) {
    if(file.exists(x)){
        filinf <- file.info(x)
        if(filinf$size < 8){
            return(NULL)
        }
        stored_prof <- read.csv(x, colClasses = "character")
        return(stored_prof)
    } else {
        return(NULL)
    }
}
ao <- aotearoa %>% select(user_id) %>% mutate(cc = "NZ")
profset <- bind_rows(lapply(profiles, read_profile)) #40798

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

kwness <- profset %>% 
    mutate(locat = country_code,
           locat = ifelse(str_detect(description, "[āēīōūĀĒĪŌŪ]"), "NZ", locat),
           user_id = sub("x","", user_id)
           ) %>%
    left_join(ao) %>%
    mutate(locat = ifelse(!is.na(cc) & locat=="", "NZ", locat)) %>% select(-cc)

table(kwness$locat)
# 590 people at this stage
# check word frequncy of location field in unknowns to look for easy gains


kwness %>% filter(locat=="" & location != "" & !is.na(location)) %>% select(user_id, location) %>% 
    unnest_tokens(word, location) %>% count(word) %>% 
    arrange(desc(n)) %>% View()

# repeat earlier step with added rules learn from that check

kwness <- profset %>% 
    mutate(
        user_id = sub("x","", user_id),
        locat = country_code,
        locat = case_when(
            str_detect(description, "[āēīōūĀĒĪŌŪ]") ~ "NZ",
            str_detect(tolower(location), "usa") ~ "US",
            str_detect(tolower(location), "states") ~ "US",
            str_detect(tolower(location), "australia") ~ "AU",
            str_detect(tolower(location), "england") ~ "GB",
            str_detect(tolower(location), "zealand") ~ "NZ",
            str_detect(tolower(location), "canada") ~ "CA",
            str_detect(tolower(location), "texas") ~ "US",
            str_detect(tolower(location), "london") ~ "GB",
            str_detect(tolower(location), "california") ~ "US",
            str_detect(tolower(location), "florida") ~ "US",
            str_detect(tolower(location), "sydney") ~ "AU",
            str_detect(tolower(location), "uk") ~ "GB",
            str_detect(tolower(location), "tx") ~ "US",
            str_detect(tolower(location), "melbourne") ~ "AU",
            str_detect(tolower(location), "ny") ~ "US",
            str_detect(tolower(location), "ontario") ~ "CA",
            str_detect(tolower(location), "auckland") ~ "NZ",
            str_detect(tolower(location), "fl") ~ "US",
            str_detect(tolower(location), "washington") ~ "US",
            str_detect(tolower(location), "africa") ~ "ZA",
            str_detect(tolower(location), "wales") ~ "UK",
            str_detect(tolower(location), "america") ~ "US",
            str_detect(tolower(location), "france") ~ "FR",
            str_detect(tolower(location), "dc") ~ "US",
            str_detect(tolower(location), "angeles") ~ "US",
            str_detect(tolower(location), "carolina") ~ "US",
            str_detect(tolower(location), "toronto") ~ "CA",
            str_detect(tolower(location), "scotland") ~ "GB",
            str_detect(tolower(location), "queensland") ~ "AU",
            str_detect(tolower(location), "virginia") ~ "US",
            str_detect(tolower(location), "india") ~ "IN",
            str_detect(tolower(location), "kingdom") ~ "GB",
            str_detect(tolower(location), "az") ~ "US",
            str_detect(tolower(location), "ga") ~ "US",
            str_detect(tolower(location), "ireland") ~ "IR",
            str_detect(tolower(location), "il") ~ "US",
            str_detect(tolower(location), "chicago") ~ "US",
            str_detect(tolower(location), "nc") ~ "US",
            str_detect(tolower(location), "alberta") ~ "CA",
            str_detect(tolower(location), "or") ~ "US",
            str_detect(tolower(location), "ma") ~ "US",
            str_detect(tolower(location), "oh") ~ "US",
            str_detect(tolower(location), "tn") ~ "US",
            str_detect(tolower(location), "sweden") ~ "SE",
            str_detect(tolower(location), "vegas") ~ "US",
            str_detect(tolower(location), "netherlands") ~ "NL",
            str_detect(tolower(location), "deutschland") ~ "DE",
            str_detect(tolower(location), "atlanta") ~ "US",
            str_detect(tolower(location), "vancouver") ~ "CA",
            str_detect(tolower(location), "francisco") ~ "US",
            str_detect(tolower(location), "poland") ~ "PL",
            str_detect(tolower(location), "nj") ~ "US",
            str_detect(tolower(location), "ohio") ~ "US",
            str_detect(tolower(location), "seattle") ~ "US",
            str_detect(tolower(location), "brasil") ~ "BR",
            str_detect(tolower(location), "boston") ~ "US",
            str_detect(tolower(location), "arizona") ~ "US",
            str_detect(tolower(location), "calgary") ~ "CA",
            str_detect(tolower(location), "brazil") ~ "BR",
            str_detect(tolower(location), "colorado") ~ "US",
            str_detect(tolower(location), "british columbia") ~ "CA",
            str_detect(tolower(location), "ca") ~ "US", #looking through the data this was Calif not Canada
            str_detect(tolower(location), "u s ") ~ "US",
            str_detect(tolower(location), "brisbane") ~ "AU",
            str_detect(tolower(location), "nz") ~ "NZ",
            str_detect(tolower(location), "mn") ~ "US",
            str_detect(tolower(location), "polska") ~ "PL",
            str_detect(tolower(location), "switzerland") ~ "CZ",
            str_detect(tolower(location), "finland") ~ "FI",
            str_detect(tolower(location), "denver") ~ "US",
            str_detect(tolower(location), "wellington") ~ "NZ",
            str_detect(tolower(location), "kansas") ~ "US",
            str_detect(tolower(location), "pittsburgh") ~ "US",
            str_detect(tolower(location), "aotearoa") ~ "NZ",
            TRUE ~ locat)
    ) %>%
    left_join(ao) %>%
    mutate(locat = ifelse(!is.na(cc) & locat=="", "NZ", locat)) %>% select(-cc)

table(kwness$locat)
# 1467 NZ

ao2 <- ao %>% select(user_id, locat=cc)

longlist = kwness %>% select(user_id,locat) %>% filter(locat != "", !is.na(locat)) %>% rbind(ao2) %>%
    mutate(isNZ = ifelse(locat == "NZ", "NZ", "offshore")) %>% select(user = user_id, isNZ)




frnset_coded <- frnset %>% left_join(longlist, by=c("friends_id" = "user")) %>%
    group_by(user) %>%
    summarise(NZ = sum(isNZ == "NZ" & !is.na(isNZ)),
              offshore = sum(isNZ == "offshore" & !is.na(isNZ)),
              unknown = sum(is.na(isNZ))) %>%
    mutate(nprop = NZ / (NZ + offshore + unknown),
           oprop = offshore / (NZ + offshore + unknown),
           uprop = unknown / (NZ + offshore + unknown))

frnset_coded %>%
    ggplot(aes(x=nprop, y=oprop)) + geom_point(alpha=0.05)

ggtern(frnset_coded, aes(x=NZ, z=offshore, y=unknown)) + geom_point(size=0.3) +
    ggtitle("For each acocunt, the % of friends NZ, offshore, or unknown")

frnset_coded %>%
    ggplot(aes(y=nprop, x=NZ)) + geom_point(alpha=0.05)

rejoin <- frnset_coded %>%
    mutate(newloc = case_when(
        NZ > 25 | nprop > .5 ~ "NZ",
        NZ = 0 | (nprop < .02 & oprop > .25) ~ "offshore",
        TRUE ~ "unknown"
    )) %>%
    filter(newloc != unknown) %>%
    select(user_id = user, newloc)

kwness <- kwness %>% left_join(rejoin) %>%
    mutate(
        locat = case_when(
            locat == "" & newloc == "NZ" ~ "NZ",
            locat == "" & newloc == "offshore" ~ "offshore",
            TRUE ~ locat
        )
    ) %>% select(-newloc)

table(kwness$locat)
# NZ 1803

longlist = kwness %>% select(user_id,locat) %>% filter(locat != "", !is.na(locat)) %>% rbind(ao2) %>%
    mutate(isNZ = ifelse(locat == "NZ", "NZ", "offshore")) %>% select(user = user_id, isNZ)

frnset_coded <- frnset %>% left_join(longlist, by=c("friends_id" = "user")) %>%
    group_by(user) %>%
    summarise(NZ = sum(isNZ == "NZ" & !is.na(isNZ)),
              offshore = sum(isNZ == "offshore" & !is.na(isNZ)),
              unknown = sum(is.na(isNZ))) %>%
    mutate(nprop = NZ / (NZ + offshore + unknown),
           oprop = offshore / (NZ + offshore + unknown),
           uprop = unknown / (NZ + offshore + unknown))

frnset_coded %>%
    ggplot(aes(x=nprop, y=oprop)) + geom_point(alpha=0.05)

rejoin <- frnset_coded %>%
    mutate(newloc = case_when(
        NZ > 20 | nprop > .20 ~ "NZ",
        NZ < 5 & offshore > 100 | (nprop * 30) < oprop ~ "offshore",
        TRUE ~ "unknown"
    )) %>%
    filter(newloc != unknown) %>%
    select(user_id = user, newloc)

kwness <- kwness %>% left_join(rejoin) %>%
    mutate(
        locat = case_when(
            locat == "" & newloc == "NZ" ~ "NZ",
            locat == "" & newloc == "offshore" ~ "offshore",
            TRUE ~ locat
        )
    ) %>% select(-newloc)

table(kwness$locat)
# NZ 1901

write_out <- kwness %>% select(user_id, screen_name, locat)
write.csv(write_out, file="~/Syncplicity Folders/support_files/assignment.csv", row.names = FALSE)



