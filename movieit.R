library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(purrr))
library(tidyr)
library(ggraph)
library(igraph)
library(ggthemes)
library(gganimate)

set1 = "../fprog/canadian_racists"
set2 = "../fprog/mol_racist"
set3 = "../fprog/South_racist"
set4 = "../fprog/recent_direct"
set5 = "../fprog/recent_direct_regather600"
f1 <- paste0(set1, "/", list.files(set1, pattern=".csv$"))
f2 <- paste0(set2, "/", list.files(set2, pattern=".csv$"))
f3 <- paste0(set3, "/", list.files(set3, pattern=".csv$"))
f4 <- paste0(set4, "/", list.files(set4, pattern=".csv$"))
f5 <- paste0(set5, "/", list.files(set5, pattern=".csv$"))
full_list = c(f1,f2,f3,f4,f5)

v_for_visitor <- bind_rows(lapply(full_list, read_csv, col_types=cols(
    .default = col_character()))) %>% distinct()
# 666092 tweets

# generate graph data
nzm <- read.csv("~/Syncplicity Folders/support_files/nzmatches.csv",
                colClasses = "character")
graph <- graph_from_data_frame(nzm)
layout <- create_layout(graph, layout = 'fr') %>% select(x,y,name) %>%
    mutate(name = paste0("x",as.character(name)))
# 1840 NZ people

# mentions

m_for_mentions <- v_for_visitor %>% select(created_at, user_id, mentions_user_id) %>%
    mutate(mentions = map(mentions_user_id, function(x){unlist(strsplit(x, split=" "))})) %>%
    unnest(mentions) %>% 
    mutate(NZ_time = with_tz(ymd_hms(created_at), tz="Pacific/Auckland"),
           step1 = floor_date(NZ_time,unit = "hour")) %>%
    filter(step1 >= ISOdatetime(2018,8,1,0,0,0, tz="Pacific/Auckland"),
           step1 < ISOdatetime(2018,8,4,0,0,0, tz="Pacific/Auckland"))

# 598612 mentions

toNZer <- m_for_mentions %>% left_join(layout, by=c("user_id" = "name")) %>%
    rename(x1=x, y1=y) %>% left_join(layout, by=c("mentions" = "name")) %>%
    rename(x2=x, y2=y)  %>% filter(!is.na(x2)) %>%
    count(user_id, mentions, x1, y1,x2,y2,step1)

# sum(toNZer$n) 44062

allNZ <- toNZer %>% filter(!is.na(x1))
# sum(allNZ$n) 19111 messages

messagingNZ <- layout %>% filter(name %in% c(allNZ$user_id, allNZ$mentions))

# 1117 NZers
anibase <- ggplot(data=messagingNZ, aes(x=x,y=y)) + geom_point(size=0.05) + theme_void() +
    geom_segment(data=allNZ, aes(x=x1, y=y1, xend=x2, yend=y2), lwd=0.1) + 
    theme(legend.position = "none") +
    labs(title = 'total 1117 NZ accounts & 19111 notifications, NZTime: {frame_time}') +
    transition_time(step1) +
    enter_fade() + 
    exit_fade()

animate(anibase, nframes = length(unique(allNZ$step1)))
anim_save("NZ.gif")
    
from_oe <- toNZer  %>% filter(is.na(x1)) %>%
    group_by(mentions,x2,y2,step1) %>% 
    summarise(n=sum(n)) %>% ungroup() %>%
    rename(n_oe = n)
from_nz <- toNZer  %>% filter(!is.na(x1)) %>%
    group_by(mentions,x2,y2,step1) %>% 
    summarise(n=sum(n)) %>% ungroup() %>%
    rename(n_nz = n)

from_all <- from_oe %>%
    full_join(from_nz) %>%
    mutate(n_oe = ifelse(is.na(n_oe), 0, n_oe),
           n_nz = ifelse(is.na(n_nz), 0, n_nz),
           n_total = n_oe + n_nz,
           log10mentions = log10(n_total),
           oe_prop = n_oe / n_total)

plusbase <- ggplot(data=messagingNZ, aes(x=x,y=y)) + geom_point(size=0.05) + theme_void() +
        geom_segment(data=allNZ, aes(x=x1, y=y1, xend=x2, yend=y2), lwd=0.2) +
    geom_point(data=from_all, aes(x=x2,y=y2, size=log10mentions, colour=oe_prop)) +
    scale_colour_viridis_c() +
        labs(title = '1117 NZ accounts & 34105 notifications, {frame_time} NZtime') +
        transition_time(step1) +
        enter_appear() + 
        exit_disappear()
    
animate(plusbase, nframes = length(unique(allNZ$step1)) * 6)
anim_save("NZplus.gif")

