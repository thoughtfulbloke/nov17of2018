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
           step15 = floor_date(NZ_time,unit = "15 minutes"))

# 3065709 mentions

anyNZ <- m_for_mentions %>% left_join(layout, by=c("user_id" = "name")) %>%
    rename(x1=x, y1=y) %>% left_join(layout, by=c("mentions" = "name")) %>%
    rename(x2=x, y2=y)  %>% filter(!is.na(x1) | !is.na(x2)) %>%
    count(user_id, mentions, x1, y1,x2,y2,step15) %>% mutate(scaled_alpha = n/max(n))

# 185756

allNZ <- anyNZ %>% filter(!is.na(x1) & !is.na(x2))
# 31818 messages
messagingNZ <- layout %>% filter(name %in% c(allNZ$user_id, allNZ$mentions))

# 1505 NZers
anibase <- ggplot(data=messagingNZ, aes(x=x,y=y)) + geom_point(size=0.05) + theme_void() +
    geom_segment(data=allNZ, aes(x=x1, y=y1, xend=x2, yend=y2, alpha=scaled_alpha)) + 
    theme(legend.position = "none") +
    labs(title = 'total 1505 NZ accounts & 67836 notifications, NZTime: {frame_time}') +
    transition_time(step15) +
    enter_fade() + 
    exit_fade()

animate(anibase, nframes = length(unique(allNZ$step15)))
anim_save("NZ.gif")
    
toNZ <- anyNZ  %>% filter(is.na(x1) & !is.na(x2)) %>%
    group_by(mentions,x2,y2,step15) %>% 
    summarise(n=sum(n)) %>% ungroup() %>%
    mutate(scaled_size=log10(n) + .001)
# 67715

plusbase <- ggplot(data=messagingNZ, aes(x=x,y=y)) + geom_point(size=0.05) + theme_void() +
        geom_segment(data=allNZ, aes(x=x1, y=y1, xend=x2, yend=y2, alpha=scaled_alpha)) +
    geom_point(data=toNZ, aes(x=x2,y=y2, size=scaled_size), colour="red") +
        theme(legend.position = "none") +
        labs(title = '1505 NZ accounts & 67836 NZ 67715 nonNZ notifications, {frame_time}NZ') +
        transition_time(step15) +
        enter_appear() + 
        exit_disappear()
    
animate(plusbase, nframes = length(unique(allNZ$step15)))
anim_save("NZplus.gif")

