#testing gganimate
library(rtweet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(ggthemes)
library(gganimate)
library(tidyr)

mytz = "Pacific/Auckland"
subject = "thoughtfulnz"
#testac <- get_timeline(subject, n=3000)
dftwt <- testac %>% mutate(inNZ = with_tz(created_at, tz=mytz),
                           dayw = wday(inNZ, label = TRUE),
                           dayn = wday(inNZ),
                           day_hour = ISOdatetime(2018,7,7,hour(inNZ), 0,
                                                  0, tz=mytz),
                           which_day = wday(day_hour, label = TRUE),
                           weekly_hour = day_hour + days(dayn)) %>%
    select(source, weekly_hour) 


dftwt %>% count(source, weekly_hour) %>% filter(source != "Twitter Lite") %>%
    spread(source, n, fill=0) %>% 
    ggplot(aes(ymax=`Twitter Web Client`,xmin=weekly_hour - 60*30, xmax=weekly_hour + 60*30)) +
    geom_rect(aes(ymin=-1 * `Twitter Web Client`), ymax=0, fill="darkblue") + 
    geom_rect(aes(ymax=`Twitter for iPad`), fill="green", ymin=0) + theme_tufte() + coord_flip() +
    labs(title = 'time', x = 'hour', y = 'activity') +
    transition_reveal(weekly_hour, weekly_hour) +
    ease_aes('linear')
anim_save("~/example2.gif")
