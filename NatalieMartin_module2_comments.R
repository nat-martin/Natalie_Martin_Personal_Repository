### my comments preceded by `#CPK:`
#1 
library(tidyverse)
library(features)

#CPK: Need the data! not in repo. [-1] Added. 
pseed <- read_csv("pseed.fin.amps.csv")
pseed %>% 
  print
pseed.bl <- read_csv("pseed.lengths.csv")
pseed.bl %>%
  print()
speeds <- read_csv("pseed.calibration.csv")
speeds %>% 
  print()
pseed2 <- pseed %>% 
  left_join(speeds,by=c("speed"="vol")) %>% 
  print()
pseed.bl %>% 
  print()
pseed2 %>% 
  select(fish) %>% 
  unique()
pseed2 <- pseed2 %>% 
  left_join(pseed.bl,by="fish") %>% 
  print()
pseed2 <- pseed2 %>% 
  mutate(bl.s=cm.s/bl) %>% 
  print()
pseed.wide <- pseed2 %>% 
  select(-amp) %>% 
  pivot_wider(names_from = fin,values_from = amp.bl) %>% 
  mutate(amp.sum=L+R) %>% 
  print()

#2
SE <- function(x) {(sd(x)/sqrt(length(x)))}

#3 #CPK: didn't use find.peak to find the max amplitudes for each fish and speed [-2]
pseed.sum.max <- pseed.wide %>%
  group_by(speed, fish) %>%
  summarize(
    amp.sum.mean = mean(amp.sum, na.rm = TRUE),
    amp.sum.se = SE(amp.sum),
    .groups = 'drop'
  )

#4
ggplot(pseed.sum.max, aes(y=amp.sum.mean, x=speed, color=fish)) + geom_point() + geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se)) + labs(x="Speed", y="Mean Max Amplitude") +theme_classic()

#5
pseed.met.rate <- read_csv("pseed.met.rate.csv")
print(pseed.met.rate)
pseed.wide2 <- pseed.wide %>% 
  left_join(pseed.sum.max, by="speed")
print(pseed.wide2)
final_dat<- pseed.wide2 %>%
  left_join(pseed.met.rate, by = "date") 
print(final_dat)

#7
ggplot(final_dat, aes(x = amp.sum.mean, y = met.rate, color = fish)) + geom_point() + labs(x = "Mean Max Amplitude", y = "Metabolic Power Output") + theme_classic()




###CPK: This is really good. Nice work and excellent effort.


#Total points: 22

