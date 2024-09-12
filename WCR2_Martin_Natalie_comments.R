### my comments preceded by `CPK:`
#Question 1

body_dat <- read_csv("body_data (1).csv")
head_dat <- read_csv("head_xy.csv")

head_wide <- head_dat %>% 
  pivot_wider(names_from = point, values_from = X:Y) %>% 
  mutate() %>% 
  print()

tibq3 <- body_dat %>% 
  mean(body_length) %>% 
  mean(body_depth) %>% 
  mean(head_wide) %>% 
  print()

###CPK: Solid attempt, but no operations on combined head and body data [-3]. This is something like what we needed. Notice how the objects head_dat and body_dat are established. 
head_dat <- read_csv("head_xy.csv")
body_dat <- read_csv("body_data.csv")

#CPK: widen head data with names from the point column and values from X and Y columns
head_wide <- head_dat %>%
  pivot_wider(names_from = point, values_from =X: Y) %>%
  mutate(jaw_length = (X_maxilla - Y_lowerjaw))

###CPK: now group the body data by fish then compute means with summarize
body_dat %>%
  group_by(fish) %>%
  summarize(
    mean_body_length = mean(body_length),
    mean_body_depth = mean(body_depth)
  ) %>% 
  ###CPK: join output with widened head data and means 
  left_join(
    head_wide %>%
      group_by(fish) %>%
      summarize(mean_jaw_length = mean(jaw_length)
      )
  )


###CPK: good effort, but have a look at the proposed solution and see if you can pick out the important steps

#Total points: 7

