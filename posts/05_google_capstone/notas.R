# fitdata<-
#   fs::dir_ls("data/Fitabase Data 4.12.16-5.12.16/") %>%
#   map(~ .x %>% import(.)) %>%
#   Map(cbind, ., data_id=basename(names(.))) %>% #sirvió para la data origen
#   map(~.x %>% relocate(data_id, .before = everything()))

# fitdata_all<-
#   fitdata %>%
#   map(~.x %>% clean_names()) %>%
#   reduce(full_join, by=c("id")) # no corre porque la data es muy grande.

# weight<-import("data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv") %>%
#   mutate(Date=date(mdy_hms(Date)))
#
# sleep<-import("data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
#
# data_merged<-inner_join(dailyactivity, weight, by = c("Id", "Date"))

dailyactivity2<-
  dailyactivity %>%
  mutate(Id = as.character(Id)) %>%
  select(-c(Date, LoggedActivitiesDistance)) %>%
  group_by(Id) %>%
  summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE)))) %>%
  mutate(
    calories_int = Hmisc::cut2(Calories_mean, g = 3)
  ) #%>%
  # count(calories_int)

sleep2<-
  sleep %>%
  mutate(Id = as.character(Id)) %>%
  select(-c(SleepDay, TotalSleepRecords)) %>%
  group_by(Id) %>%
  summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE)))) %>%
  mutate(
    asleep_int = Hmisc::cut2(TotalMinutesAsleep_mean, g = 3)
  ) #%>%
  # count(asleep_int)

data_m<-
  inner_join(dailyactivity2, sleep2, by = "Id")

data_m %>%
  # filter(!(TotalSteps < 1)) %>%
  ggplot(aes(TotalSteps_mean, Calories_mean, color = calories_int)) +
  geom_point() #+
  # geom_smooth(method = "lm", se =FALSE)

data_m %>%
  # filter(!(TotalSteps < 1)) %>%
  ggplot(aes( Calories_mean,TotalMinutesAsleep_mean,  color = calories_int)) +
  geom_point() #+
# geom_smooth(method = "lm", se =FALSE)

data_m %>%
  # filter(!(TotalSteps < 1)) %>%
  ggplot(aes( Calories_mean,TotalMinutesAsleep_mean)) +
  geom_point()+
  geom_smooth(method = "lm", se =FALSE)


data_m %>%
  # filter(!(TotalSteps < 1)) %>%
  ggplot(aes(TotalSteps_mean, Calories_mean, color = calories_int)) +
  geom_point() #+
# geom_smooth(method = "lm", se =FALSE)

data_m %>%
  # filter(!(TotalSteps < 1)) %>%
  ggplot(aes(TotalSteps_mean, Calories_mean, color = asleep_int, shape = calories_int)) +
  geom_point() #+
# geom_smooth(method = "lm", se =FALSE)



####







mean_activity <-
  dailyactivity %>%
  select(-c(Id, Date, LoggedActivitiesDistance)) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
  # summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE))))


dailyactivity2 <-
  dailyactivity %>%
  mutate(
    activity_type = factor(case_when(
      SedentaryMinutes > mean_activity$SedentaryMinutes & LightlyActiveMinutes < mean_activity$LightlyActiveMinutes & FairlyActiveMinutes < mean_activity$FairlyActiveMinutes & VeryActiveMinutes < mean_activity$VeryActiveMinutes ~ "Sedentary Active",
      SedentaryMinutes < mean_activity$SedentaryMinutes & LightlyActiveMinutes > mean_activity$LightlyActiveMinutes & FairlyActiveMinutes < mean_activity$FairlyActiveMinutes & VeryActiveMinutes < mean_activity$VeryActiveMinutes ~ "Lightly Active",
      SedentaryMinutes < mean_activity$SedentaryMinutes & LightlyActiveMinutes < mean_activity$LightlyActiveMinutes & FairlyActiveMinutes > mean_activity$FairlyActiveMinutes & VeryActiveMinutes < mean_activity$VeryActiveMinutes ~ "Fairly Active",
      SedentaryMinutes < mean_activity$SedentaryMinutes & LightlyActiveMinutes < mean_activity$LightlyActiveMinutes & FairlyActiveMinutes < mean_activity$FairlyActiveMinutes & VeryActiveMinutes > mean_activity$VeryActiveMinutes ~ "Very Active",
      TRUE ~ "Average"
    ),levels=c("Average", "Sedentary Active", "Lightly Active", "Fairly Active", "Very Active")))

# ya hago estos niveles coloreo los dots y conclusiones


data_merged %>%
  ggplot() +
  geom_point(aes(Date, WeightKg))

data_merged %>%
  ggplot() +
  geom_point(aes(Date, TotalSteps))

data_merged %>%
  ggplot() +
  geom_point(aes(WeightKg, TotalSteps))

data_merged %>%
  ggplot(aes(TotalSteps, Calories, color = WeightKg)) +
  geom_point() +
  geom_smooth(method = "lm")

#----

dailyactivity2 %>%
  filter(!(TotalSteps < 1)) %>%
  ggplot(aes(TotalSteps, Calories, color = activity_type)) +
  geom_point() +
  geom_smooth(method = "lm", se =FALSE)

dailyactivity %>%
  filter(!(TotalSteps < 1)) %>%
  ggplot(aes(LightActiveDistance, Calories)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

dailyactivity %>%
  pivot_longer(cols = c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes"), names_to = "minute_type", values_to = "minute_count") %>%
  View()

dailyactivity %>%
  group_by(Id) %>%
  summarise(mean_cal=mean(Calories)) %>%
  arrange(-mean_cal) %>%
  ggplot(aes(x=mean_cal)) +
  geom_density()
