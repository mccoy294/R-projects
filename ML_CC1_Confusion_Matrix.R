library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

total_female <- filter(dat, y == "Female") %>% count()
total_female

online_female <- filter(dat, y == "Female", x =="online")%>% count()
online_female

inclass_female <- filter(dat, y == "Female", x =="inclass")%>% count()
inclass_female

inclass_total <- filter(dat, x =="inclass")%>% count()
inclass_total

online_total <-filter(dat, x =="online")%>% count()
online_total

inclass_female/inclass_total

online_female/online_total




y <- dat$sex
x <- dat$type
x

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$type == "online", "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

max(F_1)
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff


y_hat <- ifelse(test_set$type > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)