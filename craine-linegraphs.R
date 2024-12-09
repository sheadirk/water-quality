# Libraries ----
library(tidyverse)
library(car)
library(AICcmodavg)
library(lubridate)
# library(xlsx)

# my data ---- 
craine<- read.csv("data/craine_ysi.csv")
glimpse(craine)

# create a column for year 
craine$date = as.Date(craine$date, 
                      format = "%m/%d/%Y")

# get month
craine$month = month(craine$date)
craine$month = month.name[craine$month]
craine$month = factor(craine$month,
                       levels = month.name,
                       labels = month.name)


# data manipulation ----
month_temps = craine %>% 
  group_by(month, depth) %>% 
  summarise(temp = mean(temp)) %>% 
  filter(!is.na(month))

# data analysis ---- 
temp = ggplot(month_temps, aes(x = depth, y = temp)) +
  geom_line() + geom_point() +
  facet_wrap(~ month, ncol = 8) +
  xlab("Depth (m)") +
  ylab(expression(paste("Temperature", " (\u00B0C)"))) +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
  ) +
  scale_x_reverse() +
  coord_flip()
print(temp)

# converting to jpeg
jpeg("shea_temp.jpg",
     width = 3500, height = 1800, res = 300
)
temp
dev.off()

glimpse(craine)


# dissolved oxygen 
month_do = craine %>% 
  filter(craine$date != "2022-10-13") %>% 
  group_by(month, depth) %>% 
  summarise(do = mean(do.mgl)) %>% 
  filter(!is.na(month))
 

# data analysis
shea_do = ggplot(month_do, aes(x = depth, y = do)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Dissolved Oxygen (mg/l)"))) +
  xlab("Depth (m)")+
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_do)

# converting to jpeg
jpeg("shea_do.jpg",
     width = 3500, height = 1800, res = 300
)
shea_do
dev.off()

# pH data
month_ph = craine %>% 
  filter(craine$date != "2022-10-13") %>% 
  group_by(month, depth) %>% 
  summarise(do = mean(ph)) %>% 
  filter(!is.na(month))

# data analysis
shea_ph = ggplot(month_ph, aes(x = depth, y = do)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("pH"))) +
  xlab("Depth (m)")+
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_ph)

# converting to jpeg
jpeg("shea_ph.jpg",
     width = 2400, height = 1800, res = 300
)
shea_ph
dev.off()

# spcond data
month_spcond = craine %>% 
  group_by(month, depth) %>% 
  summarise(do = mean(spcond)) %>% 
  filter(!is.na(month)) 
  

# data analysis
shea_spcond = ggplot(month_spcond, aes(x = depth, y = do)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Specific Conductance (mS/cm)"))) +
  xlab("Depth (m)")+
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_spcond)

# converting to jpeg
jpeg("shea_spcond.jpg",
     width = 2400, height = 1800, res = 300
)
shea_spcond
dev.off()


# data manipulation --
shea_data = craine %>% 
  filter(month %in% c("September", "October")) %>% 
  mutate(source = "shea")
glimpse(shea_data)


