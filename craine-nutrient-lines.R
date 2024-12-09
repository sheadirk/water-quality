# Libraries
library(tidyverse)
library(car)
library(AICcmodavg)
library(lubridate)
# library(xlsx)

# my data ---- 
craine = read.csv("data/craine_nutrients.csv")
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


# data manipulation --
month_nitro = craine %>% 
  group_by(month, depth) %>% 
  summarise(nitro = mean(nitro)) %>% 
  filter(!is.na(month))

# data analysis  
shea_nitro = ggplot(month_nitro, aes(x = depth, y = nitro)) +
  geom_line() + geom_point() +
  facet_wrap(~ month, ncol = 8) +
  xlab("Depth (m)") +
  ylab(expression(paste("Nitrate + Nitrite (mg/L)"))) +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_nitro)

# converting to jpeg
shea_nitro
jpeg("shea_nitro.jpg",
     width = 3200, height = 1800, res = 300
)
shea_nitro
dev.off()

# total nitrogen
month_tn = craine %>% 
  group_by(month, depth) %>% 
  summarise(tn = mean(tn)) %>% 
  filter(!is.na(month))

# data analysis
shea_tn = ggplot(month_tn, aes(x = depth, y = tn)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Total Nitrogen (mg/L)"))) +
  xlab("Depth (m)")+
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_tn)

# converting to jpeg
jpeg("shea_tn.jpg",
     width = 2400, height = 1800, res = 300
)
shea_tn
dev.off()

# tp data
month_tp = craine %>% 
  group_by(month, depth) %>% 
  summarise(tp = mean(tp)) %>% 
  filter(!is.na(month))

# data analysis
shea_tp = ggplot(month_tp, aes(x = depth, y = tp)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Total Phosphorus (μg/L)"))) +
  xlab("Depth (m)") +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_tp)

shea_tp_no_outlier = ggplot(month_tp, aes(x = depth, y = tp)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Total Phosphorus (μg/L)"))) +
  xlab("Depth (m)")+
  ylim(c(0,1000)) +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()

# converting to jpeg
shea_tp

jpeg("shea_tp.jpg",
     width = 3200, height = 1800, res = 300
)
shea_tp
dev.off()


shea_tp_no_outlier
jpeg("shea_tp_no_outlier.jpg",
     width = 3200, height = 1800, res = 300
)
shea_tp_no_outlier
dev.off()



