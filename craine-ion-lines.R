# Libraries ----
library(tidyverse)
library(car)
library(AICcmodavg)
library(lubridate)
# library(xlsx)

# my data ---- 
craine = read.csv("craine_ions.csv")
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
month_calc = craine %>% 
  group_by(month, depth) %>% 
  summarise(calc = mean(calc)) %>% 
  filter(!is.na(month))

# data analysis ---- 
calc = ggplot(month_calc, aes(x = depth, y = calc)) +
  geom_line() + geom_point() +
  facet_wrap(~ month, ncol = 8) +
  xlab("Depth (m)") +
  ylab(expression(paste("Calcium", " (mg/L)"))) +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
  ) +
  scale_x_reverse() +
  coord_flip()
print(calc)

# converting to jpeg
jpeg("shea_calc.jpg",
     width = 3500, height = 1800, res = 300
)
calc
dev.off()

glimpse(craine)


# Chloride 
month_chlor = craine %>% 
  filter(craine$date != "2022-09-20") %>%
  group_by(month, depth) %>% 
  summarise(chlor = mean(chlor, na.rm = TRUE)) %>% 
  filter(!is.na(month))


# data analysis
shea_chlor = ggplot(month_chlor, aes(x = depth, y = chlor)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Chloride (mg/l)"))) +
  xlab("Depth (m)")+
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_chlor)

# converting to jpeg
jpeg("shea_chlor.jpg",
     width = 3500, height = 1800, res = 300
)
shea_chlor
dev.off()

# Alkalinity data
month_alk = craine %>% 
  filter(craine$date != "2022-04-10") %>% 
  group_by(month, depth) %>% 
  summarise(alk = mean(alk, na.rm = TRUE)) %>% 
  filter(!is.na(month))

# data analysis
shea_alk = ggplot(month_alk, aes(x = depth, y = alk)) +
  geom_point() + geom_line() +
  facet_wrap(~ month, ncol = 8) +
  ylab(expression(paste("Alkalinity (mg/L)"))) +
  xlab("Depth (m)")+
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
  ) +
  scale_x_reverse() +
  coord_flip()
print(shea_alk)

# converting to jpeg
jpeg("shea_alk.jpg",
     width = 2400, height = 1800, res = 300
)
shea_alk
dev.off()
