# Libraries ----
library(tidyverse)
library(lubridate)
library(AICcmodavg)
library(car)

# Data read ----
zoopers <- read.csv("data/shea_zoop.csv")

glimpse(zoopers)

# . Date manipulation ----
# Change date column to date format
zoopers$date <- as.Date(zoopers$date,
                          format = "%m/%d/%Y"
                        )

# Get month
zoopers$month <- month(zoopers$date)
zoopers$month <- month.name[zoopers$month]

glimpse(zoopers)


# Get monthly total mass ----
unique(zoopers$month)

# Calculate monthly total mass
march_total <- sum(zoopers$specific.weight[zoopers$month=="March"])
may_total <- sum(zoopers$specific.weight[zoopers$month=="May"])
june_total <- sum(zoopers$specific.weight[zoopers$month=="June"])
aug_total <- sum(zoopers$specific.weight[zoopers$month=="August"])
sept_total <- sum(zoopers$specific.weight[zoopers$month=="September"])
oct_total <- sum(zoopers$specific.weight[zoopers$month=="October"])

# Add column to data to hold monthly total
# Start with June total, then replace for month = september
zoopers$total <- march_total
zoopers$total[zoopers$month == "May"] <- may_total
zoopers$total[zoopers$month == "June"] <- may_total
zoopers$total[zoopers$month == "August"] <- june_total
zoopers$total[zoopers$month == "September"] <- sept_total
zoopers$total[zoopers$month == "October"] <- sept_total


# Calculate relative abundance for each zooplankton
zoopers$relative <- zoopers$specific.weight/zoopers$total

# . Order months chronologically ----
zoopers$month = factor(zoopers$month, levels = month.name)

# Data summary
zoops <- zoopers %>% 
  group_by(month, class, group) %>% 
  summarize(
    count = n(),
    sp_wt = sum(relative))


# Make the fucking plot
zooper_plot <- ggplot(zoops, aes(x=month, y=sp_wt, color = group, fill = group)) + 
    geom_bar(position="stack", stat="identity", color = "black") +
    scale_x_discrete(limits = month.name, breaks = c("March","May","June","August","September","October")) +
    scale_fill_manual(values = c('#FFFFFF','#000000','#66CDAA','#606060')) +
    xlab("Month") +
    ylab("Specific Weight (μg/L)")


zooper_plot

png("zooper_plot.png",
     width = 3500, height = 2400, res = 300
)
zooper_plot
dev.off()
