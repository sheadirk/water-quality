# Libraries
library(tidyverse)
library(car)
library(AICcmodavg)
library(lubridate)
# library(xlsx)

# my data ---- 
craine = read.csv("data/craine_bathy_contour_stats.csv")
glimpse(craine)

# reformat data
craine$depth <-  c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

craine$hypso <- craine$Total.Area


#plot this hypso curve
craine_hypso <- ggplot(data = craine, aes(x=depth, y=Total.Area))+
  geom_line() +
  scale_x_reverse(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  xlab("Depth (m)" ) +
  ylab("Area (m^2)") +
  coord_flip() +
  scale_y_reverse() +
  theme_gray(base_size = 18)
  

  
print(craine_hypso)

craine_hypso
jpeg("craine_hypso.jpg",
     width = 3500, height = 1800, res = 300
)
craine_hypso
dev.off()
