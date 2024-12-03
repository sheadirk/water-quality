# . Libraries ----
library(tidyverse)
library(lubridate)
library(AICcmodavg)
library(car)

# . Data read ----
craineslap <- read.csv("data/crainewq.csv") 

# . Have a quick look ----
# Data structure
glimpse(craineslap)

# Make all columns lower case
names(craineslap) <- tolower(names(craineslap))

# . Date manipulation ----
# Change date column to date format
craineslap$date <- as.Date(craineslap$sample_time,
                           format = "%m/%d/%Y"
                           )

# Get year for each observation
craineslap$year <- year(craineslap$date)

# Get month
craineslap$month <- month(craineslap$date)
craineslap$month <- month.name[craineslap$month]

# . Data screening ----
# What kinds of samples do we have?
unique(craineslap$sample_type)

# What kinds of information do we have?
unique(craineslap$information_type)

# Anything we need to be worried about?
unique(craineslap$rslt_flag)

#Not sure because these are not defined in the metadata

# What water quality parameters to we have?
unique(craineslap$characteristic_name)

# . Data analysis ----
# Temperature ----
# .. Filter the data ----
tempslap <- craineslap %>% 
  filter(characteristic_name == "TEMPERATURE" &
         information_type %in% c("Epilimnion_Sample"),
         !is.na(rslt_result_value)
        )


tempslap$temperature <- tempslap$rslt_result_value

glimpse(tempslap)


# Change the name of the result value column
# to the desired water quality parameter so it's easy
tempslap$temperature <- tempslap$rslt_result_value


# .. Analysis ----
null_mod <- lm(log(temperature) ~ 1, data = tempslap)
month_mod <- lm(log(temperature) ~ month, data = tempslap)
year_mod <- lm(log(temperature) ~ year, data = tempslap)
month_plus_year_mod <- lm(log(temperature) ~ month + year, data = tempslap)
month_int_year_mod <- lm(log(temperature) ~ month * year, data = tempslap)
#depth_mod <- lm(log(temperature) ~ information_type, data = tempslap)
#depth_month_mod <- lm(log(temperature) ~ information_type + month, data = tempslap)
#depth_year_mod <- lm(log(temperature) ~ information_type + year, data = tempslap)
#depth_month_plus_year_mod <- lm(log(temperature) ~ information_type + month + year, data = tempslap)
#depth_month_int_year_mod <- lm(log(temperature) ~ information_type + month * year, data = tempslap)
#depth_int_month_int_year_mod <- lm(log(temperature) ~ information_type * month * year, data = tempslap)
#depth_int_month_year_mod <- lm(log(temperature) ~ information_type * month + year, data = tempslap)
#depth_int_year_month_mod <- lm(log(temperature) ~ information_type * year + month, data = tempslap)

# ... Model selection ----
# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod)
  #depth_mod, depth_month_mod, depth_year_mod, depth_month_plus_year_mod,
  #depth_month_int_year_mod, depth_int_month_int_year_mod,
  #depth_int_month_year_mod, depth_int_year_month_mod


# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod')
 # 'depth_mod', 'depth_month_mod', 'depth_year_mod', 'depth_month_plus_year_mod',
 # 'depth_month_int_year_mod', 'depth_int_month_int_year_mod',
 # 'depth_int_month_year_mod', 'depth_int_year_month_mod'

# AIC model selection
aic_table <- aictab(model_list, model_names)

aic_table


# ... Residual diagnostics ----
#tempslap$residuals <- depth_int_month_int_year_mod$residuals

tempslap$residuals <- month_int_year_mod$residuals

# Resids across years within months and depths
ggplot(tempslap, aes(x = year, y = residuals,
                     color = information_type, 
                     fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(tempslap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
#summary(depth_int_month_int_year_mod)

summary(month_int_year_mod)

# Get factor-level significance from this
#Anova(depth_int_month_int_year_mod)

Anova(month_int_year_mod)

# ... Make predictions ----
# Get predictions on the log-scale
#log_preds <- predict(depth_int_month_int_year_mod,
#interval = "prediction")

log_preds <- predict(month_int_year_mod,
                     interval = "prediction"
                     )

# Convert predictions to the real scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
temp_preds <- data.frame(tempslap, real_preds)

# Let's order months by time instead of 
# alphabetical order
temp_preds$month <- factor(
  temp_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
temp_preds$information_type = factor(
  temp_preds$information_type,
  labels = c("Surface"))

#"Bottom", 

# ... Plot predictions from best model ----
crainetemp <- ggplot(temp_preds, aes(x = year, y = temperature,
                       color = information_type,
                       fill = information_type
                       )) +
  geom_point(color = "black", alpha = 0.5) +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "Depth layer" , fill = "Depth layer") +
  xlab("Year") +
  ylab(expression(paste("Temperature (", degree, "C)"))) +
  theme_bw() +
  theme(legend.position = "none")

#JPEG it
crainetemp

jpeg("temp_plot.jpg",
     width = 3500, height = 1800, res = 300
)
crainetemp
dev.off()


# ... Effect size interpretation ----
# Here is the temperature change in August
aug_min <- min(temp_preds$fit[temp_preds$month == "August"])
aug_max <- max(temp_preds$fit[temp_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August
aug_min_epi <- min(temp_preds$fit[temp_preds$month == "August" & 
                                  temp_preds$information_type == "Surface"])
aug_max_epi <- max(temp_preds$fit[temp_preds$month == "August" & 
                                  temp_preds$information_type == "Surface"])

diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 
aug_min_bottom <- min(temp_preds$fit[temp_preds$month == "August" & 
                                  temp_preds$information_type == "Bottom"])
aug_max_bottom <- max(temp_preds$fit[temp_preds$month == "August" & 
                                  temp_preds$information_type == "Bottom"])

diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
temp_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
         )


# Total phosphorus ----
# ... Filter the data ----
phoslap <- craineslap %>% 
  filter(characteristic_name == "PHOSPHORUS, TOTAL" &
         information_type %in% c("Epilimnion_Sample")   
        )
#, "Bottom_Sample"

glimpse(phoslap)

# Change the name of the result value column
# to the desired water quality parameter so it's easy
phoslap$tp <- phoslap$rslt_result_value

# ... Analysis ----
null_mod <- lm(log(tp) ~ 1, data = phoslap)
month_mod <- lm(log(tp) ~ month, data = phoslap)
year_mod <- lm(log(tp) ~ year, data = phoslap)
month_plus_year_mod <- lm(log(tp) ~ month + year, data = phoslap)
month_int_year_mod <- lm(log(tp) ~ month * year, data = phoslap)
#depth_mod <- lm(log(tp) ~ information_type, data = phoslap)
#depth_month_mod <- lm(log(tp) ~ information_type + month, data = phoslap)
#depth_year_mod <- lm(log(tp) ~ information_type + year, data = phoslap)
#depth_month_plus_year_mod <- lm(log(tp) ~ information_type + month + year, data = phoslap)
#depth_month_int_year_mod <- lm(log(tp) ~ information_type + month * year, data = phoslap)
#depth_int_month_int_year_mod <- lm(log(tp) ~ information_type * month * year, data = phoslap)
#depth_int_month_year_mod <- lm(log(tp) ~ information_type * month + year, data = phoslap)
#depth_int_year_month_mod <- lm(log(tp) ~ information_type * year + month, data = phoslap)


# ... Model selection ----
# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod)
#depth_mod, depth_month_mod, depth_year_mod, depth_month_plus_year_mod,
#depth_month_int_year_mod, depth_int_month_int_year_mod,
#depth_int_month_year_mod, depth_int_year_month_mod


# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod')
#'depth_mod', 'depth_month_mod', 'depth_year_mod', 'depth_month_plus_year_mod',
#'depth_month_int_year_mod', 'depth_int_month_int_year_mod',
#'depth_int_month_year_mod', 'depth_int_year_month_mod'


# AIC model selection 
aic_table <- aictab(model_list, model_names)

aic_table


# ... Residual diagnostics ----
phoslap$residuals <- year_mod$residuals

# Resids across years within months and depths
ggplot(phoslap, aes(x = year, y = residuals,
                     color = information_type, 
                     fill = information_type)) +
  geom_point() 

# Overall resids
ggplot(phoslap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
#summary(depth_int_month_int_year_mod)

summary(null_mod)
summary(year_mod)

# Get factor-level significance from this
#Anova(depth_int_month_int_year_mod)

Anova(null_mod)

# ... Make predictions ----
# Get predictions on the log-sdlap$scale
#log_preds <- predict(depth_int_month_int_year_mod,
#                     interval = "prediction"
#                   )

log_preds <- predict(null_mod,
                     interval = "prediction"
                     )
# Convert predictions to the real sdlap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
phos_preds <- data.frame(phoslap, real_preds)

# Let's order months by time instead of 
# alphabetical order
phos_preds$month <- factor(
  phos_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
phos_preds$information_type = factor(
  phos_preds$information_type,
  labels = c("Surface")
)

glimpse(phoslap)

mean(phoslap$tp)

# ... Plot predictions from best model ----
crainephos <- ggplot(phos_preds, aes(x = year, y = tp,
                       color = information_type,
                       fill = information_type
                       )) +
  geom_point(color = "black", alpha = 0.5) +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
#facet_wrap(~month) +
  labs(color = "Depth layer", fill = "Depth layer") +
  xlab("Year") +
  ylab("Phosphorus (mg/L)") + 
  theme_bw() +
  theme(legend.position = "none")


#JPEG it
crainephos

jpeg("phos_plot.jpg",
     width = 3500, height = 1800, res = 300
)
crainephos
dev.off()

# ... Effect size interpretation ----
# Here is the temperature change in August 
aug_min <- min(phos_preds$fit[phos_preds$month == "August"])
aug_max <- max(phos_preds$fit[phos_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(phos_preds$fit[phos_preds$month == "August" & 
                                  phos_preds$information_type == "Surface"])
aug_max_epi <- max(phos_preds$fit[phos_preds$month == "August" & 
                                  phos_preds$information_type == "Surface"])

diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 
aug_min_bottom <- min(phos_preds$fit[phos_preds$month == "August" & 
                                  phos_preds$information_type == "Bottom"])
aug_max_bottom <- max(phos_preds$fit[phos_preds$month == "August" & 
                                  phos_preds$information_type == "Bottom"])

diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
phos_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
         )

# Specific Conductance Attempt ----
condlap <- craineslap %>% 
  filter(characteristic_name == "SPECIFIC CONDUCTANCE",
         !is.na(rslt_result_value),
         information_type == "Epilimnion_Sample"
         )

glimpse(condlap)

condlap$sc <- condlap$rslt_result_value


null_mod <- lm(log(condlap$sc) ~ 1, data = condlap)
month_mod <- lm(log(condlap$sc) ~ month, data = condlap)
year_mod <- lm(log(condlap$sc) ~ year, data = condlap)
month_plus_year_mod <- lm(log(condlap$sc) ~ month + year, data = condlap)
month_int_year_mod <- lm(log(condlap$sc) ~ month * year, data = condlap)

# ... Model selection ----

# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
  )

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
  )

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table



# ... Residual differentials ----

condlap$residuals <- month_plus_year_mod$residuals

# Resids across years within months and depths
ggplot(condlap, aes(x = year, y = residuals,
                    color = information_type, 
                    fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(condlap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(month_plus_year_mod)

# Get factor-level significance from this
Anova(month_plus_year_mod)

# ... Make predictions ----
# Get predictions on the log-condlap$scale
log_preds <- predict(month_plus_year_mod,
                     interval = "prediction"
)

# Convert predictions to the real condlap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
cond_preds <- data.frame(condlap, real_preds)

# Let's order months by time instead of 
# alphabetical order
cond_preds$month <- factor(
  cond_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
cond_preds$information_type = factor(
  cond_preds$information_type,
)

# ... Plot predictions from best model ----
crainecond <- ggplot(cond_preds, aes(x = year, y = sc,
                       color = information_type,
                       fill = information_type
)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "Specific Conductance", fill = "Specific Conductance") +
  xlab("Year") +
  ylab("Specific Conductance (μS/cm)") +
  theme_bw() +
  theme(legend.position = "none")

#JPEG it 
crainecond

jpeg("cond_plot.jpg",
     width = 3500, height = 1800, res = 300
)
crainecond
dev.off()



# ... Effect size interpretation ----
# Here is the temperature change in August 
aug_min <- min(cond_preds$fit[cond_preds$month == "August"])
aug_max <- max(cond_preds$fit[cond_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(cond_preds$fit[cond_preds$month == "August" & 
                                    cond_preds$information_type == "Surface"])
aug_max_epi <- max(cond_preds$fit[cond_preds$month == "August" & 
                                    cond_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 

#aug_min_bottom <- min(cond_preds$fit[cond_preds$month == "August" & 
#                                       cond_preds$information_type == "Bottom"])
#aug_max_bottom <- max(cond_preds$fit[cond_preds$month == "August" & 
#                                      cond_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
cond_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )

# Secchi Depth Attempt ----

sdlap <- craineslap %>% 
  filter(characteristic_name == "DEPTH, SECCHI DISK DEPTH",
         !is.na(rslt_result_value), information_type != "Epilimnion_Sample"
  )

glimpse(sdlap)

sdlap$sd <- sdlap$rslt_result_value


null_mod <- lm(log(sdlap$sd) ~ 1, data = sdlap)
month_mod <- lm(log(sdlap$sd) ~ month, data = sdlap)
year_mod <- lm(log(sdlap$sd) ~ year, data = sdlap)
month_plus_year_mod <- lm(log(sdlap$sd) ~ month + year, data = sdlap)
month_int_year_mod <- lm(log(sdlap$sd) ~ month * year, data = sdlap)

# ... Model selection ----

# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table

# ... Residual differentials ----

sdlap$residuals <- year_mod$residuals

# Resids across years within months and depths
ggplot(sdlap, aes(x = year, y = residuals,
                     color = information_type, 
                     fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(sdlap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(year_mod)

# Get factor-level significance from this
Anova(year_mod)

# ... Make predictions ----
# Get predictions on the log-sdlap$scale
log_preds <- predict(year_mod,
                     interval = "prediction"
)

# Convert predictions to the real sdlap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
sd_preds <- data.frame(sdlap, real_preds)

# Let's order months by time instead of 
# alphabetical order
#sd_preds$month <- factor(
  #sd_preds$month,
  #levels = c("May", "June", "July", "August",
    #         "September", "October"))

# Let's give nice names to the information type
sd_preds$information_type = factor(
  sd_preds$information_type,
)

# ... Plot predictions from best model ----
crainesecchi <- ggplot(sd_preds, aes(x = year, y = sd,
                       )) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87",  alpha = 0.5) +
  #facet_wrap(~month) +
  labs(color = "Secchi Depth", fill = "Secchi Depth") +
  xlab("Year") +
  ylab("Secchi Depth (m)") +
  theme_bw() +
  theme(legend.position = "none")


#JPEG it 
crainesecchi

jpeg("secchi_plot.jpg",
     width = 2400, height = 1800, res = 300
)
crainesecchi

dev.off()


#How do I change the scale 

# ... Effect size interpretation ----
# Here is the temperature change in August 
aug_min <- min(sd_preds$fit[sd_preds$month == "August"])
aug_max <- max(sd_preds$fit[sd_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
#aug_min_epi <- min(sd_preds$fit[sd_preds$month == "August" & 
#                                   sd_preds$information_type == "Surface"])
# aug_max_epi <- max(sd_preds$fit[sd_preds$month == "August" & 
#                                   sd_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 

#aug_min_bottom <- min(sd_preds$fit[sd_preds$month == "August" & 
#                                       sd_preds$information_type == "Bottom"])
#aug_max_bottom <- max(sd_preds$fit[sd_preds$month == "August" & 
#                                      sd_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
sd_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )

# Nitrate-Nitrite Attempt ----

# ... Filter data ----
nitrolap <- craineslap %>% 
  filter(characteristic_name == "NITROGEN, NITRATE-NITRITE",
         !is.na(rslt_result_value), information_type != "Bottom_Sample"
  )

glimpse(nitrolap)

nitrolap$nt <- nitrolap$rslt_result_value


null_mod <- lm(log(nitrolap$nt) ~ 1, data = nitrolap)
month_mod <- lm(log(nitrolap$nt) ~ month, data = nitrolap)
year_mod <- lm(log(nitrolap$nt) ~ year, data = nitrolap)
month_plus_year_mod <- lm(log(nitrolap$nt) ~ month + year, data = nitrolap)
month_int_year_mod <- lm(log(nitrolap$nt) ~ month * year, data = nitrolap)

#...Model selection ----


# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table

# ... Residual differentials ----

nitrolap$residuals <- month_int_year_mod$residuals

# Resids across years within months and depths
ggplot(nitrolap, aes(x = year, y = residuals,
                    color = information_type, 
                    fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(nitrolap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(month_int_year_mod)

# Get factor-level significance from this
Anova(month_int_year_mod)

# ... Make predictions ----
# Get predictions on the log-nitrolap$scale
log_preds <- predict(month_int_year_mod,
                     interval = "prediction"
)

# Convert predictions to the real nitrolap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
nitro_preds <- data.frame(nitrolap, real_preds)

# Let's order months by time instead of 
# alphabetical order
nitro_preds$month <- factor(
  nitro_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
nitro_preds$information_type = factor(
  nitro_preds$information_type,
)

# ... Plot predictions from best model ----
crainenitro <- ggplot(nitro_preds, aes(x = year, y = nt,
                       color = information_type,
                       fill = information_type
)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "Nitrogen, Nitrate-Nitrite", fill = "Nitrogen, Nitrate-Nitrite") +
  xlab("Year") +
  ylab("Nitrogen, Nitrate-Nitrite (mg/L)") +
  theme_bw() +
  theme(legend.position = "none")

#JPEG it 
crainenitro

jpeg("nitro_plot.jpg",
     width = 3500, height = 1800, res = 300
)
crainenitro
dev.off()


# ... Effect size interpretation ----
# Here is the temperature change in August (Ben)
aug_min <- min(nitro_preds$fit[nitro_preds$month == "August"])
aug_max <- max(nitro_preds$fit[nitro_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(nitro_preds$fit[nitro_preds$month == "August" & 
                                    nitro_preds$information_type == "Surface"])
aug_max_epi <- max(nitro_preds$fit[nitro_preds$month == "August" & 
                                    nitro_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 

#aug_min_bottom <- min(nitro_preds$fit[nitro_preds$month == "August" & 
#                                       nitro_preds$information_type == "Bottom"])
#aug_max_bottom <- max(nitro_preds$fit[nitro_preds$month == "August" & 
#                                      nitro_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
nitro_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )

# pH Attempt ----
phlap <- craineslap %>% 
  filter(characteristic_name == "PH",
         !is.na(rslt_result_value)
  )

glimpse(phlap)

phlap$ph <- phlap$rslt_result_value


null_mod <- lm(log(phlap$ph) ~ 1, data = phlap)
month_mod <- lm(log(phlap$ph) ~ month, data = phlap)
year_mod <- lm(log(phlap$ph) ~ year, data = phlap)
month_plus_year_mod <- lm(log(phlap$ph) ~ month + year, data = phlap)
month_int_year_mod <- lm(log(phlap$ph) ~ month * year, data = phlap)

#...Model selection ----
# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table


# ... Residual differentials ----

phlap$residuals <- year_mod$residuals

# Resids across years within months and depths
ggplot(phlap, aes(x = year, y = residuals,
                     color = information_type, 
                     fill = information_type)) +
  geom_point()
  #facet_wrap(~month)

# Overall resids
ggplot(phlap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(year_mod)

# Get factor-level significance from this
Anova(year_mod)

# ... Make predictions ----
# Get predictions on the log-phlap$scale
log_preds <- predict(year_mod,
                     interval = "prediction"
)

# Convert predictions to the real phlap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
ph_preds <- data.frame(phlap, real_preds)

# Let's order months by time instead of 
# alphabetical order
ph_preds$month <- factor(
  ph_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
ph_preds$information_type = factor(
  ph_preds$information_type,
)

# ... Plot predictions from best model ----
craineph <- ggplot(ph_preds, aes(x = year, y = ph,
                        color = information_type,
                        fill = information_type
)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  #facet_wrap(~month) +
  labs(color = "PH", fill = "PH") +
  xlab("Year") +
  ylab("pH") +
  theme_bw() +
  theme(legend.position = "none")

#JPEG it 
craineph

jpeg("ph_plot.jpg",
     width = 2400, height = 1800, res = 300
)
craineph
dev.off()


# ... Effect size interpretation ----
# Here is the temperature change in August 
aug_min <- min(ph_preds$fit[ph_preds$month == "August"])
aug_max <- max(ph_preds$fit[ph_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(ph_preds$fit[ph_preds$month == "August" & 
                                     ph_preds$information_type == "Surface"])
aug_max_epi <- max(ph_preds$fit[ph_preds$month == "August" & 
                                     ph_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 

#aug_min_bottom <- min(ph_preds$fit[ph_preds$month == "August" & 
#                                       ph_preds$information_type == "Bottom"])
#aug_max_bottom <- max(ph_preds$fit[ph_preds$month == "August" & 
#                                      ph_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
ph_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )

# Chlorophyll-a attempt ----
chlorolap <- craineslap %>% 
  filter(characteristic_name == "CHLOROPHYLL A",
         !is.na(rslt_result_value)
  )

glimpse(chlorolap)

chlorolap$ca <- chlorolap$rslt_result_value


null_mod <- lm(log(chlorolap$ca) ~ 1, data = chlorolap)
month_mod <- lm(log(chlorolap$ca) ~ month, data = chlorolap)
year_mod <- lm(log(chlorolap$ca) ~ year, data = chlorolap)
month_plus_year_mod <- lm(log(chlorolap$ca) ~ month + year, data = chlorolap)
month_int_year_mod <- lm(log(chlorolap$ca) ~ month * year, data = chlorolap)

#...Model selection ----
# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table

# ... Residual differentials ----

chlorolap$residuals <- month_plus_year_mod$residuals

# Resids across years within months and depths
ggplot(chlorolap, aes(x = year, y = residuals,
                  color = information_type, 
                  fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(chlorolap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(month_plus_year_mod)

# Get factor-level significance from this
Anova(month_plus_year_mod)

# ... Make predictions ----
# Get predictions on the log-chlorolap$scale
log_preds <- predict(month_plus_year_mod,
                     interval = "prediction"
                     )

# Convert predictions to the real chlorolap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
chloro_preds <- data.frame(chlorolap, real_preds)

# Let's order months by time instead of 
# alphabetical order
chloro_preds$month <- factor(
  chloro_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October"))


# ... Plot predictions from best model ----
crainechloro <- ggplot(chloro_preds, aes(x = year, y = ca)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "Chlorophyll-A", fill = "Chlorophyll-A") +
  xlab("Year") +
  ylab(expression(italic("Chlorophyll a (μg/L)"))) +
  theme_bw() +
  theme(legend.position = "none")


#JPEG it 
crainechloro

jpeg("chloro_plot.jpg",
     width = 2400, height = 1800, res = 300
)
crainechloro
dev.off()


# ... Effect size interpretation ----
# Here is the temperature change in August 
aug_min <- min(chloro_preds$fit[chloro_preds$month == "August"])
aug_max <- max(chloro_preds$fit[chloro_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(chloro_preds$fit[chloro_preds$month == "August" & 
                                  chloro_preds$information_type == "Surface"])
aug_max_epi <- max(chloro_preds$fit[chloro_preds$month == "August" & 
                                  chloro_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 

#aug_min_bottom <- min(chloro_preds$fit[chloro_preds$month == "August" & 
#                                       chloro_preds$information_type == "Bottom"])
#aug_max_bottom <- max(chloro_preds$fit[chloro_preds$month == "August" & 
#                                      chloro_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

chloro_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )

# True Color Attempt ----
colorlap <- craineslap %>% 
  filter(characteristic_name == "TRUE COLOR",
         !is.na(rslt_result_value), information_type != "Bottom_Sample"
  )

glimpse(colorlap)

colorlap$tc <- colorlap$rslt_result_value


null_mod <- lm(log(colorlap$tc) ~ 1, data = colorlap)
month_mod <- lm(log(colorlap$tc) ~ month, data = colorlap)
year_mod <- lm(log(colorlap$tc) ~ year, data = colorlap)
month_plus_year_mod <- lm(log(colorlap$tc) ~ month + year, data = colorlap)
month_int_year_mod <- lm(log(colorlap$tc) ~ month * year, data = colorlap)


# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table

aic_table <- AIC(null_mod, month_mod, year_mod, 
                 month_plus_year_mod, month_int_year_mod)

aic_table[with(aic_table, order(AIC)), ]

# ... Residual differentials ----

colorlap$residuals <- month_int_year_mod$residuals

# Resids across years within months and depths
ggplot(colorlap, aes(x = year, y = residuals,
                      color = information_type, 
                      fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(colorlap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(month_int_year_mod)

# Get factor-level significance from this
Anova(month_int_year_mod)

# ... Make predictions ----
# Get predictions on the log-colorlap$scale
log_preds <- predict(month_int_year_mod,
                     interval = "prediction"
)

# Convert predictions to the real colorlap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
color_preds <- data.frame(colorlap, real_preds)

# Let's order months by time instead of 
# alphabetical order
color_preds$month <- factor(
  color_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
color_preds$information_type = factor(
  color_preds$information_type,
)

# ... Plot predictions from best model ----
crainecolor <- ggplot(color_preds, aes(x = year, y = tc,
                         color = information_type,
                         fill = information_type)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "True Color", fill ="True Color") +
  xlab("Year") +
  ylab("True Color") +
  theme_bw() +
  theme(legend.position = "none")


#JPEG it 
crainecolor

jpeg("color_plot.jpg",
     width = 2400, height = 1800, res = 300
)
crainecolor
dev.off()


# ... Effect size interpretation ----
# Here is the temperature change in August
aug_min <- min(color_preds$fit[color_preds$month == "August"])
aug_max <- max(color_preds$fit[color_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(color_preds$fit[color_preds$month == "August" & 
                                      color_preds$information_type == "Surface"])
aug_max_epi <- max(color_preds$fit[color_preds$month == "August" & 
                                      color_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August

#aug_min_bottom <- min(color_preds$fit[color_preds$month == "August" & 
#                                       color_preds$information_type == "Bottom"])
#aug_max_bottom <- max(color_preds$fit[color_preds$month == "August" & 
#                                      color_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
color_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )

# Acidity Attempt ----
acidlap <- craineslap %>% 
  filter(characteristic_name == "TRUE COLOR",
         !is.na(rslt_result_value), information_type != "Bottom_Sample"
  )

glimpse(acidlap)

acidlap$ac <- acidlap$rslt_result_value


null_mod <- lm(log(acidlap$ac) ~ 1, data = acidlap)
month_mod <- lm(log(acidlap$ac) ~ month, data = acidlap)
year_mod <- lm(log(acidlap$ac) ~ year, data = acidlap)
month_plus_year_mod <- lm(log(acidlap$ac) ~ month + year, data = acidlap)
month_int_year_mod <- lm(log(acidlap$ac) ~ month * year, data = acidlap)

#...Model selection ----
# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table

# ... Residual differentials ----

acidlap$residuals <- month_int_year_mod$residuals

# Resids across years within months and depths
ggplot(acidlap, aes(x = year, y = residuals,
                     color = information_type, 
                     fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(acidlap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(month_int_year_mod)

# Get factor-level significance from this
Anova(month_int_year_mod)

# ... Make predictions ----
# Get predictions on the log-acidlap$scale
log_preds <- predict(month_int_year_mod,
                     interval = "prediction"
)

# Convert predictions to the real acidlap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
acid_preds <- data.frame(acidlap, real_preds)

# Let's order months by time instead of 
# alphabetical order
acid_preds$month <- factor(
  acid_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
acid_preds$information_type = factor(
  acid_preds$information_type,
)

# ... Plot predictions from best model ----
craineacid <- ggplot(acid_preds, aes(x = year, y = ac,
                        color = information_type,
                        fill = information_type
)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "Acidity, Hydrogen Ion", fill ="Acidity, Hydrogen Ion") +
  xlab("Year") +
  ylab("Acidity, Hydrogen Ion") +
  theme_bw() +
  theme(legend.position = "none")

#JPEG it 
craineacid

jpeg("acid_plot.jpg",
     width = 2400, height = 1800, res = 300
)
craineacid
dev.off()


# ... Effect size interpretation ----
# Here is the temperature change in August
aug_min <- min(acid_preds$fit[acid_preds$month == "August"])
aug_max <- max(acid_preds$fit[acid_preds$month == "August"])

diff(c(aug_min, aug_max))

# Here is the temperature change in the epilimnion in August 
aug_min_epi <- min(acid_preds$fit[acid_preds$month == "August" & 
                                     acid_preds$information_type == "Surface"])
aug_max_epi <- max(acid_preds$fit[acid_preds$month == "August" & 
                                     acid_preds$information_type == "Surface"])

#diff(c(aug_min_epi, aug_max_epi))

# Here is the temperature change in the hypolimnion in August 

#aug_min_bottom <- min(acid_preds$fit[acid_preds$month == "August" & 
#                                       acid_preds$information_type == "Bottom"])
#aug_max_bottom <- max(acid_preds$fit[acid_preds$month == "August" & 
#                                      acid_preds$information_type == "Bottom"])

#diff(c(aug_min_bottom, aug_max_bottom))

# This is the tidy way to do it without a shoot ton of code
acid_preds %>% 
  group_by(month, information_type) %>% 
  summarize(min = min(fit),
         max = max(fit)
  )
# Total Nitrogen ----
# ... Filter data ----
tnslap <- craineslap %>% 
  filter(characteristic_name == "NITROGEN, TOTAL",
         !is.na(rslt_result_value))

glimpse(tnslap)

tnslap$tn <- tnslap$rslt_result_value


null_mod <- lm(log(tnslap$tn) ~ 1, data = tnslap)
month_mod <- lm(log(tnslap$tn) ~ month, data = tnslap)
year_mod <- lm(log(tnslap$tn) ~ year, data = tnslap)
month_plus_year_mod <- lm(log(tnslap$tn) ~ month + year, data = tnslap)
month_int_year_mod <- lm(log(tnslap$tn) ~ month * year, data = tnslap)

#...Model selection ----
# List of models
model_list <- list(
  null_mod, month_mod, year_mod, month_plus_year_mod, month_int_year_mod
)

# Vector with model names
model_names <- c(
  'null_mod', 'month_mod', 'year_mod', 'month_plus_year_mod', 'month_int_year_mod'
)

# AIC selection 
aic_table <- aictab(model_list, model_names)
aic_table

# ...Residual differentials ----

tnslap$residuals <- month_int_year_mod$residuals

# Resids across years within months and depths
ggplot(tnslap, aes(x = year, y = residuals,
                     color = information_type, 
                     fill = information_type)) +
  geom_point() +
  facet_wrap(~month)

# Overall resids
ggplot(tnslap, aes(residuals)) +
  geom_histogram()

# ... Statistical significance ----
# Get the r-squared from this
summary(month_int_year_mod)

# Get factor-level significance from this
Anova(month_int_year_mod)

# ... Make predictions ----
# Get predictions on the log-tnslap$scale
log_preds <- predict(month_int_year_mod,
                     interval = "prediction"
)

# Convert predictions to the real tnslap$scale
real_preds <- apply(log_preds, 2, exp)

# Smash them together with the original data
tn_preds <- data.frame(tnslap, real_preds)

# Let's order months by time instead of 
# alphabetical order
tn_preds$month <- factor(
  tn_preds$month,
  levels = c("May", "June", "July", "August",
             "September", "October")
)

# Let's give nice names to the information type
tn_preds$information_type = factor(
  tn_preds$information_type,
)

# ... Plot predictions from best model ----
crainetn <- ggplot(tn_preds, aes(x = year, y = tn,
                                       color = information_type,
                                       fill = information_type
)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  labs(color = "Total Nitrogen", fill = "Total Nitrogen") +
  xlab("Year") +
  ylab("Total Nitrogen (mg/L)") +
  theme_bw() +
  theme(legend.position = "none")

#JPEG it 
crainetn

jpeg("tn_plot.jpg",
     width = 2400, height = 1800, res = 300
)
crainetn
dev.off()


# Trophic Status Indices (TSIs)----
# .. Total Phosphorus ----
# Equation: TSI (TP) = 14.42*log(TP) + 4.15 
phos_tsi <- phos_preds
phos_tsi$tsi <- 14.42*log(phoslap$tp*1000) + 4.15 

phos_tsi$fit <- 14.42*log(phos_tsi$fit*1000) + 4.15
phos_tsi$lwr <- 14.41*log(phos_tsi$lwr*1000) + 4.15
phos_tsi$upr <- 14.41*log(phos_tsi$upr*1000) + 4.15

#Plot the raw TSIs based on your TP values with predictions from your best model
# of phosphorus
# ... Plot predictions from best model ----
craine_tsip <- ggplot(phos_tsi, aes(x = year, y = tsi,
                                       color = information_type,
                                       fill = information_type)) +
  geom_point(color = "black") +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  geom_hline(yintercept = 30, linetype = 2) +
  geom_hline(yintercept = 50, linetype = 2) +
  annotate("text", x = 1992, y = 60, label = "Eutrophic") +
  annotate("text", x = 1992, y = 45, label = "Mesotrophic") +
  annotate("text", x = 1992, y = 25, label = "Oligotrophic") +
  labs(color = "Depth layer", fill = "Depth layer") +
  xlab("Year") +
  ylab("TSI (TP)") +
  theme_bw() +
  theme(legend.position = "none")

craine_tsip

jpeg("tsi_tp_plot.jpg",
     width = 3500, height = 1800, res = 300
)
craine_tsip
dev.off()

# .. Secchi ----
#... TSI(SD) = 60 – 14.41 ln(SD)
sd_tsi <- sd_preds

sd_tsi$tsi <- 60-14.41*log(sdlap$sd)

# ... Make some tsi preds
sd_tsi$fit <- 60-14.41*log(sd_tsi$fit)
sd_tsi$lwr <- 60-14.41*log(sd_tsi$lwr)
sd_tsi$upr <- 60-14.41*log(sd_tsi$upr)

# Plot secchi TSI
craine_tsisd <- ggplot(sd_tsi, aes(x = year, y = tsi)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  geom_hline(yintercept = 30, linetype = 2) +
  geom_hline(yintercept = 50, linetype = 2) +
  annotate("text", x = 1995, y = 65, label = "Eutrophic") +
  annotate("text", x = 1995, y = 40, label = "Mesotrophic") +
  annotate("text", x = 1995, y = 25, label = "Oligotrophic") +
  labs(color = "Depth layer", fill = "Depth layer") +
  xlab("Year") +
  ylab("TSI (Secchi)") + 
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())

craine_tsisd

jpeg("tsi_sd_plot.jpg",
     width = 3500, height = 1800, res = 300
)
craine_tsisd
dev.off()

# .. Chlorophyll ----
# ... TSI(CHL) = 9.81 ln(CHL) + 30.6

chl_tsi <- chloro_preds

chl_tsi$tsi <- 9.81*log(chlorolap$ca) + 30.6

# ... Make some tsi preds
chl_tsi$fit <- 9.81*log(chl_tsi$fit)+30.6
chl_tsi$lwr <- 9.81*log(chl_tsi$lwr)+30.6
chl_tsi$upr <- 9.81*log(chl_tsi$upr)+30.6

#... Plot the data with predictions
craine_tsichl <- ggplot(chl_tsi, aes(x = year, y = tsi)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_line(aes(y = fit), color = "black") +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr,
                  color = NULL), fill = "gray87", alpha = 0.5) +
  facet_wrap(~month) +
  geom_hline(yintercept = 30, linetype = 2) +
  geom_hline(yintercept = 50, linetype = 2) +
  annotate("text", x = 1995, y = 60, label = "Eutrophic") +
  annotate("text", x = 1995, y = 35, label = "Mesotrophic") +
  annotate("text", x = 1995, y = 20, label = "Oligotrophic") +
  labs(color = "Depth layer", fill = "Depth layer") +
  xlab("Year") +
  ylab(expression(italic("TSI (Chlorophyll a)"))) + 
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())

craine_tsichl

jpeg("tsi_chl_plot.jpg",
     width = 3500, height = 1800, res = 300
)
craine_tsichl
dev.off()
