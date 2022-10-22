library(tidyverse)
dataset <- read_delim("./data/postprocess/BT_data_weather_nests.csv", ";", escape_double = FALSE, trim_ws = TRUE)
summary(dataset)
glimpse(dataset)

library(lme4)

# model z 14-dniowym oknem przed LD
model <- glmer(EPC ~ scale(TEMP_BROOD14_mean)+scale(PRCP_BROOD14_sum)+
                 #I(TEMP_BROOD14_max - TEMP_BROOD14_min)+ #to byla amplituda temp ale usunalem ja
                 scale(TEMP_BROOD14_var) +
                 scale(PRCP_BROOD14_var) +
                 (1|FRING),
               family = 'binomial',
               data = dataset)

summary(model)


# model z 30-dniowym oknem
model2 <- glmer(EPC ~ scale(TEMP_BROOD30_mean)+scale(PRCP_BROOD30_sum)+
                 #I(TEMP_BROOD14_max - TEMP_BROOD14_min)+ #to byla amplituda temp ale usunalem ja
                 scale(TEMP_BROOD30_var) +
                 scale(PRCP_BROOD30_var) +
                 (1|FRING),
               family = 'binomial',
               data = dataset)

summary(model2)



plot(EPC ~ scale(TEMP_BROOD14_var), data = dataset)
curve(exp(-0.76+0.35*x)/(exp(-0.76+0.35*x)+1), col = 'red', lwd = 2, add = T)

plot(EPC ~ scale(TEMP_BROOD30_var), data = dataset)
curve(exp(-0.77+0.41*x)/(exp(-0.77+0.41*x)+1), col = 'red', lwd = 2, add = T)
