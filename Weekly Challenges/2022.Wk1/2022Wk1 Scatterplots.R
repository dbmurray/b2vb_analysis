#2022.Week 1 Scatterplots

# LIBRARIES
library(tidyverse)
library(readxl)
library(ggstatsplot)

# DATA IMPORT
winningest_active_coaches <- read_excel("datasets/2022.Wk1/NCAA_DI_Winningest_Active_Coaches.xlsx")

# TIDY DATA

# DATA TRANSFORMATIONS

# Rename variables to easier to use format
winningest_active_coaches <- rename(winningest_active_coaches, seasons_coached = `Seasons Coaching`,
                                    win_percentage = `Win Percentage`)

# CHARTS & TABLES
# plot seasons coaching
ggplot(data = winningest_active_coaches) +
  geom_histogram(mapping = aes(x = seasons_coached), binwidth = 5, color="black", fill="lightblue") +
  scale_x_continuous(breaks = seq(0, 40, 5), lim = c(0, 40))

# plot win percentage
ggplot(data = winningest_active_coaches) +
  geom_histogram(mapping = aes(x = win_percentage), color="black", fill="lightblue") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), lim = c(0, 1))

# create linear model
seasons_v_percentage <- lm(`win_percentage` ~ `seasons_coached`, data = winningest_active_coaches)
# summary stats
summary(seasons_v_percentage)

# function to plot the regression model
# (function borrowed from https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/)

ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "orange") +
    labs(title = paste("R-squared = ",signif(summary(fit)$r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P statistic=",signif(summary(fit)$coef[2,4], 5)))
}

# run our data through the custom regression plotting function
ggplotRegression(lm(`win_percentage` ~ `seasons_coached`, data = winningest_active_coaches))

# EXPORTS