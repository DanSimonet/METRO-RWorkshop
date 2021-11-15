1+1
sqrt(4)
5^2

### Objects

x <- 3 + 5
x * 2
x + x

#### Vectorization

y <- c(1,2,3,4,5)      # combine to vector
z <- seq(0,10, by = 2) # seq func
2 * y
y/z

# Custom Function
mean(1:5) # base

add_two <- function(x){  # custom
  x+2
}

# Libraries + Package

library(ggplot2)

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Horsepower",
       y = "Miles per Gallon (MPG)")


####################################
# Load Data and Describe Data
####################################

install.packages("tidyverse")
library(tidyverse)
HR_dat <- read_csv("https://raw.githubusercontent.com/DanSimonet/METRO-Workshop/main/IBM_HR_Data.csv")

# Descriptives

library(modelsummary)
datasummary_skim(HR_dat)
datasummary_skim(HR_dat, type = "categorical")

# Correlations

HR_dat_sn <- HR_dat                                   # create new df
colnames(HR_dat_sn) <- abbreviate(names(HR_dat), 7)   # abbreviate variable names

corrplot::corrplot(cor(select_if(HR_dat_sn, is.numeric)),           # run correlation on only numeric
                   type = 'lower', diag = FALSE, tl.cex = .45)      # modify appearance



