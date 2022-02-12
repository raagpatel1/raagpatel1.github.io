setwd("D:/School/GitHub/raagpatel1.github.io")

library(rmarkdown)

library(dplyr)
library(ggplot2)
library(tidyverse)

rsconnect::setAccountInfo(name='raagpatel1',
                          token='80ADA9933EE7D27125672588AF37FA1D',
                          secret='oOKfSU5tjbnTak98vpaVmmzkIVValYCWKUbHPoO7')
render_site()





library(readr)
x <- read_csv("Beers.csv")

x %>% select(ABV,IBU) %>% drop_na() %>% ggplot(aes(x = IBU)) + geom_histogram(color="white", fill="black")
