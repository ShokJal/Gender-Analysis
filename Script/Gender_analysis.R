######################### LOAD THE LIBRARY ###########################################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)

### Reading and understanding the data (read.csv("Data\\No_names_all_data.csv") #####

TidyAll_data <- read.csv("Data\\Clean-ordered_WBdata.csv")    # Show variables
View(TidyAll_data)       # prints dataset in separate tab
head(TidyAll_data)       # view first six rows of data
str(TidyAll_data)        # condensed data summary
dim(TidyAll_data)        # number of rows and columns
names(TidyAll_data)      # columns names
glimpse(TidyAll_data)    # structure of data
#####################################################################################

### Subset by specific year
ss_2017 <- subset(TidyAll_data, year == 2017, )
View(ss_2017)
gg_2017 <- ggplot(ss_2017, aes(x=log10(NY.GDP.PCAP.PP.KD), y=SL.TLF.CACT.FE.ZS)) + ### U-shaped relationship!
  #  geom_point(aes(col=Country.Code)) + 
  geom_point(color="red") + 
  geom_smooth(method="loess", color="red") + 
  xlim(c(2.50, 5.50)) + 
  ylim(c(0, 100)) + 
  labs(subtitle="Data for 170 countries show a U-shaped relationship - a decline and then a rise in women's labour participation (WLPR) as economies develop (2017).", 
       y="Women's labor participaton rate", 
       x="Log of GDP per capita (PPP adjusted 2011 constant international $)", 
       title="", 
       caption = "Source: The World Bank")

gg + theme(legend.position = "none")
########################################################################################################

