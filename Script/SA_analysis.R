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

######################## Subset dataset (South Asia only, + World, + Australia) ###################################

SA_data <- TidyAll_data[TidyAll_data$Country.Code %in% c("AFG", "BTN", "BGD", "MDV", "IND", "NPL", "PAK", "LKA"), ]
View(SA_data)                 # created subset of South Asian (8 countries) plus AUS and World
describe(SA_data)
str(SA_data)

###### Start analysis (some data exploration) ##################
# plotting women's labor par rate in SA
ggplot(SA_data, aes(x=year,y=SL.TLF.TOTL.FE.ZS, color=Country.Code)) + geom_line(size=1) +
  ylab ("Labor force, female (% of total labor force)") + xlab("Year")

# Basic dot plot
p<-ggplot(SA_data, aes(x=Country.Code, y=SL.TLF.CACT.FM.ZS)) + 
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=0.5, dotsize=0.5)
p
# Rotate the dot plot
p + coord_flip()
#p + scale_x_discrete(limits=c("25", "75"))
# dot plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=18,
                 size=3, color="red")
# dot plot with median points
p + stat_summary(fun.y=median, geom="point", shape=18,
                 size=3, color="red")

# Plot a subset of the data (GDP per capita (constant 2010 US$))
ss <- subset(SA_data, year > as.numeric("1991"))
ggplot(data = ss, aes(x = year, y = NY.GDP.PCAP.KD)) + 
  geom_line(mapping = aes(color=Country.Code, size=1))

# We're ready to explore some plotting routines [ not sure what i plot :) ]
with(SA_data,
     plot(SP.DYN.LE00.FE.IN, SL.TLF.TOTL.FE.ZS,
          main="Dependancy",
          xlab="Labor force, female (% of total labor force)", ylab="Life expectancy at birth, female (years)",
          pch=20, cex=0.7, col="darkgreen"))
# Boxplot (unbelieveable!)
ggplot(data = SA_data, mapping = aes(x = Country.Code, y = SP.ADO.TFRT)) +
  geom_boxplot() +
  ylab ("Adolescent fertility rate (births per 1,000 women ages 15-19)") + xlab("Country")
ggplot(data = SA_data, mapping = aes(x = Country.Code, y = SP.ADO.TFRT)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "darkred") +
  ylab ("Adolescent fertility rate (births per 1,000 women ages 15-19)") + xlab("Country")

ggplot(data = SA_data, mapping = aes(x = year, y = SP.ADO.TFRT, color = Country.Code)) +
  geom_line(size=1)
# Faceting
ggplot(data = SA_data, mapping = aes(x = year, y = SP.ADO.TFRT)) +
  geom_line() +
  facet_wrap(~ Country.Code)
# Adjust background
ggplot(data = SA_data, mapping = aes(x = year, y = SP.ADO.TFRT)) +
  geom_line() +
  facet_wrap(~ Country.Code) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(text=element_text(size = 16))
###
ggplot(data = SA_data, mapping = aes(x = year, y = MS.MIL.XPND.GD.ZS)) +
  geom_line() +
  facet_wrap(~ Country.Code) +
  labs(title = "Military expenditure",
       x = "Year",
       y = "% of GDP") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))

############

SA_2017 <- subset(SA_data, year == 2017, )
View(SA_2017)
hist(SA_2017$NY.GDP.PCAP.KD, main="GDP per capita", xlab = "Petal length in cm",
     ylab = "Count",border="red", col="blue")