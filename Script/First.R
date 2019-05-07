######################### LOAD THE LIBRARY ###########################################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)

### Reading and understanding the data (read.csv("Data\\No_names_all_data.csv") #####

All_data <- read.csv("Data\\Data_copy_2.csv")    # Show variables
View(All_data)       # prints dataset in separate tab
head(All_data)       # view first six rows of data
str(All_data)        # condensed data summary
dim(All_data)        # number of rows and columns
names(All_data)      # columns names
glimpse(All_data)    # structure of data
#####################################################################################

# Data tidying
All_data$Country.Name <- All_data$Series.Name <- NULL  # Removed two dublicating columns (Country and Series name)
head(All_data)
View(All_data)
  
All_data_M <- gather(All_data, key = "Year", value = "Value", X1990:X2018,     # made column Year
              na.rm = FALSE, convert = FALSE, factor_key = FALSE)
View(All_data_M)
str(All_data_M)

All_data_M$year = as.numeric(gsub("\\X", "", All_data_M$Year))               # removed X from Year
#names(All_data_M) = gsub(pattern = "X", replacement = "", x = names(All_data_M))
View(All_data_M)
All_data_M$Year <- All_data_M$Year_2 <- NULL                                # remove unnessary Year columns
View(All_data_M)
All_data_MM <- All_data_M[, c(1,2,4,3)]                             # reodered columns for easy observation
View(All_data_MM)
All_data_MM$Value = as.numeric(All_data_MM$Value)                   # converted Value to numeric (was character)
str(All_data_MM)
View(All_data_MM)

All_data_M3 <- spread(All_data_MM, Series.Code, Value)  #assign separate column for each Series.Code
View(All_data_M3)

### SAVE CLEANED AND ORDERED RAW DATA FOR FUTHER ANALYSIS! ##########################################
#write.csv(All_data_M3, "Clean-ordered_WBdata.csv")   # this writes to working directory by defaults
write.csv(All_data_M3, "Data/Clean-ordered_WBdata.csv") # this writes into Data folder
### NEXT ANALYSIS SECTION COMES ####################################################################

### Subset by specific year
ss <- subset(All_data_M3, year == 2017, )
View(ss)
gg <- ggplot(ss, aes(x=log10(NY.GDP.PCAP.PP.KD), y=SL.TLF.CACT.FE.ZS)) + ### U-shaped relationship!
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

### HOPEFULLY NOW DATA IS CLEAN AND TIDY #######################################

######################## Subset dataset (South Asia only, + World, + Australia) ###################################

SA_data <- All_data_M3[All_data_M3$Country.Code %in% c("AFG", "BTN", "BGD", "MVD", "NPL", "PAK", "LKA"), ]
View(SA_data)                 # created subset of South Asian (8 countries) plus AUS and World
describe(SA_data)
str(SA_data)

###### Start analysis ##################
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
  geom_line(mapping = aes(color=Country.Code))
               
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




