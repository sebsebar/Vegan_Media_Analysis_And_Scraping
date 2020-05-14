setwd("/Users/FlowersnIce-cream/Downloads/exam/R-stuff")

library(tidyverse)
library(stringi)
library(stringr)
library(pastecs)
library(ggplot2)
library(plyr)

setwd("/Users/FlowersnIce-cream/Downloads/exam/R-stuff/csvfiler/INFK")
filenames1 = list.files(pattern = "infk*") #creating a list of filenames


infk = data.frame() #creating an empty dataframe for importing data


for (i in filenames1){ 
  file1 = read.csv(i)
  infk = rbind(infk, file1)
}  #importing data from .csv files

infk$veg_koed<- "koed"

##########

setwd("/Users/FlowersnIce-cream/Downloads/exam/R-stuff/csvfiler/INFV")
filenames2 = list.files(pattern = "infv*") #creating a list of filenames


infv = data.frame() #creating an empty dataframe for importing data


for (i in filenames2){ 
  file2 = read.csv(i)
  infv = rbind(infv, file2)
}  #importing data from .csv files

infv$veg_koed<- "veg"

###########

setwd("/Users/FlowersnIce-cream/Downloads/exam/R-stuff/csvfiler/JPK")
filenames3 = list.files(pattern = "jpk*") #creating a list of filenames


jpk = data.frame() #creating an empty dataframe for importing data


for (i in filenames3){ 
  file3 = read.csv(i)
  jpk = rbind(jpk, file3)
}  #importing data from .csv files

jpk$veg_koed<- "koed"

##########

setwd("/Users/FlowersnIce-cream/Downloads/exam/R-stuff/csvfiler/JPV")
filenames4 = list.files(pattern = "jpv*") #creating a list of filenames


jpv = data.frame() #creating an empty dataframe for importing data


for (i in filenames4){ 
  file4 = read.csv(i)
  jpv = rbind(jpv, file4)
}  #importing data from .csv files


jpv$veg_koed<- "veg"

###########

##plot til hypotese om 
Collected <- rbind(jpv, jpk, infk, infv)
Collected <- rename(Collected,c("X..r"="aar"))
Collected$aar <- as.numeric(Collected$aar)

##########Cheking assumption

#Over alle sentiment og over alle år - Not in use duo to the fact that it doesn't seperate by keywords
collectedlm <- lm(a_score ~ aar, Collected) 
summary(collectedlm)
#Over alle sentiment og over alle år - med veg_koed
collectedlm2 <- lm(a_score ~ aar*veg_koed, Collected) 
summary(collectedlm2)
"""
Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      0.0194892  0.0020240   9.629   <2e-16 ***
aar              0.0004070  0.0003186   1.277    0.202    
veg_koedveg     -0.0048870  0.0030791  -1.587    0.113    
aar:veg_koedveg -0.0007342  0.0004779  -1.536    0.125 
"""
#Over alle sentiment og over alle år - med veg_koed - trevejs interaktion
#Tid gør noget forskelligt ved kød og vegetarkost
collectedlm3 <- lm(a_score ~ aar*veg_koed*avis, Collected) 
summary(collectedlm3)
"""
Coefficients:
                                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                      0.0182572  0.0027153   6.724 2.39e-11 ***
aar                              0.0006328  0.0004261   1.485    0.138    
veg_koedveg                     -0.0006382  0.0043268  -0.148    0.883    
avisinformation                  0.0027679  0.0040572   0.682    0.495    
aar:veg_koedveg                 -0.0007937  0.0006810  -1.166    0.244    
aar:avisinformation             -0.0005080  0.0006391  -0.795    0.427    
veg_koedveg:avisinformation     -0.0094450  0.0061645  -1.532    0.126    
aar:veg_koedveg:avisinformation  0.0003175  0.0009588   0.331    0.741 
"""

subset <- filter(Collected, veg_koed == "koed")
mean(subset$a_score)

####Cheking assumption - SUND MODEL - we use all models conformed to the assumptions as tested by residiual plots

#1
#normality of residuals
qqnorm(residuals(collectedlm2))
plot(fitted)
#Linearity
plot(fitted(collectedlm2),residuals(collectedlm2))
#Homoskedasticity
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(collectedlm2)
# The plots we are interested in are at the top-left and bottom-left. The top-left is the chart of residuals vs fitted values, while in the bottom-left one, it is standardised residuals on Y axis. If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line.

#2
#normality of residuals
qqnorm(residuals(collectedlm3))
plot(fitted)
#Linearity
plot(fitted(collectedlm3),residuals(collectedlm3))
#Homoskedasticity
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(collectedlm3)
# The plots we are interested in are at the top-left and bottom-left. The top-left is the chart of residuals vs fitted values, while in the bottom-left one, it is standardised residuals on Y axis. If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line.

#########################################################################################

#Svarer på hypotese 1
ggplot(Collected, aes(x = aar, y= a_score, colour = veg_koed)) +
  geom_smooth(method = "lm") +
  geom_point()+ labs(x = "aar", y = "Mean Sentiment Score", color = "keyword")

ggplot(Collected, aes(x = aar, y= a_score, colour = veg_koed)) +
  geom_smooth(method = lm) +
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")+ facet_wrap(~veg_koed) + labs(x = "aar", y = "Mean Sentiment Score", color = "keyword")+
  scale_x_continuous(breaks = round(seq(min(Collected$aar), max(Collected$aar), by = 1),1))

# Deskriptiv model - Opfølgende eksplorativt afsnit i analysen
# Effekterne vil blive fulgt op af eksplorative analyser - Skrevet i analyseafsnittet
# Ikke en tydelig linearitet - Post Hoc
# Men husk at det er at mine sine data -> Være klar på at i kun svarer på hypoteser i analyse
ggplot(Collected, aes(aar, a_score, color = veg_koed, group=veg_koed))+
  geom_point(stat = 'summary', fun.y=mean)+
  stat_summary(fun.y=mean, geom="line", aes(group = veg_koed))+  
  geom_errorbar(stat='summary', fun.data= mean_se, width=0.1)+
  labs(x = "aar", y = "Mean Sentiment Score", color = "avis") +
  scale_x_continuous(breaks = round(seq(min(Collected$aar), max(Collected$aar), by = 1),1))


#Svarer på hypotese 2
ggplot(Collected, aes(x = aar, y= a_score, colour = veg_koed)) +
  geom_smooth(method = lm) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")+ facet_wrap(~veg_koed) + labs(x = "aar", y = "Mean Sentiment Score", color = "keyword") +
  facet_wrap(~avis) +
  scale_x_continuous(breaks = round(seq(min(Collected$aar), max(Collected$aar), by = 1),1))

ggplot(Collected, aes(aar, a_score, color = veg_koed, group=veg_koed))+
  geom_point(stat = 'summary', fun.y=mean)+
  stat_summary(fun.y=mean, geom="line", aes(group = veg_koed))+  
  geom_errorbar(stat='summary', fun.data= mean_se, width=0.1)+
  labs(x = "aar", y = "Mean Sentiment Score", color = "avis") +
  facet_wrap(~avis) + scale_x_continuous(breaks = round(seq(min(Collected$aar), max(Collected$aar), by = 1),1))



####### FREQ
aar <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10)
antal <- c(749, 752, 799, 718, 752, 865, 794, 758, 812, 790, 218, 242, 262, 204, 182, 211, 193, 179, 266, 341)
vegg_koedd <- c('koed','koed','koed','koed','koed','koed','koed','koed','koed','koed','veg','veg','veg','veg','veg','veg','veg','veg','veg','veg')
freq_data <- data.frame(aar,antal,vegg_koedd)
freq_data1 <- freq_data
freq_data1 <- filter(freq_data1, vegg_koedd == "veg")
freq_data2 <- freq_data
freq_data2 <- filter(freq_data2, vegg_koedd == "koed")
freqlm1 <- lm(antal ~ aar, data = freq_data1) 
summary(freqlm1)
"""
VEG
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  201.267     34.173   5.890 0.000366 ***
aar            5.188      5.507   0.942 0.373777   
"""
freqlm2 <- lm(antal ~ aar, data = freq_data2) 
summary(freqlm2)
"""
KOED
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  748.067     27.705  27.001 3.81e-09 ***
aar            5.606      4.465   1.256    0.245    
"""
#Plots
ggplot(freq_data, aes(x = aar, y= antal, colour = vegg_koedd)) +
  geom_smooth(method = "lm") +
  geom_point()+ labs(x = "aar", y = "Antal", color = "keyword")+
  scale_x_continuous(breaks = round(seq(min(Collected$aar), max(Collected$aar), by = 1),1))

# Måske hvis vi havde opgjort det i måneder ville den lille hældning blive signifikant på grund af mængden af datapunkter

# Find nogle kilder om meningsdannelse i medier
# Det er fint at henvise til noget der er 
# Mål for emotion - atityde til noget - Sentiment
#assumption at vi kan måle på public opinion ved at kigge på nyhedsmedier - Det må der være nogle medier
# medier kan give et prej om hvad folk mener er vigtigt
# fodnoter er fint

# Finding number of files split by 50 lines
library(plyr)
idk <- count(Collected, 'file')
idk

# Means sentiment score  - overall
subsetveg <- filter(Collected, veg_koed == "veg")
subsetkoed <- filter(Collected, veg_koed == "koed")
mean(subsetkoed$a_score)
mean(subsetveg$a_score)

#mean for freq
freq_data1
mean(freq_data1$antal)
freq_data2
mean(freq_data2$antal)
