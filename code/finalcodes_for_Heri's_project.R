#-----------------------------------------------------------------------#
#   This file contains code to                                          #
#                                                                       #
#   1. calculate the standardized z-scores                              #
#                                                                       #
#   2. Calculate the temperature variables                              #
#                                                                       #
#   3. Data visualization and Regression analysis                       #
#-----------------------------------------------------------------------#

#import the data
library(haven)
age1to5 <- read_dta("C:/Users/moumi/Desktop/internship(undp)/Africa (DHS)/(1) DHS (base dataset)/age1to5.v01.dta")
View(age1to5)

main_df <- age1to5

# ----------------------------------------------------------------------# 
#   Height and Weight                                                   #
#   
#   HW2 Weight in kilograms. There is one implied decimal place in      #
#   the weight (decimal points are not included in the data file).      #
#   To produce the weight in kilograms divide HW2 by 10.                #
#   
#   HW3 Height in centimeters. There is one implied decimal place in    #
#   the height (decimal points are not included in the data file).      #
#   To produce the height in centimeters divide HW3 by 10.              #
#                                                                       #
#-----------------------------------------------------------------------#

main_df$height <- (main_df$hw3)/10
main_df$weight <- (main_df$hw2)/10

#anthro z-scores

library(anthro)
library(dplyr)
anthrodf <- with(
  main_df,
  anthro_zscores(
    sex = b4, age = hw1, is_age_in_month= TRUE,
    weight = weight, lenhei = height
  )
)


anthro <- dplyr::mutate(anthrodf, ID = row_number())
dhs <- dplyr::mutate(main_df, ID = row_number())

dhs_anthro <- merge(anthro, dhs, by = "ID")

summary(dhs_anthro$hw1)

write.csv(dhs_anthro, "C:/Users/moumi/Desktop/internship(undp)/Africa (DHS)/(1) DHS (base dataset)/main_df_v01.csv")

# ------------------------------------------------------------------#
#     V008 Century month code of date of interview                  #
#     B3 Century month code for the date of birth of the child      #
#-------------------------------------------------------------------#

#tvar1: temp of the interview month

df<-data.frame(main_df_v01)
df

df<-df[!is.na(df$b3),]
sum(is.na(df$month1120))
df$v008<-as.double(df$v008)
df$v008[df$v008 > 1368]<-NA
df$v008[df$v008 < 1117]<-NA

df<-df[!is.na(df$v008),]

df$tvar1<-1
df$tvar1<-as.double(df$tvar1)

for (i in 1:dim(df)[1]){
  t1 <-df[i, paste0("month", eval(df$v008[i]))]
  df$tvar1[i]<-t1
}

#tvar2: degree-month measuremnt for the whole year prior to the interview
library(dplyr)

df<-data.frame(df)
colnames(df)
df$tvar2<-NA
df$b3<-as.double(df$b3)
for (i in 1:dim(df)[1]){
  
  a2<-df$v008[i]
  a1<-df$b3[i]
  a3<- a2-a1
  sumtemp<-vector()
  
  for (j in 0:a3) {
    damju<- df[i,paste0("month", eval(a1+j))]
    sumtemp<-c(sumtemp,damju)
  }
  
  #turn values less than 25 into NAs
  for (m in 1:length(sumtemp)) {
    if (sumtemp[m] < 25) {
      sumtemp[m] <- NA
    } 
  }
  
  #drop NAs, subtract 25 from every value and create sum of temp above 25
  sumtemp<-sumtemp[!is.na(sumtemp)]
  
  for (n in 1:length(sumtemp)) {
    sumtemp[n]<-sumtemp[n]-25
  }
  
  df$tvar2[i]<- sum(sumtemp, na.rm = TRUE)
}

#tvar3: degree-month measurment for the child's lifetime
df<-data.frame(df)
colnames(df)
df$tvar3<-NA
for (i in 1:dim(df)[1]){
  
  a2<-df$v008[i]
  a3<- a2-12
  
  sumtemp<-vector()
  
  for (j in 0:12) {
    damju<- df[i,paste0("month", eval(a3+j))]
    sumtemp<-c(sumtemp,damju)
  }
  
  #turn values less than 25 into NAs
  for (m in 1:length(sumtemp)) {
    if (sumtemp[m] < 25) {
      sumtemp[m] <- NA
    } 
  }
  
  #drop NAs, subtract 25 from every value and create sum of temp above 25
  sumtemp<-sumtemp[!is.na(sumtemp)]
  
  for (n in 1:length(sumtemp)) {
    sumtemp[n]<-sumtemp[n]-25
  }
  
  df$tvar3[i]<- sum(sumtemp, na.rm = TRUE)
}

#save final dataset
write.csv(df, "C:/Users/moumi/Desktop/internship(undp)/Africa (DHS)/(1) DHS (base dataset)/main_df_v02.csv")


main_df_v02 <- read.csv2("C:/Users/moumi/Desktop/internship(undp)/Africa (DHS)/(1) DHS (base dataset)/main_df_v02.csv")

df1 <- main_df_v02


# data visualization

median(df1$weight[df1$hw1<=60 & df1$hw1>=48 ], na.rm=TRUE)
median(df1$weight[df1$hw1<=48 & df1$hw1>=36 ], na.rm=TRUE)
median(df1$weight[df1$hw1<=36 & df1$hw1>=24 ], na.rm=TRUE)
median(df1$weight[df1$hw1<=24 & df1$hw1>=12 ], na.rm=TRUE)
median(df1$weight[df1$hw1<=12], na.rm=TRUE)

library(tidyverse)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ISLR)

# "Z-scored Weight for Age"

z1<-ggplot(df1, aes(x=zwei)) +  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +  geom_vline(aes(xintercept= -2), color = "orange",
                                                       linetype="dashed", size=1) + geom_vline(aes(xintercept= -3), linetype="dashed", size=1, color = "red") + 
  scale_x_continuous(limits = c(-10, 10)) +  labs(x = "Z-scored Weight for Age", title = "Africa") +
  annotate("text", x=-3, y=.29, label= "underweight", size = 5) +
  annotate("text", x=-5, y=.2, label= "severely underweight", size = 5) 


# "Z-scored Height for Age"
z2<-ggplot(df1, aes(x=zlen)) +  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+  geom_vline(aes(xintercept= -2), color = "orange",
                                                      linetype="dashed", size=1) +   geom_vline(aes(xintercept= -3), linetype="dashed", size=1, color = "red") + 
  scale_x_continuous(limits = c(-12, 10)) +  labs(x = "Z-scored Height for Age",  title = "Africa")+
  annotate("text", x=-3, y=.29, label= "stunted", size = 5) +
  annotate("text", x=-5, y=.2, label= "severely stunted", size = 5) 

#"Z-scored Weight for Height"

z3<-ggplot(df1, aes(x=zwfl)) +  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept= -2), color = "orange",
                                                      linetype="dashed", size=1) +   geom_vline(aes(xintercept= -3), linetype="dashed", size=1, color = "red") + 
  scale_x_continuous(limits = c(-12, 10)) +  labs(x = "Z-scored Weight for Height",  title = "Africa")+ 
  annotate("text", x=-4, y=.29, label= "underweight", size = 5) +
  annotate("text", x=-5, y=.2, label= "severely underweight", size =5) 

grid.arrange(z1, z2, z3, nrow=2, ncol=2)

#             Regression            #


df<-data.frame(main_df_v02)
colnames(df)
#b4 sex of child
#BORD birth order
#v106 - mother's highest education
#v012 - mother's age
#zwei - weight for age
#zlen - height for age
#zwfl - weight for height


df$newv012 <- 12*df$v012
df$newv012 <- df$newv012 - df$hw1
df$newv012 <- df$newv012/12

#country*year factor var


df$countryyear <- interaction(df$dhsyear, df$dhscc, sep = ".")
library(miceadds)

reg1 <- lm.cluster(zwei ~ tvar3 +
                     factor(dhsyear) + factor(dhscc), data = df, cluster = "countryyear")
reg1 <- lm.cluster(zwei ~ tvar2 +
                     factor(dhsyear) + factor(dhscc), data = df, cluster = "countryyear")
reg1 <- lm.cluster(zwei ~ tvar1 +
                     factor(dhsyear) + factor(dhscc) + factor(v008), data = df, cluster = "countryyear")

summary(reg1)
reg2 <- lm(zwei ~ tvar2 +
             factor(dhsyear) + factor(dhscc), data = df)

summary(reg2)

reg1 <- lm.cluster(zwfl ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
                     factor(v106) + newv012 + I(newv012^2) + 
                     factor(dhsyear) + factor(dhscc) + factor(v008), data = df, cluster = "countryyear")

################ Figure 2
main_df_v03<-read.csv2("F:/[Mou Backup]/Nigeria/main_df_v03.csv", header = TRUE)
library(stargazer)

main_df_v03$locale <-main_df_v03$urban_rura
rural<-main_df_v03
urban <-main_df_v03
rural<-subset(rural, locale != "U" & adm1fipsna != "NULL")
urban<-subset(urban, locale == "U" & adm1fipsna != "NULL")
#A
#tvar3
#month-of-survey temperature
#weight-for-age
#rural weight-for-age survey-month-temp not-clustered
library(stargazer)
reg1 <- lm(zwei ~ tvar3 +  factor(v106) + newv012 + I(newv012^2) +
             factor(b4) + factor(b4)*factor(bord)+factor(bord)+
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = rural)
sum1<-summary(reg1)

#urban weight-for-age survey-month-temp not-clustered
reg2 <- lm(zwei ~ tvar3  +  factor(v106) + newv012 + I(newv012^2) +
             factor(b4) + factor(b4)*factor(bord)+factor(bord)+
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = urban)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, out = "models3.tex")

#month-of-survey temperature
#weight-for-height
#rural weight-for-height survey-month-temp not-clustered
reg3 <- lm(zwfl ~ tvar3  +  factor(v106) + newv012 + I(newv012^2) +
             factor(b4) + factor(b4)*factor(bord)+factor(bord)+
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = rural)

#urban weight-for-height survey-month-temp not-clustered
reg4 <- lm(zwfl ~ tvar3  +  factor(v106) + newv012 + I(newv012^2) +
             factor(b4) + factor(b4)*factor(bord)+factor(bord)+
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = urban)
#height-for-age
#rural height-for-age survey-month-temp not-clustered
reg5 <- lm(zlen ~ tvar3  +  factor(v106) + newv012 + I(newv012^2) +
             factor(b4) + factor(b4)*factor(bord)+factor(bord)+
             factor(dhsyear) + factor(adm1fipsna) + factor(v008),data = rural)

#urban height-for-age survey-month-temp not-clustered
reg6 <- lm(zlen ~ tvar3  +  factor(v106) + newv012 + I(newv012^2) +
             factor(b4) + factor(b4)*factor(bord)+factor(bord)+
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = urban)

sum1<-summary(reg1)
sum1$coefficients[1:7,]



#B
#tvar1
#month-of-survey temperature
#weight-for-age
#rural weight-for-age survey-month-temp not-clustered
reg7 <- lm(zwei ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
             factor(v106) + newv012 + I(newv012^2) + 
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = rural)

#urban weight-for-age survey-month-temp not-clustered
reg8 <- lm(zwei ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
             factor(v106) + newv012 + I(newv012^2) + 
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = urban)

#month-of-survey temperature
#weight-for-height
#rural weight-for-height survey-month-temp not-clustered
reg9 <- lm(zwfl ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
             factor(v106) + newv012 + I(newv012^2) + 
             factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = rural)

#urban weight-for-height survey-month-temp not-clustered
reg10 <- lm(zwfl ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = urban)
#height-for-age
#rural height-for-age survey-month-temp not-clustered
reg11 <- lm(zlen ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = rural)

#urban height-for-age survey-month-temp not-clustered
reg12 <- lm(zlen ~ tvar1  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data  = urban)
stargazer(reg7, reg8, reg9, reg10, reg11, reg12, out = "models2.tex")


#tvar2
reg13 <- lm(zwei ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=12 & hw1<25))

reg14 <- lm(zwei ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=25 & hw1<37))

reg15 <- lm(zwei ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=37))

stargazer(reg13, reg14, reg15, out = "models4.tex")



reg16 <- lm(zwfl ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=12 & hw1<25))

reg17 <- lm(zwfl ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=25 & hw1<37))

reg18 <- lm(zwfl ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=37))

stargazer(reg16, reg17, reg18, out = "models5.tex")


reg19 <- lm(zlen ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=12 & hw1<25))

reg20 <- lm(zlen ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=25 & hw1<37))

reg21 <- lm(zlen ~ tvar2  + factor(b4) + factor(b4)*factor(bord)+
              factor(v106) + newv012 + I(newv012^2) + 
              factor(dhsyear) + factor(adm1fipsna) + factor(v008), data = main_df_v03, subset = (hw1>=37))

stargazer(reg19, reg20, reg21, out = "models6.tex")

