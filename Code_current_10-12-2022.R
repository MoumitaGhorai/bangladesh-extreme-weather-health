#import the data
library(haven)
child_full <- read_dta("C:/Users/moumi/OneDrive/Bangladesh project/dhs+temp+survey/child_full.dta")
main_df <- child_full

#calculate z scores
main_df$height <- (main_df$hw3)/10
main_df$weight <- (main_df$hw2)/10

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
df<-dhs_anthro



#v1

df<-df[!is.na(df$b3),]

df$v008<-as.double(df$v008)


df<-df[!is.na(df$v008),]

df$tvar1<-1
df$tvar1<-as.double(df$tvar1)

for (i in 1:dim(df)[1]){
  t1 <-df[i, paste0("month", eval(df$v008[i]))]
  df$tvar1[i]<-t1
}






df<-data.frame(dhs_anthro)
df

library(dplyr)

df<-data.frame(df)
colnames(df)
df$tvar2<-NA
df$b3<-as.double(df$b3)
for (i in 1:dim(df)[1]){
  
  a2<-df$v008[i]
  a1<-df$b3[i]
  a3<- a2-a1
  
  
  for (j in 0:a3) {
    sumtemp<-numeric()
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




##################### a little different code for tvar2
df<-data.frame(df)
colnames(df)
df$tvar2<-NA
df$b3<-as.double(df$b3)
s2<-numeric()
sumtemp <- numeric()
for (i in 1:dim(df)[1]){
  sumtemp<-as.double(0)
  for (j in 0:(df$v008[i]-df$b3[i])) {
    damju<- df[i, paste0("month", df$b3[i]+j)]
    sumtemp<-c(sumtemp,damju)
  }
  s2<-as.double(0)
  s2<-sum(sumtemp[sumtemp>=25]-25)
#for (n in 1:length(sumtemp)) {
#if (sumtemp[n] >= 25) {
# sumtemp[n]<-sumtemp[n]-25
#} else if (sumtemp[n] < 25)  {sumtemp[n]<-0}
#}
  df$tvar2[i]<- s2
}

################## different tvar3
##################### a little different code for tvar2
df<-data.frame(df)
colnames(df)
df$tvar3<-0
df$b3<-as.double(df$b3)
s2<-numeric()
sumtemp <- numeric()
for (i in 1:dim(df)[1]){
  sumtemp<-as.double(0)
  for (j in 0:12) {
    damju<- df[i, paste0("month", df$b3[i]+j)]
    sumtemp<-c(sumtemp,damju)
  }
  s2<-as.double(0)
  s2<-sum(sumtemp[sumtemp>=25]-25)
  #for (n in 1:length(sumtemp)) {
  #if (sumtemp[n] >= 25) {
  # sumtemp[n]<-sumtemp[n]-25
  #} else if (sumtemp[n] < 25)  {sumtemp[n]<-0}
  #}
  df$tvar3[i]<- s2
}


########################################


#tvar3: degree-month measuremnt for the whole year prior to the interview
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

write_dta(df, "C:/Users/moumi/OneDrive/Bangladesh project/df.dta")



