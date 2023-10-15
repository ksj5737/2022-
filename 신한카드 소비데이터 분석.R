install.packages("data.table")
library('data.table')
install.packages("bit64")
library('bit64')
install.packages("disk.frame")
library(disk.frame)

rm(list=ls())



text = fread("C:/Users/YAP/desktop/NATIVE(2018.1_2022.4).txt", sep2 = "|")             
fwrite(text, ".\\NATIVE(2018.1_2022.4).csv", col.names = T) 

cd

class(text)

table(text$ta_ym)
k1 = subset(text, ta_ym == c("202001", "202002","202003" ,"202004", "202005"))
k2 = subset(text, ta_ym == c("202005", "202006","202007" ,"202008"))
k3 = subset(text, ta_ym == c("202009", "202010","202011" ,"202012"))

m = cbind(k1,k2)

df2020 = filter(text, ta_ym=="202001" | ta_ym=="202002" | ta_ym == "202003" | ta_ym == "202004"
            | ta_ym=="202005" | ta_ym=="202006" | ta_ym=="202007" | ta_ym=="202008" | ta_ym=="202009"
            | ta_ym=="202010" | ta_ym=="202011" | ta_ym=="202012")


df2021 = filter(text, ta_ym=="202101" | ta_ym=="202102" | ta_ym == "202103" | ta_ym == "202104"
                | ta_ym=="202105" | ta_ym=="202106" | ta_ym=="202107" | ta_ym=="202108" | ta_ym=="202109"
                | ta_ym=="202110" | ta_ym=="202111" | ta_ym=="202112")

class(df2020)


install.packages("openxlsx")
library("openxlsx")

write.csv(df2020,file="C:/Users/YAP/Documents/shinhancard2020.csv")

sample2020 = df2020[sample(nrow(df2020),10000),]
sample2021 = df2021[sample(nrow(df2021), 10000),]
View(sample2020)
table(sample2020$ta_ym)
table(sample2021$ta_ym)

unique(sample2020$gb2)
unique(sample2020$v2)
unique(sample2020$v3)
sample2020$v2

sample2020$v2
jejucard2020 = subset(df2020, v2 == "제주")
sort(table(jejucard2020$gb3))
sort(table(jejucard2020$gb2))
table(jejucard2020$gb2, jejucard2020$cln_age_r)
table(jejucard2020$sex_ccd)

View(df2020)
View(k2)
View(cd)

colnames(jejucard2020)
x = c('red', 'green')
cols = x[as.integer((jejucard2020$sex_ccd))]
plot(jejucard2020$vlm, jejucard2020$usec , col=x)
boxplot(jejucard2020$vlm)

TRAV = subset(jejucard2020, gb3 == "여행")
View(TRAV)



mean(jejucard2020$vlm)

install.packages("arules")
library(arules)

tran = read.transactions(jejucard2020)


k = apriori(jejucard2020, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))


inspect(k)
inspect(sort(k, by = "confidence")[1:20])

unique(jejucard2020$usec)

travel = subset(df2020, v1 != v2)
View(travel)

options(scipen = 3)
options(digits = 2)
prop.table(table(travel$v2, travel$gb2))
prop.table((table(travel$gb2, travel$v2)))

table

transport = subset(travel, gb2 == "교통")
mean(transport$vlm)
jejutransport = subset(transport, v2 == "제주")
mean(jejutransport$vlm)

seoultransort = subset(transport, v2 == "서울")
mean(seoultransort$vlm)


        


prop.table(table(travel$gb2, travel$v2)

prop.table(table(travel$v2, travel$gb2))

a=prop.table(table(travel$v2,travel$gb2))
ab=as.data.frame.matrix(a)

unique(travel)

table(transport$cln_age_r)
