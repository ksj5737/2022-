rm(list=ls())

df2020 = read.csv("C:/Users/YAP/Desktop/데이터분석 공모전/필수데이터 전처리/2020여행조사최종.csv", header = T, fileEncoding = "euc-kr")
df2021 = read.csv("C:/Users/YAP/Desktop/데이터분석 공모전/필수데이터 전처리/2021여행조사최종.csv")


View(df)

sort(colSums(is.na(df)))

sort(table(df2021$비여행이유))


table(df2021)


d2020 = head(df2020, 50000)
d2021 = head(df2021, 50000)



options(scipen = 3)
options(digits=1)
old2021 = prop.table(table(d2021$비여행이유, d2021$연령))
old2020 = prop.table(table(d2020$비여행이유, d2020$연령))


old2021 - old2020

sort(table(d2020$비여행이유))
sort(table(d2021$비여행이유))

boxplot()

d2021[,]

food2021 = na.omit(d2021$음식점비_총액.1인.지출비용.원.)
mean(food2021)
food2020 = na.omit(d2020$음식점비_총액.1인.지출비용.원.)
mean(food2020)

food2021 <- d2021 %>% filter( !is.na(d2021$음식점비_총액.1인.지출비용.원.) )

colSums(is.na(d2021$음식점비_총액.1인.지출비용.원)

        
        
sort(table(d2021$휴가.방학.여부, d2021$방문지역))
table(d2020$휴가.방학.여부)



sort(table(d2020$방문지역))
sort(table(d2021$방문지역))

colnames(d2021)



d2021$여행유형 = as.factor(d2021$여행유형)
d2021$휴가.방학.여부 =  as.factor(d2021$휴가.방학.여부) 
d2021$방문지역 = as.factor(d2021$방문지역)
d2021$숙박지역 = as.factor(d2021$숙박지역)
d2021$숙박시설 = as.factor(d2021$숙박시설)
d2021$주요교통수단 1순위 = as.factor(d2021$주요교통수단 1순위)
d2021$여행정보획득경로_1순위 = as.factor(d2021$여행정보획득경로_1순위)
d2021$참고한인터넷사이트_1순위. = as.factor(d2021$참고한인터넷사이트_1순위.)
d2021$전반적만족도 = as.factor(d2021$전반적만족도)
d2021$비여행이유 = as.factor(d2021$비여행이유)
d2021$학력 = as.factor(d2021$학력.이수.여부)
d2021$혼인.상태 = as.factor(d2021$혼인.상태)
d2021$동거.자녀.현황_1..동거.자녀.없음 = as.factor(d2021$동거.자녀.현황_1..동거.자녀.없음)
d2021$직업 = as.factor(d2021$직업)
d2021$직장.지위 = as.factor(d2021$직장.지위)
d2021$주.5일.근무제.실시.여부 = as.factor(d2021$주.5일.근무제.실시.여부)
d2021$차량.보유.여부 = as.factor(d2021$차량.보유.여부)
d2021$성별 = as.factor(d2021$성별)
d2021$연령 = as.factor(d2021$연령)
d2021$직업_1..주업 = as.factor(d2021$직업_1..주업)
d2021$혼인상태 = as.factor(d2021$혼인상태)

str(d2021)


colnames(d2021)

table(d2021$연령, d2021$여행정보획득경로_1순위)

unique(d2021$주요교통수단1순위)
table(d2021$주요교통수단1순위, d2021$차량.보유.여부)

sort(table(d2021$주요교통수단1순위))

x = table(d2021$방문지역, d2021$주요교통수단1순위)
y= table(d2020$방문지역, d2020$주요이동교통수단.1순위)

daegu = subset(d2021,방문지역 == "22010")
table(daegu$주요교통수단1순위)

groundtrans = subset(d2021, 주요교통수단1순위 == c(1,2,6,7,8,10))
groundtrans$교통수단1순위종류[groundtrans$주요교통수단1순위== 1] = "개인"
groundtrans$교통수단1순위종류[groundtrans$주요교통수단1순위== 2] = "대중"
groundtrans$교통수단1순위종류[groundtrans$주요교통수단1순위== 6] = "대중"
groundtrans$교통수단1순위종류[groundtrans$주요교통수단1순위== 7] = "대중"
groundtrans$교통수단1순위종류[groundtrans$주요교통수단1순위== 8] = "개인"
groundtrans$교통수단1순위종류[groundtrans$주요교통수단1순위== 10] = "개인"

table(groundtrans$방문지역, groundtrans$주요교통수단1순위)

colnames(d2020)

m = subset(d2020, 여행유형 == 1)

table(d2021$방문지역,d2021$주.5일.근무제.실시.여부)

tr2020 = subset(d2020, 방문지역 == c(37020,39020,32030))
unique(tr2020$방문지역)

tr2020 = filter(d2020, 방문지역==37020 | 방문지역==39020 | 방문지역 == 32030)
tr2020경 =  filter(d2020, 방문지역==37020)
table(tr2020경$주요이동교통수단.1순위)

r = table(tr2020$연령, tr2020$주요이동교통수단.1순위)


tr2021 = filter(d2021, 방문지역==37020 | 방문지역==39020 | 방문지역 == 32030)
r2 = table(tr2021$연령, tr2021$주요교통수단1순위)
plot(d2020$방문지역, d2020$음식점비_일반음식점_1인.지출비용.원.)

mean(d2020$음식점비_총액.1인.지출비용.원.)


jeju2020 = filter(d2020, 방문지역==39020 | 방문지역 == 39010)
prop.table(table(jeju2020$주요이동교통수단.1순위)) #제주도 교통수단1순위에서 자가용,렌터카가 제일 높음 
table(jeju2020$연령)

jeju2021 = filter(d2021, 방문지역==39020 | 방문지역 == 39010)
prop.table(table(jeju2021$주요교통수단1순위))

table(jeju2020$주요이동교통수단.1순위)
table(jeju2021$주요교통수단1순위)

d2020 <- d2020 %>% filter(!is.na(d2020$음식점비_총액.1인.지출비용.원.))
mean(d2020$음식점비_총액.1인.지출비용.원.)
datak100 <- datak2 %>% group_by(연령대) %>% 
  summarize(datam=mean(교통비_총액.1인.지출비용.원.),data2sd=sd(교통비_총액.1인.지출비용.원.))
kable(datak100)


jeju2020 <- jeju2020 %>% filter(!is.na(jeju2020$음식점비_총액.1인.지출비용.원.))
mean(jeju2020$음식점비_총액.1인.지출비용.원.)

table(jeju2020$여행동반자수_본인포함, jeju2020$주요이동교통수단.1순위)
table(jeju2020$주요이동교통수단.1순위)
table(jeju2020$여행지선택이유.1순위)

colnames(jeju2020)

k = subset(jeju2020$주요이동교통수단.1순위 == 6)

bus = subset(jeju2020, 주요이동교통수단.1순위 == 6)
car = subset(jeju2020, 주요이동교통수단.1순위 == 1 | 주요이동교통수단.1순위 == 8)
ca2 = subset(jeju2020,주요이동교통수단.1순위 == 8 )

par(mfrow = c(1, 2))
boxplot(bus$교통비_총액.1인.지출비용.원., main = "버스")
boxplot(ca2$교통비_총액.1인.지출비용.원., main = "렌트카")


mean(bus$교통비_총액.1인.지출비용.원.)
mean(d2020$교통비_총액.1인.지출비용.원.)
