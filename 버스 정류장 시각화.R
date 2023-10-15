library(ggplot2)
rm(list=ls())

bustation = read.csv("C:/Users/YAP/Downloads/12-13/22.01~22.10.25/전국버스정류장 위치정보 - 복사본.csv", header = T, fileEncoding = "euc-kr")

bustation$도시명
sort(table(bustation$도시명))

national = filter(bustation, 도시명=="서울특별시" | 도시명=="인천광역시" | 도시명 == "대구광역시"
                  | 도시명 == "경산시" | 도시명 =="제주도" | 도시명 == "울산광역시" | 도시명 == "목포시"
                  |도시명 == "광주광역시" | 도시명 =="원주시")



ggplot(data=national, aes(fill = 도시명))+
  ylab("정류장 개수")+
  ggtitle("                                                전국 버스정류장 개수")+
  geom_bar(mapping=aes(x=도시명))+
  scale_x_discrete(limit = c("서울특별시","인천광역시","대구광역시", "경산시", "제주도", "울산광역시"
                             ,"목포시", "광주광역시", "원주시"))



SView(bustation)



jejustation = subset(bustation, 도시명 == "제주도")




options(digits = 6)
View(jejustation)


register_google(key = "AIzaSyDnWqVmEK5VhHZVROUVc_aU3W7S0Vdn650")

install.packages("ggmap")
install.packages("ggplot2")
library(ggmap)

cen = c(mean(jejustation$경도), mean(jejustation$위도))


map = get_googlemap(center = cen, maptype = "roadmap", zoom = 11, markers = gc)



df = data.frame(name=jejustation$정류장.명칭, lon=jejustation$경도, lat=jejustation$위도)
map = get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
gmap = ggmap(map)

unique(jejustation$정류장.유형)
color = c("red", "green", "blue", "orange", "purple", "yellow", "pink")

gmap+geom_point(data = df, aes(x=lon, y=lat),
                alpha=0.5, col = "orange")

ggmap(map) + stat_density_2d(data= df, aes(x=lon, y=lat))



