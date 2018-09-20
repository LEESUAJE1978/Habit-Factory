require(readxl); require(tidyverse);require(psych); require(pastecs);require(MASS); require(outliers); require(lubridate); require(doBy);require(nycflights13); require(reshape2); require(stringr)

str(COFFEE_F)
dim(COFFEE_F)
names(COFFEE_F)
options(scipen = 100)

#https://statkclee.github.io/ml/ml-eda.html

#1. 범주형 변수 변동(Variation)확인
names(COFFEE_F)
#1.1 CARD TYPE 확인
ggplot(data = COFFEE_F )+
  geom_bar(mapping = aes(x=CARD_TYPE))
table(COFFEE_F$CARD_TYPE)

#1.2 MONTH확인
ggplot(data = COFFEE_F)+
  geom_bar(mapping = aes(x= MONTH))

#1.3 days 확인
ggplot(data = COFFEE_F)+
  geom_bar(mapping = aes(x= WDAYS))

ggplot(data = COFFEE_F)+
  geom_bar(mapping = aes(x= DAY))


#1.4 time확인
ggplot(data = COFFEE_F)+
  geom_bar(mapping = aes(x= HOUR))

ggplot(data = COFFEE_F)+
  geom_bar(mapping = aes(x= factor(HOUR)))

ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000) %>% head(30), aes(x=factor(CARD_APPROVAL_PRICE)))+ geom_bar()

max(COFFEE_F$CARD_APPROVAL_PRICE)
#2. 연속형 변수 변동성 확인
#2.1 CARD_APPROVAL_PRICE
#2.1.1 Histogram
ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000))+ geom_histogram(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 1000)

ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000))+ geom_histogram(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 500)

#2.1.2 freqpoly
ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000))+ geom_bar(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 1000)

ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000))+ geom_freqpoly(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 500)

#2.1.3 Histogram+Freqpoly
ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000))+ geom_histogram(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 1000)+geom_freqpoly(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 1000)

ggplot(data= COFFEE_F %>% filter(CARD_APPROVAL_PRICE<20000))+ geom_freqpoly(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 500)+
geom_histogram(mapping =aes(x= CARD_APPROVAL_PRICE),binwidth = 500)


#2.2 DAY, TIME
ggplot(data=COFFEE_F %>%filter(CARD_APPROVAL_PRICE<100000))+geom_histogram(mapping = aes(x=DAY), binwidth = 10)

ggplot(data=COFFEE_F %>%filter(CARD_APPROVAL_PRICE<100000))+geom_histogram(mapping = aes(x=HOUR), binwidth = 3)

ggplot(data=COFFEE_F %>%filter(CARD_APPROVAL_PRICE<100000))+geom_histogram(mapping = aes(x=MINUTE), binwidth = 15)

#discrete과 continous variable에 대해 생각해보기 (factor 개념이 적용 되는 경계점)

#3. 이상값 확인
#이상값의 기준을 어떻게 잡을 것인가?

COFFEE_F %>% filter(CARD_APPROVAL_PRICE>1000000) %>% 
  tally()

ggplot(COFFEE_F %>% filter(between(CARD_APPROVAL_PRICE,1000000,2000000)))+
  geom_histogram(mapping = aes(x = CARD_APPROVAL_PRICE), binwidth = 50000)

OUTLIER<-COFFEE_F %>% 
  filter(CARD_APPROVAL_PRICE<10 |CARD_APPROVAL_PRICE>10000000) %>% 
  arrange(CARD_APPROVAL_PRICE)

plot(OUTLIER$CARD_APPROVAL_PRICE)
options(scipen = 100)

#4. 결측치 제거 방법
#4.1. 결측치 제외: 다른 변수 값들도 같이 삭제되어 데이터 왜곡
NORMAL<-COFFEE_F %>% 
  filter(between(CARD_APPROVAL_PRICE, 10, 1000000))

subset(COFFEE_F, subset= CARD_APPROVAL_PRICE>10 & CARD_APPROVAL_PRICE<1000000)

#4.2. 결측치를 NA값으로 대체
NORMAL_NA<-COFFEE_F %>% 
  mutate(CARD_APRROVAL_PRICE = ifelse(CARD_APPROVAL_PRICE <10 | CARD_APPROVAL_PRICE>1000000, NA, CARD_APPROVAL_PRICE))

sum(is.na(ifelse(COFFEE_F$CARD_APPROVAL_PRICE< 10 | COFFEE_F$CARD_APPROVAL_PRICE >1000000, NA, COFFEE_F$CARD_APPROVAL_PRICE)))

#5. stringr 스타벅스와 투썸플레이스로 값 대체
#5.1 sub 사용
COFFEE_F$CATEGORY_GROUP_CODE<-sub(pattern = '00044', replacement = '스타벅스', x= COFFEE_F$CATEGORY_GROUP_CODE)

#5.2 gsub 사용
COFFEE_F$CATEGORY_GROUP_CODE<-gsub(pattern = '00058', replacement = '투썸플레이스',x=COFFEE_F$CATEGORY_GROUP_CODE)

#5.3 str_replace 사용
COFFEE_F$CATEGORY_GROUP_CODE<-str_replace(COFFEE_F$CATEGORY_GROUP_CODE, '00044', '스타벅스')


#5.4. 스타벅스와 투썸 이용횟수 확인
ggplot(data = subset(COFFEE_F, subset=CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스')))+
  geom_bar(mapping = aes(x =CATEGORY_GROUP_CODE))
 
subset(COFFEE_F, subset=CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스')) %>% 
  group_by(CATEGORY_GROUP_CODE) %>% 
  tally()


#6.스타벅스와 투썸 플레이스 비교
#6.1.histogram
ggplot(data = COFFEE_F %>% filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') & CARD_APPROVAL_PRICE<100000),aes(x=CARD_APPROVAL_PRICE))+geom_histogram(aes(fill= CATEGORY_GROUP_CODE),binwidth = 500)

ggplot(data = COFFEE_F %>% filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') &CARD_APPROVAL_PRICE<100000),aes(x=CARD_APPROVAL_PRICE))+geom_histogram(aes(fill= CATEGORY_GROUP_CODE),binwidth = 1000)

#6.2.freqpoly
ggplot(data = COFFEE_F %>% filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') &CARD_APPROVAL_PRICE<100000),aes(x=CARD_APPROVAL_PRICE))+geom_freqpoly(aes(color=CATEGORY_GROUP_CODE),binwidth = 500)

ggplot(data = COFFEE_F %>% filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') &CARD_APPROVAL_PRICE<100000),aes(x=CARD_APPROVAL_PRICE))+geom_freqpoly(aes(color=CATEGORY_GROUP_CODE),binwidth = 1000)

ggplot(data = COFFEE_F %>% filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') & between(CARD_APPROVAL_PRICE,10000, 12000),aes(x=CARD_APPROVAL_PRICE))+geom_histogram(aes(fill= CATEGORY_GROUP_CODE),binwidth = 1000))

#tip 구간 인터벌 값
COFFEE_F %>% count(cut_width(CARD_APPROVAL_PRICE,5000))


#7.두 변수의 공변동(Covariation)
#Variation은 단일변수의 변동성을 확인하는 것, Covariation은 변수 2개 혹은 그 이상 변수가 어떠한 모습으로 공변하는지를 파악하는 것.
#공변동을 파악하는 방법은 geom_point(산점도), geom_boxplot(박스플롯), geom_count, geom_tile, geom_jitter


#7.1. 연속형, 연속형_geom_point
#결제분과 승인액간의 관계
ggplot(data = COFFEE_F,
       mapping = aes(x=MINUTE, y= CARD_APPROVAL_PRICE))+geom_point()

ggplot(data =COFFEE_F %>% filter(between(CARD_APPROVAL_PRICE,1000,50000)), mapping = aes(x= MINUTE, y = CARD_APPROVAL_PRICE))+geom_point()

ggplot(data = COFFEE_F,
       mapping = aes(x=MINUTE, y= CARD_APPROVAL_PRICE))+geom_smooth()

ggplot(data =COFFEE_F %>% filter(between(CARD_APPROVAL_PRICE,1000,50000)), mapping = aes(x= MINUTE, y = CARD_APPROVAL_PRICE))+geom_point(alpha =1/100)



#7.2. 연속형, 범주형_Box plot

#7.2.1. 요일별 결제 현황 box plot으로 보기
ggplot(data = COFFEE_F %>% 
         filter(CARD_APPROVAL_PRICE<100000), mapping = aes(x = WDAYS, y= CARD_APPROVAL_PRICE))+geom_boxplot()

ggplot(data = COFFEE_F %>% 
         filter(CARD_APPROVAL_PRICE<50000), mapping = aes(x = WDAYS, y= CARD_APPROVAL_PRICE))+geom_boxplot()

ggplot(data = COFFEE_F %>% 
         filter(CARD_APPROVAL_PRICE<30000), mapping = aes(x = WDAYS, y= CARD_APPROVAL_PRICE))+geom_boxplot()

ggplot(data = COFFEE_F %>% 
         filter(CARD_APPROVAL_PRICE<20000), mapping = aes(x = WDAYS, y= CARD_APPROVAL_PRICE))+geom_boxplot()


#7.2.2. 스타벅스와 투썸의 요일별 결제 현황
ggplot(data = COFFEE_F %>% 
         filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') & CARD_APPROVAL_PRICE<50000), mapping = aes(x = WDAYS, y= CARD_APPROVAL_PRICE))+geom_boxplot()

ggplot(data = COFFEE_F %>% 
         filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') & CARD_APPROVAL_PRICE<50000), mapping = aes(x = CATEGORY_GROUP_CODE, y= CARD_APPROVAL_PRICE))+geom_boxplot()

ggplot(data = COFFEE_F %>% 
         filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스') & CARD_APPROVAL_PRICE<20000), mapping = aes(x = CATEGORY_GROUP_CODE, y= CARD_APPROVAL_PRICE))+geom_boxplot()


#7.3. 범주형, 범주형: geom_count, geom_tile, geom_jitter
#geom_count()를 사용하여 빈도수 시각화
ggplot(data= COFFEE_F %>% filter(between(CARD_APPROVAL_PRICE,1000,50000)), mapping = aes(x=CARD_TYPE, y= factor(DATE_OF_BIRTH)))+geom_count()

COFFEE_F %>% count(CARD_TYPE, DATE_OF_BIRTH) %>% 
  ggplot(mapping = aes(x= CARD_TYPE, y= DATE_OF_BIRTH))+geom_tile(mapping = aes(fill=n))

COFFEE_F %>% filter(DATE_OF_BIRTH >1950) %>% count(CARD_TYPE, DATE_OF_BIRTH) %>% 
  ggplot(mapping = aes(x= CARD_TYPE, y= DATE_OF_BIRTH))+geom_tile(mapping = aes(fill=n))

COFFEE_F %>% filter(DATE_OF_BIRTH >1950) %>% count(CARD_TYPE, DATE_OF_BIRTH) %>% 
  ggplot(mapping = aes(x= CARD_TYPE, y= DATE_OF_BIRTH))+geom_jitter(mapping = aes(fill=n))

#dplyr count() 함수를 사용하여 변수간 빈도를 구한 후 geom_tile() 함수로 시각화
#범주가 많은 경우 seriation, d3heatmap, heatmaply를 사용 하여 처리


require(xlsx)
SAMPLE<-COFFEE_F[,1:5]
write.csv(SAMPLE,"sample.csv")
write.xlsx(SAMPLE,"sample.xlsx")            
