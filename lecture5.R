require(readxl); require(tidyverse);require(lubridate); require(stringr)

getwd()
COFFEE_F<-read.csv('coffee_f.csv')
SUCCESS<-read.csv('01_SUCCESS.csv')
str(SUCCESS)
names(COFFEE_F)
COFFEE_F<-COFFEE_F[,-1]
COFFEE_F$BRAND_NAME<-COFFEE_F$브랜드명
COFFEE_F$브랜드명

#1데이터 값 한글 변환
#1.1. 브랜드 명 변환
BRANDCODE<-read_excel('brand.xlsx') %>% 
  data.frame()
class(BRANDCODE$BRANDCODE) #confirm type 'character'
class(COFFEE_F$CATEGORY_GROUP_CODE) #confirm type 'character'
COFFEE_F$CATEGORY_GROUP_CODE<-as.character(COFFEE_F$CATEGORY_GROUP_CODE)
names(BRANDCODE)
for (i in 1:nrow(COFFEE_F)) {
  try({
    COFFEE_F[i,3] = BRANDCODE[grep(pattern = COFFEE_F[i,3], x=BRANDCODE[,1]),2]
  },silent = T)
}
table(COFFEE_F$CATEGORY_GROUP_CODE)

#1.2. 성별명 변한
COFFEE_F$GENDER<-str_replace(COFFEE_F$GENDER, '1','남자')
COFFEE_F$GENDER<-str_replace(COFFEE_F$GENDER, '2', '여자')
table(COFFEE_F$GENDER)

#1.3 카드 타입
CARD_TYPE<-read_excel('card_type.xlsx') %>% 
  data.frame()

class(CARD_TYPE$KOREAN)
class(COFFEE_F$CARD_TYPE) 
COFFEE_F$CARD_TYPE<-as.character(COFFEE_F$CARD_TYPE) #type 확인

for(i in 1:nrow(COFFEE_F)){
  try({
    COFFEE_F[i,10] = CARD_TYPE[grep(pattern = COFFEE_F[i,10], x=CARD_TYPE[,1]),2]
  })
}
table(COFFEE_F$CARD_TYPE)

names(COFFEE_F)
COFFEE_F<-COFFEE_F[,-27]
#데이터 셋 만들기_스타벅스와 투썸 플레이스
COFFEE_PAIR<-as.data.frame(COFFEE_F %>% 
                             filter(CATEGORY_GROUP_CODE %in% c('스타벅스','투썸플레이스') & between(CARD_APPROVAL_PRICE,50,50000)))

#정규성 확인
#시각화를 통해
#histogram
options(scipen=100)
hist(COFFEE_F$CARD_APPROVAL_PRICE)
#스타벅스와 투썸 플레이스 결제 그래프
hist(COFFEE_PAIR$CARD_APPROVAL_PRICE) #빈도수로 표현
hist(COFFEE_PAIR$CARD_APPROVAL_PRICE, freq = F)#밀도 함수로 표현

#스타벅스 결제 그래프
hist(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=="스타벅스", "CARD_APPROVAL_PRICE"])
hist(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=="스타벅스", 16],freq=F )
par(mfrow=c(1,1))
#투썸 플레이스 결제 그래프
hist(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=='투썸플레이스', 16])
hist(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=='투썸플레이스', 16], freq=F)

#kernel density plot을 통해
lines(density(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=='스타벅스', 16]))
lines(density(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=='투썸플레이스', 16]))


#qqnorm(Quantile- Quantile plot)을 통해 https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot
qqnorm(COFFEE_PAIR$CARD_APPROVAL_PRICE)#실제 분포
qqline(COFFEE_PAIR$CARD_APPROVAL_PRICE)#정규분포를 따른다는 가정하의 분포


qqnorm(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=="스타벅스", 16])
qqline(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=="스타벅스", 16])

qqnorm(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=='투썸플레이스', 16])
qqline(COFFEE_PAIR[COFFEE_PAIR$CATEGORY_GROUP_CODE=='투썸플레이스', 16])


#그래프 상으로 정규 분포를 따르지 않는 것으로 보임

#정규성 검정(Normality Validation)
#http://rfriend.tistory.com/tag/Shapiro-Wilk%20normality%20test
#data 5000개 미만 shapiro-wilk test  https://www.google.com/search?q=shapiro+wilk+test&oq=shapiro&aqs=chrome.1.69i57j0l5.20854j0j7&sourceid=chrome&ie=UTF-8
#H0:모집단은 정규 분포를 따른다
#H1:모집단은 정규 분포를 따르지 않는다. 
by(COFFEE_PAIR$CARD_APPROVAL_PRICE, COFFEE_PAIR$CATEGORY_GROUP_CODE, shapiro.test)

#결론: 모집단은 정규 분포를 따르지 않아 t-test를 사용할 수 없고 비모수 방법인 Wilcoxon's Rank Sum Test 를 사용해야 함. 



#정규성 확인_tip
options(scipen = 100)
SAMPLE<-COFFEE_F[1:4999,]
nortest::ad.test(subset(SAMPLE, SAMPLE$CARD_APPROVAL_PRICE<50000)$CARD_APPROVAL_PRICE)
nortest::ad.test(COFFEE_F$CARD_APPROVAL_PRICE)#5000개 이상의 variable 정규성 확인은 nortest::ad.test로
shapiro.test(SAMPLE$CARD_APPROVAL_PRICE)#5000개 미만의 variable 정규성 확인은 shapiro.test

#정규성 http://rpubs.com/Statdoc/204929



#등분산 검정
#H0:두 집단은 등분산이다. (Null Hypothesis)
#H1:두 집단은 등분산이 아니다. (Alternative Hypothesis)
var.test(COFFEE_PAIR$CARD_APPROVAL_PRICE~COFFEE_PAIR$CATEGORY_GROUP_CODE)

#검정 결과 두 집단은 이분산을 가정한다.


#t-test 검정
#http://www.incodom.kr/%EC%9D%B4%EB%A1%A0_%EB%B0%8F_T%EA%B2%80%EC%A0%95
#t-test: http://www.dodomira.com/2016/04/02/r%EC%9D%84-%EC%82%AC%EC%9A%A9%ED%95%9C-t-test/
#http://rfriend.tistory.com/127
#http://rfriend.tistory.com/130

#한집단간 분석(One-Sample test)
#내가 궁금한 것
#스타벅스의 5월 평균 결제 금액 12,650원이 7월 평균 결제 금액 11,497원으로 평균 결제 금액이 감소 했는데 이 차이가 통계적으로 유의미한 차이일까?
COFFEE_PAIR %>% 
  filter(CATEGORY_GROUP_CODE == '스타벅스') %>%
  group_by(MONTH) %>% 
  summarise(MEAN=mean(CARD_APPROVAL_PRICE), SD=sd(CARD_APPROVAL_PRICE))
names(a)
a<-subset(COFFEE_PAIR, select = CARD_APPROVAL_PRICE, subset= (MONTH =='7'& CATEGORY_GROUP_CODE =='스타벅스'))$CARD_APPROVAL_PRICE
#정규성 조건을 만족하지 못하므로 wilcox.test 진행
wilcox.test(subset(COFFEE_PAIR, select = CARD_APPROVAL_PRICE, subset= (MONTH =='7'& CATEGORY_GROUP_CODE =='스타벅스'))$CARD_APPROVAL_PRICE,alternative = 'two.sided', mu=12650,
            conf.int = T, conf.level= 0.90)

t.test(subset(COFFEE_PAIR, select = CARD_APPROVAL_PRICE, subset= (MONTH =='7'& CATEGORY_GROUP_CODE =='스타벅스'))$CARD_APPROVAL_PRICE,alternative = 'two.sided', mu=12650,
       conf.int = T, conf.level= 0.99)



#질문 만들기


#두집단 간 분석(two-sample test)
#내가 궁금한 것
#결제 금액 50원 이상 50,000원 미만의 이용자들을 분석 해보니 스타벅스 이용자의 평균 결제 금액은 12,099원이고 투썸 플레이스 이용자의 평균 결제 금액 9,973원 인데 두 집단간 평균의 차이가 통계적으로 유의미한가?

COFFEE_PAIR %>% 
  group_by(CATEGORY_GROUP_CODE) %>% 
  summarise(MEAN= mean(CARD_APPROVAL_PRICE))


#가정을 충족시키지 못한 상태에서 t.test를 시행

t.test(COFFEE_PAIR$CARD_APPROVAL_PRICE ~ COFFEE_PAIR$CATEGORY_GROUP_CODE,
       alternative = "two.sided",
       var.equal   = FALSE,
       conf.int=T)





#However 가정 충족 못할 시 시행하는 비모수 방법인 wilcox Rank Sum Test를 시행하면 
wilcox.test(COFFEE_PAIR$CARD_APPROVAL_PRICE ~ COFFEE_PAIR$CATEGORY_GROUP_CODE,
            alternative = "two.sided", conf.int= T, conf.level=0.95)

#두 집단간 결제 금액 평균의 차이는 통계적으로 유의미 하지 않다. 


#ANOVA 분석
COFFEE_TRI<-as.data.frame(COFFEE_F %>% 
                             filter(CATEGORY_GROUP_CODE %in% c('스타벅스','투썸플레이스','앤젤리너스') & between(CARD_APPROVAL_PRICE,50,50000)))


#일원분산분석(one-way ANOVA Analysis): 세 종류 이상의 한개의 인자가(factor)가 종속 변수에 영향을 미치는 영향 분ㅅ
#http://rfriend.tistory.com/131

COFFEE_PAIR$MONTH<-as.factor(COFFEE_PAIR$MONTH)
class(COFFEE_PAIR$GENDER)

summary(aov(data= COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE))

#오차의 등분산 가정
bartlett.test(data= COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE)#모수적 방법론

kruskal.test(data = COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE)#비모수적 방법론


#이원분산분석(Two-way ANOVA Analysis):  세종류 이상의  2개의 인자(factor)가 종속 변수에 영향을 미치는 영향 분석 
aov(data = COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE+MONTH)
summary(aov(data = COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE+MONTH)
)
COFFEE_TRI$CATEGORY_GROUP_CODE<-as.factor(COFFEE_TRI$CATEGORY_GROUP_CODE)
COFFEE_TRI$MONTH<-as.factor(COFFEE_TRI$MONTH)
anova(lm(data = COFFEE_TRI, CARD_APPROVAL_PRICE~ CATEGORY_GROUP_CODE+MONTH))
summary(anova(lm(data = COFFEE_TRI, CARD_APPROVAL_PRICE~ CATEGORY_GROUP_CODE+MONTH))
)


bartlett.test(data= COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE+MONTH)#모수적 방법론

kruskal.test(data = COFFEE_TRI, CARD_APPROVAL_PRICE~CATEGORY_GROUP_CODE+MONTH)
