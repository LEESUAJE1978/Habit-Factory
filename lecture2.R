require(readxl); require(tidyverse);require(psych); require(pastecs);require(MASS)

R은 ##C:\Users\LEESUJAE\Documents\Habit-Factory "\"을 "/"로 바꾸어야 함

MEMBER_INFO<-read_csv("00_MEMBER_INFO.csv")
SUCCESS<-read_csv("01_SUCCESS.csv")
PAYMENT<-read_csv("02_PAYMENT.csv" )
FUND_PENSION<-read_csv("03_FUND_PENSION.csv" )
LOAN<-read_csv("04_LOAN.csv" )
SIMPLE_PAY<-read_csv("05_SIMPLE_PAY.csv" )
INSURANCE<-read_csv("06_INSURANCE.csv" )
MEMBERSHIP<-read_csv("07_MEMBERSHIP.csv" )

str(MEMBER_INFO);
str(SUCCESS);
str(PAYMENT);
str(FUND_PENSION)
str(LOAN);
str(SIMPLE_PAY)
str(INSURANCE)
str(MEMBERSHIP)

#벡터 및 데이터 프레임 생성
x1<-c(1,2,3,NA) #숫자와 결측값으로 구성된 벡터
x2<-c("P1","P2","P3","P4") #문자로 값으로 구성된 벡터 생성
df<- data.frame(value1=x1, value2=x2)  #벡터로 구성된 데이터 프레임 생성
data.frame(var1=x1, var=c(12,-1,-3,10), var3=10)


#타 구조를 데이터 프레임으로 변경
v1<-c(1,2,3,NA) #숫자 값으로 구성된 벡터 생성
m1<-matrix(1:6,2,3) # 숫자 값으로 구성된 matrix 생성
l1<-list(1,2,4) # 숫자 값으로 구성된 리스트 생성

df1<-as.data.frame(v1) #벡터를 데이터 프레임으로 생성
df2<- as.data.frame(m1) #행렬을 데이터 프레임으로 생성
df3<-as.data.frame(l1) #리스트를 데이터 프레임으로 생성


#데이터 프레임의 부분집합 선택
df[1:2,]
df[c(1,3,5),]
df[df$value1>2,]

SUCCESS[,1:5] %>% #Q1처음 다섯 변수를 반환
  head()
SUCCESS[,c(4,2,1)] #Q2원하는 변수의 위치를 지정해 변환
SUCCESS[SUCCESS$CARD_APPROVAL_PRICE>1000000,] #Q3 백만원 이상인 데이터 수 13,110


#데이터 프레임의 값 교체
df$value1 #Q1
df[,1]

df$value1<-40 #데이터 내 존재하는 value1 변수 값을 40으로 대체
df$value2<-5:8 #데이터 내 존재하는 value2 변수 값을 5:8로 대체
df$value1<-sum(df$value2) #데이터 내 존재하는 va1ue1 변수 값을 value2 평균값으로 대체

df[2,2]<-80 #Q2
SUCCESS$ADDRESS<-paste("서울", "강서구", sep = "+") #Q3

#subset 함수 사용
subset(df, value1>2, select = c(value1, value2)) #value1 값이 2보다 큰 value1, value2 값 선택
subset(df, value1 ==10 & value2 >3) #value1==10이고 value2 >3 보다 큰 변수 선택
subset(df, select = -value1) #value1 컬럼 제외

subset(SUCCESS, select = c("USER_ID","CATEGORY_CODE", "CARD_TYPE", "CARD_APPROVAL_PRICE","CARD_APPROVAL_AMOUNT_BALANCE", "CARD_APPROVAL_CURRENCY_UNIT"), CARD_APPROVAL_AMOUNT_BALANCE == CARD_APPROVAL_CURRENCY_UNIT) #Q1 카드승인 밸런스 금액과 카트 통화 유닛 금액이 같은 값 중 USER ID외 6개 컬럼 선택해서 출력하세요.

#데이터 선택 및 삭제 방법 요약
SUCCESS[c(3,5,7),c(2:5)] #숫자 벡터를 이용해 데이터 선택
SUCCESS[SUCCESS$CATEGORY_CODE==c("00044"),] #조건을 이용해 데이터를 선택, 스타벅스
SUCCESS[!SUCCESS$CATEGORY_CODE==c("00044"),]#부정조건을 이용해 데이터를 선택
SUCCESS[SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE>1000000 | SUCCESS$CARD_APPROVAL_PRICE>200000,] 
subset(SUCCESS, CARD_APPROVAL_AMOUNT_BALANCE >1000000, select = CATEGORY_CODE) #함수를 사용 해 변수 선택
SUCCESS[2]; SUCCESS['USER_ID'] #인덱싱 또는 문자 값을 사용해 변수 선택하여 데이터 프레임 형태로 반환
SUCCESS[1:2]; SUCCESS[c('SMS_ID','USER_ID')] #인덱싱/ 문자 벡터를 사용해 변수를 데이터 프레임 형태로 반환
SUCCESS$ADDRESS<-c('서울','대구')  #변수 변경
SUCCESS$ADDRESS<-NULL #추출방식을 사용해 변수 삭제


#데이터 기초 탐색 및 기초 통계 값
str(SUCCESS) #구조확인
dim(SUCCESS) #차원 확인

head() #상위 6개확인
tail() #하위 6개 확인
sum(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE, na.rm = T) #합계 확인
mean(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE, na.rm= T) # 평균값 확인
var(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE, na.rm=T) #분산확인
min(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE,na.rm=T) #최소값 확인
max(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE,na.rm=T) #최대값 확인
length(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE) #개수 확인
median(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE, na.rm = T)
sd(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE, na.rm = T) #표준 편차 값
unique(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE) #고유 값 확인

#psysh, pastecs 함수 사용, MASS 패키지의 Cars93 데이터 활용
require(psych); require(pastecs)
summary(SUCCESS$CARD_APPROVAL_PRICE) #base
describe(SUCCESS) #psych
stat.desc(SUCCESS) #pastecs

require(MASS)
str(Cars93)
summary(Cars93)#base
describe(Cars93) #psych
stat.desc(Cars93) #pastecs

dim(SUCCESS)

describe(SUCCESS)
stat.desc(SUCCESS)


#간단한 시각화
boxplot(CARD_APPROVAL_AMOUNT_BALANCE ~CATEGORY_GROUP_CODE, data=SUCCESS)
boxplot(log(CARD_APPROVAL_AMOUNT_BALANCE) ~CATEGORY_CODE, data=SUCCESS)
boxplot(log(CARD_APPROVAL_AMOUNT_BALANCE) ~COMPANY_NAME, data=SUCCESS)

##데이터셋 추출

# 커피/디저트 - 브랜드 : 커피 전문점 '00301', '00399'카테고리의 월별 평균 결제 횟수와 개인별 결제 단가 
COFFEE<-subset(SUCCESS, select = c(2,6,8,14:20, 21,23,26:31), subset = CATEGORY_CODE == c('00301', '00399'))
subset(COFFEE,select=c('CARD_APPROVAL_AMOUNT_BALANCE'~'SMS_REGISTRATION_MONTH'))

summary(COFFEE)
names(COFFEE)

COFFEE %>% 
  group_by(SMS_REGISTRATION_MONTH, USER_ID) %>% 
  summarise(count=n(), sum=sum(CARD_APPROVAL_PRICE,na.rm=T),meanle=mean(CARD_APPROVAL_PRICE, na.rm=T)) %>%
  arrange(desc(count))

names(SUCCESS)
#https://wsyang.com/2011/05/data-set-in-r/ 데이터셋 정의

# library(doBy)
# summaryBy(CARD_APPROVAL_AMOUNT_BALANCE~SMS_REGISTRATION_MONTH,COFFEE, FUN = sum)
# aggregate(SMS_REGISTRATION_MONTH~CARD_APPROVAL_AMOUNT_BALANCE, COFFEE, sum)
# length(COFFEE$CARD_APPROVAL_AMOUNT_BALANCE)
# length(COFFEE$SMS_REGISTRATION_MONTH)
# #https://datascienceschool.net/view-notebook/0a66597ee82f464b9e2015e41cff8205/ 자료형 내용
# 
# class(v)
# names(SUCCESS)
# str(SUCCESS)
# attach(SUCCESS)
# sum(is.na(CATEGORY_CODE))
# table(c(CATEGORY_CODE, CATEGORY_CODE_USER))
# names(SUCCESS)
# table(CATEGORY_CODE_USER)
# table(c(DATA_SOURCE,DATA_CHANNEL))
# table(CATEGORY_GROUP_CODE)
# sum(is.na(CARD_APPROVAL_AMOUNT_BALANCE))
# a<-!(is.na(CARD_APPROVAL_AMOUNT_BALANCE))
# subset(!is.na(SUCCESS$CARD_APPROVAL_AMOUNT_BALANCE))
# plot(subset(SUCCESS, select= CARD_APPROVAL_AMOUNT_BALANCE,subset = !is.na(CARD_APPROVAL_AMOUNT_BALANCE)))
# BALANCE<-subset(SUCCESS, select=c(CATEGORY_CODE, CARD_APPROVAL_AMOUNT_BALANCE),subset = !is.na(CARD_APPROVAL_AMOUNT_BALANCE))
# 
# BALANCE %>%
#   group_by(CATEGORY_CODE) %>%
#   summarize(sum(CARD_APPROVAL_AMOUNT_BALANCE)) %>%
#   arrange(desc)
# 
# BALANCE<-BALANCE %>%
#   group_by(CATEGORY_CODE) %>%
#   tally() %>%
#   mutate()
#   arrange(desc(n)) %>%
#   head()
# 
# BALANCE %>%
#   group_by(CATEGORY_CODE) %>%
#   summarize(mean(CARD_APPROVAL_AMOUNT_BALANCE, na.rm = TRUE))
# 
# T_BALANCE<-BALANCE %>%
#   group_by(CATEGORY_CODE) %>%
#   summarize(sum=sum(CARD_APPROVAL_AMOUNT_BALANCE), mean=mean(CARD_APPROVAL_AMOUNT_BALANCE)) %>%
#   arrange(desc(sum))
# 
# ggplot(data=subset(T_BALANCE, sum >10702854667), aes(x=CATEGORY_CODE, y=sum))+geom_area(stat="bin")
# 
# 
# ggplot(data=a, aes(x=subset(SUCCESS, subset(CATEGORY_CODE, y=log(CARD_APPROVAL_AMOUNT_BALANCE)))))+geom_histogram(stat="bin", binwidth =0.2)
# 
# sort(table(CATEGORY_CODE), descending = TRUE)
# sort(table(CATEGORY_CODE))
# order(table(CATEGORY_CODE))
# plot(log(a))
# 
