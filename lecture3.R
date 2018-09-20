require(readxl); require(tidyverse);require(psych); require(pastecs);require(MASS); require(outliers); require(lubridate); require(doBy);require(nycflights13)
str(airlines);str(airports);str(planes); str(weather); str(flights)
flights

R은 ##C:\Users\LEESUJAE\Documents\Habit-Factory "\"을 "/"로 바꾸어야 함

MEMBER_INFO<-read_csv("00_MEMBER_INFO.csv")
SUCCESS<-read_csv("01_SUCCESS.csv")
PAYMENT<-read_csv("02_PAYMENT.csv" )
FUND_PENSION<-read_csv("03_FUND_PENSION.csv" )
LOAN<-read_csv("04_LOAN.csv" )
SIMPLE_PAY<-read_csv("05_SIMPLE_PAY.csv" )
INSURANCE<-read_csv("06_INSURANCE.csv" )
MEMBERSHIP<-read_csv("07_MEMBERSHIP.csv" )

#http://rfriend.tistory.com/34 결측값 처리 방법

str(FUND_PENSION)
sum(is.na(FUND_PENSION))
colSums(is.na(FUND_PENSION))

str(INSURANCE)
sum(is.na(INSURANCE))
colSums(is.na(INSURANCE))

str(LOAN)
sum(is.na(LOAN))
colSums(is.na(LOAN))

str(MEMBER_INFO)
sum(is.na(MEMBER_INFO))

str(MEMBERSHIP)
sum(is.na(MEMBERSHIP))
colSums(is.na(MEMBERSHIP))

str(PAYMENT)
sum(is.na(PAYMENT))
colSums(is.na(PAYMENT))

str(SIMPLE_PAY)
sum(is.na(SIMPLE_PAY))
colSums(is.na(SIMPLE_PAY))

str(SUCCESS)
sum(is.na(SUCCESS))
colSums(is.na(SUCCESS))
names(SUCCESS)

# 1. 커피 디저트 브랜드의 월 평균 결제 횟수, 결제 건당 객 단가 파악하기
#1.1. 커피 디저트 브랜드 관련 데이터 셋 생성
COFFEE<-subset(SUCCESS, CATEGORY_CODE =="00301" | CATEGORY_CODE =="00399", select = c(2,6,8, 14:21, 23:27) )
str(COFFEE)
str(SUCCESS)
names(SUCCESS)

#1.2. 결측값 처리
sum(is.na(COFFEE))
colSums(is.na(COFFEE))
sum(is.na(COFFEE$CARD_NAME))
COFFEE$CARD_NAME[is.na(COFFEE$CARD_NAME)]<-"HABIT" #카드이름 결측값은 Habit으로 변경
COFFEE$CARD_APPROVAL_DATE[is.na(COFFEE$CARD_APPROVAL_DATE)]<-"0824" #승인일 결측값은 0824로 변경
COFFEE$CARD_APPROVAL_TIME[is.na(COFFEE$CARD_APPROVAL_TIME)]<-"0000" #승인시간은 0000시간으로 변경
COFFEE$CATEGORY_GROUP_CODE[is.na(COFFEE$CATEGORY_GROUP_CODE)]<-"HABIT"
COFFEE[,-1]
as.Date('')
SUCCESS$SMS_REGISTRATION_TIMESTAMP

#1.3 날짜 및 시간 변환(날짜 변환시 character 형태로 되어 있어야 함)
options(scipen = 100) #숫자 표기 함수
names(SUCCESS)
names(MEMBER_INFO)
str(COFFEE)
class(d)
d<-str_sub(COFFEE$SMS_REGISTRATION_TIMESTAMP,1,14) #문자 추출
COFFEE$DATE<-ymd_hms(d)#시간변환 및 삽입
COFFEE$SMS_REGISTRATION_DATE<-as.Date(as.character(COFFEE$SMS_REGISTRATION_DATE), format = "%Y%m%d") #기본함수 사용
COFFEE$YEAR<-year(COFFEE$DATE)
COFFEE$MONTH<-month(COFFEE$DATE, label=T)
COFFEE$DAY<-day(COFFEE$DATE)
COFFEE$WDAYS<-wday(COFFEE$DATE, label = T ) #factor(weekdays(COFFEE$DATE), levels = c("SUN", "월요일","화요일","수요일", "목요일","금요일", "토요일"))
COFFEE$HOUR<-hour(COFFEE$DATE)
COFFEE$MINUTE<-minute(COFFEE$DATE)
COFFEE$SECOND<-second(COFFEE$DATE)
str(COFFEE)
dim(COFFEE)

#2.EDA를 통한 데이터 프레임 변경
#2.1. aggregate함수

#Flights df 에서 년도별, 도착시간의 합계, 평균, 표준편차를 나타내세요.
aggregate(data = flights, arr_time ~ year, FUN = mean) #계산 대상 변수, 기준변수 각 1개

#Flights df에서 년도별, 월별 비행시간의 합계, 평균, 표준 편차를 나타내세요
aggregate(data = flights, arr_time ~ year+month, FUN = mean) #계산 대상 변수 1개, 기준변수 2개

#Flights 에서 도착 예정시간과 실제 도착시간의 차이를 구한 후 flights df에 삽입하고 ‘origin’별 평균을 구하시오
flights$difference<-flights$sched_arr_time-flights$arr_time #도착 예정시간과 실제 도착시간 차이
aggregate(data =flights, cbind(arr_time, sched_arr_time, difference) ~origin, FUN= mean) #cbind로 기준값 여러가지 설정 가능

# COFFEE 에서 월별, 브랜드별  승인 금액의 합계, 평균, 표준 편차를 나타내세요
aggregate(data= COFFEE, CARD_APPROVAL_PRICE ~ CATEGORY_GROUP_CODE+MONTH, FUN = length)
#COFFEE 에서 성별, 카드 타입별, 합계, 평균, 표준 편차를 나타내세요


#2.2 doBy 패키지 summaryBy 함수
#Flights df 에서 년도별, 도착시간의 합계, 평균, 표준편차를 나타내세요.
flights_df<-as.data.frame(flights)
summaryBy(data = flights_df, arr_time ~ year, FUN = c(sum,mean,sd), na.rm=T) #계산 대상 변수, 기준변수 각 1개

#Flights df에서 년도별, 월별 비행시간의 합계, 평균, 표준 편차를 나타내세요
flights_df<-as.data.frame(flights)
str(flights_df)
summaryBy(data = flights_df, arr_time ~ year+month, FUN = c(sum,mean,sd), na.rm=T) #계산 대상 변수 1개, 기준변수 2개

#flights_df 에서 도착 예정시간과 실제 도착시간의 차이를 구하여 flights_df df에 삽입하고 ‘origin’별 평균을 구하시오

flights_df$difference<-flights_df$sched_arr_time-flights_df$arr_time #도착 예정시간과 실제 도착시간 차이
summaryBy(data =flights_df, arr_time + sched_arr_time+ difference ~origin, FUN= c(sum,mean, sd), na.rm = T) #cbind로 기준값 여러가지 설정 가능

COFFEE_DF<-as.data.frame(COFFEE)
#브랜드별 월별, 평균결제금액, 결제액 함계, 결제액 편차, 결제횟수
summaryBy(data= COFFEE_DF, CARD_APPROVAL_PRICE ~ CATEGORY_GROUP_CODE+MONTH, FUN = c(sum, mean, sd, length))
names(COFFEE_DF)
#2.3. dplyr
#Flights df 에서 년도별, 도착시간의 합계, 평균, 표준편차를 나타내세요.
flights %>% 
  group_by(year) %>% 
  summarise(SUM=sum(arr_time, na.rm=T), MEAN=mean(arr_time,na.rm = T), SD=sd(arr_time, na.rm = T))

#Flights_df df에서 년도별, 월별 비행시간의 합계, 평균, 표준 편차를 구하고 결제금액을 내림 차순 정렬 하시오
flights_df %>% 
  group_by(year, month) %>% 
  summarise(SUM=sum(arr_time, na.rm=T), MEAN=mean(arr_time,na.rm = T), SD=sd(arr_time, na.rm = T)) %>% 
  arrange(desc(SUM))

#flights_df 에서 도착 예정시간과 실제 도착시간의 차이를 구하여 flights_df df에 삽입하고 ‘origin’별 평균을 구하시오
flights_df %>% 
  mutate(DIFF = sched_arr_time-arr_time) %>% 
  group_by(origin) %>% 
  summarise(MEANofARR=mean(arr_time, na.rm = T),MEANofSCHE=mean(sched_dep_time,na.rm = T),MEANofDIFF=mean(DIFF,na.rm = T))

flights_df %>% 
  group_by(origin) %>% 
  mutate(DIFF = sched_arr_time-arr_time) %>% 
  summarise(MEANofARR=mean(arr_time, na.rm = T),MEANofSCHE=mean(sched_dep_time,na.rm = T),MEANofDIFF=mean(DIFF,na.rm = T)) %>% 
  arrange(desc(MEANofARR))

#브랜드별 월별, 평균결제금액, 결제액 함계, 결제액 편차, 결제횟수
COFFEE %>% 
  group_by(CATEGORY_GROUP_CODE, MONTH) %>% 
  summarise(MEAN=mean(CARD_APPROVAL_PRICE), SUM=sum(CARD_APPROVAL_PRICE),FREQ=n())

COFFEE %>% 
  group_by(CATEGORY_GROUP_CODE, MONTH) %>% 
  summarise(MEAN=mean(CARD_APPROVAL_PRICE, na.rm=T), SUM=sum(CARD_APPROVAL_PRICE, na.rm=T), FREQ=n(), SD=sd(CARD_APPROVAL_PRICE)) %>% 
  arrange(desc(MEAN),)
  
#3. 데이터 프레임 정렬
#3.1.order 함수 사용
#COFFEE 월별, 브랜드별 평균 결제금액, 결제액 합계, 결제액 편차, 결제횟수 값을 연산한 데이터 프레임 COFFEE_BRAND를 생성하고, 월 컬럼은 오름차순으로 평균 결제금액 합계는 내림차순으로 정렬하시오  
COFFEE_BRAND[order(COFFEE_BRAND$MONTH, -COFFEE_BRAND$MEAN),]

#3.2. orderBy( )함수 사용
COFFEE_BRAND<-as.data.frame(COFFEE_BRAND)
orderBy(~MONTH + MEAN, data = COFFEE_BRAND)

#3.3. dplyr 사용
COFFEE_BRAND %>% 
  arrange(MONTH, desc(MEAN))

#4. 데이터 프레임 단순 결합

df1<-data.frame(name = c('Park','Lee','Kim','Kang'),
                gender = c('f','m','f','m'))
df2<-data.frame(name = c('Min','Ahn','Choi','Kyeon'),
                gender = c('m','m','f','f'))
df3<-data.frame(age = c(22,24,28,25), city = c('Seoul','Incheon','Seoul','Busan'))
df4<-data.frame(name = c('Yoon', 'Seo', 'Park', 'Lee', 'Kim', 'Kang'),
                age = c(30, 31, 22, 24, 28, 25))

df5<-data.frame(name = c('Park', 'Lee', 'Kim', 'Kang', 'Ahn', 'Go'),
                gender=c('f', 'f', 'm', 'm', 'f', 'm'),
                city = c('Seoul', 'Incheon', 'Seoul', 
                         'Busan', 'Gwangju', 'Deagu'))
#4.1.rbind: rbind는 열의 갯수와 이름이 다르면 오류 발생
rbind(df1, df3)
dim(df1)
#4.2 cbind: cbind는 행의 개수가 다르면 오류 발새
cbind(df1,df3)

# 5.데이터 프레임 병합
#5.1. merge
merge(df4,df5,by='name', all=F) #innerjoin
merge(df4, df5, by='name', all.x = T) #leftouterjoin
merge(df4, df5, by='name', all.x = F) 
merge(df4, df5, by='name', all.y = T) #rightouterjoin
merge(df4, df5, by='name', all.y = F)
merge(df4, df5, by='name', all = T) # fullouterjoin

#.5.2. dplyr
inner_join(df4,df5,by='name')
left_join(df4,df5,by='name')
right_join(df4,df4,by='name')
full_join(df4,df5,by='name')

str(COFFEE_F)

#6. 할부결제 관련
names(COFFEE)
table(COFFEE$CARD_APPROVAL_METHOD)
margin.table(a, margin = NULL)
addmargins(COFFEE$CARD_APPROVAL_METHOD)

#COFFEE df와 MEMBER_INFO innerjoin
dim(MEMBER_INFO)
dim(COFFEE)
COFFEE_F<-inner_join(COFFEE, MEMBER_INFO, by='USER_ID')
COFFEE_L<-left_join(COFFEE, MEMBER_INFO, by='USER_ID')
COFFEE_R<-right_join(COFFEE, MEMBER_INFO, by='USER_ID')
COFFEE_A<-full_join(COFFEE, MEMBER_INFO, by='USER_ID')
dim(COFFEE_L)
dim(COFFEE_F)
dim(COFFEE_R)
dim(COFFEE_A)
length(unique(COFFEE_F$USER_ID))
COFFEE_F %>% 
  filter(10<=HOUR & HOUR<=12) %>% 
  ggplot(aes(HOUR))+
  geom_freqpoly(binwidth = 1) #600s = 10 minutes

COFFEE_F %>% 
  ggplot(aes(HOUR))+
  geom_freqpoly(binwidth = 1)


COFFEE_F %>% 
  ggplot(aes(DATE))+
  geom_freqpoly(binwidth =86400) #86400 seconds =1day

COFFEE_F %>% 
  ggplot(aes(WDAYS))+
  geom_bar()#600s = 10 minutes

COFFEE_F %>% 
  group_by(MONTH,USER_ID) %>% 
  summarise(N=n(), SUM=sum(CARD_APPROVAL_PRICE)) %>% 
  arrange(desc(N, SUM))

COFFEE_F %>% 
  group_by(MONTH,USER_ID) %>% 
  summarise(N=n(), SUM=sum(CARD_APPROVAL_PRICE)) %>% 
  arrange(desc(SUM, N)) #과도하게 구매 기록이 높은 사람들 확인 필요


COFFEE_F %>% 
  group_by(COMPANY_NAME) %>% 
  summarise(N=n(), SUM= sum(CARD_APPROVAL_PRICE), MEAN =mean(CARD_APPROVAL_PRICE), SD = sd(CARD_APPROVAL_PRICE), VAR= var(CARD_APPROVAL_PRICE)) %>%
  arrange(desc(N))


#병원을 다니는 사람과 다니지 않는 사람(기준필요) 중 어느 집단이 보험료 지출이 높은지
#(SUCCSS$CATEGORY에서 병원 뽑고, 사용자 ID를 INSURANCE df의 보험료 납부 비용과 비교)
dim(INSURANCE)
names(INSURANCE)
unique(INSURANCE$INSUR_NAME)
table(INSURANCE$INSUR_TARGET_USER)
table(INSURANCE$COMPANY_NAME)
INSURANCE_DF<-as.data.frame(INSURANCE)
summaryBy(data= INSURANCE_DF, TOTAL_INSUR_PAY_PRICE ~ COMPANY_NAME, FUN=c(sum, mean, sd),na.rm=T)

sum(is.na(INSURANCE$TOTAL_INSUR_PAY_PRICE))

#1.3.평균 값 및 이상치 확인
boxplot(COFFEE$CARD_APPROVAL_PRICE)
boxplot(log(COFFEE$CARD_APPROVAL_PRICE))
mean(COFFEE$CARD_APPROVAL_PRICE) #28,264원
outlier(COFFEE$CARD_APPROVAL_PRICE);outlier(COFFEE$CARD_APPROVAL_PRICE, opposite = T) #53,863,764

table(COFFEE$CARD_APPROVAL_METHOD)
