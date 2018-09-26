#http://everyday-deeplearning.tistory.com/entry/R로-하는-지도학습-나이브베이즈Naive-Bayes
#http://www.dodomira.com/2016/07/08/r을-사용한-베이지언-분류/
#https://datascienceschool.net/view-notebook/79140e6a9e364bcbb04cb8e525b9dba4/
#http://akas.tistory.com/6
COFFEE_F<-read_xlsx('COFFEE_F.xlsx')
str(COFFEE_F)
table(COFFEE_F$CATEGORY_GROUP_CODE)
table(COFFEE_F$CATEGORY_GROUP_CODE)
table(COFFEE_F$CARD_TYPE)
table()


install.packages('e1071'); 
require(e1071);library(dplyr);require(stringr); library(readxl)
df<-read.csv('http://www-bcf.usc.edu/~gareth/ISL/Heart.csv')
str(df)
create
BIG2[COFFEE_F$USER_ID == 'LTtvpRKikgaRS3G8cNQoDlufe1QPef72qQIHlByGAHPcsoqwUQFFGFKkz45Df']
BIG5<-COFFEE_F %>% filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스', '엔젤리너스', 
                                               '카페베네','이디야'))
#what we want to prove or what we want to know
ftable()
addmargins(table(BIG5$CATEGORY_GROUP_CODE, BIG5$CARD_TYPE))
addmargins(prop.table(table(BIG5$CATEGORY_GROUP_CODE, BIG5$CARD_TYPE)))
class(BIG2$CATEGORY_GROUP_CODE)
class(BIG2$USER_ID)
BIG2$CATEGORY_GROUP_CODE<-as.factor(BIG2$CATEGORY_GROUP_CODE)
addmargins(table(BIG2$USER_ID,BIG2$CATEGORY_GROUP_CODE))
addmargins(prop.table(table(BIG5$CATEGORY_GROUP_CODE, BIG5$MONTH)))
length(unique(BIG5$USER_ID))

#스타벅스 이용 고객의 투썸 플레이스 이용 빈도  투썸 플레이스 이용 고객 중 스타벅스 이용 비율
BIG2<-COFFEE_F %>% dplyr::filter(CATEGORY_GROUP_CODE %in% c('스타벅스', '투썸플레이스')) 
addmargins(table(BIG2$USER_ID,BIG2$CATEGORY_GROUP_CODE)) %>% tail()
table(BIG2$CATEGORY_GROUP_CODE)
BIG2 %>% filter(USER_ID == 'UIS02151791787657818449216603411')
BIG2$USER_ID
BIG5_MATRIX<-
as.data.frame.matrix(addmargins(table(BIG5$USER_ID, BIG5$CATEGORY_GROUP_CODE)))
a<-head(BIG5_MATRIX)
b<-tail(BIG5_MATRIX)
colSums(BIG5_MATRIX[-1614,])
class(BIG5$CATEGORY_GROUP_CODE)

      

#COFFEE_F 에서 결제 건수가 높은 사용자 10명을 추출하고 이들의 결제 금액의 합계와 평균 결제 금액을 산출하세요. 
COFFEE_F %>% filter(CATEGORY_GROUP_CODE != "HABIT") %>% 
  group_by(USER_ID) %>% 
  summarise(n=n(), SUM= sum(CARD_APPROVAL_PRICE),MEAN = mean(CARD_APPROVAL_PRICE)) %>% arrange(desc(n)) %>% head(10)

#COFFEE_F 에서 결제 건수가 많은 사용자 순으로 데이터 셋을 생성하고 상위 10 위 사용자의 
COFFEE_F %>% filter(CATEGORY_GROUP_CODE != "HABIT") %>% 
  group_by(USER_ID,CATEGORY_GROUP_CODE) %>% 
  summarise(n=n(), SUM= sum(CARD_APPROVAL_PRICE),MEAN = mean(CARD_APPROVAL_PRICE)) %>% arrange(desc(n)) %>% head(10)
  #https://statkclee.github.io/ml/ml-categorical-var-table.html

COFFEE_F %>% 
  group_by(CATEGORY_GROUP_CODE, CATEGORY_GROUP_CODE) %>% 
  summarise(n=n())

COFFEE_F %>% filter(CATEGORY_GROUP_CODE != "HABIT") %>% 
  group_by(USER_ID) %>% 
  summarise(n=n(), SUM= sum(CARD_APPROVAL_PRICE),MEAN = mean(CARD_APPROVAL_PRICE)) %>% arrange(desc(n))
filter(COFFEE_F, CATEGORY_GROUP_CODE != "HABIT")

COFFEE_F$USER_ID<-as.character(COFFEE_F$USER_ID)
str(COFFEE_F)
table(COFFEE_F$COMPANY_NAME)
COFFEE_CARD<-COFFEE_F %>% filter(COMPANY_NAME %in%  c("신한카드", "KB국민카드")) %>% 

names(COFFEE_CARD)


data(mgus)
install.packages('Cprob'); install.packages('prodlim')
library(Cprob);library(prodlim)

data("mgus")
str(mgus)
CP<-cpf(Hist(time,ev), data = mgus)
CP
median(mgus$age)
  mgus$AGE<-ifelse(mgus$age<64, 0,1)
cpf(Hist(time,ev)~AGE,data =mgus)
CP.death<-cpf(Hist(time,ev), data =mgus, failcode = 2)

mgus$A<- ifelse(mgus$age <64, 0,1)
fit.cpfpo<-cpfpo(Hist(time, ev)~factor(A) +creat,
                 data = mgus, tis = seq(10,30,0.3),
                 w=rep(1,67))

if(require("lattice")){
  xyplot(fit.cpfpo, scales= list(relation ='free'), layout =c(3,1))
}
str(mgus)

fit<-cpf(Hist(time, ev)~A,mgus)
plot(fit, curvlab=c("Age<64", "Age>=64"),
     main = "Conditional Probability of Cancer", xlab = "Years")

test <-cpf(Hist(time, ev)~A, data = mgus)
predict(test, c(10,20))

cutoffs<-quantile(mgus$time, probs = seq(0,1,0.05))[-1]
fit1<-pseudocpf(Hist(time,ev)~age+ creat,mgus, id =id, timepoints = cutoffs,corstr ='independence', scale.value =TRUE)
summary(fit1)

fit2<-pseudocpf(Hist(time,ev)~age +creat,mgus, id =id, timepoints = cutoffs, corstr ='independence', scale.value=TRUE, jack =TRUE)

summary(fit2)
