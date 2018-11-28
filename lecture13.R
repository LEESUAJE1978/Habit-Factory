require(dplyr); require(stringr); require(KoNLP); require(wordcloud); require(ggplot2); require(lubridate)

str(bium.cart)

theme_set(theme_gray(base_family="AppleGothic"))
par(family='AppleGothic')#그래프 한글 깨짐 해결 방법
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8") 

# 1. 성별에 따른 구매금액의 차이
class(bium.cart$gender)
addmargins(table(bium.cart$gender))
sum(is.na(bium.cart$gender))
qplot(bium.cart$gender)

class(bium.cart$price)
summary(bium.cart$price)
qplot(bium.cart$price)+xlim(1,20000)
sum(is.na(bium.cart$price))
gender.price = bium.cart %>% filter(!is.na(gender)) %>% group_by(gender) %>% summarise(avg = mean(price))
ggplot(data =gender.price, aes(x=gender, y =avg))+geom_col()

str(bium.order)
class(bium.order$total_price)
summary(bium.order$total_price)
qplot(bium.order$total_price)+xlim(1,100000)
sum(is.na(bium.order$total_price))
gender.total.price = bium.order %>% filter(!is.na(gender)) %>% group_by(gender) %>% summarise(avg = mean(total_price))
ggplot(data =gender.total.price, aes(x=gender, y =avg))+geom_col()

#con1:성별에 따른 구매금액의 차이는 나타나지 않음



# 2. 나이에 따른 구매금액의 차이
class(bium.cart$birth_year)
summary(bium.cart$birth_year)
table(is.na(bium.cart$birth_year))
bium.cart$age = 2018- bium.cart$birth_year +1
summary(bium.cart$age)
qplot(bium.cart$birth_year)+xlim(1950, 2018)
qplot(bium.cart$age)+xlim(10, 90)

age.price = bium.cart %>% filter(!is.na(age), age<100) %>% group_by(age) %>% summarise(avg=mean(price))
ggplot(data = age.price, aes(x = age, y= avg))+geom_line()
class(bium.order$birth)

class(bium.order$birth)
bium.order$year = as.numeric(substr(bium.order$birth, 1,4))
qplot(bium.order$year)+xlim(1950,2018)
qplot(bium.order$age)+xlim(10,90)
bium.order$age = 2018-bium.order$year -1
table(is.na(bium.order$year))
age.total.price = bium.order %>% filter(!is.na(age), between(age, 0, 100)) %>% group_by(age) %>% summarise(avg=mean(total_price))
ggplot(data = age.total.price, aes(x=age, y=avg))+geom_line()
ggplot(data = age.total.price, aes(x=age, y=avg))+geom_col()+geom_line()
#객단가는 50대 중반이 가장 높은 것으로 나타남
#시각화에 따라 데이터 인식이 달라질 수 있음에 주의


# 3. 연령대에 따른 구매금액의 차이
bium.order = bium.order %>% mutate(age.group = ifelse(between(age,0,10), "10대미만",
       ifelse(between(age,11,20),"10대",
              ifelse(between(age,21,30),"20대",
                     ifelse(between(age, 31,40),"30대",
                            ifelse(between(age,41,50),"40대",
                                   ifelse(between(age,51,60),"50대",
                                          ifelse(between(age, 61,70),"60대","60대이상"))))))))


table(bium.order$agegroup)

bium.cart = bium.cart %>% mutate(age.group = ifelse(between(age,0,10), "10대미만",
                                                     ifelse(between(age,11,20),"10대",
                                                            ifelse(between(age,21,30),"20대",
                                                                   ifelse(between(age, 31,40),"30대",
                                                                          ifelse(between(age,41,50),"40대",
                                                                                 ifelse(between(age,51,60),"50대",
                                                                                        ifelse(between(age, 61,70),"60대","60대이상"))))))))

table(is.na(bium.cart$agegroup))
agegroup.price = bium.cart %>% filter(!is.na(age.group)) %>% group_by(age.group) %>% summarise(avg.price = mean(price))
agegroup.total.price = bium.order %>% filter(!is.na(agegroup)) %>% group_by(age.group) %>% summarise(avg.price = mean(total_price))
ggplot(data = agegroup.price, aes(x=age.group, y=avg.price))+geom_col()
ggplot(data = agegroup.total.price, aes(x=age.group, y= avg.price))+geom_col()
#평균 구매금액은 40대 60대 50대 순으로 나타남

# 4.연령 및 성별에 따른 구매금액과 구매빈도의 차이
avg.price.freq = bium.order %>% 
  filter(!is.na(age) & !is.na(gender)) %>%
  group_by(age.group, gender)%>%
  summarise(avg = mean(total_price), freq =n())

ggplot(data = avg.price.freq, aes(x=age.group, y=avg,fill=gender))+geom_col()
ggplot(data = avg.price.freq, aes(x=age.group, y=avg,fill=gender))+geom_col(position = "dodge")

ggplot(data=avg.price.freq, aes(age.group, y=freq, fill =gender))+geom_col()
ggplot(data=avg.price.freq, aes(age.group, y=freq, fill =gender))+geom_col(position = "dodge")
ggplot(data = avg.price.freq, aes(x=age.group, y=avg))+geom_line() #이 데이터 셋에 geom.line을 사용하면 안되는 이유?


#5. 지역에 따른 구매 금액의 차이 
str(bium.order)
bium.order$region = substr(bium.order$address1, 1,2)
table(is.na(bium.order$region))
region.price = bium.order %>% filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(avg = mean(total_price), freq=n()) %>% 
  mutate(total = avg *freq)
ggplot(data = region.price, aes(x= region, y=avg)) + geom_col()
ggplot(data = region.price, aes(x= region, y= freq)) + geom_col()
ggplot(data = region.price, aes(x= region, y= total)) + geom_col()

write.xlsx(region.price,"test1.xlsx" )
cor(bium.order$region, bium.order$total_price)
install.packages("treemap")
require(treemap)
png(filename="tree1.png",width=800, height=800)
treemap(region.price, vSize = "total", index = "region",title = "지역별 매출현황")
dev.off()

#매출액
options(scipen = 100)
str(bium.order)
cor.price = bium.order %>% 
  filter(between(age,0,100)) %>% 
  select(age, total_price)



#######Review Text Mining
useNIADic()
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8") #문자 깨짐 해결방법
theme_set(theme_gray(base_family="AppleGothic"))
par(family='AppleGothic')#그래프 한글 깨짐 해결 방법

txt = readLines('review.txt')
head(txt)
txt = str_replace_all(txt,"\\W"," ")

noun = extractNoun(txt)
wordcount = table(unlist(noun))
df.word = as.data.frame(wordcount, stringsAsFactors = F)
df.word = rename(df.word, word = Var1, freq =Freq)
df.word = filter(df.word, nchar(word) >=2, word !=c("반찬","주문")) #2개 글자 이상 단어 추출
df.word %>% filter(!(word %in% c("반찬", "주문"))) %>% arrange(desc(freq))
df.word.rev = df.word %>% filter(!(str_detect(word, "반찬|주문|해서|^ㅎ"))) %>% arrange(desc(freq))

top50 = df.word %>% arrange(desc(freq)) %>% head(50)

pal = brewer.pal(8,"Dark2")
set.seed(1234)

wordcloud(words = top50$word,
          freq = top50$freq,
          min.freq = 2,
          max.words = 200,
          random.order =F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = pal )

wordcloud(words = df.word$word,
          freq = df.word$freq,
          min.freq = 2,
          max.words = 200,
          random.order =F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = pal )


wordcloud(words = df.word.rev$word,
          freq = df.word.rev$freq,
          min.freq = 2,
          max.words = 200,
          random.order =F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = pal )

