require(xlsx);require(readxl); require(dplyr); require(ggplot2); require(lubridate)
#https://github.com/youngwoos/Doit_R/issues/9 한글깨짐 해결 방법
#theme_set(theme_gray(base_family="AppleGothic")), par(family='AppleGothic')#그래프 한글 깨짐 해결 방법
#Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8") 문자 깨짐 해결방법

#data load 
bium.order = read_excel('order_list.xlsx', sheet = 1) %>% as.data.frame()
bium.cart = read_excel('order_cart_list.xlsx', sheet = 1) %>% as.data.frame()
str(bium.order)
bium.order$is_fromshop
# wrangle NA value 
colSums(is.na(bium.cart))  #345808
bium.cart$birth
str(bium.cart)
#index practice 
#bium.cart에서 90번째부터 100번째까지 고객의 생년월일만 출력하세요.
bium.cart[90:100,4]
colSums(is.na(bium.order))

#date practice
#lubridate 함수: https://lovetoken.github.io/r/2016/09/18/lubridate_package.html

#customer info
bium.cart$birth = as.POSIXct(as.character(bium.cart$birth), format = "%Y%m%d")
bium.cart[,10] = year(bium.cart$birth)#출생년도별 정리 
names(bium.cart)[10] = "birth_year"#컬럼명 변경하기
#bium.cart = rename(bium.cart, "birth_year" = "V10")
plot(table(subset(bium.cart, select = birth_year, subset = birth_year >1950)))
plot(table(bium.cart %>% filter(birth_year>1950) %>%  select(birth_year)))
unique(bium.cart$uid)

#오더 df에 구매빈도 수 vs 카트 df 구매 빈도 수 의미 차이?
#오더 df의 구매 빈도는 한개의 반찬을 구매해도 1건, 여러개 반찬을 한번에 구매해도 1건
#카드 df의 구매 빈도는 각각의 반찬을 개별 합. 
#카드 df의 구매 빈도가 높은 고객과 오더 df의 구매 빈도가 높은 고객은 다를까?


bium.cart$birth_month = month(bium.cart$birth) #고객 출생 월 정리하기
addmargins(table(bium.cart$birth_month))
sum(is.na(bium.cart$month))
plot(table(bium.cart$month)) ##별자리 매칭해보면 (?) 

sum(is.na(bium.cart$gender))
addmargins(table(bium.cart$gender))
barplot(table(bium.cart$gender))

#order time
class(bium.cart$created_at)
bium.cart$created_at[1]
bium.cart = bium.cart %>% mutate(order_year = year(created_at), order_month = month(created_at, label = T), order_day = day(created_at), order_wday = wday(created_at, label = T), order_hour = hour(created_at))
addmargins(table(bium.cart$order_year))
barplot(table(bium.cart$order_year))
addmargins(table(bium.cart$order_month))
barplot(table(bium.cart$order_month))
addmargins(table(bium.cart$order_wday))
barplot(table(bium.cart$order_wday))
addmargins(table(bium.cart$order_hour))
barplot(table(bium.cart$order_hour))
#구매에 영향미치는 변수: 카톡메시지,11시와  7시  보내는데 구매 전환률 비율

#valuable customers
#http://rfriend.tistory.com/240 grouping
table(bium.cart$uid)
bium.cart %>% group_by(uid, order_year,order_month) %>% summarise(count = n(), price_mean=mean(price), max=max(price), min =min(price)) %>% arrange(desc(count))

bium.cart[bium.cart$uid == "U515",] %>% group_by(order_month) %>% tally() %>% arrange(desc(n)) 

bium.cart[bium.cart$uid == "U13826",] %>% group_by(order_year,order_month) %>% tally() %>% arrange(desc(n)) 

bium.cart[bium.cart$uid == "U14172",] %>% group_by(order_year,order_month) %>% tally() %>% arrange(desc(n)) 

#popular sidedishes?
order.rank = bium.cart %>% group_by(product_name) %>% summarise(Freq = n()) %>% arrange(desc(Freq)) %>% head(10)
ggplot(order.rank, aes(x=product_name, y=Freq))+geom_bar(stat='identity')

#popular sidedishes by wdays?
wday_order = bium.cart %>% group_by(order_wday, product_name) %>% summarise(Freq =n()) %>% arrange(order_wday, desc(Freq)) %>% top_n(n=3, wt=Freq)
table(bium.cart[,c(15,7)]) %>% as.data.frame() %>% arrange((order_wday))
b %>% arrange(desc(Freq))
ggplot(wday_order, aes(x=product_name, y=Freq, fill=order_wday))+geom_bar(stat='identity', position = 'dodge')

#popular sidedishes by month?
month_order = bium.cart %>% group_by(order_month, product_name) %>% summarise(Freq =n()) %>% arrange(order_month, desc(Freq)) %>% top_n(n=3, wt=Freq)
ggplot(month_order,aes(x=product_name, y=Freq, fill= order_month))+geom_bar(stat='identity', position = 'dodge')

#popular sidedishes by gender
gender_order = bium.cart %>% filter(!is.na(gender)) %>% group_by(gender,product_name) %>% summarise(Freq=n()) %>% top_n(n=5,wt=Freq) %>% arrange(desc(Freq))
ggplot(gender_order, aes(x=product_name, y=Freq, fill= gender))+geom_bar(stat="identity", position = "dodge")


#나이별로 좋아하는 반찬은 무엇일까?
age_order = bium.cart %>% filter(!is.na(birth_year) & birth_year>1950) %>% group_by(birth_year, product_name) %>% summarise(Freq=n()) %>% top_n(n=3, wt=Freq) %>% arrange(desc(Freq)) %>% head(20)
ggplot(age_order, aes(x=product_name, y=Freq, fill=birth_year))+geom_bar(stat = 'identity', position = 'dodge')

#지역별로 좋아하는 반찬은 무엇일까?
bium.order$address = substr(bium.order$address1,1,2) #지역명 추출
bium.order %>% filter(!is.na(address)) %>% group_by(address,tracking_type ) %>% summarise(Freq=n(), Sum =sum(total_price))
bium.join = inner_join(bium.cart,bium.order,by='oid')
address_order = bium.join %>% filter(!is.na(address)) %>% group_by(address,product_name) %>% summarise(Freq = n()) %>% top_n(n=3, wt=Freq)
ggplot(address_order, aes(x=product_name, y= Freq, fill= address))+geom_bar(stat = 'identity', position = 'dodge')