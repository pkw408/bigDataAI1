###############################################################################################################################
# 과제
# 구글 드라이브에 과제 폴더 생성 -> twitter.csv 파일
# 국정원 트윗파일
# 명사를 추출해서 트위터를 분석하는 프로그램을 만들어본다.
# 열 이름: 번호, 계정이름, 작성일, 내용
# 내용을 기준으로 형태소 분석을 하고 빈도수에 따른 워드 클라우드 작성.

# data frame 으로 csv 파일을 읽어준다.
twitter <- read.csv('twitter.csv',fileEncoding = "UTF-8",header=T, stringsAsFactors = F) # UTF-8 인코딩 방식 설정정
class(twitter)
head(twitter)

# 데이터프레임의 변수 이름을 영어오 변경합니다.
# 내용(V5) 열을 ctx로 바꿔준다
# install.packages('dplyr')
library(dplyr) # rename을 사용하기 위해
twitter_copy <- rename(twitter, no='번호', id= '계정이름', writeDate = '작성일', tw = '내용')
head(twitter_copy)

# 특수 문자와 제어문자를 없애준다.
twitter_copy$tw <- gsub("[[:punct:][:cntrl:]]","",twitter_copy$tw)

# 형태소 분석 - 명사 추출
library(KoNLP)
nouns <- extractNoun(twitter_copy$tw)
class(nouns)
head(nouns)

# 백터형식을 풀어준다.
nouns <- unlist(nouns)

# 단어별 출현 빈도수를 계산하고 데이터 프레임으로 만들어준다.
wordCount <- table(nouns)
class(wordCount)
df_wordCount <- as.data.frame(wordCount) # 계산에 용이하게 데이터 프레임형태로 만들어준다,
class(df_wordCount)

# 데이터프레임의 변수 이름을 수정하고 2글자 이상인 6글자 이하인 단어만 뽑아냅니다.
df_wordCount <- rename(df_wordCount,words=nouns, freq=Freq)

df_wordCount$words<-gsub("^[0-9]*$","",df_wordCount$words) # 숫자제거
df_wordCount$words <- gsub(" ","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <-gsub("들이","",df_wordCount$words)
df_wordCount$words<-gsub("하게","",df_wordCount$words)
df_wordCount$words<-gsub("하지","",df_wordCount$words)
df_wordCount$words<-gsub("하기","",df_wordCount$words)
df_wordCount$words<-gsub("이유","",df_wordCount$words)
df_wordCount$words<-gsub("때문","",df_wordCount$words)
df_wordCount$words<-gsub("비롯","",df_wordCount$words)

# 상위 200개의 단어를 뽑아준다,
top200 <- df_wordCount %>% filter(nchar(words) >=2 & nchar(words) <= 6) %>%  arrange(desc(freq)) %>% head(200)

useSejongDic()  # 우리나라말 사전 등록

# 워드 클라우드를 만듭니다.
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
# brewer.pal(표현할 색상 개수, "팔레트 이름")
pal <- brewer.pal(8, "Dark2")

# 워드 클라우드를 만듭니다.
wordcloud(
    words = top200$word,    # 표시할 단어 목록
    freq = top200$freq,     # 단어의 출현 빈도
    min.freq = 2,  # 단어의 최소 개수
    max.word = 200, # 단어의 최대 개수
    rot.per = 0.1, # 단어의 회전 비율(10% 설정)
    random.order = F, # 빈도수 높은 단어를 중앙에 정렬 옵션 -- False  시킴
    scale = c(7, 0.5), # 워드 클라우드의 단어 크기
    colors = pal     # 단어의 표시할 색상 목록이 저장된 팔레트트
)

# 상위 20개의 단어들의 빈도수를 그래프로 그려라.
# 상위 20개의 단어를 뽑아준다,
top20 <- df_wordCount %>% filter(nchar(words) >=2 & nchar(words) <= 6) %>%  arrange(desc(freq)) %>% head(20)

library(ggplot2)
order_desc <- arrange(top20,desc(freq)) # 그래프의 순서를 지정해주기 위해 저장.
ggplot(top20, aes(words,freq)) + geom_col() + coord_flip() +
    ylim(0,2500) +
    scale_x_discrete(limit = order_desc$words) +
    xlab("단어") +
    ylab("빈도수") +
    ggtitle("국정원 트윗 분석") +
    geom_text(aes(label=freq), hjust=-1, color="#FF0000") # 글자색 설정가능
###############################################################################################################################
# 개인적 실습!
msg <- readLines("msg.txt",encoding = "UTF-8")

library(KoNLP)
useSejongDic()  # 우리나라말 사전 등록

#[:punct:] (출력 가능한 (눈에 보이는) 특수 문자)      # 위 3가지 다양한 함수(같은역할)
library(stringr) # gusb을 사용하기 위해
msg <- gsub("[[:punct:][:digit:][:cntrl:]]", "" ,msg) # --> 숫자랑 특수문자, 제어문자 제거
msg <- gsub("^[a-zA-Z]*$","",msg)
msg <- gsub("\\W","",msg)
msg <- gsub('[~!@#$%^&*()_+=?]<>','',msg)
msg <- gsub('저장한 날짜 : \\d{,2}','',msg)
msg <- gsub('\\(이모티콘\\)','',msg)
msg <- gsub('[ㄱ-ㅎ]','',msg)
msg <- gsub('(ㅜ|ㅠ)+','',msg)

nouns <- extractNoun(msg)
class(nouns)
nouns <- unlist(nouns)
class(nouns)


wordCount <- table(nouns)
df_wordCount <- as.data.frame(wordCount)
df_wordCount <- rename(df_wordCount,words = nouns, freq=Freq)

df_wordCount$words <- gsub("PM","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("AM","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("Oct","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("Sep","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("Aug","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("박경원","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("이쁘니유나","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("이쁘니유","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("박경","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("VoiceCal","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("Emoticon","",df_wordCount$word)         # 빈칸을 삭제
df_wordCount$words <- gsub("Phot","",df_wordCount$word)         # 빈칸을 삭제


top200 <- df_wordCount %>% filter(nchar(words)>=2) %>% arrange(desc(freq)) %>%  head(30)

# 워드 클라우드
library(wordcloud)
library(RColorBrewer)
# brewer.pal(표현할 색상 개수, "팔레트 이름")
pal <- brewer.pal(8, "Dark2")

# 워드 클라우드를 만듭니다.
wordcloud(
    words = top200$word,    # 표시할 단어 목록
    freq = top200$freq,     # 단어의 출현 빈도
    min.freq = 2,  # 단어의 최소 개수
    max.word = 200, # 단어의 최대 개수
    rot.per = 0.1, # 단어의 회전 비율(10% 설정)
    random.order = F, # 빈도수 높은 단어를 중앙에 정렬 옵션 -- False  시킴
    scale = c(7, 0.5), # 워드 클라우드의 단어 크기
    colors = pal     # 단어의 표시할 색상 목록이 저장된 팔레트트
)

library(ggplot2)
order_desc <- arrange(top200,desc(freq)) # 그래프의 순서를 지정해주기 위해 저장.
ggplot(top200, aes(words,freq)) + geom_col() + coord_flip() +
    ylim(0,500) +
    scale_x_discrete(limit = order_desc$words) +
    xlab("단어") +
    ylab("빈도수") +
    geom_text(aes(label=freq), hjust=-20, color="#FF0000") # 글자색 설정가능


###############################################################################################################################
# 정적 웹 스크레이핑(크롤링)

# 네이버 영화 사이트 --> 평점/리뷰 --> csv 파일로 저장
# 시나리오 : 네이버 영화 사이트 데이터 중에서 영화제목, 평점, 리뷰만 추출하여 csv 파일에 저장합니다.
# Target Site : https://movie.naver.com/movie/point/af/
# 우클릭 --> 파일 소스코드 보기


# 네이버 컴퓨터에 접속하기

# 웹 스크레이칭에 사용할 패키지를 설치하고 로드 --> 원하는 html을 읽어올수있다.
install.packages("rvest")
library(rvest)

# 스크레이핑 할 웹 사이트 주소를 지정합니다.
url <- "https://movie.naver.com/movie/point/af/list.nhn"

content <- read_html(url, encoding = "CP949") # UTF-8은 사용하면 안된다. / 해당 url의 페이지를 가져옴.

# rvest 패키지의 html_noodes() 함수로 읽어온 HTML 문서에서 필요한 데이터를 읽어옵니다.

    # html 문법
    # <a> : 클릭하면 해당 링크로 이동.
    # class = 꾸며주는 문법 (디자인을 조정해주기 위한 속성.)

# css 문법중 class 속성이 지정된 데이터를 읽으려면 "."을 찍어찾아준다.
# class = "movie color_b"
nodes <- html_nodes(content, ".movie") # 태그와 함꼐 데이터를 뽑아옴

# rvest 패키지 html_text() 함수로 trim=T 옵션을 지정해 불필요한 빈 칸을 제거하고 텍스트만 읽어옵니다.
movie <- html_text(nodes, trim=T)

# 평점은 div 태크의 list_netizen_score 에서 em 태그를 사용해서 읽어온다.
nodes <- html_nodes(content, "div.list_netizen_score > em") # > : div태크 list_netizen_score 안의 em 
points <- html_text(nodes, trim=T)

# 리뷰는 읽어들일 리뷰에 제목, 평점, 리뷰가 모두 포함되어 있습니다.
# 1) 따라서 아래와 같이 데이터를 정제해야합니다.
nodes <- html_nodes(content,".title")
title <- html_text(nodes, trim=T)
# 제어문자 제거하자 (tab)
title <- gsub("[[:cntrl:]]","",title)
# 2) strsplit() 삼수를 사용해서 영화제목, 평점, 리뷰를 분리합니다. -> 분리한 결과는 list타입입니다.
# 중[0-9]{1,2} : "중" 뒤에 숫자가 1글자 이상 2글자 이하 
title <- strsplit(title, "중[0-9]{1,2}") # list 타입
class(title)
title <- unlist(title) # 다시 백터형을 바꿔준다.
# 3) 신고라는 문자를 지워준다.
title <- gsub("신고","",title)

# 리뷰를 기억할 빈 변수를 선언합니다.
review = NULL
################################################FOR 문(CSS 문법)#####################################################################

# for(변수명 in 반복횟수) {
#   반복할 문장
#   ...
# }

# 변수 i가 1부터 5까지 1씩 중가하면서 반복
for(i in 1:5){
    print(i)
}

# 변수가 i가 5부터 1까지 1씩 감소하면서 반복
for(i in 5:1){
    print(i)
}

# 변수가 i가 1,3,5,7,9로 변경되면서 반복
for(i in c(1,3,5,7,9)){
    print(i)
}

# seq(초기치, 최종치, by=증가치)
for(i in seq(1,10,2)){
    print(i)
}

# by를 생략하고 증가치만 써도 가능하다.
for(i in seq(1,10)){
    print(i)
}

# i가 10부터 1까지 2씩 감소한다.
for(i in seq(10,1,-2)){
    print(i)
}

####################################################################################################################################

for(i in seq(2,20,2)){
    review = c(review, title[i]) # 짝수번째 자료를 추가해준다.
}

movie # 영화 제목
points # 평점
review # 영화 리뷰

# cbind() 함수를 사용해서 제목, 평점, 리뷰를 합쳐준다.
page <- cbind(movie,points)
page <- cbind(page,review)

# 웹 스크레이핑 된 데이터를 csv 파일로 저장시킨다.
write.csv(page,"movie_review.csv")

###############################################################################################################################
# 반복문을 사용해서 여러 패이지를 읽어오기
# https://movie.naver.com/movie/point/af/list.nhn?&page=i <- page 번호
# paste() 함수로 변경되지 않은 주소와 변경되는 주소를 이어 붙여서 읽어들일 페이지의 주소를 만듭니다.
# print(paste(url,I,sep=""))
for(i in seq(1,10)) {
    url <- "https://movie.naver.com/movie/point/af/list.nhn?&page="
    print(paste(url,i,seq=""))
}

# 한페이지를 읽어 들인 후 다음 페이지가 로딩되는 시간동안 잠시 스크레이핑을 멈춰주는 동작이 필요합니다. -> 페이지가 바뀔때 실행
# Sys.sleep(1) 1초

# 문제 
# 네이버 평점 페이지를 1~100까지 크롤링하여 제목, 평점, 리뷰 를 합쳐 movie_revie.csv 파일로 저장합니다.

review = NULL

movie_review = NULL
site <- "https://movie.naver.com/movie/point/af/list.nhn?&page="

for(i in seq(1, 100)){
    url <- paste(site, i, sep="")
    print(url)
    content <- read_html(url, encoding = "CP949")
    Sys.sleep(1)
    
    nodes <- html_nodes(content, ".movie")
    movie <- html_text(nodes, trim = T)
    
    nodes <- html_nodes(content, "div.list_netizen_score > em")
    point <- html_text(nodes, trim=T)
    
    nodes <- html_nodes(content, ".title")
    title <- html_text(nodes, trim=T)
    title <- gsub("[[:cntrl:]]", "", title)
    title <- strsplit(title, "중[0-9]{1,2}")
    class(title)
    title <- unlist(title)
    title <- gsub("신고", "", title)
    review = NULL
    for(i in seq(2, 20, by=2)){
        review = c(review, title[i])
    }
    page <- cbind(movie, point)
    page <- cbind(page, review)
    movie_review <- rbind(movie_review, page)
}
movie_review
write.csv(movie_review, "movie_review2.csv")

###############################################################################################################################

# 네이버 영화 사이트에서 1~100 페이지 분량의 영화 리뷰 데이터를 이용해 영화별 평점 그래프를 출력합니다.
movie_review <- read.csv("movie_review2.csv",stringsAsFactors=F)


movie_review_mean <- movie_review %>% group_by(movie) %>%  summarise(
    count = n(),
    mean = round(mean(point),2)
) %>% filter(count >= 6) %>%  arrange(desc(count)) %>%  head(10)

movie_review_mean

ggplot(movie_review_mean, aes(reorder(movie,mean),mean)) + geom_col() + coord_flip() +
    ggtitle("네이버 영화 평점순위") +
    xlab("영화제목") + 
    ylab("평점") +
    geom_text(aes(label=mean),hjust=-0.5) +
    geom_text(aes(label=count),hjust=4, color='white', size =8)


