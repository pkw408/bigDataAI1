# 동적 크롤링
# 웹 브라우저 원격 조작에 사용하는 Selenium 사용하기
# 동적 웹 사이트를 동적 웹 스크레이핑하기 (움직이는 데이터)

# Selenium 사용하는 방법
# 크롬 드라이브 설치
# 1. http://chromedriver.chromium.org/downloads 에서 크롬버전 84.0.4147.105 에 맞는 드라이브를 다운받는다 / win32 버전을 다운받아준다.

# 2. 구글 드라이브에 selenum-server-standalone-master.zip을 다운받아 압축을 해제한 후 크롬드라이버 압축을 풀고 chromedriver.exe를 bin 폴더 안에 넣어준다.

# 해당 창에서 shift + 마우스 우클릭, 여기에서 파워쉘을 실행합니다.

# 4. 파워쉘창에서 java -jar selenium-server-standalone.jar -port 4445를 입력해서 서버를 실행시켜준다. 해당창은 작업중 계속 유지를 시켜준다.

# R에서 selenium 을 사용하기 위해 패키지를 설치하고 로드시켜준다.
install.packages("RSelenium")
library(RSelenium)

# remoteDriver 라는 변수
remDr <- remoteDriver(remoteServerAddr = 'localhost',port=4445L, browserName = 'chrome') # localhost : 셀레니움이 내가 작업하는 컴퓨터에서 실행되고있다.
remDr$open() # 크롬을 열어준다.
remDr$navigate("https://www.google.com") # 가고싶은 사이트를 적어준다. --> 해당사이트로 이동

webElem <- remDr$findElement('css',"[name='q']")
webElem$sendKeysToElement(list("날씨",key='enter'))

########################################################################################################################

# 호텔스컴바인 사이트를 불러온다.
# https://www.hotelscombined.co.kr/

remDr$open()
remDr$navigate("https://www.hotelscombined.co.kr/hotels/%EC%9D%B4%ED%83%9C%EC%9B%90-%ED%81%AC%EB%9D%BC%EC%9A%B4-%ED%98%B8%ED%85%94,%EC%84%9C%EC%9A%B8,%EB%8C%80%ED%95%9C%EB%AF%BC%EA%B5%AD-c35982-h83176-details/2020-08-20/2020-09-17/2adults?placeId=H1063682&placeName=hotel:Itaewon_Crown_Hotel")

# findElements() 함수에 using='css selector' 속성을 지정해서 css 선택자를 사용해 웹 스크레이핑을 하겠다고 지정하고  2번째 인수로 읽어올 데이터가 포함된css선택자를 지정합니다.
# 리뷰는 li 태그 자손인 div 태즈중에서 class 속성이 text로 지정된 것이 리뷰이므로 li div.text와같이 선택자를 지정합니다.
doms <- remDr$findElements(using='css seletor','li div.text')

# sapply() 함수를 사용해서 읽어들인 내용에서 getElementText() 함수로 문자열만 가져옵니다.
review <- sapply(doms, function(x) {x$getElementText()}) # doms 행의 갯수만큼 함수를 반복한다.
class(review)
# getElementText() 함수의 실행 결과는 list 타입이므로 unlist() 함수를 이용해서 vector로 변환한다.
review <- unlist(review)
review


#다음 후기 링크의 css 선택자
nextLink <- "a.moreReviews"
nextPage <- remDr$findElements(using='css selector',nextLink)
# clickElement() 함수로 얻어온 css 선택자에게 click 이벤트를 걸 수있습니다.
sapply(nextPage, function(x) {x$clickElement()})
# clickElement() 함수를 실행하면 findElements() 함수로 가져온 링크가 클릭되면서 브라우저 화면이 변경되는데 변경되는 페이지가 로딩되는 시간을 Sys.sleep() 함수로 잠시 멈춰 줍니다.
Sys.sleep(1)

########################################################################################################################
remDr$open()
remDr$navigate("https://www.hotelscombined.co.kr/hotels/%EC%9D%B4%ED%83%9C%EC%9B%90-%ED%81%AC%EB%9D%BC%EC%9A%B4-%ED%98%B8%ED%85%94,%EC%84%9C%EC%9A%B8,%EB%8C%80%ED%95%9C%EB%AF%BC%EA%B5%AD-c35982-h83176-details/2020-08-20/2020-09-17/2adults?placeId=H1063682&placeName=hotel:Itaewon_Crown_Hotel")
# 리뷰 전체를 기억햐ㅕㄹ 변수를 선언하고 초기화 시킨다.

review <- NULL
point <- NULL

repeat {
    
    #review
    doms <- remDr$findElements(using='css selector','li div.text')
    result <- sapply(doms, function(x) {x$getElementText()}) 
    result <- unlist(result)
    result <- result[result != ""] # 빈 result는 넣어주지 않는다.
    review <- c(review,result)  # review 에 계속 덮어씌어준다,

    #point
    doms <- remDr$findElements(using='css selector','li div._iqD')
    result <- sapply(doms, function(x) {x$getElementText()}) 
    result <- unlist(result)
    result <- result[result != ""]
    point <- c(point,result)
    
    nextLink <- "a.moreReviews.disabled"
    nextPage <- remDr$findElements(using='css selector',nextLink)
    if(length(nextPage) != 0){
        break;
    }
        
    #다음키
    nextLink <- "a.moreReviews"
    nextPage <- remDr$findElements(using='css selector',nextLink)
    sapply(nextPage, function(x) {x$clickElement()})
    
    Sys.sleep(1)
        
}
review_point <- data.frame(review,point)

###########################################if 조건문#######################################################
# if(조건식){
#     break;
# }

class(review_point$point)
point_mean <- review_point 

##############################################################################################################

# 네이버 웹툰 베스트 댓글 스크레이핑

# https://comic.naver.com/comment/comment.nhn?titleId=683496&no=178
remDr <- remoteDriver(remoteServerAddr = 'localhost',port=4445L, browserName = 'chrome') # localhost : 셀레니움이 
remDr$open()
remDr$navigate("https://comic.naver.com/comment/comment.nhn?titleId=683496&no=178")

# 베스트 댓글을 읽습니다.
review = NULL
doms <- remDr$findElements(using='css selector',"#cbox_module span.u_cbox_contents")
review <- sapply(doms, function(x) {x$getElementText()}) 
review <- unlist(review)

# 다음 페이지로 넘어가기
# 첫번째 페이지에서 보이지 않는 '맨앞' '이전' 키를 생각해줘야한다....
# 4 5 6 7 8 9 10 11 12 13 이걸 반복
# 전체 댓글 페이지가 몇 페이지인지 모르기 때문에 repeat 명령을 이용해서 무한 루프를 돌리고 끝까지 읽으면 무한 루프를 탈출시킨다.

# https://comic.naver.com/comment/comment.nhn?titleId=683496&no=178
remDr <- remoteDriver(remoteServerAddr = 'localhost',port=4445L, browserName = 'chrome') # localhost : 셀레니움이 
remDr$open()
remDr$navigate("https://comic.naver.com/comment/comment.nhn?titleId=683496&no=178")

review = NULL

repeat {
    for(i in 4:12){
        nextLink <- paste("#cbox_module > div > div.u_cbox_paginate > div > a:nth-child(",i,")", sep="")
        nextPage <- remDr$findElements(using="css selector", nextLink)
        if (length(nextPage) == 0) { 
            break
        }
        sapply(nextPage, function(x) { x$clickElement() })
        Sys.sleep(1)
        doms <- remDr$findElements(using = "css selector", "#cbox_module span.u_cbox_contents")
        result <- sapply(doms, function(x) { x$getElementText() })
        result <- unlist(result)
        review <- c(review, result)
    }
    nextLink <- "#cbox_module > div > div.u_cbox_paginate > div > a:nth-child(13)"
    nextPage <- remDr$findElements(using="css selector", nextLink)
    if (length(nextPage) == 0) { 
        break
    }
    sapply(nextPage, function(x) { x$clickElement() })
    Sys.sleep(1)
    doms <- remDr$findElements(using = "css selector", "#cbox_module span.u_cbox_contents")
    result <- sapply(doms, function(x) { x$getElementText() })
    result <- unlist(result)
    review <- c(review, result)
}

write.csv(review, "webtoonReview.csv")
##############################################################################################################

# 데이터/텍스트 마이님
movie_review <- read.csv("webtoonReview.csv",stringsAsFactors = F)
class(movie_review$x) # character 가 댓글이다.

library(KoNLP)
library(dplyr)
library(stringr)
review <- movie_review$x
txt <- gsub("[[:punct:][:cntrl:]]","",review) # 특수문자와 제어문자를 제거합니다.

# 텍스트 마이닝을 실시해서 명사를 추출합니다.
nouns <- extractNoun(txt)
class(nouns)
nouns <- unlist(nouns)

# 단어별 빈도표를 만들고 데이터프레임으로 변환합니다.
wordCount <-table(nouns)
class(wordCount)

df_wordCount <- as.data.frame(wordCount)
class(df_wordCount)

df_wordCount <- rename(df_wordCount, words=nouns, freq=Freq)
class(df_wordCount$words)
df_wordCount$words <- as.character(df_wordCount$words)
class(df_wordCount$words)

# 불필요한 단어를 제거합니다.
df_wordCount <- df_wordCount %>% filter(nchar(words)>=2 & nchar(words) <= 6 & freq >= 2) %>% 
    arrange(desc(freq))

head(df_wordCount,200)

# 숫자로만 구성된 단어를 제거합니다.
df_wordCount$words <- gsub("^[0-9]*$","",df_wordCount$words)
# ㅋㅋㅋ,ㅎㅎㅎㅎ,ㅜㅜㅜ,ㅠㅠㅠㅠㅠ,와 같이 한글 자음 또는 모음으로만 구성된 단어를 제거합니다.
df_wordCount$words <- gsub("^[ㄱ-ㅎㅏ-ㅣ]*$","",df_wordCount$words)

df_wordCount1 <- df_wordCount
df_wordCount1 <- df_wordCount1 %>%  group_by(words) %>%  summarise(freq=sum(freq))
# 출현 빈도수 상위 단어 200개를 뽑아준다.
top200 <- df_wordCount1 %>% arrange(desc(freq)) %>%  head(200)

library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8,"Dark2")
wordcloud(
    words = top200$words,
    freq = top200$freq,
    min.freq = 2,
    max.freq = 200,
    random.order = F,
    rot.per = 0.2,
    scale = c(7,0.2),
    colors = pal
    
)



