###############################################################################################################################
#  Day7.R 패키지를 메모리에 올려줘야한다.
# 설치 안될시 재설치 작업을 해줘야한다. 문서 참조!!!!!!

# extractNoun() 함수를 사용해 명사를 추출합니다. -> 결과는 vector로 리턴됩니다.
library(multilinguer)
library(KoNLP)

noun <- extractNoun("대한민국의 영토는 한반도와 그 부속 도서로한다.")
class(noun)

# class(noun) 함수를 실행했을 때 결과가 list라고 나오면 아래와 같이 unlist() 함수를 이용하여 vector 형태롤 변환 후 작업을 해야한다.

# 형태소 분석 작업을 실행할 데이터를 읽어옵니다.
txt <- readLines('hiphop.txt')
class(txt)

# 정규식:
# https://highcode.tistory.com/6  사이트 참조
# ^ : 문자열 시작
# $ : 문자열 종료
# . : 임의의 한문자 (문자의 종류 가리지 않음. 단, |는 넣을 수 없다.)
# * : 앞 문자가 없을 수도 무한정 많을 수도 있음
# + :앞 문자가 하나 이상
# ? : 앞 문자가 없거나 하나있음
# {} : 횟수 또는 범위를 나타낸다.
# () : 소괄호 안의 문자를 하나의 문자로 인식
# | : 패턴 안에서 or 연산을 수행할 때 사용
# \w :알파벳이나 숫자
# \W :알파벳이나 숫자를 제외한 문자


# 자주 쓰이는 패턴 
# 숫자만: ^[0-9]*$
# 양문자만 : ^[a-zA-Z]*$
# 한글만 : ^[가-힣]*$
# 영어 & 슷지 : ^[a-zA-Z0-9]*$
# e-mail : ^[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-zA-Z0-9]+$
# 주민등록번호 : \d{6}\-\[1-4]\d{6}  -- \d{6} --> 숫자로 6글자만

# 텍스트 마이닝을 수행할 데이터에서 정규 표현식 또는 gsub() 함수를 사용하여 불필요한 문자를 제거합니다.(전처리 작업)
# R에서 정규식 표현을 사용하려면  stringr 패키지를 설치하고 로드 합니다.
install.packages("stringr")
library(stringr)

#str_replace_all(변수, "찾을 문자열", "바꿀 문자열")
txt <- str_replace_all(txt, "\\W","")

# gsub("찾을 문자열","바꿀 문자열", 변수)
txt <- gsub("\\W","",txt)

#[:punct:] (출력 가능한 (눈에 보이는) 특수 문자)      # 위 3가지 다양한 함수(같은역할)
txt <- gsub("[[:punct:]]","", txt) 
txt <- gsub("[[:punct:][:digit:]]", "" ,txt) # --> 숫자랑 특수문자만 제거

# extractNoun() 함수를 사용해서 명사를 출력합니다.
noun <- extractNoun(txt)
useNIADic()

class(noun)
noun <- unlist(noun) # list 타입을 unlist 로 바꿔준다.
# table() 함수를 이용해 단어 빈도표를 만들어준다.
table(noun)

wordCount <- table(noun)

# table 타입으로 생성된 단어별 빈도표를  as.data.frame() 함수를 사용해 데이터 프레임으로 변환합니다.
df_wordCount <- as.data.frame(wordCount)
head(df_wordCount,10)

# 데이터프레임의 변수 이름은 워드 클라우드 옵션에 맞게 변경합니다.
library(dplyr)
df_wordCount <- rename(df_wordCount, word = noun, freq = Freq)

# 데이터 프레임에서 데이터를 정제합니다.
# 숫자로만 구성된 단어와 단어에 포함된 모든 숫자가 제거됩니다.
# df_wordCount$word <- gsub("[[:digit:]]","",df_wordCount$word)
df_wordCount$word <- gsub("^[0-9]*$","",df_wordCount$word)
df_wordCount$word <- gsub(" ","",df_wordCount$word)
class(df_wordCount)

# 워드 클라우드로 구성할 단어를 추출합니다.
# 2음절 이상인 단어를 출현 빈도수의 내림차순으로 정렬해서 필요한 워드 클라우드로 구성할 단어를 추출합니다.
# nchar() : 글자의 개수를 셉니다.
df_wordCount <- df_wordCount %>% filter(nchar(word) >= 2)

# top200 <- df_wordCount %>% head(200)
top200 <- df_wordCount %>% arrange(desc(freq)) %>% head(20)

# 워드 클라우드
# 단어의 출현 빈도를 구름 모양으로 표현한 그래프로 단어의 출현 빈도에 따라 글자의 크기와 색상이 다르게 표현되기 때문에 어떤 단어가 얼마나 많이 사용되었는지 한 눈에 파악할 수 있습니다.

# 워드 클라우드 패키지를 설치합니다.
install.packages("wordcloud")
library(wordcloud)

# RColorBrewer 패키지는 R에 내장된 패키지 입니다. brewer.pal() 함수를 사용해서 단어에 표시할 색 목록을 만듭니다.
# '팔레트 in r" 로 검색하거나 아래 url 참조
#   URL 붙여넣기
# brewer.pal(표현할 색상 개수, "팔레트 이름")
pal <- brewer.pal(8, "Dark2")


# 워드 클라우드를 만듭니다.
wordcloud(
    words = top200$word,
    freq = top200$freq,
    min.freq = 2,
    max.word = 20,
    rot.per = 0.1, # 단어의 회전 비율(10% 설정)
    random.order = F, # 빈도수 높은 단어를 중앙에 정렬 옵션 -- False  시킴
    scale = c(5, 0.5), # 워드 클라우드의 단어 크기 
    colors = pal
    )
###############################################################################################################################
# 행정안전부
# 애국가 실습
useSejongDic()  # 우리나라말 사전 등록
# 1. 텍스트 파일을 읽어 변수에 저장한다.
song_txt <- readLines('애국가(가사).txt')
class(song_txt) # vector 형 데이터

# song_txt <- gsub("[[:punct:]]","", song_txt) 
# song_txt <- gsub("[[:punct:][:digit:]]", "" ,song_txt)

# 2. 명사추출
noun <- extractNoun(song_txt) 
#------크롤링에 많이 쓰이는 다른 방법--------#
# sapply(): 행렬구조의 데이터에서 모든 행에 함수를 적용할 사용하는 함수입니다.
# sapply(데이터, 적용할 할수)
noun <- sapply(song_txt,extractNoun)

# 2.5 사전에 단어를 등록해준다.
add_words = c("백두산","하느님", "남산","철갑","구름","가을","하늘","달")
buildDictionary(user_dic=data.frame(add_words,rep("ncn",length(add_words))),replace_usr_dic = T)

noun <- sapply(song_txt,extractNoun)

# 3. list르 풀어준다.
class(noun)

noun <- unlist(noun)
song_wordCount <- table(noun)
sort(song_wordCount,decreasing = T) # sorting 시킨다.

# 3.5 data.frame으로 바꿔준다.
df_song_wordCount <- as.data.frame(song_wordCount)
head(df_song_wordCount,10)
# dplyr 패키지 이용하여 열의 이름을 바꿔준다.
library(dplyr)
df_song_wordCount <- rename(df_song_wordCount, word = noun, freq = Freq)
df_song_wordCount$word <- gsub("^[0-9]*$","",df_song_wordCount$word)  # 숫자를 삭제
df_song_wordCount$word <- gsub(" ","",df_song_wordCount$word)         # 빈칸을 삭제

# 3.5 단어를 filtering 해준다.
# df_song_wordCount <- Filter(function(x) {nchar(x) >=2},df_song_wordCount)
df_song_wordCount <- df_song_wordCount %>%  filter(nchar(word)>=2)  # 2 자 이상

topWord <- df_song_wordCount %>% arrange(desc(freq))

# 4. 워드 클라우드
# install.packages("wordcloud")
# library(wordcloud)

# brewer.pal(표현할 색상 개수, "팔레트 이름")
pal <- brewer.pal(8, "Dark2")


# 워드 클라우드를 만듭니다.
wordcloud(
    words = topWord$word,    # 표시할 단어 목록
    freq = topWord$freq,     # 단어의 출현 빈도
    min.freq = 1,  # 단어의 최소 개수
    max.word = 10, # 단어의 최대 개수
    rot.per = 0.1, # 단어의 회전 비율(10% 설정)
    random.order = F, # 빈도수 높은 단어를 중앙에 정렬 옵션 -- False  시킴
    scale = c(5, 1), # 워드 클라우드의 단어 크기 
    colors = pal     # 단어의 표시할 색상 목록이 저장된 팔레트트
)

install.packages("wordcloud2")
library(wordcloud2)

# 배경 및 색상을 변경할 수 있다.
# color(색상), backgroundColor(배경색) 옵션 사용.

wordcloud2(topWord, fontFamily = "맑은 고딕", size=0.8, color='random-light', backgroundColor = 'black', shape='star')

# shape : circle, cardioid, diamond, triangle-forward, pentagon, star

###############################################################################################################################
# 과제
# 구글 드라이브에 과제 폴더 생성 -> twitter.csv 파일
# 국정원 트윗파일
# 명사를 추출해서 트위터를 분석하는 프로그램을 만들어본다.
# 열 이름: 번호, 계정이름, 작성일, 내용
# 내용을 기준으로 형태소 분석을 하고 빈도수에 따른 워드 클라우드 작성.


# data frame 으로 csv 파일을 읽어준다.