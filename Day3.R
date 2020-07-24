# DataFrame(데이터프레임) : 2차원
# 데이터프레임은 분석에서 가장 많이 볼 수 있는 자료형.
# 행렬과 달리 다양한 자료형을 동시에 사용할 수 있기 때문에 분석에서 많이 사용됩니다.

# data.frame(): 데이터프레임을 만듭니다.

df.a <- c(1,2,3,4,1,2) # 백터형
df.b <- c('a','b','c','d','e','f')
df.c <- c(1,'b','c','d',1,'f')
class(df.c) # charater 문자 > 숫자

# 위 세가지 백터를 합쳐 데이터 프레임을 만들어보자.
df.d <- data.frame(df.a,df.b,df.c) # 열 방향으로 데이터가 저장된다.
class(df.d) # 데이터프레임 형식

# 데이터프레임 요소 접근
# 데이터프레임의 특정 열의 값을 선택하는 방법은 크게 2가지가 있습니다.
# 1. 데이터프레임이름(열번호) --> 테이터프레임의 형태로 값을 선택
df_field1 <- df.d[1] # 1번 열
class(df_field1)

df_field2 <- df.d[1:2] # 1~2번 열

# 2. 데이터프레임$열이름 --> 백터데이터 형태로 값을 저장합니다.
df_field3 <- df.d$df.a
class(df_field3) 
df_field4 <- df.d$df.b
class(df_field4) 

# 외부데이터를 읽어오는 방법(csv)
# read.csv() : csv파일을 읽어 데이터프레임으로 저장합니다.
# 절대경로 : 파일의 위치를 물리적으로 저장
csv_exam <- read.csv('C:/KyoungPark/R/csv_exam.csv')

# 상대경로 : 파일의 위치를 작업폴더를 기준으로 저장(working directory)
csv_exam <- read.csv('csv_exam.csv')
csv_exam[1,] # 1 행 데이터
class(csv_exam[1,]) # $를 쓰지않는이상, 데이터프레임형이다.

csv_exam[,1] # 1 열만 데이터 선택 (데이터프레임 형식)

# 데이터 프레임에서 열을 선택하는 방법 5가지!!
class(csv_exam[,1]) # 백터형 데이터                           ** 주의 **
class(csv_exam[,'math']) # 백터형 데이터                      ** 주의 **
class(csv_exam[1]) # 데이터프레임형식 데이터
class(csv_exam['math']) # 열 이름으로 데이터 선택 (데이터프레임 형식 데이터)
class(csv_exam$math) # 백터형 데이터

csv_exam[20,5] # 20행 5열의 값 선택
class(csv_exam[20,5]) # scalar

# 조건에 만족하는 데이터프레임의 데이터를 선택하는 방법
csv_exam
# 데이터프레임의 class열에 저장된 값이 1인 행만 선택
csv_exam[csv_exam$class == 1,]

# 데이터프레임의 math열에 저장된 데이터가 80이상인 행만 선택
csv_exam[csv_exam$math >= 80,]

# 데이터프렝미의 class열에 저장된 데이터가 1이고, english열에 데이터가 90이상인 행만 선택
# && : 스칼라 경우 (||)                                      ** 주의 **
# & : 백터연산 (|)
csv_exam[(csv_exam$class == 1 & csv_exam$english >= 90),]

# 데이터프레임의 math열에 저장된 데이터가 50이하이거나, english열에 저장된 데이터가 50이하인 행만
csv_exam[(csv_exam$math <= 50 | csv_exam$english <= 50),]


# 읽어올 csv 파일의 첫번째 줄(행)이 열이름이 아닌경우, 데이터 손실이 일어날 수 있다.
# 그래서, header=F 옵션을 사용하여 읽어오면 데이터 손실을 막을 수 있다
csv_exam_noheader <- read.csv('csv_exam_noheader.csv') # 데이터 손실 발생
csv_exam_noheader <- read.csv('csv_exam_noheader.csv',header=F) # 데이터 손실을 막는다.
colnames(csv_exam_noheader) <- NULL # 첫번째 열이름을 날려버린다.
colnames(csv_exam_noheader) <- c('id', 'class' , 'math' , 'english' , 'science') # 열 이름 달아주기
csv_exam_noheader






# 외부 페키지를 받아보기
# install.packages('페키지이름') --> 패키지 이름에 꼭!!! 따옴표를 붙여야한다.
install.packages('dplyr')
library(dplyr) # 메모리에 로드시킬때에는 따옴표를 사용하지 않는다. 위 패키지를 메모리에 올려줘야 직접 사용할 수 있다.

# 만약, 패키지 설치가 안될경우
# 바이러스 차단 프로그램을 잠시 끄고 설치한 후, 다시 실행하면 된다.

# 1. rename() --> 데이터프레임의 열 이름을 변경하기 위한 함수
# rename(데이터 프레임이룸, 새이름 = 기존이름, 새이름, 기존이름, ...)
csv_exam_noheader <- read.csv('csv_exam_noheader.csv',header=F) # 데이터 손실을 막는다
csv_exam_copy <- csv_exam_noheader # 데이터 원본의 훼손을 막는다.
csv_exam_copy <- rename(csv_exam_copy, id=V1, class = V2, math=V3, english=V4, science=V5)


# excel 파일을 읽어오려면, readxl 패키지를 설치하고 로드한 후 사용합니다.
install.packages('readxl')
library(readxl)

excel_exam <- read_excel('excel_exam.xlsx') #--> csv 와 데이터를 불러오는 방식이 조금 다르다.(tibble 형태)


# tibble
# 데이터 프레임을 현대적으로 재구성한 타입으로 Simple Data Frames라고 부릅니다. tibble은 data.frame을 만들때 사용하고, 데이터를 간략하게 표현하고 대용량 데이터를 다룰수 있습니다. tibble 은 이력 유형이 변경되지 않아 character 변수를 이용할 때 보다 데이터 변경이 없는 자료에 자주 사용됩니다.

# tibble 자료분석애는 효율적이지만, 비번 자료수정(조작)에는 비효율적이다.
# 그럴때는 data.frame 사용을 권장한다.

# 읽어올 excel 파일의 첫번빼 줄이 열 이름이 아닌 경우 col_names=F 옵션으로 저장하여 읽어옵니다.
excel_exam_noheader <- read_excel('excel_exam_noheader.xlsx',col_names=F)

# 열 이름 바꿔주기
excel_exam_noheader_copy <- excel_exam_noheader
excel_exam_noheader_copy <- rename(excel_exam_noheader_copy, id='...1', class='...2', math='...3',english='...4',science='...5')
excel_exam_noheader_copy

# 읽어올 excel 파일에 sheet가 여러개 있을 경우 특정 시트의 데이터를 읽어오려면 sheet=n(n=읽어올 sheet 위치)
# 또는 sheet='시트이름' 옵션을 지정해서 불러옵니다.
excel_exam <- read_excel('excel_exam.xlsx', sheet=2)
excel_exam <- read_excel('excel_exam.xlsx', sheet='Sheet2')

# tibble 은 사용하는 패키지에 따라 사용할 수 없는 함수들이 많으므로 데이터프레임으로 변환시키고 사용하는것 일반적이다.
# as.data.frame()
excel_exam <- as.data.frame(excel_exam)

# 외부로 파일을 내보낸 법
# write.csv(데이터프레임, file='파일명')
write.csv(excel_exam,file='df_excel_exam2.csv',row.names=F) # 첫번 째 생기는 열을 생성하지 않아준다.

# 엑셀 파일을 읽고 쓰러면 xlsx 패키지를 설치해야한다.
# 1. xlsx 패키지는 자바JDK 가 설치되야 사용가능하다.
#    http://java.sun.com
#    Java SE 8u261 -->  JDK Download --> windows x64 Download
# 2. 자바가 설치되있는 jdk.1.8.0_251/bin 폴더의 환경변수를 등록시켜준다.
# 내 PC(우측버튼) --> 속성 --> 고급시스템설정 --> 환경변수 --> 시스템변수에서 path 편집 --> 새로 만들기 --> C:\Program Files\Java\jdk1.8.0_251\bin 등록
install.packages('xlsx')
library(xlsx)

write.xlsx(excel_exam, "df_excel_exam2.xlsx")
# sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_251') # 안될경우 path를 직접 R에 설정해준다.

# 문제1-1. 아래와 같이 구성되는 2행 2열 매트릭스 alpha를 생성합니다.
#
#       [,1] [,2] [,3]
#   [1,] "a"  "c"  "e"
#   [2,] "b"  "d"  "f"

alpha <- matrix(letters[1:6],nrow=2)
alpha
# 문제1-2. 'x', 'y', 'z'라는 행을 추가합니다.

finalAlpha.1 <- rbind(alpha, c('x','y','z'))
# rownames(finalAlpha.1) <- NULL

# 문제1-3. 's', 'p' 라는 열을 추가합니다.

finalAlpha.2 <- cbind(finalAlpha.1,newCol <- c('s','p'))
# colnames(finalAlpha.2) <- NULL

# 문제2. 아래와 같이 값이 구성되는 데이터프레임을 정의하여 df1에 저장합니다.
#   x   y
#1  1   2
#2  2   4
#3  3   6
#4  4   8
#5  5  10

d1 <- c(seq(1,5))
d2 <- c(seq(2,10,by=2))
df1 <- data.frame(d1,d2)

# 정답 : df1 <- data.frame(x=1:5, y=seq(2,10,2))

# 문제3. 아래와 같이 값이 구성되는 데이터프레임을 정의하여 df2에 저장합니다.
#   col1  col2 col3
#1  1       a    6 
#2  2       b    7
#3  3       c    8
#4  4       d    9
#5  5       e   10

col1<-(seq(1,5))
col2 <- c(letters[1:5])
col3 <- c(seq(6,10))
df2 <- data.frame(col1,col2,col3)

# 문제4-1. data.frame()과 c()를 조합해서 표의 내용을 데이터 프레임으로 만들어 출력합니다.
#   제품        가격        판매량
#   사과        1800        24
#   딸기        1500        38
#   수박        3000        13

chart <- data.frame(제품 = c("사과", " 딸기", "수박"), 가격=c(1800,1500,3000), 판매량 = c(24,38,13))


# 문제4-2. 앞에서 만든 데이터 프레임을 이용하여 과일 가격 평균, 판매량 평균을 구합니다.

mean_df <- mean(chart[,2])
mean_df
mean(chart[,3])

# 문제5-1. 다음 세 벡터를 이용하여 데이터프레임을 생성하고, gender 변수의 값을 반대 성별로 변경합니다. 그리고 name 변수는 문자, gender 변수는 팩터, math 변수는 숫자 데이터의 유형이라는 것을 확인합니다.
#   name <- c("류정원", "김사과", "오렌지", "반하나", "이멜론")
#   gender <- factor(c("M", "F", "M", "F", "M"))
#   math <- c(85, 76, 99, 88, 40)

name <- c("류정원", "김사과", "오렌지", "반하나", "이멜론")
gender <- factor(c("M", "F", "M", "F", "M"))
math <- c(85, 76, 99, 88, 40)

# stringsAsFactors = F 옵션 : 데이터를 읽어드릴때 문자가 아닌 factor 타입으로 읽어올 경우, 오류가 발생할 수 있어 문자타입으로 읽으라고 처리해줌
member_df <- data.frame(name,gender,math, stringsAsFactors=F)
member_df$gender <- ifelse(member_df$gender == 'F','M','F')  # ifelse 조건문

class(member_df$name)
class(member_df$gender)
class(member_df$math)


# 위에서 만든 데이터프레임에 대해 아래 작업을 수행합니다.
# 문제5-2. stat변수를 추가합니다. stat <- c(76,73,95,82,35)

stat <- c(76,73,95,82,35)
member_df <- data.frame(member_df,stat)

# 정답 : member_df$stat <- c(76,73,95,82,35) # 파생변수라고 한다. 

# 문제5-3. math변수와 stat변수의 합을 구하여 score변수에 저장합니다.

member_df$score <- member_df$math + member_df$stat
member_df

# 문제5-4. 논리 연산 인덱싱을 이용하여 score가 150이상이면 A, 100이상 150미만이면 B, 70이상 100미만이면 C등급을 부여하고 grade 변수에 저장합니다.

member_df$grade <- ifelse(member_df$score >= 150, 'A', ifelse(member_df$score >= 100, 'B', ifelse(member_df$score >= 70, 'C', NA)))
member_df
