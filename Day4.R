##############################모든 형태의 데이터형 공부 완료#########################################

# array(배열) : 다차원, matrix 로 구성되어 있습니다. matrix 가 여러개 있는 구조

# array() 함수를 이용해 만듭니다. dim 속성을 이용하여 행, 열, 면의 순서로 array 구조를 지정합니다.
# array(..., dim = c(2,3,4)) # --> 2행 3열 4면 (2행 3열 matrix가 4개 있다는 의미)

arr1 <- array (c(1:24), dim=c(2,3,4))

# array 요소 접근 방법
# arr1[행, 열, matrix]

arr1[,,1] # 1번째 matrix의 데이터 선택
arr1[1,,1] # 1번째 matrix의 첫번째 행 데이터 선택
arr1[,2,2] # 2번째 matrix의 두번째 열 데이터 선택
arr1[1,,] # 모든 matrix의 첫번째 행 데이터 선택
arr1[1,2,]# 모든 matrix의 1행 2열 데이터를 선택




# 리스트(List) : 다차원, 데이터 프레임으로 구성되어있다.
# list() 함수를 통해 만든다.

vec1 <- 1
mat <- matrix(c(1:12),ncol=6)
df <- data.frame(x1=c(1,2,3), x2=letters[1:3])
arr <- array(c(1:20), dim=c(2,5,2))

li<- list(list1=vec1, list2= mat, list3=arr, list4=df)
li
class(li)

############################## 파생 변수 (subset, transform, mutate)#########################################

# 파생변수 만들기 : 계산에 의해 데이터가 채워진 변수 (열)
df_raw <- data.frame(var1=c(1,2,1),var2=c(2,3,4))
df_var <- df_raw # 원본 데이터를 복사하여 df_raw 변수에 저장

# 파생변수 만드는 방법
# 1. 데이터프레임$파생변수
df_var$var_sum <- df_var$var1 + df_var$var2
df_var$var_mean <- df_var$var_sum/2
df_var

# 엑셀 읽어오기기
excel_exam <- read_excel('excel_exam.xlsx',sheet=2)
df_excel_exam_copy <- excel_exam

# 총점 파생변수
df_excel_exam_copy$var_sum <- df_excel_exam_copy$math2 + df_excel_exam_copy$english2 + df_excel_exam_copy$science2
df_excel_exam_copy

# 평균 파생변수
df_excel_exam_copy$var_mean <- df_excel_exam_copy$sum / 3
df_excel_exam_copy

# subset() : 데이터프레임에서 특정 변수의 데이터만 뽑아낼 수 있다. *****************************************
# subset(데이터프레임, select=시작변수명:끝변수명) # 연속적인 열 추출
# subset(데이터프레임, select=c(변수명, 변수명, ...)) #비 연속적인 열 추출
subset(df_excel_exam_copy, select=math2:science2)

# 동일한 2가지 방법
subset(df_excel_exam_copy, select=c(math2,science2))
df_excel_exam_copy[,c('math2','science2')] 

# rowSums(): 행의 합을 계산할 수 있다.
df_excel_exam_copy$var_sum2 <- rowSums(subset(df_excel_exam_copy,select=math2:science2))
df_excel_exam_copy
# rowMeans*( ): 행의 평균을 계산할 수 있다


# 2. 함수를 통한 파생변수 만들기 : transform()
# transform(데이터프레임, 파생변수이름=데이터)
df_excel_exam_copy <- transform(df_excel_exam_copy, var_sum3 = rowSums(subset(df_excel_exam_copy,select=math2:science2)))

df_excel_exam_copy

# 3. 함수를 통한 파생변수 만들기 : mutate() --> dplyr 패키지 안에 존재
# 한번에 여러개의 파생변수를 추가할 수 있다.
df_excel_exam_copy <- excel_exam

# %>% : pipe 연산자라고 한다. dplyr 패키지에 있는 대입문으로 왼쪽의 데이터를 오른쪽의 함수로 전달.
# %>% 왼쪽의 데이터 프레임이 %>%  오른쪽 함수로 전달되기 때문에 데이터프레임의 열을 사용해야할 때 [데이터프레임$변수이름] 과 같이 사용하지 않고 데이터 프레임의 이름을 생략하요 변수명만 사용한다.
# 단축키 : ctrl + shift + m

# install.packages('dplyr')
library(dplyr)
df_excel_exam_copy <- df_excel_exam_copy %>% mutate(
    var_sum = rowSums(subset(df_excel_exam_copy,select=math2:science2)),
    var_mean = rowMeans(subset(df_excel_exam_copy, select=math2:science2))
    ) # *****************************************

df_excel_exam_copy # Checking

# ifelse() : 조건을 사용하여 파생 변수를 만들 수 있다.

# 문제
# 평균 점수(var_mean) 가 60점 이상이면 'pass', 그렇지 않으면 'fail'을 가지는 파생변수 result를 추가
# 평균 점수(var_mean) 가 90점 이상이면 'A' , 80점 이상이면 'B' , 70점 이상이면 'C'. 60점 이상이면 'D', 나머지는 'F' 인 학점을 나타내는 파생변수 grade를 추가

df_excel_exam_copy$result <- ifelse(df_excel_exam_copy$var_mean >= 60 , 'pass','fail')
df_excel_exam_copy <- df_excel_exam_copy %>% mutate(
    grade = ifelse(var_mean>=90,'A',ifelse(var_mean>=80,'B',ifelse(var_mean>=70,'C',ifelse(var_mean>=60,'D','F'))))
)

df_excel_exam_copy # 확인

############################데이터 처리 및 분석하기기 (filter, select, table)#######################################

# 데이터 전처리 : 데이터 분석 작업에 적합하게 데이터를 가공하는 일.

df_excel_exam_copy <- excel_exam

# 함수 
# filter() : 행 단위 데이터를 추출합니다.
df_excel_exam_copy %>% filter(df_excel_exam_copy$class2 == 1)  # class2 가 1인 데이터만 뽑아준다.
# 데이터 프레임이 %>%  연산자를 통해 함수로 넘어오기 때문에 특정변수를 선택할 때 데이터프레임 이름을 적지 않아도 됩니다.
df_excel_exam_copy %>%  filter(class2 == 1)
df_excel_exam_copy %>%  filter(class2 == 1 | class2 == 2 | class2 == 5) # 백터형 이상의 데이터를 비교하기 때문에 | 사용

# matching 연산자 in 과 c()함수를 이용하면 or 연산자와 같은 효과를 낼 수 있습니다.
df_excel_exam_copy %>%  filter(class2 %in% c(1,2,5))

# mpg 데이터를 활용하기 위해 ggplot2 패키지 설치
install.packages('ggplot2')
library(ggplot2)

mpg

# 제조사(manufacturer)가 audi인 조시주행연비(cty)의 평균을 출력
mpg_audi <- mpg %>%  filter(manufacturer == 'audi')
mean(mpg_audi$cty)

# 제조사(manufacturer)가 toyota 인 도시주행연비(cty)의 평균을 출력
mpg_toyota <- mpg %>% filter(manufacturer == 'toyota')
mean(mpg_toyota$cty)

# 제조사가 chevrolet, ford, honda 인 자동차의 고속도록주행연비(hwy) 전체 평균 출력
mpg_ch_ford_honda <- mpg %>%  filter(manufacturer == 'chevrolet' | manufacturer == 'ford' | manufacturer == 'honda')
mean(mpg_ch_ford_honda$hwy)


# table() : 데이터의 항목(빈도수)
table(mpg_ch_ford_honda$manufacturer)

# select() : 열 단위 데이터 추출하기
df_excel_exam_copy %>% select(math2,english2,science2)
# select() 함수의 인수로 지정하는 변수 이름에 '-'를 붙여주면 '-'을 붙여준 변수를 제외하고 나머지 데이터만 검색
df_excel_exam_copy %>%  select(-math2, -science2)

# 문제
# class2가 1인 데이터 중에서 class2, math2 과목만 출력
df_excel_exam_copy %>% filter(class2==1) %>%  select(class2,math2)

# class2가 1, 3, 5 인 데이터의 class2, science2 열만 출력
df_excel_exam_copy %>%  filter(class2==1|class2==3|class2==5) %>% select(class2,science2)
df_excel_exam_copy %>%  filter(class2 %in% c(1,3,5)) %>% select(class2,science2) # 두가지 방법 가능

# head() : 데이터 앞부분의 데이터 개수를 정해서 볼수 있다, 개수를 생락하면 기본값으로 6개의 데이터를 보여준다.
# tail() : 데이터 뒷부분의 데이터 개수를 정해서 볼수 있다, 개수를 생락하면 기본값으로 6개의 데이터를 보여준다.

df_class135_science <-df_excel_exam_copy %>%  filter(class2 %in% c(1,3,5)) %>% select(class2,science2)
head(df_class135_science,3)
tail(df_class135_science,3)

# arrange() : 데이터를 정렬
df_excel_exam_copy %>% arrange(math2) # 오름차순 정렬
df_excel_exam_copy %>% arrange(desc(math2)) # 내림차순 정렬

# math2 점수로 내림차순 후, math2 점수가 같을 경우, science2 점수로 내림차순 정렬 후 5등까지 출력
df_rank5_science2 <- df_excel_exam_copy %>% arrange(desc(math2),desc(science2)) # 2차 정렬해줌 
head(df_rank5_science2,5)







# 검산 
#df_excel_exam_copy_copy <- df_excel_exam_copy
#df_excel_exam_copy_copy$PASS <- ifelse(df_excel_exam_copy$sum == df_excel_exam_copy$sum2, 'T', 'F')
#df_excel_exam_copy_copy


