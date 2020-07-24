# 문제
# mpg 데이터에서 audi에서 생산한 자동하 충 hwy가 1~5 해당하는 차종을 출력

library(dplyr) # %>% 연산자를 사용하기 위해해
library(ggplot2) # mpg를 사용하기 위해해
mpg

mpg %>%  filter(manufacturer == 'audi') %>%  arrange(desc(hwy)) %>% head(5)

# 문제
# mpg 데이터에서 cty와 hwy를 더한 합산 연비 변수를 만들고, 합산 연비 변수의 평균 연비 변수를 만든뒤, 평균 연비 변수의 값이 가장 큰 자동차 3개를 추출(제조사, 모델, 평균)

# 내가 풀어본 풀이이
mpg_copy <- mpg
mpg_copy$var_sum <- rowSums(subset(mpg_copy,select=c(cty,hwy)))
mpg_copy$var_mean <- rowMeans((subset(mpg_copy,select=c(cty,hwy))))
mpg_copy %>% select(manufacturer,model,var_mean) %>% arrange(desc(var_mean)) %>%  head(3)

# 정답
mpg %>% mutate(
    total = cty + hwy,
    mean = total / 2
) %>%  select(manufacturer,model,mean) %>% arrange(desc(mean)) %>%  head(3)

########################################################################################################

# summarise() : 그룹별로 통계치를 산출할 수 있다. ***********
# group_by() : 데이터의 그룹을 맺을 수 있다.      **********

df_excel_exam_copy %>% mutate(mean_math = mean(math2)) # 위 방법은 파생변수를 만드는 법이다.

df_excel_exam_copy %>% summarise(mean_math = mean(math2)) # 통계치를 만들어 하나의 값만 출력

# 위 투식의 실행결과를 보면 mutate() ㅎ마수는 mean_math라는 파생변수를 만들고 모든 math2 데이터릐 평균을 계산해 mean_math에 넣어주지만, summarise() 함수는 모든 math2 데이터의 평균만 게산해서 따로 출력하게 된다.

# group_by() 함수를 사욧ㅇ하여 먼저 그룹을 맺어주고 summarise() 함수를 사용해 요약할 수 있다.
df_excel_exam_copy %>% group_by(class2) %>%  summarise(mean_math2 = mean(math2))

df_excel_exam_copy %>%  group_by(class2) %>%  summarise(
    합계 = sum(math2),
    평균 = mean(math2),
    개수 = n(),
    최대값 = max(math2),
    최소값 = min(math2),
    표준편차 = sd(math2)
)

# 문제 
# mpg 데이터에서 자동차회사별로 차종(class)이 'suv'인 자동차의 cty와 hwy의 평균을 계산해서 내림차순으로 정렬하고 상위 5개 출력
mpg %>% group_by(manufacturer)%>% filter(class  == 'suv') %>% mutate(
    avg = (cty + hwy) / 2
) %>% summarise(avg_mean = mean(avg)) %>% arrange(desc(avg_mean)) %>%  head(5)

# mutate --> 차량에 대한 평균
# summarise --> 각 해당 자료의 summarise  를 찾아낸다. --> 회사에 대한 평균


# 차종별 class로 도시연비평균(cty) 의평균을 계산해서 평균이 높은 순서대로 출력합니다.
mpg %>%  group_by(class) %>% summarise(mean_cty = mean(cty)) %>%  arrange(desc(mean_cty))

# 고속도로연비의 평균이 가장 높은 회사 3곳 출력
mpg %>% group_by(manufacturer) %>%  summarise(mean_hwy = mean(hwy)) %>%  arrange(desc(mean_hwy)) %>%  head(3)

# 각 회사별 경차(compact)의 차종(class) 수를 내림차순으로 정렬해 출력합니다.
mpg %>% group_by(manufacturer) %>%  filter(class == 'compact') %>%  summarise(compact_개수 = n()) %>% arrange(desc(compact_개수))


######################################################################################################

# left_join() : 가로로 데이터를 합칠 수 있다.
# left_join(데이터프레임1, 데이터프레임2, 데이터프레임3, ..., by='기준변수명')
# by 옵션에는 합칠 때 기준이되는 변수의 일믕르 입력해야 하며 합쳐질 두 개의 데이터프레임에 반드시 같은 이름의 변수가 있어야한다. 
# by 옵션을 지정하지 않은면 R 이 알아서 같은 이름의 변수를 기준으로 합치기를 실행한다!
test1 <- data.frame(id=c(1,2,3,4,5), middle=c(60,80,70,90,85))
test2 <- data.frame(id=c(1,2,3,4,5), middle=c(70,83,63,95,80))
left_join(test1,test2,by='id') # id를 기준으로 두개의 데이터가 합쳐진다.

df_excel_join <- excel_exam
df_teacher_name <- data.frame(class2=c(1,2,3,4,5), teacher=c('김사과','오렌지','반하나','이메론','류정원'))
left_join(df_excel_join,df_teacher_name,by='class2') # left_join()으로 합치기를 실행할때 두개의 데이터프레임 행(데이터)의 개수가 반드시 같을 필요는 없다

# bind_rows() : 새로롤 데이터를 합칠 수 있다.
group1 <- data.frame(id=c(1,2,3,4,5),test=c(60,70,80,90,100))
group2 <- data.frame(id=c(6,7,8,9,10),test=c(70,90,30,50,60))
bind_rows(group1,group2)


#######################################데이터 정제###################################################
# 데이터 정제란? 빠진 데이터 또는 잘못된 데이터를 제거하는 작업
# 빠진 데이터, 측정이 안된 데이터, 없는 데이터 --> 결측지(NA)
# 문자열 결측지는 <NA>로 표시되고, 문자열을 제외한 나머지 자료형의 결측지는 NA로 표신된다.

df_na <- data.frame(gender=c('M','F',NA,'F','M'),score=c(5,4,3,2,NA))

# is.na() : 데이터에 결측지가 포함되어 있는지 여부를 확인할 수 있다.
# 결측지는 TRUE, 결측치가 아니면 FALSE로 표시된다.
is.na(df_na)

# 빈도수를 알아내는 함수 : table()
# is.na(), table() 두 가지 함수를 이용해 결측지의 빈도수를 파악할 수 있다.
table(is.na(df_na))
table(is.na(df_na$gender))
table(is.na(df_na$score))

# 결측치가 포함된 데이터를 함수에 적용시키면 정상적으로 연산되지 않고 NA가 출력됩니다.
sum(df_na$score)
mean(df_na$score)

# 결측치를 처리하는 방법
# 1) NA 값을 제거해버린다.  

# 1. dplyr 패키지의 filter()를 사용해서 결측치를 제외한 데이터만 추출
# 특징 : 원하는 행을 걸러낼 수 있다.
# gender에서 NA가 아니고 score도 NA가 아닌 데이터 추출
df_no_na <- df_na %>% filter(!is.na(score)&!is.na(gender))

sum(df_no_na$score)
mean(df_no_na$score)

# 2. na.omit() 함수를 사용해서 결측치가 있는 모든 행을 한꺼번에 제거
df_no_na <- na.omit(df_na)

sum(df_no_na$score)
mean(df_no_na$score)

#3. 함수를 실행할때 na.rm=T 속성을 지정하면 결측치를 제외하고 함수를 실행
sum(df_no_na$score,na.rm=T)
mean(df_no_na$score,na.rm=T)
max(df_no_na$score,na.rm=T)
min(df_no_na$score,na.rm=T)

#######################################결측치 제거 실습###################################################
df_excel_exam_na <- excel_exam
df_excel_exam_na[c(3,8,15), 'math2'] <- NA
df_excel_exam_na[20,'science2'] <- NA

# a.
df_excel_exam_na %>%  group_by(class2) %>%  summarise(
    math_sum = sum(math2,na.rm=T),
    math_mean = mean(math2, na.rm=T),
    math_count = n() # 개수는 NA가 포함되어 출력.
)

# b.결측치의 개수를 세지 않으려면 filter() 함수를 사용하여 미리 결측치를 걸러내고 계산한다.
df_excel_exam_na %>% group_by(class2) %>%  filter(!is.na(math2)) %>%  summarise(
    math_sum = sum(math2,na.rm=T),
    math_mean = mean(math2, na.rm=T),
    math_count = n() # 개수는 NA가 포함되어 출력력.
)

# c. filter()대신 na.omit()를 사용하여 사용가능
# 단, na.omit() 함수는 특정열만 na를 제거하는게 아니라 전체데이터의 na를 제거한다.
na.omit(df_excel_exam_na) %>% group_by(class2) %>%  summarise(
    math_sum = sum(math2,na.rm=T),
    math_mean = mean(math2, na.rm=T),
    math_count = n() # 개수는 NA가 포함되어 출력.
) # science행의 na역시 제거된다. 

# d. ifelse() 사용하여 결측치가 아닌 데이터로의 평균값으로 대체합니다.
# 데이터의 평균이나 중위수 처럼 특정 데이터 집단을 대표할 수 있는 값으로 대체시켜 사용한다.
# 순서
# da. NA를 제외한 math와 science의 평균을 게산한다.
mean_math2 <- mean(df_excel_exam_na$math2,na.rm=T)
mean_science2 <- mean(df_excel_exam_na$science2,na.rm=T)

# db. NA 데이터를 각 열의 평균데이터로 대체
df_excel_exam_na$math2 <- ifelse(is.na(df_excel_exam_na$math2),mean_math2,df_excel_exam_na$math2)
df_excel_exam_na$science2 <- ifelse(is.na(df_excel_exam_na$science2),mean_science2,df_excel_exam_na$science2)
df_excel_exam_na


#######################################이상한 데이터 ###################################################
# 이상한 데이터 : 극단치, 이상점, outliers
# 이상한 데이터는 존재할 수 없는 값이 데이터에 포함되어 있음을 의미합니다.
# 발견된 이상한 데이터는 결측치로 변환한 후 제거하거나 다른 값으로 대체한다.


# gender는 1~5까지의 데이터만 가질 수 있고, score는 1~10까지만 데이터로 가질 수 있다.
outliers <- data.frame(gender = c(1,2,1,3,5,1,9,2,2,1), score = c(5,6,9,1,4,6,2,0,10,7))

# table()를 사용해서 이상치가 존재하는지 확인
table(outliers$gender)  # 9는 이상한 데이터
table(outliers$score)   # 0은 이상한 데이터   --> 이상데이터 확인 

# 이상치가 존재할 경우, ifelse() 함수를 결측치로 변환한다.
outliers$gender <- ifelse(outliers$gender > 5, NA, outliers$gender)
outliers$score <- ifelse(outliers$score > 0 & outliers$score <= 10, outliers$score, NA)

# ggplot2를 이요한 그래프 그리기기

library(ggplot2)

# 간단한 그래프 그리기
# ggplot2 그래프 작성법은 레이어 구조로 되어 있습니다.
# 배경을 먼저 만들고, 그 위에 그래프를 그리고, 그 위에 축, 색, 표식.... 추가해서 완성합니다.

# 1. 그래프가 출력될 배경을 만듭니다.
# ggplot(data=데이터프레임, aes(x=가로축,y=세로축))
ggplot(data=mpg, aes(x=hwy, y=cty))
ggplot(data=mpg, aes(x=hwy, y=cty)) + geom_point() # 점으로 찍어주기
ggplot(data=mpg, aes(x=hwy, y=cty)) + geom_point() + xlim(10,60) + ylim(5,50) # x,y 축 범위 설정


# 문제
# 구동방식별(drv) 고속도로 연비 평균을 막대 그래프(geom_col()) 로 표현하기
# (x :drv, y:mean_hwy)
mpg_drv <- mpg %>% group_by(drv) %>% summarise(mean_hwy = mean(hwy))
ggplot(data=mpg_drv, aes(x=drv, y=mean_hwy)) + geom_col()

# 차종(class)별 도시연비(cty) 평균을 막대 그래프로 표현
mpg_class <- mpg %>% group_by(class) %>%  summarise(mean_cty = mean(cty))
ggplot(data=mpg_class,aes(x=class,y=mean_cty)) + geom_col() # x 축 알파벳 기준준

# reorder()를 이용해서 x축 항목의 정렬 순서를 변경할 수 있다.
# reorder(정렬할 데이터가 저장된 변수명, 정렬 기준으로 사용할 변수명)
# 예) reorder(class, mean_cty) : class를 mean_cty의 오름차순으로 정렬
# 정렬 기준으로 사용할 변수명만 써주면 오름 차순으로 정렬되고, 정렬 기준으로 사용할 변수앞에 '-'르 붙여주면 내림 차순으로 정렬된다.

ggplot(data=mpg_class,aes(x=reorder(class,mean_cty), y=mean_cty)) + geom_col()
ggplot(data=mpg_class,aes(x=reorder(class,-mean_cty), y=mean_cty)) + geom_col()




