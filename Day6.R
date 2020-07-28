# 문제
# 어떤 회사에서 생산한 suv 자동차가 연비가 높은지 알아보기 위해 suv 차종(class)을 대상으로 도시연비(cty) 평균이 가장 높은 5개의 회사를 나타내는 막대 그래프를 연비가 높은 순서로 정렬하여 표현합니다.

library(ggplot2)
library(dplyr)

mpg
mpg_suv <- mpg %>%                  # mpg 데이터 셋에서
  group_by(manufacturer) %>%      # 제조사 별로 그룹화하고
  filter(class == 'suv') %>%      # 차종이 suv 인것만
  summarise(mean_cty = mean(cty)) %>%  # 도시 연비 평균을 계산하고
  arrange(desc(mean_cty)) %>%     # 내림차순으로 정렬
  head(5)                         # 상위 5건만 출력력

# 그래프 작성전에 사용할 데이터를 정렬시켜도 reorder()르 사용하지 않으면 x축 이름의 오름차순으로 정렬되어 출려한다.
ggplot(data = mpg_suv, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty)) + geom_col()


##########################################################################################################


# 한국 복지 패널 데이터 분석
# 데이터가 통계 전용 소프트웨어인 SPSS, SAS< STATA 프로그램의 데이터 형태로 제공되어 있기 때문에 foreign 패키지로 R 에서 사용할 수 있는 데이터 형태로 변환시킨다.

install.packages('foreign')
library(foreign)

# foreign 패키지의 read.spss() 함수를 사용해 SPSS 타입의 데이터를 리스트 타입으로 가져온다.
raw_welfare <- read.spss('Koweps_hpc10_2015_beta1.sav')
class(raw_welfare) # list 형태
# 데이터 프레임 형태로 변환
raw_welfare <- read.spss('Koweps_hpc10_2015_beta1.sav',to.data.frame = T)
class(raw_welfare)

# 복사본을 만듭니다.
welfare <- raw_welfare
str(welfare) # 간단한 자료를 출력해준다.
summary(welfare) # 열 단위의 최소값, 최대값 등을 보여준다.
head(welfare)
head(welfare,1)

View(welfare) # 데이터를 보기 쉽게 띄어준다
dim(welfare) # 데이터 프레임이 몇행 몇열인지 출력

# 기초. 열 이름을 분석하기 쉽게 만들어준다.
# Koweps_Codebook.xlsx(코드북) 파일을 참조해서 rename() 함수로 변수의 이름을 변경한다.
welfare <- rename(welfare, gender = h10_g3)     # 성별
welfare <- rename(welfare, birth = h10_g4)      # 태어난 연도
welfare <- rename(welfare, marriage = h10_g10)  # 혼인 여부
welfare <- rename(welfare, religion = h10_g11)  # 종교
welfare <- rename(welfare, code_job = h10_eco9) # 직종코드
welfare <- rename(welfare, income = p1002_8aq1) # 급여
welfare <- rename(welfare, code_region = h10_reg7) # 지역코드

View(welfare)

# 주제 설정
# 성별에 따른 급여 차이가 존재할까???

# 1. 성별 데이터 전처리
welfare$gender
class(welfare$gender)
table(welfare$gender) # 카테고리 확인 --> 무응답은 존재하지않음

welfare$gender <- ifelse(welfare$gender == 9, NA, welfare$gender) # 만약 이상치가 존재할때 결측처리를 해준다.
table(is.na(welfare$gender)) # 결측치 확인

# gender가 1일 때 "male". 2일 때 "female"
welfare$gender <- ifelse(welfare$gender == 1, 'male', 'female')
table(welfare$gender)

# 2. 급여 데이터 전처리
welfare$income # 결측치가 많다는걸 확인할 수 있다.
class(welfare$income) # numeric(숫자) 확인
table(welfare$income) # 카테고리가 많다는 거 확인

# 급여는 1~9998 사이의 값을 가지므로 급여의 범위를 벗어나는 경우 결측치로 처리
welfare$income <- ifelse(welfare$income > 9998 | welfare$income < 1, NA, welfare$income)

# 성별에 따른 평균 급여 표를 만든다.
gender_income <- welfare %>%  filter(!is.na(income)) # 계산을 위해 NA가 아닌 데이터만 추출
table(is.na(gender_income$income)) # 확인

# 최종
gender_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(gender) %>%  summarise(mean_income = mean(income))

ggplot(data=gender_income , aes(x=gender, y=mean_income)) + geom_col()


##########################################################################################################

# 주제: 몇살때 급여를 가장 많이 받을까?

# 태어난 년도 전처리
welfare$birth
class(welfare$birth)
table(welfare$birth)

# 이상치 처리
welfare$birth <- ifelse(welfare$birth == 9999,NA,welfare$birth) # 무응답 처리
welfare$birth <- ifelse(welfare$birth < 1900 | welfare$birth >2014, NA, welfare$birth)


# 나이를 기억하는 파생 변수를 만들어 나이를 저장합니다.
welfare$age <- 2015 - welfare$birth
table(welfare$age)

# 나이에 따른 급여 평균표를 만듭니다.
age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income = mean(income))

# ggplot(data=age_income, aes(x=reorder(age,-mean_income), y=mean_income)) + geom_col() # 순서대로 정렬
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_col()
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line() #선 그래프

##########################################################################################################

# 주제: 어떤 연령대가 급여가 가장 많을까?
# 연령대(young : 1~29, middle : 30 ~ 59, old : 60 ~ )

# 연령대를 기억하는 파생변수를 만들어준다.
welfare <- welfare %>% mutate(
  age_group = ifelse(age <30, 'young',ifelse(age<60,'middle','old')))

table(welfare$age_group)

ageGroup_income <- welfare %>%  filter(!is.na(income)) %>% group_by(age_group) %>%
  summarise(mean_income = mean(income))

ggplot(data=ageGroup_income, aes(x=age_group, y= mean_income)) + geom_col()
ggplot(data=ageGroup_income, aes(x=reorder(age_group,mean_income), y= mean_income)) + geom_col()

# scale_x_discrete() 함수를 사용해서 기본적으로 x축 레이블의 오름차순으로 정렬되서 작성되는 그래프의 xcnr 출력 순서를 변경할 수 있습니다.

# scale_x_discrete() 함수의 limit 속성에 c() 함수를 사용해 보고싶은 순서를 지정하면됩니다.
ggplot(data=ageGroup_income, aes(x=reorder(age_group,mean_income), y= mean_income)) + geom_col() + scale_x_discrete(limit = c('young','middle','old'))



##########################################################################################################

# 주제: 성별별 급여 차이는 어떤 연령대에서 가장 클까??

# 연령대별, 성별 평균 급여표를 만듭니다.
ageGroup_gender_income <- welfare %>% filter(!is.na(income)) %>% group_by(age_group, gender) %>%
  summarise(mean_income = mean(income))

ggplot(data=ageGroup_gender_income, aes(x=age_group, y= mean_income)) + geom_col() + scale_x_discrete(limit = c('young','middle','old')) # (틀린방법)

# 축을 만드는 aes() 함수에 fill 옵션을 지정하면 그룹이 2 개 이상일 경우 그룹별로 색을 다르게 출력한다.
ggplot(data=ageGroup_gender_income, aes(x=age_group, y= mean_income, fill=gender)) + geom_col() + scale_x_discrete(limit = c('young','middle','old')) # 그래도 비교가 쉽지않다.

# geom_col() 함수의 position='dodge' 옵션을 지정하면  그룹이 2개이상일 경우에 막대 그래프를 쌓아서 출력하지 않고 그룹별로 그래프를 출력합니다.

ggplot(data=ageGroup_gender_income, aes(x=age_group, y= mean_income, fill=gender)) + geom_col(position = 'dodge') + scale_x_discrete(limit = c('young','middle','old')) # 그래도 비교가 쉽지않다.

##########################################################################################################

# 주졔 : 어떤 직업이 급여를 많이 받을까?
welfare$code_job
class(welfare$code_job)
table(welfare$code_job)

# 엑셀 시트2 를 읽어와서 코드별 직종정보를 메모리에 올린다.
library(readxl)
job_list <- read_excel('Koweps_Codebook.xlsx', sheet=2)
class(job_list)

welfare_job <- welfare
# left_join()함수로 welfare_job에 job_list를 결합시키다.
welfare_job <- left_join(job_list, welfare_job, by='code_job')
welfare_job$job
table(is.na(welfare_job$code_job))
table(is.na(welfare_job$job))

# 직업코드나 직업 또는 급여가 NA가 아닌 데이터만 추출해서 직업별 평균 급여를 만듭니다.
codeJob_income <- welfare_job %>%  filter(!is.na(code_job)) %>% filter(!is.na(income)) %>%  
  select(code_job,job,income)

codeJob_income_top35 <- welfare_job %>%  filter(!is.na(code_job)) %>% filter(!is.na(income)) %>%  
  select(code_job,job,income) %>% group_by(job) %>% summarise(mean_income = mean(income)) %>%
  arrange(desc(mean_income)) %>%  head(35)

ggplot(data=codeJob_income_top35, aes(x=reorder(job,-mean_income),y=mean_income)) + geom_col()

# x 축 레이블에 표시되는 직업의 일므이 너무 길어서 겹쳐 보이기 때문에 잀을수 없는 경우 coord_flip()함수를 사용해 차트를 회전시켜 출력할 수 있다.
ggplot(data=codeJob_income_top35, aes(x=reorder(job,mean_income),y=mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0,1000) +
  xlab("직업") +
  ylab("평균급여") +
  ggtitle("직업별 상위 35개의 평균 급여 그래프") # 차트 제목

##########################################################################################################

# 주제: 어떤 직업이 가장 급여를 적게 받을까? (하위 35개의 직업 출력)

job_income_low35 <- welfare_job %>%  filter(!is.na(code_job)) %>% filter(!is.na(income)) %>%  
  select(code_job,job,income) %>% group_by(job) %>% summarise(mean_income = mean(income)) %>%
  arrange(mean_income) %>%  head(35)

ggplot(data=job_income_low35, aes(x=reorder(job,mean_income),y=mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0,1000) +
  xlab("직업") +
  ylab("평균급여") +
  ggtitle("직업별 하위 35개의 평균 급여 그래프") # 차트 제목


##########################################################################################################
# 문제 1
# 직업이 "정보시스템 개발 전문가(0222) 일 경우 나이별 평균 급여는 얼마인가?? (막대 그래프로 표현)
welfare_copy <- welfare
welfare_copy <- left_join(welfare_copy,job_list,by='code_job')
table(welfare_copy$code_job)
table(welfare_copy$job)

welfare_copy <- welfare_copy %>% mutate(
  age = 2015 - birth 
)

job_developer <- welfare_copy %>% filter(code_job == 0222) %>%  filter(!is.na(code_job)) %>% filter(!is.na(income)) %>% 
  select(job,age,income) %>% group_by(age) %>% summarise(mean_income = mean(income))

ggplot(data=job_developer,aes(x=age,y=mean_income)) + geom_col() +
  ylim(0,1000) +
  xlab("나이") +
  ylab("평균급여") +
  ggtitle("정보시스템 개발 전문가 겨우, 나이별 평균 급여")

# 문제 2
# 직업이 "정보시스템 개발 전문가(0222) 일 경우 나이별, 성별 평균 급여는 얼마인가( 막대그래프 표현, 색으로 성별 표현)

job_developer <- welfare_copy %>% filter(code_job == 0222) %>%  filter(!is.na(code_job)) %>% filter(!is.na(income)) %>% 
  group_by(age,gender) %>% summarise(mean_income = mean(income))

ggplot(data=job_developer,aes(x=age,y=mean_income,fill=gender)) + geom_col() +
  ylim(0,1000) +
  xlab("성별") +
  ylab("평균급여") +
  ggtitle("정보시스템 개발 전문가 겨우, 나이별 평균 급여")

# 문제 3
# 성별별로 어떤 직업에 종사하는 사람이 많을까? 

# 3-1 성별별 직업 빈도표르 만든다. (막대그래프 표현)
welfare_copy <- welfare
welfare_copy <- left_join(welfare_copy,job_list,by='code_job')
welfare_copy <- welfare_copy %>%  mutate(
  count = n()
)
welfare$gender
jobCount_gender <- welfare_copy %>% filter(!is.na(code_job)) %>%  filter(!is.na(income)) %>% 
  group_by(gender,job) %>% head(10)
ggplot(data=jobCount_gender,aes(x=reorder(job,count),y=count), fill=gender) + geom_col(position='dodge') +
  coord_flip()

# 3-2 남자 직업 빈도표를 만듭니다.
jobCount_male <- welfare_copy %>% filter(!is.na(code_job)) %>%  filter(!is.na(income)) %>% 
filter(gender == "male") %>% select(job,gender,count) %>% head(30)

ggplot(data=jobCount_male,aes(x=reorder(job,-count),y=count)) + geom_col() +
  coord_flip() +
  ggtitle("남성 직업 빈도표")

# 3-3 여자 직업 빈도표를 만듭니다.
jobCount_female <- welfare_copy %>% filter(!is.na(code_job)) %>%  filter(!is.na(income)) %>% 
  filter(gender == "female") %>% select(job,gender,count) %>% head(30)

ggplot(data=jobCount_female,aes(x=reorder(job,-count),y=count)) + geom_col() +
  coord_flip() +
  ggtitle("여성 직업 빈도표")

# 3-4 남자 연령대 (young) 직업 빈도표를 만듭니다.


# 3-5 여자 연령대 (young) 직업 빈도표를 만듭니다.
