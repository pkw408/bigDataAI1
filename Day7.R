# 주제: 종교가 있는 사람은 이혼을 덜할까??

# 데이터 전처리
welfare_copy_copy <- welfare
table(welfare_copy_copy$religion) 

welfare_copy_copy$religion <- ifelse(welfare_copy_copy$religion > 9, NA, welfare_copy_copy$religion) #결측치 처리
welfare_copy_copy$religion <- ifelse(welfare_copy_copy$religion == 1, "yes", "no")

# 혼인상태 전처리
# 혼인상태 데이터는 파생변수를 만들어 혼인상태를 별도로 저장합니다.
# 파생변수이름: marriage_group
# 혼인유지중: marriage(1,4) / 이혼 : divorce(3), 나머지: NA
welfare_copy_copy$marriage_group <- ifelse(welfare_copy_copy$marriage %in% c(1,4), 'marriage',ifelse(welfare_copy_copy$marriage == 3, 'divorce', NA))

table(welfare_copy_copy$marriage_group)
table(is.na(welfare_copy_copy$marriage_group)) # NA 여부확인

# 종교 유무에 따른 이혼률을 표로 만들어줍니다.
religion_marriage <- welfare_copy_copy %>%  filter(!is.na(marriage_group)) %>% 
    group_by(religion, marriage_group) %>% 
    summarise(n=n()) %>% 
    mutate( pct = n / round(sum(n) * 100))

religion_marriage


####필기노트##### 추가


ggplot(data = religion_marriage, aes(x=marriage_group, y=pct, fill = religion)) + geom_col(position='dodge')
ggplot(data = religion_marriage, aes(x=religion, y=pct, fill = marriage_group)) + geom_col(position='dodge')
##########################################################################################################
# 연령대별(young,middle,old)
# 연령대별 이혼율 표를 만듭니다.

age_group_marriage <- welfare_copy_copy %>%  filter(!is.na(marriage_group)) %>% 
    mutate(age_group = ifelse(age <30, 'young',ifelse(age<60,'middle','old'))) %>% 
    group_by(age_group,marriage_group) %>% 
    summarise(n=n()) %>% 
    mutate( pct = n / round(sum(n) * 100,1))

ggplot(data=age_group_marriage, aes(x=age_group, y=n, fill=marriage_group)) + 
    geom_col(position='dodge') +
    scale_x_discrete(limit=c('young','middle','old'))

ggplot(data=age_group_marriage, aes(x=marriage_group, y=n, fill=age_group)) + 
    geom_col(position='dodge') +
    scale_x_discrete(limit=c('marriage','divorce'))

##########################################################################################################
# 주제: 지역 전처리
table(welfare_copy_copy$code_region)

welfare_region <- welfare_copy_copy
welfare_region <- ifelse(welfare_region$code_region < 1 | welfare_region$code_region > 7, NA, welfare_region$code_region)

table(welfare_region$code_region)

list_region <- data.frame(code_region = c(1:7), region = c('서울','수도권(인천/경기)','부산/경남/울산', '대구/경북','대전/충남','강원/충북','광주/전남/전부/제주도'))

# welfare_region와 list_region을 left_join()함수로 code_region변수를 기준으로 결합합니다.
welfare_region <- left_join(welfare_region,list_region, by='code_region')
welfare_region %>% select(code_region, region)
table(welfare_region$region)

##########################################################################################################
# 주졔 : 어떤 지역에 어떤 연령대가 많이 사는가?
# 지역별, 연령대별 비율표를 만듭니다.
age_region <- welfare_region %>% mutate(
    age_group = ifelse(age <30, 'young',ifelse(age<60,'middle','old'))) %>% 
    group_by(region,age_group) %>% 
    summarise(n=n()) %>% 
    mutate( pct = n / round(sum(n) * 100,1)) %>% 
    arrange(desc(n))

age_region
ggplot(data=age_region, aes(x=reorder(region,n), y=pct, fill=age_group)) + geom_col(position='dodge') +
    coord_flip() +
    ggtitle("지역별 연령대 그래프") +
    xlab("지역") +
    ylab("인구수")

ggplot(data=age_region, aes(x=reorder(age_group,n), y=pct, fill=region)) + geom_col(position='dodge') +
    ggtitle("지역별 연령대 그래프") +
    xlab("연령별") +
    ylab("인구수")

##########################################################################################################
# 주제: 연령대가 young 인 사람들이 많이 사는 지역 어디일까??
young_region <- age_region %>% filter(age_group == 'young') 

ggplot(data=young_region, aes(x=region, y=n)) +geom_col() +
    coord_flip() +
    ggtitle("젊은 사람들의 지역분포")

# x 축 레이블 출력 순서를 별도로 저장합니다.
order_young_list <- young_region$region

ggplot(data=young_region, aes(x=region, y=n)) +geom_col() +
    coord_flip() +
    ggtitle("젊은 사람들의 지역분포") +
    scale_x_discrete(limit=order_young_list)


# 주제: 연령대가 middle인 사람들이 많이 사는 지역은 어디일까?
middle_region <- age_region %>%  filter(age_group == 'middle') %>% arrange(pct)
order_middle_list <- middle_region$region

ggplot(data=middle_region, aes(reorder(region,pct),pct)) +geom_col(position = 'dodge') +
    coord_flip() +
    ggtitle("중년 사람들의 지역분포") +
    xlab("지역") +
    ylab("비율") +
    scale_x_discrete(limit = order_middle_list)


##########################################################################################################

# 범례 순서 변경하기
# aes() 함수의 fill 속성에 지정할 변수를 factor()함수를 실행해서 vector타입을 factor 타입으로 변경할때 levels 속성으로 출력할 범례순서를 지정합니다.

class(age_region$age_group) # character --> vector
# age_group은 factor가 아니기 때문에 levels() 함수를 실행하면 NULL이 출력됩니다.
levels(age_region$age_group)

# vector를 factor로 변환하기
# factor() 함수를 사용해서 age_group 변수 타입을 factor로 변환하고 levels 속성으로 범례순서를 변경할 수 있다.

# factor() 함수만 사용함녀 범주 순서가 문자열의 오름차순으로 설정됩니다.
age_region$age_group <- factor(age_region$age_group)
class(age_region$age_group)
levels(age_region$age_group) # middle old young

# factor() ㅎ마수의 levels  옵션으로 범주 순서를 지정할 수 있다.
age_region$age_group <- factor(age_region$age_group, levels = c('young','middle','old'))


ggplot(data=age_region, aes(region,pct,fill=age_group)) +
    geom_col(position = 'dodge') +
    coord_flip() +
    xlab("지역") +
    ylab("비율") +
    scale_x_discrete(limit = order_young_list)

###########################################################################################################

# 텍스트 마이닝
# 텍스트 마이닝이란 문자로 된 데이터에서 가치있는 정보를 얻어내어 분석하는 기법입니다.

# KoNLP 를 설치하기 위한 준비
# 1. 자바가 컴퓨터에 설치되어 있어야하고 아래 패키지를 설치하고 로드합니다.
# java, rJava설치 <-- 엑셀 사용을 위해 미리 설치해둠
install.packages("multilinguer")
library(multilinguer)
install_jdk()

# 의존성패키지 설치 (KoNLP 사용을 위해)
install.packages(c("stringr","hash","tau","Sejong","RSQLite","devtools"),type="binary")

# github 버전 설치
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP',upgrade="never",INSTALL_opts = c("--no-multiarch"))

# 문제가 있을시 기존 Module을 날려주고 다시 깔아준다.
library("KoNLP")
useSystemDic() # 사전을 받아오는것 (Enter 를 쳐준다.)
useSejongDic()
useNIADic()

#### 정상적으로 설치되고 메모리에 올라감.




