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

