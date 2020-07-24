# 문제
# mpg 데이터에서 audi에서 생산한 자동하 충 hwy가 1~5 해당하는 차종을 출력

library(dplyr) # %>% 연산자를 사용하기 위해해
library(ggplot2) # mpg를 사용하기 위해해
mpg

mpg %>%  filter(manufacturer == 'audi') %>%  arrange(desc(hwy)) %>% tail(5)
