# 한국복지패널데이터 

install.packages("foreign")
# excel 읽히는 애랑 비슷한 애
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)
# ?????????????? 저 뒤에 to.data.frame 해줘야함??

welfare = raw_welfare
head(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

# 변수 확인 후, - 1.전처리(결측치, 이상치) - 2.분석(변수 간의 관계) - 3.결과(그래프)


## 성별에 따라 분석

class(welfare$sex) # numeric! 숫자로 되어 있구나!
table(welfare$sex) # 1과 2로 되어 있군. 1이 남자, 2로 여자로 해놨대여.

# 이상치 확인
table(welfare$sex)

# 이상치 결측 처리 (혹시 9(램덤수)가 있다면)
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)


class(welfare$income)  # 숫자로 되어 있군!
summary(welfare$income)  # 매우 유용! 결측치(NA's)도 바로 알 수 있음!!!
is.na(welfare$income)

qplot(welfare$income)  # x값이 쓸 때 없이 너무 크게 되있어서, 중요한 애들 잘 안 보임!!!
qplot(welfare$income) + xlim(0,1000)

summary(welfare$income)
table(is.na(welfare$income))

# 이상치 결측치로 처리하자!!
welfare$income - ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
# 너어어어무 큰 애 하나 땜에 그래프 다 작아져... ex.회장님은 막 몇 십억 받쟈나. 일단 여기서는 빼쟈

table(is.na(welfare$income))

# 성에 따른 수입을 welfare로 걸러낸다.
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>%
  summarise(mean_income = mean(income))
# female 162, male 312. (선생님은 female 163으로 나왔넹...)

sex_income

ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()


## 나이에 따라서
class(welfare$birth)

summary(welfare$birth) 

qplot(welfare$birth)

summary(welfare$birth)

table(is.na(welfare$birth))

welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))


# 우리나라 계산하기
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)


age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) +
  geom_line()



## 연령대별 분석

welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young", ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)


welfare = welfare %>% 
  mutate(ageg2 = ifelse(age < 10, "young",
                        ifelse(age < 20, "10s",
                               ifelse(age < 30, "20s",
                                      ifelse(age < 40, "30s",
                                             ifelse(age < 50, "40s",
                                                    ifelse(age < 60, "50s",
                                                           ifelse(age < 70, "60s", "elderly"))))))))
welfare                                                           
table(welfare$ageg2)                                                                  
qplot(welfare$ageg2)


## 연령대별 성별
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))


# 성별 연령별 월급 평균표 만들기
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()
# 여성은 30대 이상 더 이상 올라가지 않아여. 남성은 쭉쭉.
# 역시 그림을 그리니깐, 한 눈에 분석이 잘 된다능.

class(welfare$code_job)
table(welfare$code_job)  # 오호! 잡코드가 대단하군!

# 아하! 엑셀파일에 코드별 직업이름을 넣어놓은거예요~~

library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

welfare = left_join(welfare, list_job, id = "code_job")
head(welfare$job)
table(welfare$job)

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

## 분석 시작!!
job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()  # 원래는 수직인 걸, 옆으로 틀어라 라는 뜻!!

# 숙제 : 오늘 한거 쭉 매뉴얼로 다시 쳐보고, github url로 올리세용.

# 호잉. 처음 github에 올릴 때 넘 큰거 올렸어서. 더이상 안 올라감.
그 sav 파일 자체가 넘 컸었음. 이거 지우는건? 다음에 차차. 일단 새 디렉토리re머시기 만들어서 해유.

raw_welfare = read.spss(file = "C:/Users/Youmi/Desktop/R/Rtest/Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)

welfare = raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)

welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

welfare$income = ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(welfare$income)

sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()

class(welfare$birth)
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)
table(welfare$age)

age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(income) %>% 
  summarise(mean_income = mean(income))

ggplot(data = age_income, aes(x = age, y = mean_income)) +
  geom_line()

