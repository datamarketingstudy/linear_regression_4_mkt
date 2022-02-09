library(readxl)
library(tidyverse)
library(magrittr)

# Data load

df <- read_excel("regression4mkt.xlsx")
str(df)

colnames(df) <- c('ym', 'vip_cust', 'tot_cust')

df %<>%
    mutate(vip_prop = vip_cust/tot_cust,
           ym1 = as.Date(paste(ym, '01', sep = '-')),
           ym2 = seq.int(nrow(df)))

mkt.fit1 <- lm(vip_prop ~ ym2, data = df)
summary(mkt.fit1)

### 데이터 나누기

## train
df2017 <- df %>% 
    filter(ym2 <= 12)
df2018 <- df %>%
    filter(ym2 <= 24)
## test
df_other <- df %>%
    filter(ym2 > 12)

### == 2017년 데이터로 만든 모델

mkt.fit2 <- lm(vip_prop ~ ym2, data = df2017)
summary(mkt.fit2)
coef(mkt.fit2)

predict(mkt.fit2, newdata = df_other)
df_other$pred <- predict(mkt.fit2, newdata = df_other)
tail(df_other, 10)

ggplot(df_other) +
    geom_line(aes(x = ym2, y = pred), color = 'blue') +
    geom_line(aes(x = ym2, y = vip_prop)) +
    scale_y_continuous(limits = c(0, 0.1), labels = scales::percent)


# ym1 (날짜형 데이터 사용시)

mkt.fit3 <- lm(vip_prop ~ ym1, data = df2017)
summary(mkt.fit3)
coef(mkt.fit3)

df_other$pred <- predict(mkt.fit3, newdata = df_other)

df_other

ggplot(df_other) +
    geom_line(aes(x = ym1, y = pred), color = 'blue') +
    geom_line(aes(x = ym1, y = vip_prop))

### ====== 2017년부터 2018년도까지 데이터로 만든 모델

## ym2 (numeric)
mkt.fit2 <- lm(vip_prop ~ ym2, data = df2018)
summary(mkt.fit2)
coef(mkt.fit2)

predict(mkt.fit2, newdata = df_other)
df_other$pred <- predict(mkt.fit2, newdata = df_other)
tail(df_other, 10)

ggplot(df_other) +
    geom_line(aes(x = ym2, y = pred), color = 'blue') +
    geom_line(aes(x = ym2, y = vip_prop)) +
    scale_y_continuous(limits = c(0, 0.1), labels = scales::percent)


# ym2 (날짜형 데이터 사용시)

mkt.fit3 <- lm(vip_prop ~ ym1, data = df2018)
summary(mkt.fit3)
coef(mkt.fit3)

df_other$pred <- predict(mkt.fit3, newdata = df_other)

df_other

ggplot(df_other) +
    geom_line(aes(x = ym1, y = pred), color = 'blue') +
    geom_line(aes(x = ym1, y = vip_prop))