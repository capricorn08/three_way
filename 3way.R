# 1. CSV 파일 불러오기
shopping <- read.csv("color_onlineshopping.csv", header = TRUE)
colnames(shopping)

# 2. 독립변수 factor 지정
shopping$hue <- as.factor(shopping$hue)
shopping$bright <- as.factor(shopping$bright)
shopping$price <- as.factor(shopping$price)

# 3. 삼원 변량분석 실시
fit_all <- aov(quality ~ hue*bright*price, data = shopping)
summary(fit_all)

# 4. 주효과 분석
mytable(hue~quality, data = shopping)
mytable(bright~quality, data = shopping)
mytable(price~quality, data = shopping)

# 5. 독립변수 통제
library(dplyr)
hue1 <- shopping %>% filter(hue == 1)
hue2 <- shopping %>% filter(hue == 2)
bright1 <- shopping %>% filter(bright == 1)
bright2 <- shopping %>% filter(bright == 2)
price1 <- shopping %>% filter(price == 1)
price2 <- shopping %>% filter(price == 2)

# 하나의 독립변수를 통제한 이원변량분석을 실시한다.
# 주효과가 의미 있게 나올 경우, mytable을 이용하여 평균비교를 하였고
# 상호작용 효과가 의미 있게 나올 경우, contrast를 수행하였다.
# 각 그래프는 ggplot을 이용하였다.
###############################################################

# 6. hue=1일때 이원변량분석
hue1_aov <- aov(quality ~ bright*price, data = hue1)
summary(hue1_aov)
mytable(price~quality, data = hue1)
library(lsmeans)
ref <- lsmeans(hue1_aov, specs = c("bright","price"))
ref_df <- as.data.frame(summary(ref))
library(ggplot2)
pd <- position_dodge(0.1)
g4 <- ggplot(ref_df, aes(x=bright, y=lsmean, group=price, colour=price))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd)+geom_line(position=pd)+geom_point(position=pd)
print(g4)

###############################################################

# 7. hue=2일때 이원변량분석
hue2_aov <- aov(quality ~ bright*price, data = hue2)
summary(hue2_aov)
mytable(bright~quality, data = hue2)
library(lsmeans)
ref <- lsmeans(hue2_aov, specs = c("bright","price"))
summary(ref)
ref_df <- as.data.frame(summary(ref))
library(ggplot2)
pd <- position_dodge(0.1)
g4 <- ggplot(ref_df, aes(x=bright, y=lsmean, group=price, colour=price))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd)+geom_line(position=pd)+geom_point(position=pd)
print(g4)
hue2$bright <- as.numeric(hue2$bright)
hue2$price <- as.numeric(hue2$price)
hue2$bright_price <- as.factor(10*hue2$bright + hue2$price)
interaction <- aov(quality ~ bright_price, data = hue2)
summary(interaction)
test1 <- c(1, 0, -1, 0)
test2 <- c(0, 1, 0, -1)
summary(contrast(ref, list(bright1_price = test1, bright2_price = test2)))

###############################################################

# 8. bright=1일때 이원변량분석
bright1_aov <- aov(quality ~ hue*price, data = bright1)
summary(bright1_aov)
mytable(price~quality, data = bright1)
library(lsmeans)
ref <- lsmeans(bright1_aov, specs = c("hue","price"))
ref_df <- as.data.frame(summary(ref))
library(ggplot2)
pd <- position_dodge(0.1)
g4 <- ggplot(ref_df, aes(x=hue, y=lsmean, group=price, colour=price))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd)+geom_line(position=pd)+geom_point(position=pd)
print(g4)

###############################################################

# 9. bright=2일때 이원변량분석
bright2_aov <- aov(quality ~ hue*price, data = bright2)
summary(bright2_aov)
mytable(hue~quality, data = bright2)
library(lsmeans)
ref <- lsmeans(bright2_aov, specs = c("hue","price"))
ref_df <- as.data.frame(summary(ref))
library(ggplot2)
pd <- position_dodge(0.1)
g4 <- ggplot(ref_df, aes(x=hue, y=lsmean, group=price, colour=price))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd)+geom_line(position=pd)+geom_point(position=pd)
print(g4)
bright2$hue <- as.numeric(bright2$hue)
bright2$price <- as.numeric(bright2$price)
bright2$hue_price <- as.factor(10*bright2$hue + bright2$price)
interaction <- aov(quality ~ hue_price, data = bright2)
summary(interaction)
test1 <- c(1, 0, -1, 0)
test2 <- c(0, 1, 0, -1)
summary(contrast(ref, list(hue1_price = test1, hue2_price = test2)))

###############################################################

# 10. price=1일때 이원변량분석
price1_aov <- aov(quality ~ hue*bright, data = price1)
summary(price1_aov)
mytable(hue~quality, data = price1)
library(lsmeans)
ref <- lsmeans(price1_aov, specs = c("hue","bright"))
ref_df <- as.data.frame(summary(ref))
library(ggplot2)
pd <- position_dodge(0.1)
g4 <- ggplot(ref_df, aes(x=hue, y=lsmean, group=bright, colour=bright))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd)+geom_line(position=pd)+geom_point(position=pd)
print(g4)

###############################################################

# 11. price=2일때 이원변량분석
price2_aov <- aov(quality ~ hue*bright, data = price2)
summary(price2_aov)
mytable(hue~quality, data = price2)
mytable(bright~quality, data = price2)
library(lsmeans)
ref <- lsmeans(price2_aov, specs = c("hue","bright"))
ref_df <- as.data.frame(summary(ref))
library(ggplot2)
pd <- position_dodge(0.1)
g4 <- ggplot(ref_df, aes(x=hue, y=lsmean, group=bright, colour=bright))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd)+geom_line(position=pd)+geom_point(position=pd)
print(g4)
price2$hue <- as.numeric(price2$hue)
price2$bright <- as.numeric(price2$bright)
price2$hue_bright <- as.factor(10*price2$hue + price2$bright)
interaction <- aov(quality ~ hue_bright, data = price2)
summary(interaction)
test1 <- c(1, 0, -1, 0)
test2 <- c(0, 1, 0, -1)
summary(contrast(ref, list(hue1_bright = test1, hue2_bright = test2)))