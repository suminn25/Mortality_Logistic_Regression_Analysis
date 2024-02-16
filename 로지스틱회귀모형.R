library(tidyverse)
library(readxl)
library(MASS)
library(GGally)
library(pROC)
#library(smotefamily)
library(car)
library(caret)
library(bestglm)
heart <- read_excel("C:/Users/sum/OneDrive/바탕 화면/3-1/통계자료분석실습/분석데이터1/heart_failure.xlsx")
heart %>% print(n=10)
str(heart)
heart <- heart %>% rename(creat='creatinine_phosphokinase', eject='ejection_fraction',
                 blood='high_blood_pressure', s_creat='serum_creatinine',
                 s_sodium='serum_sodium', death='DEATH_EVENT')%>%
  mutate(across(c(anaemia, diabetes, blood, sex, smoking, death), factor))
str(heart)
heart %>% ggpairs(aes(color=death))
h1 <- mutate(heart, creat=log(creat), s_creat=log(s_creat))
h1 %>% ggpairs(aes(color=death))
#mean(heart$platelets==0)
#이상값으로 그래프가 좌우대칭을 이루지 않는 것으로 보아 크게 영향x
set.seed(1234)
x.id <- createDataPartition(h1$death, p=0.7,
                            list=FALSE)
train_h <- h1 %>% slice(x.id)
test_h <- h1 %>% slice(-x.id)
fit_full <- glm(death ~ ., family = binomial, train_h)
fit_null <- glm(death ~ 1, family = binomial, train_h)
fit1 <- stepAIC(fit_full, direction = "both", trace=FALSE)
fit2 <- stepAIC(fit_null,
                scope = list(upper=fit_full, lower=fit_null),
                trace=FALSE)
fit3 <- stepAIC(fit_full, direction="both",
                trace=FALSE, k=log(nrow(train_h)))
fit4 <- stepAIC(fit_null,
                scop= list(upper=fit_full, lower=fit_null),
                trace=FALSE, k=log(nrow(train_h)))
Xy <- train_h %>% as.data.frame()
fit5 <- bestglm(Xy, family=binomial)$BestModel
fit6 <- bestglm(Xy, family=binomial, IC="AIC")$BestModel
names(fit1$model)[-1] %>% sort() #[-1];intercept 제외
names(fit2$model)[-1] %>% sort()
names(fit3$model)[-1] %>% sort()
names(fit4$model)[-1] %>% sort()
names(fit5$model)[-1] %>% sort()
names(fit6$model)[-1] %>% sort()
residualPlots(fit1)
residualPlots(fit3)
vif(fit1)
vif(fit3)
vif(fit1.1)
fit1.1 <- update(fit1, .~.+I(eject^2)+I(time^2))
fit3.1 <- update(fit3, .~.+I(time^2))
fit3.2 <- update(fit3, .~.+I(eject^2)+I(time^2))
fit3.3 <- update(fit3, .~.+I(eject^2)+I(time^2)+I(s_creat^2))
summary(fit1.1)
summary(fit3.1)
summary(fit3.2)
summary(fit3.3)
fit1.2 <- update(fit1, .~.-diabetes +I(eject^2)+I(time^2))
summary(fit1.2)
AIC(fit1, fit1.1, fit1.2)
BIC(fit1, fit1.1, fit1.2)
confusionMatrix(data=factor((fit1.1$fitted>0.5)*1),
                ref=train_h$death, positive="1")
confusionMatrix(data=factor((fit1.2$fitted>0.5)*1),
                ref=train_h$death, positive="1")

auc(roc(train_h$death, fit1.1$fitted))
auc(roc(train_h$death, fit1.2$fitted))

f1.1 <- confusionMatrix(data=factor((fit1.1$fitted>0.5)*1),
                        ref=train_h$death, positive="1")
f1.2 <- confusionMatrix(data=factor((fit1.2$fitted>0.5)*1),
                        ref=train_h$death, positive="1")

f1.1$table
f1.1$overall[1]
f1.1$byClass[c(1, 2, 5, 7)]
f1.2$table
f1.2$overall[1]
f1.2$byClass[c(1, 2, 5, 7)]

pred <- predict(fit1.1, newdata=test_h,
               type="response")
ypred <- factor((pred>=0.5)*1)
confusionMatrix(data=ypred, ref=test_h$death,
                positive="1")
heart %>%
  count(death) %>% mutate(p = n/sum(n))
set.seed(1234)
train_up <- upSample(x = dplyr::select(train_h, -death),
                     y = train_h$death, yname='death')
fit_up <- update(fit1.1, .~., data=train_up)
pred_up <- predict(fit_up, newdata = test_h, type= "response")
ypred_up <- factor((pred_up >= 0.5)*1)
confusionMatrix(data= ypred_up, ref = test_h$death,
                positive = "1", mode = "everything")
set.seed(1234)
x.id <- createDataPartition(test_h$death, p=0.5, list=FALSE)
valid_h <- test_h %>% slice(x.id)
test_vh <- test_h %>% slice(-x.id)
pred_val <- predict(fit1.1, newdata=valid_h, type="response")
thres_val <- coords(roc(valid_h$death, pred_val),
                    x="best", transpose=TRUE)
thres_val
pred_t <- predict(fit1.1, newdata=test_vh, type="response")
ypred_t <- factor((pred_t>=thres_val[1])*1)
confusionMatrix(data = ypred_t, ref = test_vh$death,
                positive = "1", mode = "everything")
