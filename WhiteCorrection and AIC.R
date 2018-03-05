install.packages("MASS")
library("foreign")
library("MASS")
library("lmtest")
h=read.spss("D:/Database.sav", to.data.frame=TRUE)
#Линейная модель
model<-lm(data=h, Спрос~Новые+Бесплатно+Год)
summary(model)
#Устойчивая к гетероскедастичности ковариационная матрица
whiteCorrection=vcovHC(model, type = "HC0")
coeftest(model, vcov = whiteCorrection)
#Тест Бреуша-Пагана
bptest(model)
#AIC
#5, потому что оцениваемые параметры это константа, 3 независимых переменных и дисперсия ошибки
2*(5-sum(log(dnorm(na.omit(model$residuals), mean = 0, sd=sqrt(var(model$residuals))))))
AIC(model)
#Полная модель
model2<-lm(data=h, Спрос~Новые+Бесплатно+Год+Детское)
model3<-lm(data=h, Спрос~Новые+Бесплатно)
#Проверим все на основании AIC и LR теста
lrtest(model, model2)
lrtest(model, model3)
AIC(model)-AIC(model2)
AIC(model)-AIC(model3)
waldtest(model, model2)
waldtest(model, model3)
#Проверьте, чтобы в model, model2 и model3 были одни и те же наблюдения!
length(na.omit(model$residuals))
length(na.omit(model2$residuals))
length(na.omit(model3$residuals))
