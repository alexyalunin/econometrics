install.packages("MASS")
library("foreign")
library("MASS")
library("lmtest")
h=read.spss("D:/Database.sav", to.data.frame=TRUE)
#�������� ������
model<-lm(data=h, �����~�����+���������+���)
summary(model)
#���������� � �������������������� �������������� �������
whiteCorrection=vcovHC(model, type = "HC0")
coeftest(model, vcov = whiteCorrection)
#���� ������-������
bptest(model)
#AIC
#5, ������ ��� ����������� ��������� ��� ���������, 3 ����������� ���������� � ��������� ������
2*(5-sum(log(dnorm(na.omit(model$residuals), mean = 0, sd=sqrt(var(model$residuals))))))
AIC(model)
#������ ������
model2<-lm(data=h, �����~�����+���������+���+�������)
model3<-lm(data=h, �����~�����+���������)
#�������� ��� �� ��������� AIC � LR �����
lrtest(model, model2)
lrtest(model, model3)
AIC(model)-AIC(model2)
AIC(model)-AIC(model3)
waldtest(model, model2)
waldtest(model, model3)
#���������, ����� � model, model2 � model3 ���� ���� � �� �� ����������!
length(na.omit(model$residuals))
length(na.omit(model2$residuals))
length(na.omit(model3$residuals))
