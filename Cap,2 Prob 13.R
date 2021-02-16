
x=c(28.3,26.8,26.6,26.5,28.1,24.8,27.4,26.2,29.4,28.6,24.9,25.2,30.4,27.7,27.0,26.1,28.1,26.9,28.0,27.6,25.6,29.5,27.6,27.3,26.2,27.7,27.2,25.9,26.5,28.3,26.5,29.1,23.7,29.7,26.8,29.5,28.4,26.3,28.1,28.7,27.0,25.5,26.9,27.2,27.6,25.5,28.3,27.4,28.8,25.0,25.3,27.7,25.2,28.6,27.9,28.7)
n=length(x)
media=mean(x)
std=sd(x)

hist(x,freq=FALSE,breaks=10)
lines(density(x),col="red",lwd=2)
media
std
sd(x)

tmin=qt(0.025,n-1)
tmax=qt(0.975,n-1)
tmin
tmax

media+tmin*std/sqrt(n)
media+tmax*std/sqrt(n)
tmax*std/sqrt

df=read.csv("cap3-2.csv")
df

str(df)
df$Tratamiento=factor(df$Tratamiento)
boxplot(tiempo~tratamiento,data=df,main="Comparacion de los tratamientos")
modelo=aov(tiempo)~tratamiento,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library(car)

leveneTest(df$tiempo~df$tratamiento)

plot(modelo$residuals)
abline(h=0)

plot(df$Tratamiento,modelo$residuals)
abline(h=0)

plot(modelo$fitted,values,modelo$residuals)
abline(h=0
       