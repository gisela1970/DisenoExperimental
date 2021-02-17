
df=read.csv("giselita.csv")
df 

str(df)
df$Tratamiento=factor(df$Tratamiento)

boxplot(Tiempo~Tratamiento,data=df,main="Comparar los tiempos de coccion con diferentes tratamientos")

modelo=aov(Tiempo~Tratamiento,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library(car)

leveneTest(df$Tiempo~df$Tratamiento)

plot(modelo$residuals)
abline(h=0)

plot(df$Tratamiento,modelo$residuals)
abline(h=0)

plot(modelo$fitted.values,modelo$residuals)
abline(h=0)


