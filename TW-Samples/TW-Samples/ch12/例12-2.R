x = c(100,200,300,450,600,800,1000)
y = c(253, 337,395,451,495,534,574)
lm.1 = lm(y ~ x)
lm.2 = lm(y ~ x + I(x^2))
lm.3 = lm(y ~ x + I(x^2) + I(x^3))
summary(lm.1)$coef
summary(lm.2)$coef
summary(lm.3)$coef
plot(x,y)
lines(x,fitted(lm.1),lty=1)
lines(x,fitted(lm.2),lty=2)
lines(x,fitted(lm.3),lty=3)
legend(700,400,c("直線","二次曲線","三次曲線"),lty=1:3)
summary(lm.1)$r.squared
summary(lm.2)$r.squared
summary(lm.3)$r.squared
