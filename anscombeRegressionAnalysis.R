data("anscombe")
View(anscombe)
mod1 = lm(y1~x1,data=anscombe)
mod2 = lm(y2~x2,data=anscombe)
mod3 = lm(y3~x3,data=anscombe)
mod4 = lm(y4~x4,data=anscombe)
summary(mod1)
# Y1 = 3 + 0.5X1, R^2 = 0.67
summary(mod2)
# Y2 = 3 + 0.5X2, R^2 = 0.67
summary(mod3)
# Y3 = 3 + 0.5X3, R^2 = 0.67
plot(mod1)
attach(anscombe)
plot(x1,y1)
plot(mod2)
plot(x2,y2)
plot(mod3)
plot(x3,y3)
plot(mod4)
plot(x4,y4) # Influential observation
