library(car)
library(tidyverse)
library(leaps)
mtcars
?mtcars


y<-mtcars$mpg
x<-data.matrix(mtcars[,c('cyl','disp','hp','drat','wt','qsec','vs','am','gear','carb')])
view(x)
bestmods<-leaps(x,y,nbest=1)
bestmods
min(bestmods$Cp)
bestmods$which[3,]

# 3 vars
reg1=lm(mpg~wt+qsec+vs,data=mtcars) 
summary(reg1)
vif(reg1)
mtcars %>% select(wt,qsec,vs) %>% cor()

# 2 vars
reg2=lm(mpg~wt+qsec,data=mtcars)
summary(reg2)
vif(reg2)
mtcars %>% select(wt,qsec) %>% cor()

# 4 vars
reg3=lm(mpg~wt+qsec+hp+vs,data=mtcars)
summary(reg3)
vif(reg3)
mtcars %>% select(wt,qsec,hp,vs) %>% cor()
