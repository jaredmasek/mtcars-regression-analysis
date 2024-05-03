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

#Looking at the mtcars data set within R, a linear regression for mpg is found using the leaps method.
#It initially finds a 3 variable regression using weight, 1/4 mile time, and engine type.
#Upon further inspection, there is collinearity between 1/4 mile time and engine type, meaning both are not necessary.
#1/4 mile time has a higher correlation with weight, so it was the variable taken out of the model.
#Finding a regression with only weight and 1/4 mile time leads to a model with very little collinearity, and a higher R-squared adjusted
#Value than the previous model. To further test models, the best 4-variable model was tested but similar issues arose with collinearity.
#Therefore the 2-variable model seems best, since in practical terms would require less resources while also having better predicting accuracy.