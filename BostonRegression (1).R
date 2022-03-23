library(tidyverse)


Lab=TRUE

if (!require(mlbench)) {
  if (Lab){
    .libPaths(new=c("C:/Program Files/R/Libs",Sys.getenv("R_User")))
    install.packages("mlbench", lib=Sys.getenv("R_User"))
  } else {
    install.packages("mlbench")
  }  
}
library(mlbench)

if (!require(ggcorrplot)) {
  if (Lab){
    .libPaths(new=c("C:/Program Files/R/Libs",Sys.getenv("R_User")))
    install.packages("ggcorrplot", lib=Sys.getenv("R_User"))
  } else {
    install.packages("ggcorrplot")
  }  
}
library(ggcorrplot)

data("BostonHousing2")
View(BostonHousing2)

DataCl=as_tibble(BostonHousing2[,-c(1:4)]) %>% 
  mutate(chas=as.numeric(chas)-1)

ggplot(DataCl,aes(x=lstat,y=cmedv))+
  geom_point()
last_plot()+
  geom_smooth(method = lm, se=FALSE, level=0.95)

lm(data=DataCl, cmedv~lstat)
ModelLm1Var=lm(data=DataCl, cmedv~lstat)
summary(ModelLm1Var)

ModelLmWithDummy=lm(data=DataCl, cmedv~lstat+chas)
summary(ModelLm1Var)

ggcorrplot(cor(DataCl), hc.order = FALSE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("blue", "white", "red"),
           ggtheme=theme_bw)
View(DataCl)
?BostonHousing2

DataCl[!complete.cases(DataCl),]

summary(BostonHousing$medv)



##
## model linear regression
##
DataCl=DataCl %>% select(-medv)
ModelLm <- lm(cmedv ~ ., data=DataCl)

LmPredict <- predict(ModelLm)

# mean squared error:
mean((LmPredict - DataCl$cmedv)^2) 

# Plotting the Prediction/True Value Diagram
# for linear model
ResultDataLm=cbind(DataCl, LmPredict) %>% 
  select(cmedv,LmPredict)

ggplot(ResultDataLm, aes(x=cmedv,y=LmPredict))+
  geom_point()+
  geom_abline(size=1, color="red")


