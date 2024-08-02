install.packages('tidyverse')
install.packages('ggpubr')
install.packages('ggplot2')
library(tidyverse)
library(ggplot2)
library(dplyr)
library(BAS)
library(ggpubr)
movies<-get(load("C:/Users/ravit/Downloads/movies.Rdata"))
movies=movies %>% mutate(feature_film=as.factor(if_else(title_type=="Feature Film","yes","no")))
movies=movies %>% mutate(drama=as.factor(if_else(genre=="Drama","yes","no")))
movies=movies %>% mutate(mpaa_rating_R=as.factor(if_else(mpaa_rating=="R","yes","no")))
movies=movies %>% mutate(oscar_season=as.factor(if_else(thtr_rel_month %in% c(10,11,12),"yes","no")))
movies=movies %>% mutate(summer_season=as.factor(if_else(thtr_rel_month %in% c(5,6,7,8),"yes","no")))
movies_eda=movies %>% select(audience_score,feature_film,drama,mpaa_rating_R,oscar_season,summer_season)
summary(movies_eda)
ggplot(movies_eda)+geom_histogram(aes(x=audience_score),binwidth=5,color="black")+labs(title="Audience Score Distribution")+theme_classic()
ggplot(movies_eda)+geom_boxplot(aes(x=feature_film,y=audience_score,fill=feature_film))+
  labs(title="Audience Score Distribution for Feature & Non-feature Film")+theme_classic()
movies_eda %>% group_by(feature_film)%>% select(audience_score) %>% summarize(count=n(),mean=mean(audience_score),median=median(audience_score),min=min(audience_score),max=max(audience_score))
ggplot(movies_eda)+geom_boxplot(aes(x=drama,y=audience_score,fill=drama))+
  labs(title="Audience Score Distribution for Drama & Non-Drama")+theme_classic()
movies_eda %>% group_by(drama)%>% select(audience_score) %>% summarize(count=n(),mean=mean(audience_score),median=median(audience_score),min=min(audience_score),max=max(audience_score))
ggplot(movies_eda)+geom_boxplot(aes(x=mpaa_rating_R,y=audience_score,fill=mpaa_rating_R))+
  labs(title="Audience Score Distribution for Restricted & Non-Restricted")+theme_classic()
movies_eda %>% group_by(mpaa_rating_R)%>% select(audience_score) %>% summarize(count=n(),mean=mean(audience_score),median=median(audience_score),min=min(audience_score),max=max(audience_score))
ggplot(movies_eda)+geom_boxplot(aes(x=oscar_season,y=audience_score,fill=oscar_season))+
  labs(title="Audience Score Distribution for Oscar & Non-Oscar Season")+theme_classic()
movies_eda %>% group_by(oscar_season)%>% select(audience_score) %>% summarize(count=n(),mean=mean(audience_score),median=median(audience_score),min=min(audience_score),max=max(audience_score))
ggplot(movies_eda)+geom_boxplot(aes(x=summer_season,y=audience_score,fill=summer_season))+
  labs(title="Audience Score Distribution for Summer & Non-Summer Season")+theme_classic()
movies_eda %>% group_by(summer_season)%>% select(audience_score) %>% summarize(count=n(),mean=mean(audience_score),median=median(audience_score),min=min(audience_score),max=max(audience_score))
variable_list=c("feature_film","drama","runtime","mpaa_rating_R","thtr_rel_year","oscar_season","summer_season","imdb_rating","imdb_num_votes","critics_score","best_pic_nom","best_pic_win","best_actor_win","best_actress_win","best_dir_win","top200_box")

summary(movies %>% select(variable_list))
movies_model=movies %>% select(audience_score,variable_list) %>% drop_na()
bma_model=bas.lm(audience_score~.,prior="BIC", modelprior= uniform(), data=movies_model)
plot(bma_model,1,add.smooth=TRUE)
plot(bma_model,2,add.smooth=TRUE)
plot(bma_model,3)
plot(bma_model,4)
summary(bma_model)
model_cog
coef_movies_bicprior=coefficients(bma_model, estimator="BMA")
plot(coef_movies_bicprior)
BPM_movies=predict(bma_model,estimator = "BPM")
variable.names(BPM_movies)
HPM_movies=predict(bma_model,estimator = "HPM")
variable.names(HPM_movies)
image(bma_model, rotate=F)
imdb_plot=ggplot(movies_model,aes(x=imdb_rating,y=audience_score))+geom_jitter()+labs(title="IMDB Rating vs Audience Score")+geom_smooth(method = lm, se = FALSE,formula=y~x)+theme_classic()
crit_plot=ggplot(movies_model,aes(x=critics_score,y=audience_score))+geom_jitter()+labs(title="Critics Score vs Audience Score")+geom_smooth(method = lm, se = FALSE,formula=y~x)+theme_classic()
rt_plot=ggplot(movies_model,aes(x=runtime,y=audience_score))+geom_jitter()+labs(title="Run-Time vs Audience Score")+geom_smooth(method = lm, se = FALSE,formula=y~x)+theme_classic()
ggarrange(imdb_plot,crit_plot,rt_plot,ncol=3,nrow = 1)
data.frame("imdb_rating"=c(cor(movies_model$imdb_rating,movies_model$audience_score)),
           "critics_score"=c(cor(movies_model$critics_score,movies_model$audience_score)),
           "runtime"=c(cor(movies_model$runtime,movies_model$audience_score)))
confint(coef(bma_model))
dp2_df=data.frame(runtime =119,
                  thtr_rel_year = 2018,
                  imdb_rating =7.7,
                  imdb_num_votes =551,253,
                  critics_score =84,
                  best_pic_nom = "no",
                  best_pic_win = "no",
                  best_actor_win = "no",
                  best_actress_win = "no",
                  best_dir_win = "no",
                  top200_box = "no",
                  feature_film = "yes",
                  drama = "yes",
                  mpaa_rating_R = "yes",
                  oscar_season = "no",
                  summer_season = "yes")

predict_result=predict(bma_model, newdata =dp2_df, estimator = "BMA",se.fit=TRUE)
confint(predict_result)
bvs_df=data.frame(runtime =152,
                  thtr_rel_year = 2016,
                  imdb_rating =6.4,
                  imdb_num_votes =685,249,
                  critics_score =29,
                  best_pic_nom = "no",
                  best_pic_win = "no",
                  best_actor_win = "no",
                  best_actress_win = "no",
                  best_dir_win = "no",
                  top200_box = "no",
                  feature_film = "yes",
                  drama = "yes",
                  mpaa_rating_R = "no",
                  oscar_season = "no",
                  summer_season = "no")

predict_result=predict(bma_model, newdata =bvs_df, estimator ="BMA",se.fit=TRUE)
confint(predict_result)
