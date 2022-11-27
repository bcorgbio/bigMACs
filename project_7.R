library(ggplot2)
library(tidyverse)
library(vroom)
library(readr)
library(dplyr)
library(MuMIn)
sub <- list.files(path = "Project 8 data", recursive = TRUE,
                  pattern = ".csv", 
                  full.names = TRUE)

dat.l <- list() #empty list

for(i in sub){
  met.dat<- unlist(strsplit(i,"_"))
  group <- gsub("Project 8 data/", " ", met.dat[1])
  subject <- met.dat[2]
  ang <- as.numeric(met.dat[3])
  activity <- gsub(".csv","",met.dat[4])
  
  dat.l[[i]]<- read_delim(i, delim= " ", col_names = c("Reading","Force","Unit"))%>%
    mutate(Group=group,Subject=subject, Angle=ang, Activity = activity)
  
}
dat <- do.call(rbind,dat.l) #get data into one tibble
view(dat)

dat.1 <- arrange(dat, Group, Subject, Angle) %>% 
  filter(Angle != 22.5)
#group 7, subject 27, had a reading at angle 22.5, which is outside the range of angles we would like to analyze
view(dat.1)

dat.max_recording <- dat.1 %>% 
  group_by(Subject, Angle, Activity) %>% 
  summarize(max_recording = max(Force))
view(dat.max_recording)

dat.max_subject <- dat.1 %>% 
  group_by(Subject, Activity) %>% 
  summarize(max_subject = max(Force))
view(dat.max_subject)

dat.max_joined <- dat.max_subject %>% 
  left_join(dat.max_recording) %>% 
  group_by(Subject, Angle, Activity) %>% 
  mutate(max_norm = max_recording/max_subject)
view(dat.max_joined)

dat.max_joined %>% 
  ggplot(aes(Angle,max_norm,col=Activity))+geom_point()
view(dat.max_joined)


dat.class_means <- dat.max_joined %>% 
  group_by(Angle, Activity) %>% 
  mutate(class_mean = mean(max_norm))
view(dat.class_means)

dat.class_means %>% 
  ggplot(aes(x=Angle, y=class_mean, col = Activity))+geom_point() 




AICs <- dat.max_joined%>%
  group_by(Subject,Activity)%>%
  summarize(
    m2=AICc(lm(max_norm~poly(Angle,2))), #second order
    m3=AICc(lm(max_norm~poly(Angle,3))), #third order
    m4=AICc(lm(max_norm~poly(Angle,4))) #fourth order
  )%>%
  pivot_longer(m2:m4,names_to="model",values_to="AICc")%>%
  print()

x.pred<-seq(45,157.5,length.out=1000)
fits <- dat.max_joined%>%
  group_by(Subject,Activity)%>%
  summarize(
    m2=predict(lm(max_norm~poly(Angle,2)),newdata=data.frame(Angle=x.pred)), #second order
    m3=predict(lm(max_norm~poly(Angle,3)),newdata=data.frame(Angle=x.pred)), #third order
    m4=predict(lm(max_norm~poly(Angle,4)),newdata=data.frame(Angle=x.pred)) #fourth order
  )%>%
  pivot_longer(m2:m4,names_to="model")%>%
  group_by(Subject,Activity,model)%>%
  summarize(theta_max=x.pred[which.max(value)])%>%
  print()

best.models <- fits%>%
  left_join(AICs)%>%
  group_by(Subject,Activity)%>%
  mutate(best=AICc==min(AICc))%>%
  filter(best==TRUE)%>%
  dplyr::select(-best)%>%
  print()
view(best.models)
anova(lm(theta_max~Activity,best.models))

best.models%>%
  filter(Subject != 25) %>% 
  pivot_wider(id_cols=Subject,names_from = Activity,values_from=theta_max)%>%
  mutate(shift=fatigue-control)%>%
  ungroup()%>%
  summarise(mean.shift=mean(shift),se.shift=sd(shift)/sqrt(length(shift)))
#subject 25 only has a control reading without a paired fatigue reading, which caused issues in calculations.

