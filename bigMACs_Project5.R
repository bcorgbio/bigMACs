library(tidyverse)
dat.f <- list.files(pattern = "couch|JustDance.csv") #find the files

dat.l <- list() #make an empty list

for(i in dat.f){
  met.dat<- unlist(strsplit(i,"_")) #split of the filename string
  who <- met.dat[1] #store the first element
  activity <- gsub(".csv","",met.dat[2]) #store second element, dropping CSV with gsub()
  dat.l[[i]]<- read_csv(i)%>%
    mutate(who=who,activity=activity) %>% #read, then store data with additional columns of metada in list as position unique to i
    group_by(who, activity) %>% 
    mutate(mean = mean(degC))
}
dat <- do.call(rbind,dat.l) #combine the list into table

dat%>%
  ggplot(aes(x=activity,y=degC))+geom_boxplot()+stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+facet_grid(.~who) #plot to see what we get

