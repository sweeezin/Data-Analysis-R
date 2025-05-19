library(tidyverse)
library(ggridges)
library(patchwork)
library(viridis)
library(hrbrthemes)
library(gapminder)
library(ggplot2)
iris.plot<-plot(ggplot(iris,aes(x=Species,y=Petal.Length,
                     fill=Species))
     +geom_violin()
     +geom_boxplot()
     #+ggtitle("Iris species vs. petal length")
     +ylab("Petal Length")
     )
#there are 15 different eye colors
#length(unique(starwars$eye_color))
plot(starwars %>%
  filter(eye_color %in% c("black","brown","blue","yellow")) %>%
    ggplot(aes(x=eye_color, fill = gender))
                      +geom_bar(stat = "count",
                                position="dodge",
                                
                                ))

plot(chickwts %>%
       group_by(feed)%>%
       mutate(weight_group = mean(weight))%>%
       ungroup()%>%
       mutate(feed=fct_reorder(feed, weight_group))%>%
     ggplot(aes(weight,
                y=feed,
                color = feed))+
       
     stat_summary(fun=mean,
                  geom = "point", size=5)+
       geom_jitter()+
       geom_vline(aes(xintercept=mean(weight)))+
       geom_segment(aes(y=feed, yend=feed,
                        x=mean(weight),
                        xend=weight_group))
)


plot(ggplot(lincoln_weather,
            aes(x=`Mean Temperature [F]`, y = Month, fill=after_stat(x)))
     +geom_density_ridges_gradient(scale = 5,)
     +scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
)
#height of curves tell how often a value occurs
plot(gapminder %>%
       filter(continent %in% c("Africa", "Europe") )%>%
       filter(gdpPercap < 30000)%>%
       ggplot(aes(x=gdpPercap,
                  y=lifeExp,
                  color=year))
     +geom_point()
     +facet_wrap(~continent))
#data collection
survey_data<-read_csv("surveydata.csv")
#i can't find the raw data set for my refrence but i have the visualizations, check files
plot(
     ggplot(survey_data,aes(x=popular, fill=popular))
     +facet_wrap(~gender)
     +geom_bar())
plot(
  ggplot(survey_data,aes(x=health.problem, fill=gender))
  +facet_wrap(~gender)
  +geom_bar())
plot(
  ggplot(survey_data,aes(x=drama, fill=gender))
  +facet_wrap(~gender)
  +geom_bar()
  +coord_flip())
plot(
  ggplot(survey_data,aes(x=age,y=cyberbully))
         +geom_point()
         )
#it seems like nobody thinks their social media use is a major problem
#
#random data generation
randomData <- data.frame(gender = character(), age = numeric(), use = character(), problem= character(),
                         h.problem = character(),drama = character(),bully = character())
for (i in 1:30){
  randomData[i,] <-c(sample(survey_data$gender,1),sample(survey_data$age,1),
                           sample(survey_data$use,1),sample(survey_data$popular,1),
                           sample(survey_data$health.problem,1), sample(survey_data$drama,1),
                           sample(survey_data$cyberbully,1))
}
print(randomData)
#p-value is how likely the data occurred by random chance
#a p value less than 0.05 means the null hypothesis is likely true
#maybe gender and getting overwhelmed by online drama have a connection?

