library(tidyverse)
library(ggforce)
library(ggthemes)
library(ggrepel)
###
dt <- read.csv("prem-lig.csv")
View(dt)

## 
# -- Transform

## Get Goals Scored - Recall that goals scored does not equal Goals For, since
## own goals are not considered
dt %>% group_by(Squad) %>% summarise(teamGls = sum(Gls)) -> team.goals.scored
View(team.goals.scored)
dt <- merge(x = dt, y = team.goals.scored, by = "Squad")[-1,]
View(dt)

## Get Total Assists
dt %>% group_by(Squad) %>% summarise(teamAssists = sum(Ast)) -> team.assists
View(team.goals.scored)
dt <- merge(x = dt, y = team.assists, by = "Squad")[-1,]
View(dt)

## Get Percentage of Team Goals Scores and Team Assists for each player
dt %>% mutate(
  percentage.of.goals = Gls/teamGls,
  percentage.of.assists = Ast/teamAssists
) -> dt

View(dt)

## Plot percentages against each other
ggplot(data = dt,
       aes(x = percentage.of.goals,
           y = percentage.of.assists))+ theme_light()+
  geom_point(alpha = 0.35)+
  xlab("Percentage of Squad's Goals Scored")+
  ylab("Percentage of Squad's Assists Created")+
  geom_text_repel(aes(label=ifelse((percentage.of.goals>=0.15 &
                                percentage.of.assists >= 0.15),
                             as.character(Player), ''), 
                      #color = dt$Squad
                      ),
            size = 2.75, color = "Red") 

