library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(scales)

Pew <- read.csv("SatsifactionPew.csv")
Pew$Net <- Pew$Satisfied-Pew$Dissatisfied
Pew$F <- ifelse(Pew$Net>0,"y","n")

ggplot(Pew, aes(x=reorder(Country,-Net), y=Net, fill=F)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual( values = c( "y"="cadetblue1", "n"="tomato" ), guide = FALSE ) +
  theme_ipsum_rc(grid="Y") +
  theme(axis.text.x = element_text(angle=90, vjust=0.3),
        axis.title.y = element_text(hjust=0.75, size=11),
        plot.title = element_text(size=18.5),
        axis.title.x = element_blank()) +
  geom_segment(aes(y = -Inf, 
                   x = Country, 
                   yend = ifelse(Net>0,0,Net), 
                   xend = Country), 
               color = "black",
               linetype = "longdash") +
  scale_y_continuous(breaks=c(-0.75,-0.5,-0.25,0,0.25), labels = scales::percent) +
  coord_cartesian(clip ='off') +
  labs(title = "Where Are People Satisfied With How Things Are Going? (2019)",
       subtitle = "Net Satisfaction = Percent Satisfied - Percent Unsatisfied",
       y = expression("More Unsatiafied" %<->% "More Satisfied"),
       caption="Source: Pew Research Global Attitudes & Trends Spring 2019") +
  geom_text(aes(label=abs(Net*100)), vjust=ifelse(Pew$Net>0,1,0), size=4.6)