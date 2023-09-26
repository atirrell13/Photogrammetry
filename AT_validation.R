

#load packages
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(agricolae) #You need this for the Tukey test
library(EnvStats) #rosnerTest
library(ggdist)
library(gghalves)
library(ggridges)

#load data file----
data <- read_csv("Photogram_complete.csv")

#add column of simplified site names
#BUT this is based on position so if you change someing in the origianl dataframe you will have to 
#double check the alignment 
data1 <- data
data1$'2D_measured' <- NULL
data1$site_simple <- c("Guyot","Guyot","Guyot","Guyot","Guyot","Guyot","Guyot",
                       "Guyot","Guyot","Guyot","Guyot","Guyot","Guyot","Guyot",
                       "Abraham","Abraham","Abraham","Abraham","Abraham",
                       "Mansfield","Mansfield","Mansfield","Mansfield","Mansfield","Mansfield",
                       "Adams","Adams","Adams","Adams","Adams","Adams","Adams",
                       "Bigelow","Bigelow","Bigelow","Bigelow","Bigelow","Bigelow",
                       "Lafayette","Lafayette","Lafayette","Lafayette","Lafayette",
                       "Lafayette","Lafayette","Lafayette","Lafayette","Lafayette",
                       "Adams","Adams","Adams","Adams","Adams","Adams")
#add in species abreviations
data1$spec_abrev <-  c("Dila", "Vaul", "Rock", "Grav","Bgr","Vaul","Pisp","Dila",
                       "Emni","Rock","Dewo","Migr","Vavi","Grav","Rhgr","Vavi",
                       "Rhla","Vaul","Rock","Bgr","Migr","Rock","Cabi","Jutr",
                       "Vaul","Jutr","Sitr","Bgr","Cabi","Rock","Vavi","Migr",
                       "Besp","Absp","Rock","Jutr","Vaul","Sitr","Dila","Vavi",
                       "Vaul","Cabi","Grav","Rock", "Bgr","Sitr","Migr","Husp",
                       "Emni","Vavi","Rock","Vaul","Cabi","Jutr")

#group things by specis abreviations
longdata <- data %>% 
  select(!c("Species", "Site"))%>%
  gather(key=ID, value=value, Field_measured:Lab_measured, 
         factor_key = TRUE) #pivoting data to long format for figure
longdata %>%
  ggplot(aes(x=ID, y=value, fill=ID)) +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")


#plotting a raincloud plot to show the variance in the data points on the raw data
labels <- ("Location")
longdata$location <- "Location"

data_rain <- ggplot(longdata, aes(x = value, y = location, fill= ID, color = ID))+
  ggdist::stat_halfeye(adjust = 1, # this changes the intervals used for calculating the density plot (bigger=smoother)
                       justification = -0.25,
                       .width = 0,
                       width = 0.8,
                       alpha = 0.85,
                       point_colour = NA,
                       trim=FALSE, # whether to trim to data range-- looks nicer without trimming, but can be misleading
                       normalize="xy", # this normalizes each line, rather than having them all on the same scale
                       scale=0.6) + # change to adjust height (scale=1 means that they fill the whole line height)
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width=0.001, height = 0.1
    )
  ) +
  geom_boxplot(width = 0.3,
               outlier.colour = NA,
               alpha = 0.25,
               position = position_dodge(width=0)) + # this makes the boxplots overlap-- remove if you'd rather they be side-by-side :)
  theme_classic()+
  theme(axis.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=16)) +
  coord_cartesian(xlim=c(-10,80),clip="off") +
  #scale_y_discrete(labels= labels)+
  scale_fill_manual(values=c("#94D2BD","#BB3E03"))+
  scale_color_manual(values=c("#94D2BD","#BB3E03"))+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=16)) +
  #theme(legend.position = "none")+
  xlab("Percent (%) Cover")+
  ylab("")
data_rain

#analysis of varience 
lm <- lm(Field_measured ~ Lab_measured, data = data)
summary(lm)
av <-  aov(lm)
summarise(av)
tukey <-  HSD.test(av, "Lab_measured")
tukey
TukeyHSD(av)

all.lm <- lm(longdata2$value[ID == "perc.dam"]~ longdata2$time[ID == "perc.dam"], data = longdata2)
summary(all.lm)
all.av <- aov(all.lm)
summary(all.av)
total_test <- HSD.test(all.av, 'longdata2$time[ID == "perc.dam"]')
total_test
TukeyHSD(all.av)



