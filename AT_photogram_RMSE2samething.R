#Running RMSE for AT data
#02.23.2023
#R version: 4.2.2 

#Packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("Metrics")
library(Metrics)
install.packages("ggpmisc")
library(ggpmisc)

#Load data
data <- read.csv("/Users/owner/Desktop/AT_photo_data.csv") #load in data

data1 <-  data #replicating data so that I can add species abbreviations and manipluate data later
data1$spec_abrev <-  c("Dila", "Vaul", "Rock", "Grav","Bgr","Vaul","Pisp","Dila", #adding species abreviations for subsetting later
                       "Emni","Rock","Dewo","Migr","Vavi","Grav","Rhgr","Vavi",
                       "Rhla","Vaul","Rock","Bgr","Migr","Rock","Cabi","Jutr",
                       "Vaul","Jutr","Sitr","Bgr","Cabi","Rock","Vavi","Migr",
                       "Besp","Absp","Rock","Jutr","Vaul","Sitr","Dila","Vavi",
                       "Vaul","Cabi","Grav","Rock", "Bgr","Sitr","Migr","Husp",
                       "Emni","Vavi","Rock","Vaul","Cabi","Jutr")

#Clean data
data_clean <- data %>%
  select(!c("Site", "Species")) %>% #removing site and species columns
  rename(c(actual = "Field_measured", predicted = "Lab_measured")) %>% #renaming the columns
  drop_na() #removing NAs

# spec.data_clean <- data1 %>% #cleaning the data but keeping species so we can maybe subset by species in the RMSE model
#   select(!c("Site", "Species")) %>% #removing site and species columns
#   rename(c(actual = "Field_measured", predicted = "Lab_measured")) %>% #renaming the columns
#   group_by(spec_abrev) %>% #groups by the species abreviation
#   summarise(across(c("actual", "predicted"), ~ sum (.x, na.rm = TRUE))) #gives the sum for each species actual and predicted values

spec.data_clean2 <- data1 %>% #cleaning the data but keeping species so we can maybe subset by species in the RMSE model
  select(!c("Site", "Species")) %>% #removing site and species
  rename(c(actual = "Field_measured", predicted = "Lab_measured"))

#RMSE on RAW data. 
#I'm commenting this out because I'm not sure that is something we need to do but I did it incase
#using base R without a package
# sqrt(mean((data_clean$actual - data_clean$predicted)^2))
# 
# #using the package Metrics
# rmse(data_clean$actual, data_clean$predicted)

#################################################################################
######Field/3D analyses####
################################################################################
#lm on cleaned data
mod1 <-  lm(actual ~ predicted, data = data_clean)
summary(mod1)
AIC(mod1) #377.95

#calculating RMSE from model residuals using base
sqrt(mean(mod1$residuals^2)) #This is taking the square root of the mean for the residuals^2

#calculating RMSE from model residuals using Metrics package
rmse(data_clean$actual, predict(mod1, data_clean)) #same thing as above

#can we break it out by species?
# == means take exactly this "value" or "character"
# != means take all values/character besides what you're calling
modabsp <-  lm(actual[spec_abrev == "Absp"] ~ predicted[spec_abrev == "Absp"], data = spec.data_clean2)
summary(modabsp)
sqrt(mean(modabsp$residuals^2)) #no enough data

modbesp <-  lm(actual[spec_abrev == "Besp"] ~ predicted[spec_abrev == "Besp"], data = spec.data_clean2)
summary(modbesp)
sqrt(mean(modbesp$residuals^2)) #not enough data

modbgr <-  lm(actual[spec_abrev == "Bgr"] ~ predicted[spec_abrev == "Bgr"], data = spec.data_clean2)
summary(modbgr)
sqrt(mean(modbgr$residuals^2)) #2.72

modcabi <-  lm(actual[spec_abrev == "Cabi"] ~ predicted[spec_abrev == "Cabi"], data = spec.data_clean2)
summary(modcabi)
sqrt(mean(modcabi$residuals^2)) #2.25

modDewo <-  lm(actual[spec_abrev == "Dewo"] ~ predicted[spec_abrev == "Dewo"], data = spec.data_clean2)
summary(modDewo)
sqrt(mean(modDewo$residuals^2)) #not enough data

moddila <-  lm(actual[spec_abrev == "Dila"] ~ predicted[spec_abrev == "Dila"], data = spec.data_clean2)
summary(moddila)
sqrt(mean(moddila$residuals^2)) #6.19

emni <- spec.data_clean2 %>%
  filter(spec_abrev =="Emni") 
modEmni <-  lm(actual ~ predicted, data = emni)
#modEmni <-  lm(actual[spec_abrev == "Emni"] ~ predicted[spec_abrev == "Emni"], data = spec.data_clean2)
summary(modEmni) 
sqrt(mean(modEmni$residuals^2)) #not enough data

modGrav <-  lm(actual[spec_abrev == "Grav"] ~ predicted[spec_abrev == "Grav"], data = spec.data_clean2)
summary(modGrav) #almost significant
sqrt(mean(modGrav$residuals^2)) #0.82

modHusp <-  lm(actual[spec_abrev == "Husp"] ~ predicted[spec_abrev == "Husp"], data = spec.data_clean2)
summary(modHusp)
sqrt(mean(modHusp$residuals^2)) #not enough data

modJutr <-  lm(actual[spec_abrev == "Jutr"] ~ predicted[spec_abrev == "Jutr"], data = spec.data_clean2)
summary(modJutr) #significant
sqrt(mean(modJutr$residuals^2)) #2.52

modMigr <-  lm(actual[spec_abrev == "Migr"] ~ predicted[spec_abrev == "Migr"], data = spec.data_clean2)
summary(modMigr)
sqrt(mean(modMigr$residuals^2)) #0.78

modPisp <-  lm(actual[spec_abrev == "Pisp"] ~ predicted[spec_abrev == "Pisp"], data = spec.data_clean2)
summary(modPisp)
sqrt(mean(modPisp$residuals^2)) #not enough data

modRhgr <-  lm(actual[spec_abrev == "Rhgr"] ~ predicted[spec_abrev == "Rhgr"], data = spec.data_clean2)
summary(modRhgr)
sqrt(mean(modRhgr$residuals^2)) #not enough data
#rare species

modRhla <-  lm(actual[spec_abrev == "Rhla"] ~ predicted[spec_abrev == "Rhla"], data = spec.data_clean2)
summary(modRhla)
sqrt(mean(modRhla$residuals^2)) #not enough data
#rare species

modRock <-  lm(actual[spec_abrev == "Rock"] ~ predicted[spec_abrev == "Rock"], data = spec.data_clean2)
summary(modRock) #significant
sqrt(mean(modRock$residuals^2)) #12.61

modSitr <-  lm(actual[spec_abrev == "Sitr"] ~ predicted[spec_abrev == "Sitr"], data = spec.data_clean2)
summary(modSitr)
sqrt(mean(modSitr$residuals^2)) #0.47

modVaul <-  lm(actual[spec_abrev == "Vaul"] ~ predicted[spec_abrev == "Vaul"], data = spec.data_clean2)
summary(modVaul) #significant
sqrt(mean(modVaul$residuals^2)) #5.38

modVavi <-  lm(actual[spec_abrev == "Vavi"] ~ predicted[spec_abrev == "Vavi"], data = spec.data_clean2)
summary(modVavi)
sqrt(mean(modVavi$residuals^2)) #3.31

#################################################################################
######Field/2D analyses####

data_clean2 <- data %>%
  select(!c("Site", "Species")) %>%
  rename(c(actual = "Field_measured", predicted = "X2D_measured")) %>%
  drop_na() 

spec.data_clean3 <- data1 %>% #cleaning the data but keeping species so we can maybe subset by species in the RMSE model
  select(!c("Site", "Species")) %>% #removing site and species
  rename(c(actual = "Field_measured", predicted = "X2D_measured"))

modabsp <-  lm(actual[spec_abrev == "Absp"] ~ predicted[spec_abrev == "Absp"], data = spec.data_clean3)
summary(modabsp)
sqrt(mean(modabsp$residuals^2)) #no enough data

modbesp <-  lm(actual[spec_abrev == "Besp"] ~ predicted[spec_abrev == "Besp"], data = spec.data_clean3)
summary(modbesp)
sqrt(mean(modbesp$residuals^2)) #not enough data

modbgr <-  lm(actual[spec_abrev == "Bgr"] ~ predicted[spec_abrev == "Bgr"], data = spec.data_clean3)
summary(modbgr) # p=.38
sqrt(mean(modbgr$residuals^2)) #1.6

modcabi <-  lm(actual[spec_abrev == "Cabi"] ~ predicted[spec_abrev == "Cabi"], data = spec.data_clean3)
summary(modcabi) #p=.85
sqrt(mean(modcabi$residuals^2)) #2.27

modDewo <-  lm(actual[spec_abrev == "Dewo"] ~ predicted[spec_abrev == "Dewo"], data = spec.data_clean3)
summary(modDewo)
sqrt(mean(modDewo$residuals^2)) #not enough data

moddila <-  lm(actual[spec_abrev == "Dila"] ~ predicted[spec_abrev == "Dila"], data = spec.data_clean3)
summary(moddila) #p= .03
sqrt(mean(moddila$residuals^2)) #1.2

emni <- spec.data_clean2 %>%
  filter(spec_abrev =="Emni") 
modEmni <-  lm(actual ~ predicted, data = emni)
#modEmni <-  lm(actual[spec_abrev == "Emni"] ~ predicted[spec_abrev == "Emni"], data = spec.data_clean2)
summary(modEmni) 
sqrt(mean(modEmni$residuals^2)) #not enough data

modGrav <-  lm(actual[spec_abrev == "Grav"] ~ predicted[spec_abrev == "Grav"], data = spec.data_clean3)
summary(modGrav) #.11
sqrt(mean(modGrav$residuals^2)) #1.3

modHusp <-  lm(actual[spec_abrev == "Husp"] ~ predicted[spec_abrev == "Husp"], data = spec.data_clean3)
summary(modHusp)
sqrt(mean(modHusp$residuals^2)) #not enough data

modJutr <-  lm(actual[spec_abrev == "Jutr"] ~ predicted[spec_abrev == "Jutr"], data = spec.data_clean3)
summary(modJutr) #.03
sqrt(mean(modJutr$residuals^2)) #2.7

modMigr <-  lm(actual[spec_abrev == "Migr"] ~ predicted[spec_abrev == "Migr"], data = spec.data_clean3)
summary(modMigr) #p=.40
sqrt(mean(modMigr$residuals^2)) #0.95

modPisp <-  lm(actual[spec_abrev == "Pisp"] ~ predicted[spec_abrev == "Pisp"], data = spec.data_clean3)
summary(modPisp)
sqrt(mean(modPisp$residuals^2)) #not enough data

modRhgr <-  lm(actual[spec_abrev == "Rhgr"] ~ predicted[spec_abrev == "Rhgr"], data = spec.data_clean3)
summary(modRhgr)
sqrt(mean(modRhgr$residuals^2)) #not enough data
#rare species

modRhla <-  lm(actual[spec_abrev == "Rhla"] ~ predicted[spec_abrev == "Rhla"], data = spec.data_clean3)
summary(modRhla)
sqrt(mean(modRhla$residuals^2)) #not enough data
#rare species

modRock <-  lm(actual[spec_abrev == "Rock"] ~ predicted[spec_abrev == "Rock"], data = spec.data_clean3)
summary(modRock) #p=.09
sqrt(mean(modRock$residuals^2)) #14.7

modSitr <-  lm(actual[spec_abrev == "Sitr"] ~ predicted[spec_abrev == "Sitr"], data = spec.data_clean3)
summary(modSitr) #p=.66
sqrt(mean(modSitr$residuals^2)) #0.41

modVaul <-  lm(actual[spec_abrev == "Vaul"] ~ predicted[spec_abrev == "Vaul"], data = spec.data_clean3)
summary(modVaul) #p=.09
sqrt(mean(modVaul$residuals^2)) #4.8

modVavi <-  lm(actual[spec_abrev == "Vavi"] ~ predicted[spec_abrev == "Vavi"], data = spec.data_clean3)
summary(modVavi) #p=.9
sqrt(mean(modVavi$residuals^2)) #1.9

################################################################################
#3d vs 2d
data777 <-  data666 #replicating data so that I can add species abbreviations and manipluate data later
data777$spec_abrev <-  c("Dila", "Vaul", "Rock", "Grav","Bgr","Vaul","Pisp","Dila", #adding species abreviations for subsetting later
                       "Emni","Rock","Dewo","Migr","Vavi","Grav","Rhgr","Vavi",
                       "Rhla","Vaul","Rock","Bgr","Migr","Rock","Cabi","Jutr",
                       "Vaul","Jutr","Sitr","Bgr","Cabi","Rock","Vavi","Migr",
                       "Besp","Absp","Rock","Jutr","Vaul","Sitr","Dila","Vavi",
                       "Vaul","Cabi","Grav","Rock", "Bgr","Sitr","Migr","Husp",
                       "Emni","Vavi","Rock","Vaul","Cabi","Jutr")

#Clean data
data_clean666 <- data777 %>%
  select(!c("Site", "Species")) %>% #removing site and species columns
  rename(c(actual = "X2D_measured", predicted = "Lab_measured")) %>% #renaming the columns
  drop_na() #removing NAs

# spec.data_clean <- data1 %>% #cleaning the data but keeping species so we can maybe subset by species in the RMSE model
#   select(!c("Site", "Species")) %>% #removing site and species columns
#   rename(c(actual = "Field_measured", predicted = "Lab_measured")) %>% #renaming the columns
#   group_by(spec_abrev) %>% #groups by the species abreviation
#   summarise(across(c("actual", "predicted"), ~ sum (.x, na.rm = TRUE))) #gives the sum for each species actual and predicted values

spec.data_clean777 <- data777 %>% #cleaning the data but keeping species so we can maybe subset by species in the RMSE model
  select(!c("Site", "Species")) %>% #removing site and species
  rename(c(actual = "X2D_measured", predicted = "Lab_measured"))

mod666 <-  lm(actual ~ predicted, data = data_clean666)
summary(mod1)
AIC(mod1) #365

#calculating RMSE from model residuals using base
sqrt(mean(mod666$residuals^2)) #This is taking the square root of the mean for the residuals^2

#calculating RMSE from model residuals using Metrics package
rmse(data_clean666$actual, predict(mod666, data_clean666)) #same thing as above

#can we break it out by species?
# == means take exactly this "value" or "character"
# != means take all values/character besides what you're calling
modabsp <-  lm(actual[spec_abrev == "Absp"] ~ predicted[spec_abrev == "Absp"], data = spec.data_clean777)
summary(modabsp)
sqrt(mean(modabsp$residuals^2)) #no enough data

modbesp <-  lm(actual[spec_abrev == "Besp"] ~ predicted[spec_abrev == "Besp"], data = spec.data_clean777)
summary(modbesp)
sqrt(mean(modbesp$residuals^2)) #not enough data

modbgr <-  lm(actual[spec_abrev == "Bgr"] ~ predicted[spec_abrev == "Bgr"], data = spec.data_clean777)
summary(modbgr) #p=.173
sqrt(mean(modbgr$residuals^2)) #1.21

modcabi <-  lm(actual[spec_abrev == "Cabi"] ~ predicted[spec_abrev == "Cabi"], data = spec.data_clean777)
summary(modcabi)#p=.09
sqrt(mean(modcabi$residuals^2)) #1.27

modDewo <-  lm(actual[spec_abrev == "Dewo"] ~ predicted[spec_abrev == "Dewo"], data = spec.data_clean2)
summary(modDewo)
sqrt(mean(modDewo$residuals^2)) #not enough data

moddila <-  lm(actual[spec_abrev == "Dila"] ~ predicted[spec_abrev == "Dila"], data = spec.data_clean777)
summary(moddila) #p=.13
sqrt(mean(moddila$residuals^2)) #5.8

emni <- spec.data_clean2 %>%
  filter(spec_abrev =="Emni") 
modEmni <-  lm(actual ~ predicted, data = emni)
#modEmni <-  lm(actual[spec_abrev == "Emni"] ~ predicted[spec_abrev == "Emni"], data = spec.data_clean2)
summary(modEmni) 
sqrt(mean(modEmni$residuals^2)) #not enough data

modGrav <-  lm(actual[spec_abrev == "Grav"] ~ predicted[spec_abrev == "Grav"], data = spec.data_clean777)
summary(modGrav) #.19
sqrt(mean(modGrav$residuals^2)) #1.22

modHusp <-  lm(actual[spec_abrev == "Husp"] ~ predicted[spec_abrev == "Husp"], data = spec.data_clean2)
summary(modHusp)
sqrt(mean(modHusp$residuals^2)) #not enough data

modJutr <-  lm(actual[spec_abrev == "Jutr"] ~ predicted[spec_abrev == "Jutr"], data = spec.data_clean777)
summary(modJutr) #.00205
sqrt(mean(modJutr$residuals^2)) #1.3

modMigr <-  lm(actual[spec_abrev == "Migr"] ~ predicted[spec_abrev == "Migr"], data = spec.data_clean777)
summary(modMigr) #.03
sqrt(mean(modMigr$residuals^2)) #0.76

modPisp <-  lm(actual[spec_abrev == "Pisp"] ~ predicted[spec_abrev == "Pisp"], data = spec.data_clean2)
summary(modPisp)
sqrt(mean(modPisp$residuals^2)) #not enough data

modRhgr <-  lm(actual[spec_abrev == "Rhgr"] ~ predicted[spec_abrev == "Rhgr"], data = spec.data_clean2)
summary(modRhgr)
sqrt(mean(modRhgr$residuals^2)) #not enough data
#rare species

modRhla <-  lm(actual[spec_abrev == "Rhla"] ~ predicted[spec_abrev == "Rhla"], data = spec.data_clean2)
summary(modRhla)
sqrt(mean(modRhla$residuals^2)) #not enough data
#rare species

modRock <-  lm(actual[spec_abrev == "Rock"] ~ predicted[spec_abrev == "Rock"], data = spec.data_clean777)
summary(modRock) #significant
sqrt(mean(modRock$residuals^2)) #4.1

modSitr <-  lm(actual[spec_abrev == "Sitr"] ~ predicted[spec_abrev == "Sitr"], data = spec.data_clean777)
summary(modSitr)
sqrt(mean(modSitr$residuals^2)) #0.47

modVaul <-  lm(actual[spec_abrev == "Vaul"] ~ predicted[spec_abrev == "Vaul"], data = spec.data_clean777)
summary(modVaul) #.007
sqrt(mean(modVaul$residuals^2)) #8.18

modVavi <-  lm(actual[spec_abrev == "Vavi"] ~ predicted[spec_abrev == "Vavi"], data = spec.data_clean777)
summary(modVavi)#p=.95
sqrt(mean(modVavi$residuals^2)) #1.16


#################################################################################
######Plot Field/3D####
################################################################################
#subset data for only the species we want to plot
spec.data_clean3 <-  spec.data_clean2%>%
  filter(spec_abrev != "Absp") %>% #removing all the species we don't want in the figure
  filter(spec_abrev != "Besp")  %>%
  filter(spec_abrev != "Dewo") %>%
  filter(spec_abrev != "Husp") %>%
  filter(spec_abrev != "Pisp") %>%
  filter(spec_abrev != "Rhgr") %>%
  filter(spec_abrev != "Rhla") %>%
  filter(spec_abrev != "Emni")

funtimecolors <-  c("#001219", "#005F73", "#0A9396", "#94D2BD","#E9D8A6","#00AACC" ,"#EE9B00", "#CA6702", 
             "#BB3E03", "#AE2012", "#9B2226")


spec_fig <-  ggplot(spec.data_clean3, aes(x=actual, y=predicted,fill = spec_abrev, color = spec_abrev)) + #data first, aes + aestetics
  geom_point(size = 5) + #changes size of points
  geom_smooth(method=lm, se=FALSE) + #gives you your lm results, turned off standard error bars for ease of visualization 
  # stat_fit_glance(method = 'lm', #should add p values but is being stupid
  #                 method.args = list(formula = y ~ x),  geom = 'text', 
  #                 aes(label = paste("p-value=", signif(..p.value.., digits = 2))),
  #                 label.x = 8.5, label.y = 25, size = 5)+ #not adding p-values to figure. What am I doing wrong?
  scale_color_manual(values = colors)+ #adding color manually
  scale_fill_manual(values = colors) + #fill manually
  labs( x = "Actual", y = "Predicted")+ #changes the x and y labels
  theme_minimal() #background theme 
spec_fig

spec_fig_er <-  ggplot(spec.data_clean3, aes(x=actual, y=predicted,fill = spec_abrev, color = spec_abrev)) + #data first, aes + aestetics
  geom_point(size = 5) + #changes size of points
  geom_smooth(method=lm, se=TRUE) + #gives you your lm results, turned off standard error bars for ease of visualization 
  # stat_fit_glance(method = 'lm', #should add p values but is being stupid
  #                 method.args = list(formula = y ~ x),  geom = 'text', 
  #                 aes(label = paste("p-value=", signif(..p.value.., digits = 2))),
  #                 label.x = 8.5, label.y = 25, size = 5)+ #not adding p-values to figure. What am I doing wrong?
  scale_color_manual(values = colors)+ #adding color manually
  scale_fill_manual(values = colors) + #fill manually
  labs( x = "Actual", y = "Predicted")+ #changes the x and y labels
  theme_minimal() #background theme 
spec_fig_er

facet_fig <-  ggplot(spec.data_clean3, aes(x=actual, y=predicted, color= spec_abrev)) + #data first, aes + aestetics
  #scale_fill_discrete(labels=c('Bare ground', 'Carex bigelowii', 'Diapensia lapponica',
  #'Gravel', 'Juncus trifidus', 'Minuartia groenlandica', 
  # 'Rock', 'Sibbaldiopsis tridentata', 'Vaccinium uliginosum',
  #'Vaccinium vitis-idea'))+
  geom_point(size = 5) + #changes size of points
  geom_smooth(method=lm, se=TRUE) + #gives you your lm results 
  #stat_fit_glance(method = 'lm', #should add p values but is being stupid
                  #method.args = list(formula = y ~ x),  geom = 'text', 
                  #aes(label = paste("p-value=", signif(..p.value.., digits = 2))),
                  #label.x = 8.5, label.y = 25, size = 1)+ #not adding p-values to figure. What am I doing wrong?
  scale_color_manual(name= "Species and Substrates",
                     labels = c("Bare ground",
                                "C. bigelowii",
                                "D. lapponica",
                                "Gravel",
                                "J. trifidus",
                                "M. groenlandica",
                                "Rock",
                                "S. tridentata",
                                "V. uliginosum",
                                "V. vitis-idaea"),
                                values = c("#001219", "#005F73", "#0A9396", "#94D2BD","#E9D8A6","#00AACC" ,"#EE9B00", "#CA6702", 
                                           "#BB3E03", "#AE2012", "#9B2226"))+ #adding color manually
  scale_fill_manual(values = c("#001219", "#005F73", "#0A9396", "#94D2BD","#E9D8A6","#00AACC" ,"#EE9B00", "#CA6702", 
                               "#BB3E03", "#AE2012", "#9B2226")) + #fill manually
  labs( x = "Actual % Coverage", y = "Predicted % Coverage")+ #changes the x and y labels
  facet_wrap(~spec_abrev, scales = "free")+ # breaking out each model by species and allwoing the x and y axis to be free
  theme_minimal() #background theme 

facet_fig <- facet_fig + theme(strip.text.x = element_blank())
facet_fig <- facet_fig + scale_x_continuous(labels=scales::number_format(accuracy = 0.1))

facet_fig1<- facet_fig + scale_x_continuous()
                                 

facet_fig



#####Save figure----
ggsave("Desktop/scat_spec_fig.pdf", spec_fig)
ggsave("Desktop/scat.er_spec_fig.pdf", spec_fig_er)
ggsave("Desktop/facet.scat_spec_fig.pdf", facet_fig, height = 8, width = 11, units = "in")

ggsave("User/owner/Sky Islands/facet.pdf", facet_fig)


