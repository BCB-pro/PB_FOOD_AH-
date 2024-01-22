data <- openxlsx::read.xlsx("/home/baptiste.criniere/Documents/PB_FOOD_AH/Data/Cleaned_Data_SM2.xlsx", na.strings = c("NA"))
head(data)
View(data)
dim(data)


data1 <- data %>% 
  dplyr::select(ID, FL_11_DO, VAS1_1, VAS2._1, VAS3_1, VAS4_1, VAS5_1) %>% 
  tidyr::pivot_longer(cols = c("VAS1_1", "VAS2._1", "VAS3_1", "VAS4_1", "VAS5_1"), names_to = "Time", values_to = "Stress") %>% 
  dplyr::mutate(Time = Time %>% 
                  factor %>% 
                  forcats::fct_recode("1" = "VAS1_1", "2" = "VAS2._1", "3" = "VAS3_1",
                                      "4" = "VAS4_1", "5" = "VAS5_1"))



data1 %>% 
  ggplot(aes(x = Time, y = log(Stress), fill = Time))+
  geom_boxplot(alpha = 0.2)+
  geom_point(alpha = 0.2)+
  geom_line(aes(group = ID), alpha = 0.2)+
  facet_wrap(~FL_11_DO)+
  theme_bw()+
  theme(legend.position = "none")

data1 %>% 
  ggplot(aes(x = Time, y = Stress))+
  geom_boxplot(aes(fill = FL_11_DO), alpha = 0.2)+
  theme_bw()



#####
data <- openxlsx::read.xlsx("/home/baptiste.criniere/Documents/PB_FOOD_AH/Data/Cleaned_Data_SM2.xlsx", na.strings = c("NA"))

# Data management



library(tidyverse)
data2 <- data %>% 
  dplyr::select(VAS1_1, VAS2_1, VAS3_1, VAS4_1, VAS5_1, dplyr::starts_with("Q")) %>% 
  mutate(across(starts_with("Q"), ~ifelse( .x ==2, 0, 1)))
  
  



data2 <- data %>% 
  dplyr::select(FL_11_DO, VAS1_1, VAS2_1, VAS3_1, VAS4_1, VAS5_1, dplyr::starts_with("Q")) %>% 
  mutate(across(starts_with("Q"), ~ifelse( .x ==2, 0, 1))) %>% 
  dplyr::mutate(Score1 = rowSums(.[6:15])) %>% 
  dplyr::mutate(Score2 = rowSums(.[16:25])) %>% 
  dplyr::mutate(Score3 = (Score1 + Score2)*5) %>% 
  dplyr::select(-dplyr::starts_with("Q")) %>% 
  dplyr::mutate(Stress = ifelse(VAS2_1 > 25, "stress", "pas stress")) %>% 
  dplyr::mutate(Score2_bin = ifelse(Score2 >= 7.5, "eleve", "faible"))


data2 %>% 
  ggplot(aes(x = VAS2_1, y = Score1))+
  geom_count(alpha = 0.2, color = "darkred")+
  geom_smooth(method = "lm", se = FALSE, color = "darkred")+
  theme_bw()

model <- lm(sqrt(Score2) ~ VAS2_1, data = data2)
model2 <- lm(sqrt(Score2) ~ Stress, data = data2)
summary(model2)


data3 <- data2 %>% 
  dplyr::filter(Stress %in% "Plus")
model <- lm(sqrt(Score2) ~ VAS2_1, data = data3)
summary(model)

summary(model)
ggResidpanel::resid_panel(model)


data2 %>% 
  ggplot(aes(x = VAS2_1))+
  geom_bar()+
  theme_bw()



table(data2$Stress, data2$Score2_bin)




#############################################################
data2 <- data %>% 
  dplyr::mutate(Score = (Hits + CRs)/(Hits + CRs + FAs + Misses)) %>% 
  dplyr::select(FL_11_DO, VAS1_1, VAS2_1, VAS3_1, VAS4_1, VAS5_1, Score)

data2 %>% 
  dplyr::filter(VAS2_1 > 25) %>% 
  dplyr::filter(FL_11_DO %in% "StressCondition") %>% 
  ggplot(aes(x = VAS2_1, y = Score))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, color = "darkred")+
  theme_bw()

