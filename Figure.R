data <- openxlsx::read.xlsx("/home/baptiste.criniere/Documents/PB_FOOD_AH/Data/Cleaned_Data_SM2.xlsx", na.strings = c("NA"))

library(tidyverse)
data2 <- data %>% 
  dplyr::select(VAS1_1, VAS2_1, VAS3_1, VAS4_1, VAS5_1, dplyr::starts_with("Q")) %>% 
  mutate(across(starts_with("Q"), ~ifelse( .x ==2, 0, 1)))





data2 <- data %>% 
  dplyr::select(VAS1_1, VAS2_1, VAS3_1, VAS4_1, VAS5_1, dplyr::starts_with("Q")) %>% 
  mutate(across(starts_with("Q"), ~ifelse( .x ==2, 0, 1))) %>% 
  dplyr::mutate(Score1 = rowSums(.[6:15])) %>% 
  dplyr::mutate(Score2 = rowSums(.[16:25])) %>% 
  dplyr::mutate(Score3 = (Score1 + Score2)*5) %>% 
  dplyr::select(-dplyr::starts_with("Q")) %>% 
  dplyr::mutate(Stress = ifelse(VAS2_1 > 25, "stress", "pas stress")) %>% 
  dplyr::mutate(Score2_bin = ifelse(Score2 >= 7.5, "eleve", "faible"))


data2 %>% 
  ggplot(aes(x = VAS2_1, y = Score3))+
  geom_count(alpha = 0.2, color = "darkred")+
  geom_smooth(method = "lm", se = FALSE, color = "darkred")+
  ggpubr::stat_cor(color = "darkred", label.y.npc = "bottom")+
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
  ggplot(aes(x = VAS2_1, y = Score))+
  geom_point(alpha = 0.2, color = "darkred")+
  geom_smooth(method = "lm", se = FALSE, color = "darkred")+
  theme_bw()

