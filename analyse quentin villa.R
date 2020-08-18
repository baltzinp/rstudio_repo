file_path <- "/Users/baltzingerphilippe/Downloads/graphe scratch_qv1.xlsx"

raw_data <- readxl::read_xlsx(file_path, sheet = "villa_r", col_names = c("cells", "l1", "l2", "l3", "l4", "l5", "moyenne", "sd", "cv", "timepoint"))

processed_data <- raw_data %>% select(1:6, 10) %>% filter(!is.na(timepoint)) %>% 
  separate(col = "cells", into = c("cells", "pic"), sep ="\\.") %>% 
  pivot_longer(3:7, names_to="largeur", values_to="mesure") %>% 
  mutate(mesure=as.numeric(mesure)) %>% 
  mutate_at(1:4, as_factor) 

mean_data <-
  processed_data %>%
  mutate(mesure=10*mesure) %>% 
  group_by(cells, timepoint) %>% 
  summarise(n=n(), mean=mean(mesure), sd=sd(mesure), sem=sd/sqrt(n), min=min(mesure), max=max(mesure)) %>%
  mutate(timepoint_num=as.numeric(str_extract(timepoint, "[0-9]{1,2}")))


mean_data %>% ggplot(aes(cells, mean, fill=timepoint))+
  geom_col(position=position_dodge())+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem, group=timepoint), position = position_dodge(width = 0.9), width= 0.2)+
  labs(title = "Largeur moyenne du scratch en fonction de la lignée cellulaire et du temps", 
       x="Lignée cellulaire", y="Largeur moyenne +/- sem (mm)", fill="Temps")+
  plot_theme

ggsave("largeur en fonction des lignées et du temps.jpeg")

mean_data %>% ggplot(aes(timepoint_num, mean, col=cells))+
  geom_point(size=3)+
  geom_line(size=1.5)+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width= 0.7, size=1)+
  labs(title = "Largeur moyenne du scratch en fonction du temps et de la lignée cellulaire", 
      x="", y="Largeur moyenne +/- sem (mm)", fill="Lignée cellulaire")+
  scale_x_continuous(breaks = c(0, 24, 48))+
  plot_theme

ggsave("largeur en fonction du temps et des lignées.jpeg")




mean_data %>% group_by(cells) %>% mutate(fold_t0 = case_when(
  cells=="A4" ~100*round(mean/44.3, 2),
  cells=="A23" ~100*round(mean/45.5, 2),
  cells=="D16" ~100*round(mean/40.3, 2),
  cells=="D45" ~100*round(mean/35.5, 2),
  cells=="WT" ~100*round(mean/39.1, 2)), 
  fold_WT=round(mean/3.91, 2)
  ) %>% 
  ggplot(aes(timepoint_num, fold_t0, col=cells))+
  geom_point(size=3)+
  geom_line(size=1.5)+
  labs(title="Evolution de la largeur par rapport à T0 en fonction des lignées",
       x="Temps", y="Pourcentage de la largeur initiale", color="Lignées")+
  plot_theme+
  scale_x_continuous(breaks = c(0, 24, 48))

gsave("fonction largeur t0.jpeg")

mean_data %>% group_by(cells) %>% mutate(fold_t0 = case_when(
  cells=="A4" ~100*round(mean/4.43, 2),
  cells=="A23" ~100*round(mean/4.55, 2),
  cells=="D16" ~100*round(mean/4.03, 2),
  cells=="D45" ~100*round(mean/3.55, 2),
  cells=="WT" ~100*round(mean/3.91, 2)), 
  fold_WT=case_when(
    timepoint=="T24" ~100-(fold_t0/56*100),
    timepoint=="T48" ~100-(fold_t0/37*100),
    timepoint=="T0" ~100-(fold_t0/100*100)
  ),
  pct_T0=case_when(
    cells=="A4" ~100*round((mean-4.43)/4.43, 2),
    cells=="A23" ~100*round((mean-4.55)/4.55, 2),
    cells=="D16" ~100*round((mean-4.03)/4.03, 2),
    cells=="D45" ~100*round((mean-3.55)/3.55, 2),
    cells=="WT" ~100*round((mean-3.91)/3.91, 2))
  ) %>% 
  ggplot(aes(timepoint_num, fold_WT, col=cells))+
  geom_point(size=3)+
  geom_line(size=1.5)+
  labs(title="Evolution de la largeur par rapport à T0 en fonction des lignées",
       x="Temps", y="Pourcentage de la largeur initiale", color="Lignées")+
  plot_theme


processed_data %>% 
  ggplot(aes(cells, mesure))+
  geom_boxplot(aes(fill=pic), position = "dodge")+
  geom_jitter(width = 0.2, aes(color=pic))+
  facet_wrap(~timepoint)

processed_data %>% 
  ggplot(aes(cells, mesure))+
  geom_boxplot(aes(fill=timepoint), position = "dodge")+
  geom_jitter(width = 0.2, aes(color=timepoint))+
  facet_wrap(~pic)
  
processed_data %>% 
  ggplot(aes(cells, mesure))+
  geom_col()
  

