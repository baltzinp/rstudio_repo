?subset()
smp_2 <- read_csv2("/Users/baltzingerphilippe/Downloads/smp2.csv")

smp_2 %>% str()

smp_2 %>% 
  filter(dep.cons ==1) %>%
  select(n.enfant) %>% 
  summary()
  
smp_2 %>% 
  group_by(dep.cons) %>% 
  descr(var = n.enfant)

smp_2 %>% 
  filter(age<35) %>% 
  summary(duree)

mean(smp_2$dur.interv[suicide.past == 1])
mean(smp_2$dur.interv[smp_2$suicide.past == 1], na.rm = T)
mean(smp_2[smp_2$suicide.past == 1, 'dur.interv'], na.rm=TRUE)
mean(smp_2$dur.interv[smp_2$suicide.past == 1], na.rm = T)

smp_2 %>% select(age) %>% summary()

age_lev <- c(19,28,37,48,83)

age <- factor(smp_2$age)
length(which(cut(smp_2$age, breaks = age_lev, include.lowest = T ) == "(37,48]"))

class(smp_2$age)

is.na(smp_2[c(20, 221, 342, 446, 531),])

length(which(smp_2$age<=30 & smp_2$age>=20))
