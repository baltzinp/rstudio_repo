smp <- read_csv2("/Users/baltzingerphilippe/Downloads/smp2.csv")


smp %>% str()
prop.table(table(smp$separation, smp$dep.cons), margin =  )

by(smp[c("age", "separation")], age, mean)
within(smp, tapply(age, separation, mean, na.rm=TRUE))
with(smp, tapply(age, separation, mean, na.rm=TRUE))

smp %>% 
  group_by(dep.cons) %>% 
  summarise(n = n(), mean = median(duree, na.rm = T))

smp %>% 
  group_by(dep.cons) %>% 
  summarytools::descr(dur.interv)


cor.test(smp$dur.interv, smp$age)

smp %>% 
  filter(!is.na(suicide.past)) %>% 
  mutate(suicide = as_factor(suicide.past)) %>% 
  ggplot(aes(suicide, dur.interv))+
  geom_boxplot()+
  stat_compare_means()

smp$suicide.past

tapply()

boxplot(age~suicide.past, data = smp)

t.test(smp$dur.interv ~ smp$dep.cons, var.equal = TRUE)


odd