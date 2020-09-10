smp1 <- read_csv2("/Users/baltzingerphilippe/Downloads/smp1.csv")
smp2 <-  read_csv2("/Users/baltzingerphilippe/Downloads/smp2.csv")

smp1 %>% str()
smp2 %>% str()

summary(smp2$dur.interv)

factor(table(smp2$n.fratrie >= 5),labels=c('<5', '5+'))
factor(smp2$n.fratrie >= 5, labels=c('<5', '5+'))
smp2$n.fratrie[smp2$n.fratrie < 5] <-'<5'
smp2$n.fratrie[smp2$n.fratrie >= 5] <- '5+'
smp2$n.fratrie


smp2 %>% select(ecole) %>% filter(ecole <4)

smp2 %>% select(prof) %>% filter(prof == 'sans emploi')

sum(table(smp2$prof))
str(smp2)

mean(smp2$age[1:10])
median(smp2$dur.interv[1:300], na.rm = T)
