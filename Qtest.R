testcurves<-read.csv("2020-09-24-1002_bethany_bern6.csv")
testcurves2<-read.csv("2020-09-24-0959_logdata_bethany_bern3.csv")
testcuves3<- read.csv("2020-09-24-0959_bethany_bern4.csv")
testcurves4<- read.csv("2020-09-24-0959_logdata_bethany_bern5.csv")

testcurves.combo<-rbind(testcurves, testcurves2, testcuves3)




library(ggplot2)

ggplot(testcurves.combo) +
 aes(x = Q, y = A, colour = canopy) +
 geom_point(size = 3L) +
 scale_color_brewer(palette = "Dark2") +
 theme_minimal() +
 facet_wrap(vars(range))


ggplot(testcurves.combo) +
  aes(x = Q, y = A, colour = canopy) +
  geom_point(size = 3L) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

ggplot(testcuves4) +
  aes(x = Q, y = A, colour = canopy) +
  geom_point(size = 3L) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  facet_wrap(vars(range))

