library(ggplot2)

admissions = read.table("admissions.txt", header=FALSE)
head(admissions)
gpa = admissions$V2
entrance_score = admissions$V1
head(admissions)
head(gpa)

scatterplot = ggplot(admissions,  aes(entrance_score, gpa)) +
  geom_point(size=4, color='darkblue') + 
  labs(title="GPA vs. Entrance Score",y="GPA",x="Entrance Score") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

plot(scatterplot)


admissions.lm = lm(gpa~entrance_score)
summary(admissions.lm)

scatterplot_reg = ggplot(admissions, aes(entrance_score, gpa)) +
  geom_point(size=4, color='darkblue') + 
  labs(title="GPA vs. Entrance Score",y="GPA",x="Entrance Score") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5) )+
          geom_smooth(method=lm)

plot(scatterplot_reg)


mean(gpa)
var(gpa)
mean(entrance_score)
var(entrance_score)

cor(gpa, entrance_score)


admissions.res = resid(admissions.lm)
sum(admissions.res)


admissions.pred = fitted(admissions.lm)
