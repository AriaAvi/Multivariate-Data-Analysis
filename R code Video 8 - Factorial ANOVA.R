#Factorial ANOVA with interaction analysis

#1 Data visualization 
#2 Assumption 1. Normality (of residuals)
#3 Assumption 2. Homogeneity of variances: Levene's test; Bartlett's test 
#4 Factorial ANOVA: one-way, two-way, three-way ANOVA
#5 Model fit: AIC, BIC, AICwt, and BICwt
#6 Effect size: (Partial) eta^2, omega^2, epsilon^2
#7 Post-hoc analysis: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"
#8 visualize post-hoc results with confidence intervals

##############################################
### Data visualization 
head(ANOVA) 
library(ggplot2)
#Density plots
P1 <- ggplot(ANOVA, aes(x=Grade, color = SES2, fill=SES2)) +
  geom_density(alpha = 0.7)
P1

P2 <- ggplot(ANOVA, aes(x=Grade, color = Gen2, fill=Gen2)) +
  geom_density(alpha = 0.4) 
P2

P3 <- ggplot(ANOVA, aes(x=Grade, color = FirstLang2, fill=FirstLang2)) +
  geom_density(alpha = 0.1)
P3

#Boxplots
library(ggpubr)
P4 <- ggboxplot(ANOVA, "SES2", "Grade",
          fill = "SES2", palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
          add = "jitter")
P4
P5 <- ggboxplot(ANOVA, "SES2", "Grade",
          fill = "SES2", palette = get_palette("Pastel2", 3),
          add = "jitter")
P5

ggarrange(P1, P2, P3, P4, P5 + rremove("x.text"), labels = c("A", "B", "C", "D", "E"), ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

#Factorial ANOVA

## Assumptions
# 1. Normality
library(psych)
describe(ANOVA)

res_aov <- aov(Grade ~ SES2, data = ANOVA)

res_aov

# par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
QQ<- qqPlot(res_aov$residuals, id = FALSE) # id = FALSE to remove point identification

shapiro.test(res_aov$residuals) #test normality of the residuals with the Shapiro-Wilk test

# 2. Homogeneity of variances: Levene's test & Bartlett's test (sphericity)
leveneTest(Grade ~ Gen2, data = ANOVA) #one variable
leveneTest(Grade ~ Gen2*FirstLang2*SES2, data = ANOVA) #multiple variables
#Levene's test is less sensitive to deviations from normality.

bartlett.test(Grade ~ Gen2, data=ANOVA) #one variable
bartlett.test(Grade ~ interaction(Gen2,FirstLang2,SES2), data=ANOVA) #multiple variable

#ANOVA tests: one-way, two-way, etc.
one.way <- aov(Grade ~ SES2, data = ANOVA)
summary(one.way)

two.way <- aov(Grade ~ SES2 + Gen2, data = ANOVA)
summary(two.way)

three.way <- aov(Grade ~ SES2 + Gen2 + FirstLang2, data = ANOVA)
summary(three.way)

interaction <- aov(Grade ~ SES2 + Gen2 + FirstLang2 + Gen2*FirstLang2, data = ANOVA)
summary(interaction)

#Model fit
library(AICcmodavg)
model.set <- list(one.way, two.way, three.way, interaction)
model.names <- c("one.way", "two.way", "three.way", "interaction")
aictab(model.set, modnames = model.names, , sort = TRUE)
bictab(model.set, modnames = model.names, , sort = TRUE)

#BICwt / AICwt (the BIC/AIC weights): The best  models receive the highest wt.
#BICwt add up to 1 (are normalized). Models with wt<0.05 to be neglected.

# Effect size
library(effectsize)
eta_squared(interaction, partial = TRUE)
eta_squared(interaction, partial = FALSE)
omega_squared(interaction, partial = TRUE)
epsilon_squared(interaction, partial = TRUE)

# Post-hoc analysis 

tukey.interaction<-TukeyHSD(interaction) 
tukey.interaction

tukey.plot.aov<-aov(Grade ~ Gen2:FirstLang2, data=ANOVA)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 2)

library(DescTools)
ScheffeTest(interaction)

hsd <- PostHocTest(interaction, which = NULL,
            method = c("hsd"),
            conf.level = 0.95, ordered = FALSE)
hsd

#"hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"

plot(hsd, las = 2) #plot 

