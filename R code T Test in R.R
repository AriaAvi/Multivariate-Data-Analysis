#Independent Samples t Test in R 
#1.Plots 2.Assumption check 3.T-test 4.Effect size
#first import the data
names(SEM)
head(SEM)

#1. Plots
SEM2 <- SEM[-c(1,17)] #removing variables from the dataframe
SEM2
boxplot(SEM2, col = rainbow(ncol(SEM2)))
#using SEM again
boxplot(SEM$swtotal~SEM$gen2) #OR
boxplot(swtotal~gen2, data=SEM)
boxplot(SEM$swtotal~SEM$gen2, col = rainbow(ncol(SEM2)))
boxplot(swtotal~gen2, data = SEM, col = "maroon")#OR blue, etc.
boxplot(swtotal~gen2, data = SEM, col = "green", horizontal = TRUE)

library(ggpubr)
Myplot <- ggboxplot(
  SEM, x = "gen2", y = "swtotal",
  ylab = "Writing", xlab = "Distribution of Gender", add = "jitter"
)
Myplot

Myplot2 <- ggboxplot(
  SEM, x = "gen2", y = "swtotal",
  color = "gen2", fill = "white",
  palette =c("#00AFBB", "#E7B800", "#FC4E07"),
  ylab = "Writing", xlab = "Distribution of Gender", add = "jitter"
)
Myplot2


ggboxplot(SEM, "gen2", "swtotal",
          fill = "gen2", palette = c("#00AFBB", "#E7B800", "#FC4E07"))
ggboxplot(SEM, "gen2", "swtotal",
          fill = "gen2", palette = "default")
ggboxplot(SEM, "gen2", "swtotal",
          fill = "gen2", palette = get_palette("Pastel2", 3))

# get_palette:Dark2,npg,RdBu,Blues,Set1,Set2,Set3,Accent
#Paired,Pastel1,Pastel2

#2.Checking assumptions:
require (psych)
describe (SEM) #M, SD, & Normality
library(car) #install and open 'car' (companion to applied regression)
leveneTest(SEM$swtotal ~ SEM$gen2)#Here homogeneity is violated.

#3.Running the t-test
?t.test #to learn about t-test
t.test(swtotal ~ gen2, data = SEM) #OR Equal variances Assumed
t.test(swtotal ~ gen2, data = SEM, mu=0, #mu: mean differences
       alt="two.sided", conf=0.95, var.eq=F, #equality of v not assumed 
       paired=F) #it is NOT a paired sample t test

#4. Effect size
library(rstatix)
#Sample size > 50
SEM %>% cohens_d(swtotal ~ gen2, 
                 var.equal = TRUE) #var.equal = TRUE means the pooled SD to be used
#Sample size < 50, use the Hedge’s Corrected version of the Cohen’s d (Hedges and Olkin 1985)
SEM %>% cohens_d(swtotal ~ gen2, 
                 var.equal = TRUE,
                 hedges.correction = TRUE)

