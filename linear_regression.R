---
title: "Final Project"
output: word_document
date: "2022-09-25"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#import data
library(readxl)
insurance <- read_excel("C:/Users/nirvi/Desktop/Harrisburg/Semester 1/Analytics/Final Project/insurance.xlsx")
head(insurance)
```
```{r}
library("plyr")
df = count(insurance, 'age')
plot(df, type = 'l', col = 'cyan3', lwd = 2.0)
hist(insurance$age)
df
```
```{r}
#barplot(setNames(df$freq,df$age),col = 'cyan3')
#library(ggplot2)
#Plot
#ggplot(df,aes(x=age,y=freq))+geom_col(color='black',fill='cyan3')
```

```{r}
sex_df = count(insurance, 'sex')
barplot(sex_df$freq,names = sex_df$sex,col = c('cyan3','black'))
```
```{r}
region_df = count(insurance, 'region')
pie_labels = paste0(round(100 * region_df$freq/sum(region_df$freq), 2), "%")
pie(region_df$freq,labels = pie_labels, col = c('black','white','cyan3','cyan4'))
legend("topleft", legend = c("Northeast", "Northwest", "Southeast", "Southwest"),
       fill =  c('black','white','cyan3','cyan4'))
```
```{r}
smoker_df = count(insurance, 'smoker')
barplot(smoker_df$freq, col = c('cyan3', 'black'), names.arg = smoker_df$smoker)
```
```{r}
children_df = count(insurance, 'children')
pie_labels = paste0(round(100 * children_df$freq/sum(children_df$freq), 2), "%")
pie(children_df$freq,labels = pie_labels, col = c('black','white','cyan3','cyan4','blue','green'), main = "Number of Dependents")
legend("topleft", legend = c("0 Child", "1 Child", "2 Child", "3 Child", "4 Child", "5 Child"),
       fill =  c('black','white','cyan3','cyan4','blue','green'))
```
```{r}
hist(insurance$bmi, xlab = "BMI", col = "cyan4", main = "Histogram of BMI")

```

```{r}
hist(insurance$charges, main = "Charges of Insurance Member", col = "cyan4",xlab = "Charges")
```
```{r}
model1 = lm(charges ~ age+sex+bmi+children+region+smoker,
            data = insurance)
summary(model1)
````
```{r}
par(mfrow=c(2,2))
plot(model1)
```

```{r}
model2 = lm(charges ~ age+bmi+children+smoker,
            data = insurance)
summary(model2)
```

```{r}
par(mfrow=c(2,2))
plot(model2)
```

```{r}
ins_nonsmoker_nochild_over30 <- insurance %>%
            filter(smoker == "no" & children == 0 & bmi >= 30)
model3 =lm(charges ~ age,data = ins_nonsmoker_nochild_over30)
summary(model3)
```

```{r}
par(mfrow=c(2,2))
plot(model3)
```
```{r}
ins_smoker_child_under30 <- insurance %>%
            filter(smoker == "yes" & children > 0 & bmi < 30)
model4 = lm(charges~age, data = ins_smoker_child_under30)
summary(model4)
```

```{r}
plot(model4)
```
```{r}
ins_smoker_child_over30 <- insurance %>%
            filter(smoker == "yes" & children > 0 & bmi >= 30)
model5 = lm(charges~age, data = ins_smoker_child_over30)
summary(model5)
plot(model5)
```

