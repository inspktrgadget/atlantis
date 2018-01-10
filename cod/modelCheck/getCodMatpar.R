library(dplyr)
library(fjolst)
library(fjolstTranslate)

stations <- 
    translate.stodvar() %>%
    filter(sampling.type %in% c(30,35)) %>%
    select(year, month, day, sample.id, lat, lon)

# get ages
age <- 
    translate.all.kv() %>%
    filter(sample.id %in% stations$sample.id, species.code == 1) %>%
    left_join(stations)

mat <- 
    age %>% 
    filter(!is.na(maturity), !is.na(age)) %>%
    mutate(mat.stage = ifelse(maturity > 1,1,0))

mat.mod <- glm(mat.stage ~ age, family=binomial(link='logit'), data=mat)

plot(mat.stage ~ age, data=mat)
curve(predict(mat.mod, data.frame(age=x), type='resp'), add=T)





