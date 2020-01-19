#pacotes
library(tidyverse)
library(knitr)
library(kableExtra)

library(Amelia)
library(DT)
library(pander)

library(MASS)
library(broom)
library(ResourceSelection)
library(pscl)

library(car)
library(InformationValue)

#library(caret)

# preparar dados
dados<-read.csv("files/osa_simulated_523.csv", na.strings = "")

dados <- dados %>% mutate(osa = case_when(OSA == "Yes" ~ 1, OSA == "No" ~ 0),
                         Age.Cat = fct_relevel(Age.Cat, ">=70", after = 3),
                         Smoking = fct_relevel(Smoking, "EX", after = 1),
                         AC.Cat = fct_rev(AC.Cat),
                         NC.Cat = fct_rev(NC.Cat),
                         ESS.Cat = fct_rev(ESS.Cat)) 

dados <- dados %>% filter(!is.na(OSA))


## função para tabelas de contingencias com valores esperados
tabela<-function(dados,variavel){
  v<-sym(variavel)

  dados %>% filter(!is.na(!!v)) %>%
    group_by(OSA,!!v) %>% count() %>% 
    ungroup() %>% 
    spread(OSA,n) %>% 
    mutate(`yes.%`= round(Yes/sum(Yes)*100,2),
           `E(yes)`= round((Yes+No)*sum(Yes)/521,2),
           `E(no)`= round((Yes+No)*sum(No)/521,2),
           `no.%` = round(No/sum(No)*100,2)) %>% 
    dplyr::select(!!v,Yes,`yes.%`,  No, `no.%`) 
  
}





