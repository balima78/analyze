

dim(dados)

with(dados,
     table(Gender))[2]

hoslem.test(dados.completos$osa, fitted(test), g=10)$p.value

test<-glm(formula = osa ~ Smoking + Age.Cat, family = "binomial",
                data = dados.completos)

length(dados.completos$osa)


dados.completos<-dados[complete.cases(dados), ]


sort(varsUni)=
sort(varsUniDT)

table(dados$OSA, dados$Gender)
