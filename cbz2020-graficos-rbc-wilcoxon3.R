#Analise com 3 classes interno borda externo 23fev2020
#teste do github
#novo teste do github
library(readxl)
Copy2_of_taxas_frutificacao_verdes_interno_borda_e_externo <- read_excel("Copy2 of taxas-frutificacao-verdes-interno-borda-e-externo.xlsx")
taxas3<-Copy2_of_taxas_frutificacao_verdes_interno_borda_e_externo
library(dplyr)
group_by(taxas3, posicao) %>%
  summarise(
    count = n(),
    median = median(score, na.rm = TRUE),
    IQR = IQR(score, na.rm = TRUE)
  )

# Plot score by posicao and color by posicao
library("ggpubr")
ggboxplot(taxas3, x = "posicao", y = "score", 
          color = "posicao", palette = c("#00AFBB", "#E7B800", "#FF0000"),
          ylab = "Score", xlab = "Posicao")
# ver se funciona
wilcox.test(taxas3$score~taxas3$posicao, data = taxas3, 
            subset=(posicao %in% c("Livre", "Isolado"))
            
# acima nao deu vou testar outro

taxas4 <- split(taxas3, taxas3$posicao)
taxas4

#analise antiga com 2 classes usada para o resumo
taxas1<-taxas_frutificacao_interno_e_externo_com_celulas_vazias
wilcox.test(taxas1$score~taxas1$posicao)
taxas2<-na.omit(taxas1)
tapply(taxas2$score, taxas2$posicao, mean)
wilcox.test(taxas2$score~taxas2$posicao)

