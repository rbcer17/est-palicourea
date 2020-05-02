#Editado por rbc para aprender - 24 abril 2020
#importar dados do copy of taxas frutificacao interno borda externo
library(readxl)
Copy_of_taxas_frutificacao_verdes_interno_borda_e_externo <- read_excel("Copy of taxas-frutificacao-verdes-interno-borda-e-externo.xlsx")
taxas<-Copy2_of_taxas_frutificacao_verdes_interno_borda_e_externo
taxas<-na.omit(taxas)
taxas
#
install.packages("ggplot2")
library("ggplot2")
install.packages("tidyverse")
library("tidyverse")

taxas %>%
  arrange(Posição)%>%
  mutate(Posição = factor(Posição, levels=c("Livre", "Borda", "Isolado"))) %>%
ggplot(aes(x=Posição, y=Score))+
  geom_boxplot(color="black", fill=c("#222021", "grey", "#222200"), alpha=0.7, width=-.7)+
  geom_jitter(color="black", alpha=0.9, position=position_jitter(width=.08, height=0), size=2) +
  ylab("Frutificação (%)")+
  xlab("Posição")+
  theme(text=element_text(size = 25),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x=element_text(colour="black", size = 20),
        axis.text.y=element_text(colour="black", size = 20),
        axis.ticks.x=element_line(colour=NA),
        panel.background = element_rect(color= NA, fill=NA))+
  scale_y_continuous(limits = c(-1.5,100), expand=c(0,1.5))+
  scale_x_discrete(expand=c(0.3, 0.2))


taxas %>%
  arrange(Score) %>%
  mutate(Planta = factor(Planta, levels = c("5", "10", "2", "1", "9", "8", "7", "4", "3","6")))%>%
  ggplot( aes(fill=Posição, y=Score, x=Planta))+
  geom_bar(position="dodge", stat="identity", width =0.8)+
  xlab("Planta") +
  ylab("Frutificação (%)") +
  scale_y_continuous(limits = c(NA,100), expand = c(0, 0))+
  scale_x_discrete(expand = c(0.1,0.0))+
  theme(
    text=element_text(size = 25),
    axis.line = element_line(size=0.5, colour = "black"),
    axis.text.x=element_text(colour="black", size = 25),
    axis.text.y=element_text(colour="black", size = 18),
    axis.ticks.x=element_line(colour=NA),
    panel.background = element_rect(color= NA, fill=NA))+
  scale_fill_manual(values = c("#222021", "grey", "#000000"))

