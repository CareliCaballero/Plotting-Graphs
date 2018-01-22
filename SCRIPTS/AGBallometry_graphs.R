#libraries
library(plotly)
library(dplyr)
library(ggplot2)
library(reshape2)
library(cowplot)
library(gridExtra)

#load the table
AGB_allom <- read.csv("OUTPUT/allometry/AGB_allometry.csv", header = T)

#convert csv into a data frame
AGB_perc <-data.frame(AGB_allom, row.names = NULL)

#add new columns for the percentage
AGB_perc["AGB_perc_TLS_Field"] <- NA
AGB_perc$AGB_perc_TLS_Field <- AGB_perc$AGB_TLS*100/AGB_perc$AGB_Field

AGB_perc["Perc_100"] <- NA
AGB_perc$Perc_100 <- AGB_perc$AGB_perc_TLS_Field-100

##selection of Mauritia
Mauritia_perc <- AGB_perc[which(AGB_perc$Specie=="Mauritia Flexuosa"),]

#creation table with columns I need
Mau.data <- data.frame("Specie" = Mauritia_perc$Specie,
                  "TreeID" = Mauritia_perc$TreeID,
                  "Difference" = Mauritia_perc$Perc_100)

#creation of bar graphs
perc.graph <- ggplot(data=Mau.data, aes(TreeID, Difference, fill="Difference")) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  facet_grid(. ~ Specie) +
  scale_fill_manual(values = c("Difference"="forestgreen")) + 
  labs(x="Tree ID", y="Difference (%)", angle = 90) +
  labs(title = "% Difference of TLS and Field AGB") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")
perc.graph


##selection of Mauritiella
Mauritiella_perc <- AGB_perc[which(AGB_perc$Specie=="Mauritiella Armata"),]

#creation table with columns I need
Mauritiella.data <- data.frame("Specie" = Mauritiella_perc$Specie,
                       "TreeID" = Mauritiella_perc$TreeID,
                       "Difference" = Mauritiella_perc$Perc_100)

#creation of bar graph
perc.graph2 <- ggplot(data=Mauritiella.data, aes(TreeID, Difference, fill="Difference")) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  facet_grid(. ~ Specie) +
  scale_fill_manual(values = c("Difference"="forestgreen")) + 
  labs(x="Tree ID", y="AGB Difference (%)", angle = 90) +
  labs(title = "% Difference of TLS and Field AGB") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

perc.graph2


##GRAPH FOR PERCENTAGES USING ERROR GRAPHS
##Mauritia species

##calculation of percentage
Mauritia_perc$Perc_Field = 100
Mauritia_perc$Perc_TLS = Mauritia_perc$AGB_TLS*100/Mauritia_perc$AGB_Field
Mauritia_perc[, 'Perc_TLS'] = round(Mauritia_perc[, 'Perc_TLS'])


#summarize
summ <- Mauritia_perc %>% group_by(Specie, TreeID) %>% summarise(Min = min(Perc_TLS), Max = max(Perc_Field))

#error bars plot
ggplot(summ, aes(x = TreeID, ymin=Min, ymax=Max)) + 
  geom_errorbar(size = 1, width = 0.4, colour = "blue") +
  labs(x = "Tree ID", y = "AGB Difference (%)", angle = 90)+
  labs(title = "Difference in AGB estimates from allometry") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(. ~ Specie, space = "free", as.table = TRUE)


##Mauritiella

##calculation of percentage
Mauritiella_perc$Perc_Field = 100
Mauritiella_perc$Perc_TLS = Mauritiella_perc$AGB_TLS*100/Mauritiella_perc$AGB_Field
Mauritiella_perc[, 'Perc_TLS'] = round(Mauritiella_perc[, 'Perc_TLS'])


#summarize
summ2 <- Mauritiella_perc %>% group_by(Specie, TreeID) %>% summarise(Min = min(Perc_TLS), Max = max(Perc_Field))

#error bars plot
ggplot(summ2, aes(x = TreeID, ymin=Min, ymax=Max)) + 
  geom_errorbar(size = 1, width = 0.4, colour = "blue") +
  labs(x = "Tree ID", y = "AGB Difference (%)", angle = 90)+
  labs(title = "Difference in AGB estimations from allometry") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(. ~ Specie, space = "free", as.table = TRUE)


###MAKING GRAPHS USING CORRELATION SCATTERPLOT
##Mauritia species
AGB_mauritia <- AGB_allom[which(AGB_allom$Specie=="Mauritia Flexuosa"),]

AGB_sources_mau <- data.frame("AGBField" = AGB_mauritia$AGB_Field,
                              "AGBTLS" = AGB_mauritia$AGB_TLS)


#coefficient of determination 
r2_mf = lm(AGBField ~ AGBTLS, data = AGB_sources_mau)
summary(r2_mf)$r.squared

#correlation scatterplot and 1:1 line
cor(AGB_sources_mau$AGBField, AGB_sources_mau$AGBTLS)
plot(AGB_sources_mau,
     main = "Mauritia: Comparison of AGB from allometric equations",
     xlab = "AGB Field (kg)",
     ylab = "AGB TLS (kg)")
legend(900, 700, bty="n", legend=paste("r= ", 
                                         formatC(cor(AGB_sources_mau$AGBField, AGB_sources_mau$AGBTLS), 3, format="f")))

legend(895, 600, bty="n", legend=paste(" r2= ", 
                                       formatC(summary(r2_mf)$r.squared, 3, format="f")))
abline(line(AGB_sources_mau), col="blue")
title(sub=paste0("r= ", formatC(cor(AGB_sources_mau$AGBField, AGB_sources_mau$AGBTLS), 3, format="f")))



##Mauritiella species
AGB_mauritiella <- AGB_allom[which(AGB_allom$Specie=="Mauritiella Armata"),]

AGB_sources_mauritiella <- data.frame("AGBField" = AGB_mauritiella$AGB_Field,
                              "AGBTLS" = AGB_mauritiella$AGB_TLS)

#coefficient of determination 
r2_ma = lm(AGBField ~ AGBTLS, data = AGB_sources_mauritiella)
summary(r2_ma)$r.squared

#correlation scatterplot
cor(AGB_sources_mauritiella$AGBField, AGB_sources_mauritiella$AGBTLS)
plot(AGB_sources_mauritiella,
     main = "Mauritiella: Comparison of AGB from allometric equations",
     xlab = "AGB Field (kg)",
     ylab = "AGB TLS (kg)")
legend(40, 32, bty="n", legend=paste("r= ", 
                                       formatC(cor(AGB_sources_mauritiella$AGBField, AGB_sources_mauritiella$AGBTLS), 3, format="f")))

legend(39.7, 26, bty="n", legend=paste(" r2= ", 
                                       formatC(summary(r2_ma)$r.squared, 3, format="f")))
abline(line(AGB_sources_mauritiella), col="blue")


###MAKING BAR PLOT FOR COMPARISONS PER TREE SPECIES
#Mauritia species
AE_MF <- data.frame("Specie" = AGB_allom$Specie,
                  "treeID" = AGB_allom$TreeID,
                  "AGB Field" = AGB_allom$AGB_Field,
                  "AGB TLS" = AGB_allom$AGB_TLS)


#selection of Mauritia
Mauritia_AE <- AE_MF[which(AE_MF$Specie=="Mauritia Flexuosa"),]

##melt
AE.mauritia <- melt (Mauritia_AE, id.vars = c("Specie", "treeID"))


##make a bar graph of AGB separated by each species
ggplot(data=AE.mauritia, aes(treeID, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Specie) +
  scale_fill_manual(name=NULL, values = c("AGB.Field"="aquamarine4", "AGB.TLS"="aquamarine2")) +
  labs(x="Tree ID", y="AGB (kg)", angle = 90) +
  labs(title = "Comparison of AGB estimates from allometric equations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")


#Mauritiella species
AE_MA <- data.frame("Specie" = AGB_allom$Specie,
                 "treeID" = AGB_allom$TreeID,
                 "AGB Field" = AGB_allom$AGB_Field,
                 "AGB TLS" = AGB_allom$AGB_TLS)


#selection of Mauritiella
Mauritiella_AE <- AE_MA[which(AE_MA$Specie=="Mauritiella Armata"),]

##melt
AE.mauritiella <- melt (Mauritiella_AE, id.vars = c("Specie", "treeID"))


##make a bar graph of AGB separated by each species
ggplot(data=AE.mauritiella, aes(treeID, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Specie) +
  scale_fill_manual(name=NULL, values = c("AGB.Field"="aquamarine4", "AGB.TLS"="aquamarine2")) +
  labs(x="Tree ID", y="AGB (kg)", angle = 90) +
  labs(title = "Comparison of AGB estimates from allometric equations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")


###MAKING ERROR BAR PLOTS FOR ONE TREE SPECIES
#Mauritia

library(plotly)


#summarize
AE.summ.mauritia <- AE.mauritia %>% group_by(treeID, Specie) %>% summarize(Min = min(value), Max = max(value))


#plot
ggplot(AE.summ.mauritia, aes(x = treeID, ymin = Min, ymax = Max)) + 
  geom_errorbar(size = 1, width = 0.2, colour = "blue") +
  labs(x = "Tree ID", y = "AGB Difference (kg)", angle = 90)+
  scale_y_continuous(breaks = c(500, 900, 1300, 1700))+
  labs(title = "Difference in AGB estimations from allometric equations") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(. ~ Specie, space = "free", as.table = TRUE)
