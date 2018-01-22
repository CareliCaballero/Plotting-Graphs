#packages
install.packages("cowplot")
install.packages("gridExtra")

#libraries
library(ggplot2)
library(reshape2)
library(cowplot)
library(gridExtra)

#load the tables
allometry <- read.csv("OUTPUT/allometry/AGB_allometry.csv", header=T)
qsm1 <- read.csv("OUTPUT/qsm/AGB_QSM.csv", header=T)

###to plot bars of 4th AGBs (2 from QSM and 2 from allometry)

#selection of fields to use
qsm2 <- data.frame("TreeID" = qsm1$treeID,
                   "Specie" = qsm1$Specie,
                   "AGB WDPerugov" = qsm1$AGB_perugov,
                   "AGB WDGWDD" = qsm1$AGB_GWDD)
                    

allom <- data.frame("TreeID" = allometry$TreeID,
                    "AGB FieldHeight" = allometry$AGB_Field,
                    "AGB TLSHeight" = allometry$AGB_TLS)
                   

#merge data frames
allom_qsm <- merge(qsm2, allom, by.x = "TreeID", by.y = "TreeID", all=TRUE)

#export to csv
write.csv(allom_qsm, "OUTPUT/qsm_allom/AGB_QSM_Allometry.csv", row.names = F)

#separate by specie
allom_qsm_mauritia <- allom_qsm[which(allom_qsm$Specie=="Mauritia Flexuosa"),]
allom_qsm_mauritiella <- allom_qsm[which(allom_qsm$Specie=="Mauritiella Armata"),]

#melt 
AGBs_mauritia <- melt(allom_qsm_mauritia, id.vars = c("Specie", "TreeID"))
AGBs_mauritiella <- melt(allom_qsm_mauritiella, id.vars = c("Specie", "TreeID"))


#creation of bar plots per specie
#Mauritia
mauritia.graph <- ggplot(data = AGBs_mauritia, aes(TreeID, value, fill = variable)) +
  geom_bar(stat="identity", position = "dodge")+
  facet_grid(. ~ Specie) +
  scale_fill_brewer(name=NULL, type="div", palette = 3) + 
  labs(x="Tree ID", y="AGB (kg)", angle = 90) +
  labs(title = "AGBs from QSM and allometric equations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")
mauritia.graph

#Mauritiella
mauritiella.graph <- ggplot(data = AGBs_mauritiella, aes(TreeID, value, fill = variable)) +
  geom_bar(stat="identity", position = "dodge")+
  facet_grid(. ~ Specie) +
  scale_fill_brewer(name=NULL, type="div", palette = 3) + 
  labs(x="Tree ID", y="AGB (kg)", angle = 90) +
  labs(title = "AGBs from QSM and allometric equations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")
mauritiella.graph

##correlation of the 4 cases

##correlation (to get the numbers)
corr1 <- cor(allom_qsm_mauritia$AGB.FieldHeight, allom_qsm_mauritia$AGB.WDPerugov)
corr2 <- cor(allom_qsm_mauritia$AGB.TLSHeight, allom_qsm_mauritia$AGB.WDPerugov)
corr3 <- cor(allom_qsm_mauritia$AGB.FieldHeight, allom_qsm_mauritia$AGB.WDGWDD)
corr4 <- cor(allom_qsm_mauritia$AGB.TLSHeight, allom_qsm_mauritia$AGB.WDGWDD)


##correlation function

corr_eqn <- function(x,y, digits = 3) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

##creating labels of correlation value to appear on graphs
#Mauritia flexuosa species
label1 = data.frame(x = 1000, y = 500, label = corr_eqn(allom_qsm_mauritia$AGB.FieldHeight,
                                               allom_qsm_mauritia$AGB.WDPerugov))

label2 = data.frame(x = 900, y = 500, label = corr_eqn(allom_qsm_mauritia$AGB.TLSHeight,
                                                        allom_qsm_mauritia$AGB.WDPerugov))

label3 = data.frame(x = 900, y = 300, label = corr_eqn(allom_qsm_mauritia$AGB.FieldHeight,
                                                        allom_qsm_mauritia$AGB.WDGWDD))

label4 = data.frame(x = 900, y = 300, label = corr_eqn(allom_qsm_mauritia$AGB.TLSHeight,
                                                        allom_qsm_mauritia$AGB.WDGWDD))



##CREATION OF 4 SCATTERPLOTS SHOWING AGB PER TREE SPECIES
#Mauritia species

head(allom_qsm_mauritia)
plot1 <- ggplot(allom_qsm_mauritia, aes(x=AGB.FieldHeight, y=AGB.WDPerugov)) +
  geom_point() + stat_smooth (method = "lm", formula = (y~x-1), se = FALSE) +
  geom_text(data = label1, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_text(colour = "grey10", size = 8),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))
  
plot1

plot2 <- ggplot(allom_qsm_mauritia, aes(AGB.TLSHeight, AGB.WDPerugov)) +
  geom_point() + stat_smooth (method = "lm", formula = (y~x-1), se = FALSE) +
  geom_text(data = label2, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))
plot2

plot3 <- ggplot(allom_qsm_mauritia, aes(AGB.FieldHeight, AGB.WDGWDD)) +
  geom_point() + stat_smooth (method = "lm", formula = (y~x-1), se = FALSE) +
  geom_text(data = label3, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
      axis.text.x = element_text(colour = "grey40", size = 7),
      axis.title.y = element_text(colour = "grey10", size = 8),
      axis.text.y = element_text(colour = "grey40", size = 7),
      panel.background = element_rect(fill = "grey90"))
plot3

plot4 <- ggplot(allom_qsm_mauritia, aes(AGB.TLSHeight, AGB.WDGWDD)) +
  geom_point() + stat_smooth (method = "lm", formula = (y~x-1), se = FALSE) +
  geom_text(data = label4, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))
plot4

##put all the scatterplots together, showing correlation line
##and correlation value
grid.arrange(plot1, plot2, plot3, plot4, top = "Mauritia Flexuosa", ncol = 2, nrow = 2)


##Mauritiella species
##creating labels of correlation value to appear on graphs
label5 = data.frame(x = 43, y = 25, label = corr_eqn(allom_qsm_mauritiella$AGB.WDPerugov,
                                                        allom_qsm_mauritiella$AGB.FieldHeight))

label6 = data.frame(x = 47, y = 25, label = corr_eqn(allom_qsm_mauritiella$AGB.WDPerugov,
                                                       allom_qsm_mauritiella$AGB.TLSHeight))

label7 = data.frame(x = 43, y = 25, label = corr_eqn(allom_qsm_mauritiella$AGB.WDGWDD,
                                                       allom_qsm_mauritiella$AGB.FieldHeight))

label8 = data.frame(x = 47, y = 25, label = corr_eqn(allom_qsm_mauritiella$AGB.WDGWDD,
                                                       allom_qsm_mauritiella$AGB.TLSHeight))

##Mauritiella
head(allom_qsm_mauritiella)
plot5 <- ggplot(allom_qsm_mauritiella, aes(x=AGB.FieldHeight, y=AGB.WDPerugov)) +
  geom_point() + stat_smooth (method = "lm") +
  geom_text(data = label5, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_text(colour = "grey10", size = 8),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))

plot6 <- ggplot(allom_qsm_mauritiella, aes(AGB.TLSHeight, AGB.WDPerugov)) +
  geom_point() + stat_smooth (method = "lm") +
  geom_text(data = label6, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))

plot7 <- ggplot(allom_qsm_mauritiella, aes(AGB.FieldHeight, AGB.WDGWDD)) +
  geom_point() + stat_smooth (method = "lm") +
  geom_text(data = label7, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_text(colour = "grey10", size = 8),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))

plot8 <- ggplot(allom_qsm_mauritiella, aes(AGB.TLSHeight, AGB.WDGWDD)) +
  geom_point() + stat_smooth (method = "lm") +
  geom_text(data = label8, aes(x = x, y = y,
                               label = label), size = 3, parse = TRUE) +
  theme(axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))

##put all the scatterplots together, showing correlation line
##and correlation value
grid.arrange(plot5, plot6, plot7, plot8, top = "Mauritiella Armata", ncol = 2, nrow = 2)



###Mauritia flexuosa
##RMSE mauritia WDPeru(allometry) vs Fieldheight(QSM)
sqrt(mean((allom_qsm_mauritia$AGB.WDPerugov-allom_qsm_mauritia$AGB.FieldHeight)^2, na.rm = TRUE))

##RMSE mauritia WDPeru(allometry) vs TLSheight(QSM)
sqrt(mean((allom_qsm_mauritia$AGB.WDPerugov-allom_qsm_mauritia$AGB.TLSHeight)^2, na.rm = TRUE))

##RMSE mauritia WDGWDD(allometry) vs Fieldheight(QSM)
sqrt(mean((allom_qsm_mauritia$AGB.WDGWDD-allom_qsm_mauritia$AGB.FieldHeight)^2, na.rm = TRUE))

##RMSE mauritia WDGWDD(allometry) vs TLSheight(QSM)
sqrt(mean((allom_qsm_mauritia$AGB.WDGWDD-allom_qsm_mauritia$AGB.TLSHeight)^2, na.rm = TRUE))

###Mauritiella armata
##RMSE WDPeru(allometry) vs Fieldheight(QSM)
sqrt(mean((allom_qsm_mauritiella$AGB.WDPerugov-allom_qsm_mauritiella$AGB.FieldHeight)^2, na.rm = TRUE))

##RMSE WDPeru(allometry) vs TLSheight(QSM)
sqrt(mean((allom_qsm_mauritiella$AGB.WDPerugov-allom_qsm_mauritiella$AGB.TLSHeight)^2, na.rm = TRUE))

##RMSE WDGWDD(allometry) vs Fieldheight(QSM)
sqrt(mean((allom_qsm_mauritiella$AGB.WDGWDD-allom_qsm_mauritiella$AGB.FieldHeight)^2, na.rm = TRUE))

##RMSE WDGWDD(allometry) vs TLSheight(QSM)
sqrt(mean((allom_qsm_mauritiella$AGB.WDGWDD-allom_qsm_mauritiella$AGB.TLSHeight)^2, na.rm = TRUE))


###only selection the AGB from QSM and allometry
###that provided the most accurate estimations
###compared with Goodman

#creation of table with both bests AGBs

allom_qsm_bestfit <- data.frame("TreeID" = allom_qsm$TreeID,
                                "Species" = allom_qsm$Specie,
                                "AGB_QSM" = qsm2$AGB.WDGWDD,
                                "AGB_AE" = allom$AGB.TLSHeight)

#export to csv
write.csv(allom_qsm_bestfit, file = "OUTPUT/qsm_allom/AGB_allom_best.csv", row.names = FALSE)

#selection per species
bestfit_mauritia <- data.frame(allom_qsm_bestfit[which(allom_qsm_bestfit$Species == "Mauritia Flexuosa"),])
bestfit_mauritiella <- data.frame(allom_qsm_bestfit[which(allom_qsm_bestfit$Species == "Mauritiella Armata"),])

####bar plot comparing AGB QSM vs AGB Allometry

#melt
best.mauritia <- melt (bestfit_mauritia, id.vars = c("Species", "TreeID"))
head(best.mauritia) 

##make a bar plot of AGB separated by each species
#Mauritia
p_best <- ggplot(data=best.mauritia, aes(TreeID, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Species) +
  scale_fill_manual(name=NULL, values = c("AGB_QSM"="green4", "AGB_AE"="darkseagreen3")) +
  labs(x="Tree ID", y="AGB (kg)", angle = 90) +
  labs(title = "Comparison of best AGB from QSM and allometric equations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

p_best

##Mauritiella
#melt
best.mauritiella <- melt (bestfit_mauritiella, id.vars = c("Species", "TreeID"))
head(best.mauritiella)
#graph
p2_best <- ggplot(data=best.mauritiella, aes(TreeID, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Species) +
  scale_fill_manual(name=NULL, values = c("AGB_QSM"="green4", "AGB_AE"="darkseagreen3")) +
  labs(x="Tree ID", y="AGB (kg)", angle = 90) +
  labs(title = "Comparison of best AGB from QSM and allometric equations ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

p2_best




#scatterplot correlation

#function to calculate correlation
corr_eqn <- function(x,y, digits = 3) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

#correlation labels
label01 = data.frame(x = 900, y = 550, label = corr_eqn(bestfit_mauritia$AGB_AE,
                                                         bestfit_mauritia$AGB_QSM))

label02 = data.frame(x = 45, y = 48, label = corr_eqn(bestfit_mauritiella$AGB_AE,
                                                        bestfit_mauritiella$AGB_QSM))


##creation of scatterplots for AGB
#Mauritia

plot01 <- ggplot(bestfit_mauritia, aes(x=AGB_AE, y=AGB_QSM)) +
  geom_point() + stat_smooth (method = "lm", formula = (y~x-1), se = FALSE) +
  geom_text(data = label01, aes(x = x, y = y,
                               label = label), size = 5, parse = TRUE) +
  facet_grid(.~ Species) +
  labs(x = "AGB AE (kg)", y = "AGB QSM (kg)",
       title = "Best AGB estimates from QSM and allometric equations") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_text(colour = "grey10", size = 8),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))

plot01

#coefficient of determination 
r2_mf_AGB = lm(AGB_AE ~ AGB_QSM, data = bestfit_mauritia)
summary(r2_mf_AGB)$r.squared

cor(bestfit_mauritia$AGB_AE, bestfit_mauritia$AGB_QSM)

#Mauritiella
plot02 <- ggplot(bestfit_mauritiella, aes(x=AGB_AE, y=AGB_QSM)) +
  geom_point() + stat_smooth (method = "lm", formula = (y~x), se = FALSE) +
  geom_text(data = label02, aes(x = x, y = y,
                                label = label), size = 5, parse = TRUE) +
  facet_grid(.~ Species) +
  labs(x = "AGB AE (kg)", y = "AGB QSM (kg)",
       title = "Best AGB estimates from QSM and allometric equations") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(colour = "grey10", size = 8),
        axis.text.x = element_text(colour = "grey40", size = 7),
        axis.title.y = element_text(colour = "grey10", size = 8),
        axis.text.y = element_text(colour = "grey40", size = 7),
        panel.background = element_rect(fill = "grey90"))


plot02

r2_ma_AGB = lm(AGB_AE ~ AGB_QSM, data = bestfit_mauritiella)
summary(r2_ma_AGB)$r.squared
 