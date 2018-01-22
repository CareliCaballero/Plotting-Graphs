##Graph about releation between stem height and AGB using Field and TLS data
##for allometry results

##install library for graphs
library(ggplot2)

##read data

height.data <- read.csv("OUTPUT/allometry/AGB_allometry.csv", header = T)


##create table with the colums I require
head(height.data)
data.a <- data.frame(height.data, row.names=NULL)

##order by Specie
data.a[order(data.a$Specie),]


##make a graph stem vs AGB
graph.a <- ggplot(data.a) +
  geom_point(aes(TLS_TLength, AGB_TLS, fill="TLS"), colour="red", size=3) +
  geom_point(aes(Field_TLength, AGB_Field, fill="Field"), colour="blue", size=3) +
  scale_fill_manual(name=NULL, values = c("TLS"="red", "Field"="blue")) +
  scale_colour_manual(name=NULL, values = c("TLS"="red", "Field"="blue"),
                      labels=c("TLS","Field")) +
  theme(legend.position = "bottom") +
  labs(x="Stem height (m)", y="AGB (kg)") +
  ggtitle("Relation between stem height and AGB using Field and TLS data")
graph.a
