library(knitr)
library(plyr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggplot2)

pokojnine <- filter(splosni, K4_NAME == "Prispevek za pokojninsko in invalidsko zavarovanje" | K4_NAME == "Premije kolektivnega dodatnega pokojninskega zavarovanja, na podlagi ZKDPZJU")
pokojnine <- subset(pokojnine, select = c("K4_NAME", "SPS2015", "SPS2016", "SPS2017",
                                          "SPS2018", "SPS2019"))
names(pokojnine) <- c("Prispevki", "2015", "2016", "2017", "2018", "2019")
pokojnine <- gather(pokojnine, "2015", "2016", "2017", "2018", "2019", key="Leto", value="Vsota")

graf_pokojnine <- ggplot(pokojnine, aes(x=Leto, y=Vsota, fill = Prispevki))+
                  geom_bar(position="dodge", stat="identity")+
                  #labs(title ="Prispevki za pokojnine")+
                  scale_y_continuous(breaks=seq(0,130000000,20000000), 
                                     labels = scales::number_format(accuracy = 1))+
                  theme(legend.position = "bottom") + scale_fill_discrete(name = "")

########################################################################################
prvo_leto_pokojnine <- subset(prvo_leto_pokojnine, select= c("GEO", "STATINFO", "Value"))
prvo_leto_pokojnine <- subset(prvo_leto_pokojnine, Value <= 58)
names(prvo_leto_pokojnine) <- c("GEO", "Kazalnik", "Leto")
graf_prvo_leto_pokojnine <- ggplot(prvo_leto_pokojnine, aes(x=GEO, y=Leto, col= Kazalnik, fill=Kazalnik))+ 
                            geom_point(size = 5)+ labs(x="Država", y="Leto prve pokojnine")#+
                            #theme(axis.text = element_text(face = "bold", size = 12)
graf_prvo_leto_pokojnine

#########################################################################################
procentualno_pokojnina <- subset(procentualno_pokojnina, select = c("GEO", "Value"))
procentualno_pokojnina <- subset(procentualno_pokojnina, Value >= 46 & Value <= 55)
procentualno_pokojnina <- arrange(procentualno_pokojnina, desc(Value))
graf_procentualno_pokojnina <- ggplot(procentualno_pokojnina, aes(x=GEO, y=Value))+
                                geom_point(size=5, color = 'skyblue')+ labs(x="Država", y="Delež")+
                                labs(title ="Delež ljudi med 50 in 69 let, ki dobivajo pokojnino")
graf_procentualno_pokojnina
