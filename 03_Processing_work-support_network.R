######################################################
#Author: Nicolas Gomez
#Created:  2/19/2020 ; Corvallis, Oregon 
#Last updated: 7/17/2022; Culebra, Puerto Rico = Revised the script to only publish data used in analysis for Frontiers in Marine Science Publication. 
#Purpose: Build, visualize and analyze several directed  networks from the relationships among the commercial fishers based on the question: 
#"Who do you receive support from useful for fishing?"
#and those that support them. 
#The first network is among fishers, including alters that were mentioned but don't meet the boundaries set for the snowball sampling. These are, nonetheless important when calculating indegree centrality among fishers. 
#The second network is the same as the first, but cutting off alter fishers outside the boundaries (thus bounded to the same nodes as the information-sharing network). This is the main network of analysis. 
#The third second is a multi-level network of institutional supporters across the pre-harvest, harvest and post-harverst stages of fishers work. The first level is work support
#received by egos (who are commercial fishers who harvest) and any other person who is a fisher (includes other ergo, but 
#also alters who nothing is known about them except their unique ID). Meanwhile, the cross-level ties are the work support 
#received by ego fishers from others in the supply chain (can be fishing association, an NGO, a manager, a mechanic, 
#a restaurant, a middleman, etc.) The reason both these people and entities fall into that samehigher-level is because they
#represent support beyond harvest phase. 

#--Preparation--------------------------------------------------------------------------
#packrat::disable() #don't run packrat in this script
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(devtools)
library(readxl)
library(ggnet)
library(xlsx)
library(dplyr)
setwd("C:/Users/nicol/Desktop/Repository_Social-Cohesion-Small-scale-Fisheries-Puerto-Rico")


#----Load edgelist as a dataframe--------------------------------------------------------------------------
w.df <- read.csv("data/03_Work_support.csv", header = TRUE, stringsAsFactors=F) #edgelist, multi-level network.  
is.data.frame(w.df) # is this a dataframe? yes!
head(w.df)
summary(w.df)

#----Dataframe Formating------------------------------------------------------------------------
# There are multiple exchanges per dyad, need to account for this. 
#AKA, need to change values in "work.support.type" column,into a single edge (row), 
#where the presence, absence of an edge type is recorded as a column (each edge 
#type is a column). 
#view(w.df)
#Group ID columns: 
grp_w_cols <- names(w.df)[c(1,2)]

#Convert character vector to list of symbols: 
dots <-lapply(grp_w_cols,as.symbol)
#view(w.df)
#Make sure Value column is numeric
w.df$support.value <- as.numeric(w.df$support.value)
w.df2 <- w.df  %>% 
  group_by(.dots=dots,ID.j.category) %>%  #group unique i and j 
  dplyr::summarise(
    num.ties = n(), #Trying this to generate number of ties per dyad as a valued edge attribute
    ID.j.boundaries = any(ID.j.boundaries=="inside"), #for bounded network, inside = TRUE
    Value = max(support.value, na.rm = TRUE), #choose the highest value (high, moderate or low) for all types of support shared as the dyad's value. Remove NA's for proper computing. 
    Tension = any(tension == "present"),    #choose to highlight presense of tension, present = TRUE
    Kinship  = any(kinship == "yes"), # identify dyads between family members
    Diversifying.catch = sum(support.type == "Diversifying species that I can catch"),
    Expanding.fishing.range = sum(support.type == "Expanding my fishing range"),
    Fishing.more = sum(support.type == "Fishing more than I can alone"), #aka, "catching more"
    Preparing.gear = sum(support.type == "Preparing gear"),
    Repairing.gear = sum(support.type == "Repairing gear"),
    Sourcing.gear = sum(support.type == "Sourcing gear"),
    Sharing.gear = sum(support.type == "Sharing gear"),
    Finding.lost.gear = sum(support.type == "Finding lost gear"),
    Selling.locally  = sum(support.type == "Selling locally"),
    Selling.beyond.usual.buyers = sum(support.type == "Selling beyond my usual buyers"),
    Reselling.catch = sum(support.type == "Re-selling catch"),
    Assistance.licenses= sum(support.type == "Assistance with licenses"),
    Alternate.occupation= sum(support.type == "Having an alternate occupation"),
    Assitance.fisheries.aid = sum(support.type == "Assistance with fisheries aid"),
    Maritime.safety = sum(support.type == "Improving Maritime safety"),
    Collective.action = sum(support.type == "Collective Action"),
    Improving.finances= sum(support.type == "Improving Finances"),
    Avoiding.conflict = sum(support.type == "Avoiding Conflict"),
    Gutting.fish = sum(support.type == "Gutting and Scaling fish")
  )


rm(w.df) # do not need i.df any longer. So, clear out of environment
number_ties <- (w.df2$num.ties)
mean(number_ties) #average number of ties is 1.58147
#view(w.df2)


#Make sure characters are not numeric: 
w.df2$Tension <- as.character(w.df2$Tension)
#TRUE = presence of tension ; FALSE = absence

#----Change column order-------------
w.df2 <-  w.df2 %>% relocate (ID.j.category, .after = ID.j) #ID needs to be first column.
#NOTE: Directionality cannot be changed before node attributes are added. 
#view(w.df2)


#----Split dataframe according to levels in the network (bipartite or not) ----------------------------
library(tidyverse)

#Create dataframe just for fisher-fisher work-support relationships (for now both bounded and non-bounded networks)[acronym: FF.ws.df]. Fishers could be egos or alters
FF.ws.df <-dplyr:: filter(w.df2, ID.j.category %in% c("ego.fisher", "alter.fisher"))

#view(FF.ws.df)

#Create dataframe just for fisher-other supporters (bipartite network)  [acronym: FML.ws.df]
FO.ws.df <-dplyr:: filter(w.df2, ID.j.category %in% c("other.supporter"))

#view(FO.ws.df)
#FO.ws.df <- subset(w.df2, ID!=ID.j)  # remove rows where source==target
#FF.ws.df <- subset(w.df2, ID!=ID.j)  
#remove dataframes no longer needed: 


#--Create networks in igraph, add node attributes for egos ----
#Load package for igraph networks: 
library(igraph)
#NOTE: There are some fishers in the node attributes dataframe that did not report work support (either of other fishers or institutional),  and therefore not in these other dataframes. 
#Need to filter accordingly. 


#-------Fisher-Other (institutional) supporter, Bipartite network (i.FO.ws.sna)------------
library(dplyr)
i.node.FO.df <- read_xlsx("data/04_Institutional_Supporter_Node_Attributes.xlsx")#importing nodes attributes for both fishers and institutions
#view(i.node.FO.df)
#view(FO.ws.df)
#These egos do not appear in the FO network: 2,7,19,20. Futhermore, several supporters that were recorded as present in the landing sites, were not mentioned by any fishers, 
#likely due to changes in support due to COVID-19. Need to subsset them fromt he node attributes. 
#Filter rows:
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "2") #mentioned zero institutional supporters
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "7") #mentioned zero institutional supporters
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "19") #mentioned zero institutional supporters
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "20") #mentioned zero institutional supporters
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "P7") #not mentioned
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "Pre6")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "R16") 
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "R19") #not mentioned
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "R20")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "Pre4")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "P11")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "P12")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "P13")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "10") #mentioned zero institutional supporters
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "45") #mentioned zero institutional supporters
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "P6") #mobile sifhs shop
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "R8") #not mentioned
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "Ba1") #not mentioned
#Eliminate egos from the SAR landing site, since it's not longer relevantfor the study: 
FO.ws.df  <- subset(FO.ws.df,FO.ws.df$ID != "21") 
FO.ws.df  <- subset(FO.ws.df,FO.ws.df$ID != "22")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "21") 
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "22")
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "R11") #buyer of SAR
i.node.FO.df  <- subset(i.node.FO.df,i.node.FO.df$ID != "P4") #fish shop of SAR

#Change directionality of the network:
#by switching the first (sender) and second (reveiver) rows. This is the way edge networks are read. 
#This must be done because the question asked for work support received, not given, by egos. 
FO.ws.df <-  FO.ws.df %>% relocate (ID.j, .before =ID)
#Now ID.j (instituaional supporters) are the sender of "work support" and ID (fisher) are receivers. 

#view(FO.ws.df)
#view(i.node.FO.df)
#Rename Node attribute s"ID" columns to "ID.j" to match new order of edgelist: 
i.node.FO.df <-i.node.FO.df %>% 
  dplyr::rename(ID.j = ID)

#IF there is an error in the creation fo the network, this is a potential workabout: 
#F NEED: If vertices is not NULL, then it must be a data frame giving vertex metadata. The first column of vertices is assumed to contain symbolic vertex names, 
#this will be added to the graphs as the ‘name’ vertex attribute. Other columns will be added as additional vertex attributes. If vertices is not NULL then 
#the symbolic edge list given in  d (edgelist dataframe) is checked to contain only vertex names listed in vertices.
#?graph.data.frame says d is "a data frame containing a symbolic edge list in the first two columns".
#merge both data.frames and sett all=TRUE, in order to fix the problem that there are IDs in the edlist not in the vertices:  
ed = data.frame(ID.j=unique(as.character(unlist(FO.ws.df[, c("ID.j", "ID")]))))
#view(ed)
nodesFOi <- merge(ed, i.node.FO.df, by="ID.j", all=TRUE) #ID.j fishers

#view(nodesFOi)
#Change to igraph network: 
i.FO.ws.sna <- igraph:: graph_from_data_frame(d= FO.ws.df, directed = TRUE, vertices = nodesFOi)
#, vertices = nodesFOi) #convert to igraph network using the new dataframe


#summary(i.FO.ws.sna)
#Exploratory plot: 
library(RColorBrewer)
#col.site <- colorRampPalette("Dark2")(4)
brewer.pal(n = 8, name = "Set1")
colrs   <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
V(i.FO.ws.sna)$color.ls <- colrs[V(i.FO.ws.sna)$landing.site]

i.ls <- V(i.FO.ws.sna)$landing.site
col.site <- brewer.pal(length(unique(i.ls)), "Dark2")
col.site
plot(i.FO.ws.sna, layout =layout_nicely, 
     #vertex.color = V(i.FO.ws.sna)$color.ls, 
     vertex.cex = 0.5, 
     vertex.label.cex = 0.5, 
     edge.arrow.size = 0.5)
#edge.arrow.width = E(i.FO.ws.sna)$Value,
#edge.lty =  E(i.FO.ws.sna)$Tension,
#edge.color= k[E(i.FO.ws.sna)$Kinship])

#---Convert network to statnet package: -----------------------------
library(statnet)
library(intergraph)
library(RColorBrewer)
library(network)
library(sna)
library(GGally) #has the gnnet function
library(ggraph)
detach(package:igraph)
#library(mlergm)
#Convert networks from igraph to statnet via intergraph: 
#using the asNetwork() function produced errors when trying obtain summaries on the new, network object. 
#Therefore, convert from dataframe instead:


#Fisher-Other institutional supporters  network: 
ws.FO.sna <- asNetwork(i.FO.ws.sna) #not two-mode yet
class(ws.FO.sna) #network OK
is.bipartite(ws.FO.sna) #FALSE
#Intergraph does not preserva bipartite status of networks. 
#summary(ws.FO.sna, print.adj = F)
detach(package:intergraph)
#---Split institutional support network by landing site with ggnet2: ----------


#Step 1: Split by landing sites: #This ignores instituional alters since they don't have a landing site expecified. 
#problem is they span more than one landin site!


CRO.ws.FO.sna <- network:: get.inducedSubgraph(ws.FO.sna, 
                                               which(ws.FO.sna %v% "ls.in.CRO" == "CRO"))
summary(CRO.ws.FO.sna, print.adj=F)
#CRO.ws.FO.sna[,]
#eid=(ws.FO.sna %e%'Value'))
CUL.ws.FO.sna <- network:: get.inducedSubgraph(ws.FO.sna, 
                                               which(ws.FO.sna %v% "ls.in.CUL" == "CUL"))
summary(CUL.ws.FO.sna, print.adj=F)
MAT.ws.FO.sna <- network:: get.inducedSubgraph(ws.FO.sna, 
                                               which(ws.FO.sna %v% "ls.in.MAT" == "MAT"))
summary(MAT.ws.FO.sna, print.adj=F)
#eid=(ws.FO.sna %e%'Value'))
HUC.ws.FO.sna <- network:: get.inducedSubgraph(ws.FO.sna, 
                                               which(ws.FO.sna %v% "ls.in.HUC" == "HUC"))
#eid=(ws.FO.sna %e%'Value'))
summary(HUC.ws.FO.sna, print.adj=F)


#-----Summarizing descriptive statisitcs of Institutional supporters by landing site: -----
#Overall: 
summary(ws.FO.sna, print.adj=F)
I.ws.FO.sna <- network:: get.inducedSubgraph(HUC.ws.FO.sna, 
                                             which(HUC.ws.FO.sna %v% "mode.shape" == "Institutional supporter"))
summary(I.ws.FO.sna, print.adj=F)

R.ws.FO.sna <- network:: get.inducedSubgraph(ws.FO.sna, 
                                             which(ws.FO.sna %v% "type.of.institutional.supporter" == "Restaurant"))

summary(R.ws.FO.sna, print.adj=F)


#--Graph Figure 3-----------------------------------------

#Packages:
library(dplyr)
library(ggsci)# nature journal palette: scale_color_npg() 	 #the palette type is: "nrc" #palette geneator is:al_npg()
library(RColorBrewer)
library(patchwork) #to output letters on each panel in combination with ggplot2 
library(oaxaca)
library(forcats)
library(hrbrthemes)
library(ggthemes)
library(scales)
library(gridExtra)
library(ggpubr) 
library(cowplot)
library(readxl)
#Import: 

WStie.counts.df  <- read_xlsx("data/05_Ties_types_counts_work_support.xlsx")


#create objects: 
#WS: 
WS_Category <- WStie.counts.df$WS_Network_Category
Work_support_types <- WStie.counts.df$Work_support_types
WS_Counts <- WStie.counts.df$WS_Counts
WS_time   <- WStie.counts.df$WS_time
WS_meta_category <- WStie.counts.df$WS_meta_category


#Barplot of frequency of ties: 


#Order factors for pleasing easthethic wrapping: 
library(dplyr)
neworder <- c("Pre-harvest","Harvest","Post-harvest")


WStie.counts.df2 <- transform(WStie.counts.df, 
                              WS_meta_category=factor(WS_meta_category,
                                                      levels=neworder))

WS.g <- ggplot(WStie.counts.df2,
               aes(x = reorder(Work_support_types, WS_Counts), y= WS_Counts,
                   fill= WS_Category)) +
  geom_bar(position = "stack", stat = "identity", show.legend = TRUE) +
  theme_bw()+
  theme(legend.position = c(0.7, 0.85),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key.height=unit(0.1,"cm"))+
  ylab("Frequencies") +
  xlab("Type of tie")+
  facet_wrap(~WS_meta_category, nrow =4, scales = "free_y")+
  #dir= "v", as.table= FALSE)+
  coord_flip()


#WS.g
#ggsave("figures/work_support_freq.png", width = 9, height = 8, dpi = 300)
ws.g2 <- ggarrange(WS.g, 
                   labels = c("a)", "b)", "c)",
                              label.x = c(0,0,0),
                              label.y = c(0.33,0.66,1),
                              vjust= 1.45,
                              hjust= 0,
                              ncol = 1, nrow = 3,
                              align = "v",
                              heights = c(0.33, 0.66, 1)))
#ws.g2




#--Graph Figure 4----
library(RColorBrewer)

#set.edge.attribute(CUL.ws.FO.sna, "ecolor",  ifelse(CUL.ws.FO.sna %e% "tension" > "brickred", "grey50", "grey50"))

CUL <- ggnet2(CUL.ws.FO.sna, 
              mode = "fruchtermanreingold",
              shape = "mode.shape",
              shape.palette = c("Harvesting fisher" = 19, "Institutional supporter" = 15),
              node.color = "so.color", palette = c("Independent" = "steelblue", "Associated" = "tomato",
                                                   "Pre-harvest" ="darkgrey", "Post-harvest"="darkgrey", 
                                                   "Pre and post-harvest"= "darkgrey"
              ), #gold and orchid were the previous colors. 
              node.size = 6, 
              label = TRUE,
              label.size = 4,
              arrow.gap = 0.02,
              arrow.size = 5,
              edge.size ="Value",
              #edge.label = "Value", 
              #edge.label.size = 4,
              edge.alpha = 0.25,
              edge.color = "grey50")+ #color
  ggtitle("a) Culebra")+
  theme(legend.title = element_blank())+
  guides(size = F, linesize= F,color=F, shape=F)+
  theme(panel.background = element_rect(color = "white"))

CUL
MAT <- ggnet2(MAT.ws.FO.sna, 
              mode = "fruchtermanreingold",
              shape = "mode.shape",
              shape.palette = c("Harvesting fisher" = 19, "Institutional supporter" = 15),
              node.color = "so.color", palette = c("Independent" = "steelblue", "Associated" = "tomato",
                                                   "Pre-harvest" ="darkgrey" , "Post-harvest" ="darkgrey", 
                                                   "Pre and post-harvest" = "darkgrey"),
              node.size = 6, 
              label = TRUE,
              label.size = 4,
              arrow.gap = 0.02,
              arrow.size = 5,
              edge.size ="Value",
              #edge.label = "Value", 
              #edge.label.size = 4,
              edge.alpha = 0.25,
              edge.color = "grey50")+
  ggtitle("b) Maternillo")+
  theme(legend.title = element_blank())+
  guides(size = FALSE, color=FALSE, shape=FALSE)+
  theme(panel.background = element_rect(color = "white"))

#MAT
CRO <- ggnet2(CRO.ws.FO.sna, 
              mode = "fruchtermanreingold",
              shape = "mode.shape",
              shape.palette = c("Harvesting fisher" = 19, "Institutional supporter" = 15),
              node.color = "so.color", palette = c("Independent" = "steelblue", "Associated" = "tomato",
                                                   "Pre-harvest" ="darkgrey" , "Post-harvest" ="darkgrey", 
                                                   "Pre and post-harvest" = "darkgrey"),
              node.size = 6, 
              label = TRUE,
              label.size = 4,
              arrow.gap = 0.02,
              arrow.size = 5,
              edge.size ="Value",
              #edge.label = "Value", 
              #edge.label.size = 4,
              edge.alpha = 0.25,
              edge.color = "grey50")+
  ggtitle("c) Las Croabas")+
  theme(legend.title = element_blank())+
  guides(size = FALSE, color=FALSE, shape=FALSE)+
  theme(panel.background = element_rect(color = "white"))

#CRO
HUC <- ggnet2(HUC.ws.FO.sna, 
              mode = "fruchtermanreingold",
              shape = "mode.shape",
              shape.palette = c("Harvesting fisher" = 19, "Institutional supporter" = 15),
              node.color = "so.color", palette = c("Independent" = "steelblue", "Associated" = "tomato",
                                                   "Pre-harvest" ="darkgrey" , "Post-harvest" ="darkgrey", 
                                                   "Pre and post-harvest" = "darkgrey"),
              node.size = 6, 
              label = TRUE,
              label.size = 4,
              arrow.gap = 0.02,
              arrow.size = 5,
              edge.size ="Value",
              edge.alpha = 0.25,
              edge.color = "grey50",
              #edge.label = "Value", 
              #edge.label.size = 4,
              legend.size = 12, 
              legend.position = "bottomleft") +
  theme(legend.title = element_blank(),
        panel.background = element_rect(color = "white"),
        legend.position="right", 
        legend.box="vertical", 
        legend.margin=margin())+
  ggtitle("d) Hucares")+
  guides( linetype = guide_legend(label = TRUE),
          color = guide_legend(
            override.aes = list(
              shape = c(
                "Independent" = 19, #circle
                "Associated" = 19,
                "Pre-harvest" = 15, #square
                "Post-harvest" =15,
                "Pre and post-harvest" = 15), size = 5),
            nrow = 8, ncolumn = 1, byrow = F), shape= guide_legend(
              nrow = 2, ncolumn = 1, byrow = F, override.aes=list(size=5)))

HUC        

library(patchwork) #to output letters on each panel in combination with ggplot2 
multiplot.full.network <- (CUL | MAT) / (CRO | HUC)
multiplot.full.network

#(CUL + MAT + CRO + HUC) + plot_layout(guides = "collect")
#ggsave("figures/001_support_grey.png", width = 12, height = 9, dpi = 300, units = "in", device='png')

