######################################################
#Author: Nicolas Gomez
#Contact: gomezann@oregonstate.edu
#Created:  2/19/2020 ; Corvallis, Oregon 
#Last updated: 7/17/2022; Culebra, Puerto Rico = Revised the script to only publish data used in analysis for Frontiers in Marine Science Publication. 
#Purpose: Build, visualize, and analyze information sharing
          #among the commercial fishers of eastern Puerto Rico
#Title: Exploring info-sharing networks of PR Fisheries
#Trick: for Windows, Alt O will collapse the markdown tabs. 

#---Preparation ------------------------------------------------------------------------
#packrat::disable() #don't run packrat in this script
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(devtools)
library(igraph)
library(readxl)
library(dplyr)

setwd("C:/Users/nicol/Desktop/Repository_Social-Cohesion-Small-scale-Fisheries-Puerto-Rico")


#-----Load edgelist as a dataframe:-----------------------------------------------------------------------
i.df <- read_xlsx("data/02_Info_sharing.xlsx") #all dyads whose egos and alters meet the a-priori boundaries. 
i.df
is.data.frame(i.df) # is this a dataframe? yes!
head(i.df)
summary(i.df)

#-----Data Formating-----------------------------------------------------------------------
# Problem: There are multiple exchanges per dyad, need to account for this. 
#AKA, need to change values in "info_sharing_type" column,into a single edge (row), 
#where the presence, absence of an edge type is recorded as a column (each edge 
#type is a column). 

#Make sure Value column is numeric
i.df$info.sharing.value <- as.numeric(i.df$info.sharing.value)
#Group ID columns: 
grp_cols <- names(i.df)[c(1,2)]

#Convert character vector to list of symbols: 
dots <-lapply(grp_cols,as.symbol)

i.df2 <- i.df  %>% 
  group_by(.dots=dots) %>%  #group unique i and j
  dplyr::summarise(
    num.ties= n(), #Trying this to generate number of ties per dyad as a valued edge attribute
    Pre.harvest = sum(info.sharing.type == "Pre-harvest"), # counts number of info sharing acts per type of info shared per dyad
    Fishing = sum(info.sharing.type == "Fishing"),
    Post.harvest  = sum(info.sharing.type == "Post-harvest"),
    Opportunities = sum(info.sharing.type == "Opportunities"),
    Problems = sum(info.sharing.type == "Problems"),
    Advice = sum(info.sharing.type == "Advice"),
    Kinship  = any(kinship == "yes"), # identify dyads between family members
    Frequency = max(info.sharing.frequency, na.rm = TRUE), #choose the most frequent interaction between dyads 
    #(sometimes it differed by info sharing type). That is sometimes the frequency of fishing info was not the same as that of post-harvest, but this was not common. 
    Value = max(info.sharing.value, na.rm = TRUE) #same as with frequency. 
  )

#view(i.df2)
rm(i.df) # do not need i.df any longer. So, clear out of environment

number_ties <- (i.df2$num.ties)
mean(number_ties) #average number of ties is 2.23
sd(number_ties) #standard deviation. 1.465

#There are some NA is the Frequency and Value edge attributes. Need to change this or summary statistics won't be calculated. Since there is no data here, the values will be change to zeros. 
#Change NA to zero: 
#i.df2$Value[is.na(i.df2$Value)] <- 0
#i.df2$Frequency[is.na(i.df2$Frequency)] <- 0

#Change to numeric edge attributes: 
i.df2$Value <- as.numeric(i.df2$Value)
i.df2$Frequency <- as.numeric(i.df2$Frequency)

#-----Convert from dataframe into network format & Add Node Attributes--------------------------------------------------------------------
i.node.df <- read_xlsx("data/01_Fisher_Node_attributes_after_de-identification.xlsx") #importing anonimized (de-identified) node (fisher) attributes for both egos and alters
#Note:after EDA, N/As were exported to excel as blank cells and manually converted back to N/As in Excel. 
#head(i.node.df)
#view(i.node.df)
#view(i.df2)

#Option 1 to add node attrbitues to the network: 


#Step 1: (if needed) Subset all nodes that are not present in the network, by filtering rows: 

#i.node.df  <- subset(i.node.df, i.node.df$ID != "14") #this ego is not mentioned for this network
#.node.FF.bounded.df  <- subset(i.node.df,i.node.df$ID != "15") #this ego is not mentioned for this network
#i.node.df  <- subset(i.node.df,i.node.df$ID != "55") #this ego is not mentioned for this network
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A66")  #this alter is not mentioned for this network
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A70")  #this alter is not mentioned for this netowork
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A16") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A18") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A19") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A20") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A23") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A32") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A34") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A35") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A44") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A45") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A50") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A51") 
#i.node.df  <- subset(i.node.df,i.node.df$ID != "A55") 

#Step 2: Rename Node attribute s"ID" columns to "ID.sender" ti match new order of edgelist: 
i.node.df <- i.node.df %>% 
  dplyr::rename(ID.sender = ID)
#Step 3: Make sure edgelist and node list match: 

#?graph.data.frame says: If vertices is not NULL, then it must be a data frame giving vertex metadata. The first column of vertices is assumed to contain symbolic vertex names, 
#this will be added to the graphs as the ‘name’ vertex attribute. Other columns will be added as additional vertex attributes. If vertices is not NULL then 
#the symbolic edge list given in  d (edgelist dataframe) is checked to contain only vertex names listed in vertices.
# d is "a data frame containing a symbolic edge list in the first two columns".
#If vertices is not NULL, then it must be a data frame giving vertex metadata. The first column of vertices is assumed to contain symbolic vertex names, 
#this will be added to the graphs as the ‘name’ vertex attribute. Other columns will be added as additional vertex attributes. If vertices is not NULL then 
#the symbolic edge list given in  d (edgelist dataframe) is checked to contain only vertex names listed in vertices.

ed = data.frame(ID.sender =unique(as.character(unlist(i.df2[, c("ID.sender", "ID.receiver")])))) ; nodesIS <- merge(ed, i.node.df, by="ID.sender", all=TRUE) 

#Create the graph: 
i_sna <- graph_from_data_frame(i.df2, directed= TRUE, vertices = nodesIS)#convert to igraph network using the new dataframe

summary(i_sna) #335 ties and 76 nodes (both egos and alters). 
ecount(i_sna) #335 ties  / size of network  (number of edges) 
print(i_sna)
graph.density(i_sna) #0.05877



#Step 4: Make value (numeric) objects of the vertex attrbitues to ease visualization: 
i.sampled <- V(i_sna)$sampled
i.gear <- V(i_sna)$gear
i.municipality <- V(i_sna)$municipality 
i.landing.site <- V(i_sna)$landing.site
i.membership<- V(i_sna)$membership 
i.so.arrangement <- V(i_sna)$so_arrangement 
i.license<- V(i_sna)$license  
i.sex<- V(i_sna)$sex   


summary(i_sna)
#-----Exploring Snowball sampling waves--------
library(RColorBrewer)

IS_waves <- ggplot(i.node.df, aes(fill= landing.site, x = Sampling.wave)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_fill_brewer(palette = "Dark2")+
  #geom_bar(position = "stack", stat = "identity", show.legend = TRUE) +
  ylab("Proportion of fishers inside boundaries \n (egos and alters)") +
  xlab("Sampling Waves")+
  theme_bw()+
  theme(legend.title = element_text("Landing Site"))

#IS_waves

#ggsave("figures/Sampling_wavea_IS.png")
rm(i.node.df)

#summary(i_sna)
#--Convert to statnet package for easier control of visualizations----------------
library(intergraph)
library(statnet)
library(network)
library(sna)
library(GGally)
library(ggraph)


#changing from igraph to statnet via intergraph: 
#using the asNetwork() function produced errors when trying obtain summaries on the new, network object. 
#Therefore, convert from dataframe instead: 
#l <- asDF(i_sna)
#i.snal <- asNetwork(l$edges, directed=TRUE, l$vertexes)
#i.snag      <-  igraph.to.graphNEL(i_sna)
#class(i.snag) #grpah, not network
#Check if the transformedd edgelist is identical: 
identical(i.snal, i.sna) #NO

i.sna <- intergraph::asNetwork(i_sna) 

class(i.sna)
summary(i.sna, print.adj = F)

#FALSE ; I don't know why, though


detach(package:intergraph)
detach(package:igraph)



rm(i_sna) #remove igraph network from environment for now. 


#-----Exploratory degree measurements of full network-------------------------------
i.sna #already has edge and vertex attributes. 
list.vertex.attributes(i.sna)
list.edge.attributes(i.sna)
network.size(i.sna) #76
summary(i.sna,print.adj=FALSE)
# 76 nodes, 331 edges
#it is a directed network
#Network Density = 0.058  (sparse)
#33 kinship ties 
#61 egos, 15 alters
#
#40 independents and 36 associated fishers (egos and alters)
#mix of integer valued attributes and logical valued attributes

#Calculating degree, closeness, eigenvector and betweenness centrality and 
#inserting it into the network: 
set.vertex.attribute(i.sna, "degree_centrality", degree(i.sna, gmode ="graph", cmode ="indegree")) 
set.vertex.attribute(i.sna, "closeness_centrality", closeness(i.sna, gmode="graph"))
set.vertex.attribute(i.sna, "betweenness_centrality", betweenness(i.sna, gmode="graph"))
set.vertex.attribute(i.sna, "eigenvector_centrality", evcent(i.sna, gmode = "graph"))
set.vertex.attribute(i.sna, "outdegree_centrality", degree(i.sna, cmode ="outdegree"))


#Export node attribtute for statisits elsewhere 
library(dplyr)
library(tidyr)
library(xlsx)
set.vertex.attribute(i.sna, "outdegree_centrality", degree(i.sna, cmode ="outdegree"))


Outdegree_IS.df <- data.frame(
  ID = i.sna %v% "vertex.names",
  landing.site = i.sna %v% "landing.site",
  so_arrangement = i.sna %v% "so_arrangement",
  membership = i.sna %v% "membership",
  outdegree_centrality_IS = i.sna %v% "outdegree_centrality") 
#view(Outdegree_IS.df)
#write.xlsx(Outdegree_IS.df, "data/02_Exploratory_Data_Analysis/09_Outdegree_centrality_infosharing.xlsx")
rm(Outdegree_IS.df)

#Create a vectors out of degree centrality calculations, in order to use in plotting easily in statnet
deg_cent_full <- degree(i.sna, gmode="graph")
closs_cent_full <-  closeness(i.sna, gmode="graph")
bet_cent_full <- betweenness(i.sna, gmode="graph")
eigen_cent_full <- evcent(i.sna, gmode = "graph")

#New DF
centralities.info.sharing.df <- data.frame(
  ID = i.sna %v% "vertex.names",
  In.degree.centrality = degree(i.sna, gmode="graph", cmode ="indegree"),
  Out.degree.centrality =  degree(i.sna, gmode="graph", cmode ="outdegree"))

#Boxplots - 


#-----Full network visualizations in Statnet, GGally, ggnet and ggraph-------------------------------
#--------I. Add color palette for the node attributes -------
library(wesanderson)
#darj5 <- wes_palette("Darjeeling1",5, type="discrete") 
col.mun <- wes_palette("Zissou1", 5, type="discrete")
library(RColorBrewer)
col.site <- brewer.pal(length(unique(i.landing.site)), "Dark2")
col.gear <- colorRampPalette("Set1")(3)

#-------II. Save coordinates to standarize vis of plots------------------------ 
#For gplot: 
mycoords.i <- gplot(i.sna, gmode="graph", vertex.cex=1.5) #for gplot

#For ggnet2: (or put set.seed(1)in front of every plot)
x = gplot.layout.fruchtermanreingold(i.sna, NULL)
i.sna %v% "x" = x[, 1]
i.sna %v% "y" = x[, 2]
g<- ggnet2(i.sna, color = "landing.site",mode = c("x", "y")) #Trial


#-------Graph Figure 2: Node attributes in info-sharing network-------------------------------
#install.packages(patchwork)
library(patchwork) #to output letters on each panel in combination with ggplot2 
library(ggplot2)
library(RColorBrewer)

#Landing site
g.site <- ggnet2(i.sna, color = "landing.site",
                 size = "degree_centrality", palette = "Dark2", arrow.gap = 0.02,
                 arrow.size = 5, edge.alpha = 0.25,
                 mode = c("x", "y"),
                 edge.color = c("color", "grey50"),
                 label = TRUE,
                 label.color = "black",
                 label.size = 2.5)+
  theme(legend.title = element_blank())+
  ggtitle("a) Landing site")+
  guides(size = FALSE)



#Gear type: 
g.ge <- ggnet2(i.sna, node.color = "primary.gear",
               size = 3, arrow.gap = 0.02,
               palette = "Set3",
               arrow.size = 5, edge.alpha = 0.25,
               mode = c("x", "y"),
               edge.color = c("color", "grey50"),
               label = TRUE,
               label.color = "black",
               label.size = 2.5) +
  theme(legend.title = element_blank())+
  ggtitle("b) Primary Gear")+
  guides(size = FALSE)


#Self-organizational arrangment (Associated vs. Independent) and which association
g.org<- ggnet2(i.sna, node.color = "membership",
                size = 3, palette = "Set1", arrow.gap = 0.02,
                arrow.size = 5, edge.alpha = 0.25,
                mode = c("x", "y"),
               edge.color = c("color", "grey50"),
               label = TRUE,
               label.color = "black",
               label.size = 2.5)+
               ggtitle("c) Organizational arrangement")+
               theme(legend.title = element_blank())+
               guides(size = FALSE)



#ggsave("figures/001_full_info_sharing_network_node_attrbitutes_patterns.png", width = 10, height = 8, dpi = 300, units = "in", device='png')
m2 <- (g.site /g.ge /g.org)

#ggsave("figures/00002_info_sharing_network.png", width = 14, height = 6, dpi = 300, units = "in", device='png')

#Caption: Figure 2: Graphs of the information sharing network amoung commercial 
#fishers of eastern Puerto Rico, highlighting key node attributes.
#CUL and HUC are close-knit communities of independent and associated fishers, 
#respectily, but don't chare gear homophily. Meanwhile, the landing sites in Fajardo, 
#are more spread out. This is because they include a lot of transiet independent fishers, but even amoung associations (P2 and P3), close relationships are lacking. 


#--Split network by landing site: --------------

#Subset network according to landing sites: 
CRO.sna <- get.inducedSubgraph(i.sna, 
                               which(i.sna %v% "landing.site" == "Las Croabas (CRO)"))

CUL.sna <- get.inducedSubgraph(i.sna, 
                               which(i.sna %v% "landing.site" == "Culebra (CUL)"))

HUC.sna <- get.inducedSubgraph(i.sna, 
                               which(i.sna %v% "landing.site" == "Hucares (HUC)"))

MAT.sna <- get.inducedSubgraph(i.sna, 
                               which(i.sna %v% "landing.site" == "Maternillo (MAT)"))

SAR.sna <- get.inducedSubgraph(i.sna, 
                               which(i.sna %v% "landing.site" == "Sardinera (SAR)"))

#--Split network by municipality (just Fajardo needed)---------- 
FAJ.sna <- get.inducedSubgraph(i.sna, 
                               which(i.sna %v% "municipality" == "Fajardo"))

#--Split network work by self-organization arrangement (in Fajardo = P2, P3 and P6, and in HUC P1----------
mean(i.sna %e% "num.ties") #2.22
sd(i.sna %e% "num.ties") #1.465


P1.HUC.sna <- get.inducedSubgraph(i.sna, 
                                  which(i.sna %v% "membership" == "Association P1")) #HUC

summary(P1.HUC.sna %e% "num.ties") #average 2.85 ties HUC associated fisher
sd(P1.HUC.sna%e% "num.ties") #standard deviation 1.094

P2.CRO.sna <- get.inducedSubgraph(i.sna, 
                                  which(i.sna %v% "membership" == "Association P2")) #CRO

summary(P2.CRO.sna %e% "num.ties") #average 2.222 ties CRO associated fisher. 
sd(P2.CRO.sna%e% "num.ties") #standard deviation 1.48

#gplot(P2.sna)
P3.MAT.sna <- get.inducedSubgraph(i.sna, 
                                  which(i.sna %v% "membership" == "Association P3")) #MAT

summary(P3.MAT.sna %e% "num.ties") #average 2.24 ties MAT associated fisher. 
sd(P3.MAT.sna %e% "num.ties") #standard deviation 1.654
#gplot(P3.sna)
associated.sna <- get.inducedSubgraph(i.sna,            
                                      which(i.sna %v% "so_arrangement" == "Associated"))

summary(associated.sna %e% "num.ties") #average 2.45 associated fisher.
sd(associated.sna%e% "num.ties") #standard deviation 1.41

independent.sna <- get.inducedSubgraph(i.sna, 
                                       which(i.sna %v% "so_arrangement" == "Independent"))

summary(independent.sna %e% "num.ties") #average 2.487 independent fisher.  

sd(independent.sna %e% "num.ties") #standard deviation 1.65

#gplot(independent.sna)





#-------Calculte density of split networks----------
#(undirected)
#Full netwwork
graph.density(ii.sna) # 0.09614035
#By municipality
graph.density(i.CUL.sna) #0.4761905
graph.density(i.FAJ.sna) # 0.1329268
graph.density(i.NAG.sna) #0.4945055

#By self-organizational arrangement
#(undirected)
graph.density(i.P1.HUC.sna) #0.5757576
graph.density(i.P2.CRO.sna) #0.7
graph.density(i.P3.MAT.sna) #0.2941176
graph.density(i.associated.sna)
graph.density(i.independent.sna)


#Create dataframe for boxplot showing modularity and density values: 
Undirected.density.IS.df <- data.frame(
  ID = i.sna %v% "vertex.names", 
  density.P1 = ii.sna %v% "", 
  density.P2= ii.sna %v% "",
  density.P3 = ii.sna %v% "",
  density.associated = ii.sna %v% "",
  density.independent = ii.sna %v% "",
  density.CUL = ii.sna %v% "",
  density.FAJ = ii.sna %v% "",
  density.NAG = ii.sna %v% ""
) 
#view(Modularity.density.IS.df)



#--Saving network object for ERGM modelling-----
#save(i.sna, file="C:/Users/nicol/Desktop/Repository_Social_Cohesion_Small-scale_Fisheries-Puerto-Rico/data/info.sharing.network.RData")
#summary(i.sna, print.adj = F)
