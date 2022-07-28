######################################################
#Author: Nicolas Gomez Andujar
#Contact: gomezann@oregonstate.edu
#Created:  8/17/2021 ; Culebra, Puerto Rico & Corvallis, Oregon
#Last updated: 7/17/2022; Culebra, Puerto Rico = Revised the script to only publish data used in analysis for Frontiers in Marine Science Publication. 
#Version: Is this the final, and "clean" version of all the ERGM's tried for this network. For previous versions, look at "04_Preliminary_ERGM.R". 
#Trick: for Windows, Alt O will collapse the markdown tabs. 

# Title:Exponential Random Graph Models (ERGMS) for the information-sharing network of fisherfolk in north-eastern Puerto Rico

#Rationale: #After Hurricane Maria, fishers leaned on  bonding social capital among themselves, pooling resources and disseminating information on local disaster
#relief opportunities (Agar et al., 2020). Information-sharing among small-scale fishers  is an informal institution that has been posited as able to aid in collective action 
#in similar SSFs, especially those  with weak formal governance  (Crona and Bodin 2009; Alexander et al., 2018). 
#These atterns in communication among fishers have been explained by gear types and landing sites, but not so much by institional arrangmeents, such the ways fishers choose to
#organize themselves (i.e. self-organizational arrangemeents). This model furthers the knowledge of this driver by comparing two arrangements for Puerto Rican fishers: associations, or independents.  


#Brief overview of ERGMs: 
# ERGMS account for the presence and absence of ties and infer on the social processes that have built the network. 
# Dependent variable is always the probability of a ties between two actors. 
# Independent variables are the factors we think shape the network via a tie formation (aka. ERGMs are for social selection processes)
#Note: technically each time a term is added, it's a different model, but here I differentiate model count according to broader categories (whether I am testing 
#for agency or structural processes)

##Interpreting results: summary() 
# 1. Look at Estimate for each model term (size, direction). The estimate is the log-likelihood of  Monte-Carlo Maximum Likelihood Estime (MLE). 
    #ergm reports log-odds, but  easier to interpret odds ratios (exp(coef()), which the baseline =1. Can also look at the probability through plogis(). 
    #Look at effect size first, then direction. 
    #If a predictor variable in a logistic regression model has an odds ratio less than 1, it means that a one unit increase in that variable is associated with
    #a decrease in the odds of the response variable occurring.
# 2. Look at p-value 
# 3. Examine model fit (AIC/BIC) Compare models via AIC.  (least amount of terms and lower AIC is better fit). IF IAC decreases when adding a new term, that term
#should be kept. 
# 4. If needed: Examine goodness of fit gof() between simulated netowork and observed networks, for terms such as triadcensus, m in geodesic distance. 
#Overall, Identify descriptive patterns that were not modelled and compare observed vs. simulated network. 

#Included here are most salient factors (IVs) for which there is node attrbiute data for all fishers in the network. Although there are over 25 nod attributes in the dataset, 
#most of these are not available for the "alters", or fishers (15 in this case) which were not surveyed, but mentioned by others.  

#Note: Cannot comapre effect sizes, or probabilities of one term with another. 

#Documentation on terms: https://rdrr.io/cran/ergm/man/ergm-terms.html ; http://personal.psu.edu/drh20/papers/v24i04.pdf ;

#The following AIC and BIC are always compared to the null model. 

#-Preparation-------
library(statnet)
library(statnet)
library(ergm)
library(network)
library(sna)
library(dplyr)
library(tidyverse)

setwd("C:/Users/nicol/Desktop/Repository_Social-Cohesion-Small-scale-Fisheries-Puerto-Rico")

#ERGM Model Terms
#help('ergm-terms')

#-Info-sharing ERGMS  (directed) ----------------------
#Load the info.sharing.network.RData 
load("C:/Users/nicol/Desktop/Thesis/Thesis Data/SNA_Survey/PR-SSF-SNA/data/info.sharing.network.RData")
summary(i.sna, print.adj = FALSE) 


#Summary of the network statistics: 
summary(i.sna~edges+triangle+idegree(1:13) + odegree(1:14)) #335 edges, 604 triangles

#----Model 1: Null model (just existing edges)----------
set.seed(40)
m1 <- ergm(i.sna ~edges, control = control.ergm(seed= 40)) #random graph model that has the same number of edges as the observed network. reflects the simple probability of a tie in the network
#seed is a random number that must be kep equal across models. 
#If the number of edges increase 1, the log-odds of any edge existing is -1.6094.
class(m1) #ergm object
summary(m1)
#AIC:2551 
#BIC: 2557
ls(ISmod0) #displays all the parts of the ergm object
plogis(coef(m1)) #This calculates the probability of a tie in the network for that term. Cannot compare across terms.  
#baseline probability is 58% likelihood of tie formation. 
plogis(coef(m1)[['edges']]) #baseline probability of a tie  is 3.9.%


exp(coef(m1)) #ergm reports in log-odds, but usually easier to interpret effect size and direction in odd ratios. 
                  #This is the calculation to odd ratios.Baseline is 1
#when odds ratios for dichotomous variables are <1, it can be easier to interpret the inverse: (output odd ratio -1) * 100 
#If a predictor variable has an odds ratio less than 1, it means that a one unit increase in that variable is associated with a 
#decrease in the odds of the response variable occurring.

gden(i.sna) # density of the network: 0.05877193

mcmc.diagnostics(m1)

#----Model 2: Reciprocity ----------
#highlighted as important in PR fisheries by García-Quijano 2009
#mutual() term. Directed networks contain more reciprocal ties than expected at random. Positive affect netowrks, such as friendships, usually exhiibt reciproated ties. 

m2 <- ergm(i.sna ~edges + mutual, control = control.ergm(seed= 40))
exp(coef(m2))
summary(m2)
plogis(coef(m1)[['edges']]) #output: 0.0587  #baseline probability of a tie  is 5.8.%
plogis(coef(m1)[['edges']]+ coef(m2)[['mutual']]) #output: 0.4639  or 46.39%
46.39 - 5.8 #40.59
##probability of reciprocal ties is 40.59%  more likely. 

#Parameter estimate: 2.62906  +-  0.18647
#Effect size and direction in odd ratios: 13.86079585, positive
#pvalues: p<0.001
#Null:AIC:2551 & BIC: 2557
#AIC: 2393
#BIC: 2406
#Observations: By itself, reciprosity significantly explain tie formation. 
#Diagnositcs: 

mcmc.diagnostics(m2) #Looking for a random variation around 0.  Look at plots for specifics. 
#Good
#----Model 3: Self-governance homophily ----------
# Self-governance arrangement: evidence inter-communicatoin for formal and informal network-based communities (independents and associated don't communicate a lot)
#dyads as IVs
#Homophily (from Ancient Greek: homoû, 'together' + philia, 'friendship'), is the tendency of individuals to associate and bond with similar others. Source: https://en.wikipedia.org/wiki/Homophily
#(actors with similar characteristics are lined to one another more often than expected by random chance)
#levels = to specify comparions 

#Sender and receiver effects: nodefactor('') examines combinations of a categorical variable

#Homophily terms: 
 
#nodematch('', diff= FALSE ) #Examines dyads that have the same factor level (uniform homophily). 
                             #Each group is assumed to have the same propensity for within-group ties.
                             #Uniform homophily = the effect of two actors sharing the same node attribute (Hunter et al., 2008) https://www.jstatsoft.org/article/view/v024i03 
                             #Default
#nodematch('', diff= TRUE)#Examines whether dyads have different degree of bias (differential homophily). 
                          #Each group is allowed to have a unique propensity for within-group ties
#nodemix('') #Examines whether dyads of one level tend to interact with dyads of other levels (heterophily). 
                          #Examines all combinations of node level, 
                          #nodemix and nodematch can produce the same result if the levels are specified the same way. 

#mixingmatrix('') -  tendency for an actors ( the same level (e.g.  primary.gear ) to form a tie. helps to explore wether homophily could be expected. 


#Expectation: 
# The big question here is whether the pattern that I have gathered qualitative is reflected in the network: 
  #That independent fishers may not have a formal association to facilitate sharing information (mainly a leader that keeps them up to speed and a place to meet), 
  #but they nevertheless  communicate a lot among themselves through other means (calls; whatssap;social media). 
  #On the flip side, I expect variability in the homophily inside each association.  Association P3 in MAT) can recent members who are associated as a consequence
  #of conflicts in Association P3 and to show to managers they should receive aid - many of them have not sold to the association's fish shop (it has been closed 
  #since hurricanes in 2017) and have not shared a landing site either (they come from CRO). Combined, I would expect there homophily to NOT occur in this 
  #association. Members of Association P2 in CRO, seems to have a mix of sharing info among members (it is quite small) and with non-members as well. 
mixingmatrix(i.sna, "so_arrangement") #more ties among same organizational arrangement than across.   

m3 <- ergm(i.sna ~edges + mutual
          #Sender and receiver effects:  
          + nodeofactor ("so_arrangement", levels = "Associated") #sender "o" for "out" (i.e send)) effect among associated fishers for categorical (text) node attribtue ; attribute-based activity
          + nodeifactor ("so_arrangement", levels = "Associated") #reciever ("i" for "in" (i.e. receive)) effect among associated fishers for categorical (text) node attribute ; attribute-based popularity 
          #receiver and sender effects: if using membership attribute to exclue P6 members use: c("Associated P1", "Associated P2", "Associated P3")
          #Uniform Homophily among independents: 
          + nodematch("so_arrangement", levels = "Independent") #among independents (default is diff = F)
          #+ nodematch("so_arrangement", levels = "Associated") #DROP FROM MODEL TO AVOID COLLINEARITY
          # Homophily among specific associations: 
          #+ nodematch("membership", levels="Association P1") #HUC association
          #+ nodematch("membership", levels="Association P2") #CRO association
          #+ nodematch("membership", levels="Association P3"), #MAT association
          + nodemix("membership", levels="Association P1") #HUC association
          + nodemix("membership", levels="Association P2") #CRO association
          + nodemix("membership", levels="Association P3"), #MAT association
           control = control.ergm(seed= 40))
exp(coef(m3))
summary(m3)
#when odds ratios for dichotomous variables are <1, it can be easier to interpret the inverse: 
#If a predictor variable has an odds ratio less than 1, it means that a one unit increase in that variable is associated with a 
#decrease in the odds of the response variable occurring.

#Sender and receiver effects: 
#associated fishers are more likely to send (out) information  (1.08 odd ratio) (significant)
#associated fishers are less likely to receive (in) information ties (0.48 odd ratio)
1/exp(coef(m3)) #aka associated fishers are 2.07194541 less likely to receive information

#Parameter estimate: 
#Effect size and direction in odd ratios: 
#pvalues: p<0.001
#Null:AIC:2551 & BIC: 2557
#AIC: 2204
#BIC: 2257
#Observations: All significant, possitive effects
mcmc.diagnostics(m3) #Looking for a random variation around 0.  Look at plots for specifics. 
#Good, thoughout. The worst fit is for Association P2, but still pasable. 

#Interpretation: 
#Receiver effect: A receiver effect with "associated fisher" as the focal category (i.e. "independent" as base), 
#this term will represent how much more/less likely a fisher is to receive information when they 
#are in an association compared to an independent fisher. So if you get an odds ratio for this
#term >1, this suggests associated fishers are more likely to receive information ties, and if 
#the odds ratio is <1, this would indicate they are less likely to receive information ties.
#Homophily effects: 

#----Model 4: Gear homophily ----------

#Expectation: likely to share specialized information on fishing strategies with people of same  primary.gear  type, or the other way around,  
#information about problems with fishers of other  primary.gear  types.
#fish traps have a lot of ties in ccommon. So do scuba divers. mechanized handlines and normal handlines also have lots of ties 
#among themselves. 

m4 <- ergm(i.sna ~edges + mutual
           #Sender and receiver effects:  
           + nodeofactor ("so_arrangement", levels = "Associated") #  #sender "o" for "out" (i.e send)) effect among associated fishers for categorical (text) node attribtue ; attribute-based activity
           + nodeifactor ("so_arrangement", levels = "Associated") #reciever ("i" for "in" (i.e. receive)) effect among associated fishers for categorical (text) node attribute ; attribute-based popularity 
           #Uniform Homophily among independents: 
           + nodematch("so_arrangement", levels = "Independent") #among independents (default is diff = F)
           # Homophily among specific associations: 
           + nodemix("membership", levels="Association P1") #HUC association
           + nodemix("membership", levels="Association P2") #CRO association
           + nodemix("membership", levels="Association P3") #MAT association
           #Primary gear: 
           + nodemix("primary.gear", levels = 1) #fish traps
           + nodemix("primary.gear", levels = 2) #gillnet
           + nodemix("primary.gear", levels = 3) #handline
           + nodemix("primary.gear", levels = 4) #lobster traps
           + nodemix("primary.gear", levels = 5) #mechanied deep-water handline
           #+ nodemix("primary.gear", levels = 6) #rod and reel ; Had to remove from model due to infinite parametrization. No convergance. 
           + nodemix("primary.gear", levels = 7) #scuba divin
           + nodemix("primary.gear", levels = 8), #skin diving
           control = control.ergm(seed= 40))
exp(coef(m4))
summary(m4)
#Parameter estimates: 
#Effect size and direction in odd ratios: 
#pvalues: 
#Null:AIC:2551 & BIC: 2557
#AIC: 2072
#BIC: 2165
#Observations:
#Diagnostics: 
mcmc.diagnostics(m4) #Looking for a random variation around 0.  Look at plots for specifics. 
#Good thoughout. 
#----Model 5: Landing site Homophily: ------

m5 <- ergm(i.sna ~edges + mutual
           + nodeofactor ("so_arrangement", levels = "Associated") #  #sender "o" for "out" (i.e send)) effect among associated fishers for categorical (text) node attribtue ; attribute-based activity
           + nodeifactor ("so_arrangement", levels = "Associated") #reciever ("i" for "in" (i.e. receive)) effect among associated fishers for categorical (text) node attribute ; attribute-based popularity 
           #Uniform Homophily among independents: 
           + nodematch("so_arrangement", levels = "Independent") #among independents (default is diff = F)
           # Homophily among specific associations: 
           + nodemix("membership", levels="Association P1") #HUC association
           + nodemix("membership", levels="Association P2") #CRO association
           + nodemix("membership", levels="Association P3") #MAT association
           #Primary gear: 
           + nodemix("primary.gear", levels = 1) #fish traps
           + nodemix("primary.gear", levels = 2) #gillnet
           + nodemix("primary.gear", levels = 3) #handline
           + nodemix("primary.gear", levels = 4) #lobster traps
           + nodemix("primary.gear", levels = 5) #mechanied deep-water handline
           #+ nodemix("primary.gear", levels = 6) #rod and reel ; Had to remove from model due to infinite parametrization. No convergance. 
           + nodemix("primary.gear", levels = 7) #scuba divin
           + nodemix("primary.gear", levels = 8) #skin diving
           #Landing site: 
           + nodemix("landing.site", levels=1) #Culebra
           + nodemix("landing.site", levels=2) #HUC
           + nodemix("landing.site", levels=3) #CRO
           + nodemix("landing.site", levels=4), #MAT
           #+ nodemix("landing.site", levels=) #SAR, only two egos and 1 alter.
           control = control.ergm(seed= 40))
exp(coef(m5))
summary(m5)
#Parameter estimates: 
#Effect size and direction in odd ratios: 
#pvalues: 
#Null:AIC:2551 & BIC: 2557
#AIC: 1846
#BIC: 1973
#Observations:
#Diagnostics: 
mcmc.diagnostics(m5) #Looking for a random variation around 0.  Look at plots for specifics. 

#----Model 6: Triadic closure--------
#Triadic closure is a property among three nodes (Simmel 1908, Sociology: Investigations on the Forms of Sociation), also Granovetter 1973,The Strenght of Weak Ties. 
#In a directed network there are 16 possible unique triads. 
#Transitivity is: 
                 # a common way to measure triadic closure. Defined as the density of the transitive paths or triples that is Closed divided by Base. 
                 # calculated by taking triples as the base. (triples = # of paths)

#Triad classification scheme: Davis, J.A. and Leinhardt, S. (1972). The Structure of Positive Interpersonal Relations in Small Groups.
triad.classify(i.sna, tri=c(1, 2, 3), mode=c("digraph")) #tri = a triple containing the indices of the triad to be classified.
#003 - a<-!->b<-!->c, a<-!->c
gplot(i.sna[1:3,1:3])

triad.census(i.sna) #finds the number of triangles in a directed graph.  
                    #adds one network statistic for each of an arbitrary subset of the 16 possible types of triads categorized by Davis and Leinhardt (1972)

#Can test for transitivity (sharing infomarnation with a friend of my friend is more common than expected by random chance), and the geometrically weighted edgewise shared partnership (i.e. GWESP term)

#Triangulation means the number of edges where the two nodes also share an edge two a third actor (i.e. a triangle).Multiple triangulation is akin to cliques and cohesive subgroups. 

# Alpha  is decay parameter such that the probability of a tie that would add an ESP between two nodes is weighted downwards as a function of the number of ESP the
#pair of nodes already share. #In other words, which determines how much weight to place on edges with multiple shared partners. 
#Fixed: 
# TRUE = can choose alpha,  
# FALSE = estimate the decay parameter for us, with a starting value of 1. It is the default. 

#Interpretation: 
# A significant, positive coefficient indicates more triangles than expected in a random graph fitting the other model constraints, while a significant, negative coefficient indicates fewer triangles than expected
# A positive effect indicates there is a high degree of closure (transitivity or transitive closure) or multiple clusters of triangles in the data. 


#Triadic closure (specifically alternating triangle ATA, on a non-directed network) was highlighted as important for Caribbean artisanal fisheries by (Alexander et al., 2018; Alexander et al., 2019). These papers had undirected data. 

#Documentation: 
#Hunter and Haddock 2006 https://www.tandfonline.com/doi/abs/10.1198/106186006X133069 alpha = 
#Using decay value (formerly called alpah) of 0.25, as chosen by Goodreau et al., 2008 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2831261/ 
#Tutorial for Full interpretation: https://eehh-stanford.github.io/SNA-workshop/ergm-predictions.html

m6 <- ergm(i.sna ~edges + mutual
           + nodeofactor ("so_arrangement", levels = "Associated") #sender "o" for "out" (i.e send)) effect among associated fishers for categorical (text) node attribtue ; attribute-based activity
           + nodeifactor ("so_arrangement", levels = "Associated") #reciever ("i" for "in" (i.e. receive)) effect among associated fishers for categorical (text) node attribute ; attribute-based popularity 
           #Uniform Homophily among independents: 
           + nodematch("so_arrangement", levels = "Independent") #among independents (default is diff = F)
           # Homophily among specific associations: 
           + nodemix("membership", levels="Association P1") #HUC association
           + nodemix("membership", levels="Association P2") #CRO association
           + nodemix("membership", levels="Association P3") #MAT association
           #Primary Gear: 
           + nodemix("primary.gear", levels = 1) #fish traps
           + nodemix("primary.gear", levels = 2) #gillnet
           + nodemix("primary.gear", levels = 3) #handline
           + nodemix("primary.gear", levels = 4) #lobster traps
           + nodemix("primary.gear", levels = 5) #mechanied deep-water handline
           #+ nodemix("primary.gear", levels = 6) #rod and reel ; Had to remove from model due to infinite parametrization. No convergance. 
           + nodemix("primary.gear", levels = 7) #scuba divin
           + nodemix("primary.gear", levels = 8) #skin diving
           #Landing site: 
           + nodemix("landing.site", levels=1) #Culebra
           + nodemix("landing.site", levels=2) #HUC
           + nodemix("landing.site", levels=3) #CRO
           + nodemix("landing.site", levels=4) #MAT
           #+ nodemix("landing.site", levels=) #SAR, only two egos and 1 alter.
           + dgwesp(0.25, fixed=T), #directed Transitive path closure (OTP)
           control = control.ergm(#MCMC.burnin=20000, MCMC.interval=5000,MCMLE.maxit= 15 used if model does not converge.
                                  seed= 40))

#If model does not converge: 
exp(coef(m6))
summary(m6)
#Parameter estimates: 
#Effect size and direction in odd ratios: 
#pvalues: 
#Null:AIC:2551 & BIC: 2557
#AIC: 1837
#BIC: 1970
#Observations:
mcmc.diagnostics(m6) 
#Save Model6: 
#save(m6, file="C:/Users/nicol/Desktop/Thesis/Thesis Data/SNA_Survey/PR-SSF-SNA/data/Info_sharing_ERGM_Model6.RData")

#Check model fit is getting better: 
round(sapply(list(m1, m2, m3, m4, m5, m6), AIC), 0)
# 2393 2393 2213 1934 1934 1847

#-----M6 MCMC Diagnostics & Model Goodness of Fit---------
#For dyad dependent models, MCMC diagnostics are checked before GOF. 
#-------Part 1: MCMC Diagnostics  to assess model convergance------
#install.packages("latticeExtra")
library(latticeExtra)
mcmc.diagnostics(m6)
#MCMC Diagnostics tell us if the estimation algorithm is mixing well, and converged to the target value

#Step 1: Sample statistic autocorrelation
#Measure the correlation between sample statistic at different point in the MCMC chain. 
#A good chain is randomly mixing. 
#Look for random variation close to 0.

#Step 2: Sample statisit burn-in diagnositc (Geweke)
#Measure convergance by comparing means of sample statistic at different points in MCMC chain. 
#If chains are stationary (randomly mixing) then the means  at different locations should be equal. 
#Look for p-values close to 1 (and far from zero) for all terms. 

#Step 3: MCMC trace plots 
#Difference between the sample statistic and observed network for every step in simulation. 
#Look for evidence of mixing around zero. 

#Step 4: MCMC density plots
#Values of sample statistics. 
#Should have bell-shaped distribution centered at zero (i.e., no difference from observed network)
#Seeing sawtooth pattern rather than a smooth distribution is OK as long as overall shape is roughly normal and the statisc are centered at zero. 
#This is a result of variables that are discrete and have a small range. 
#Observations: 
#1) All terms have somewhat "good" fit around zero. 
#2) All p-values are large. 
#3) Ok
#4) Ok

#-------Part 2: GOF--------- 
#The function gof() takes an ergm model object and examines fit between simulated network and observed networks, 
#for terms such as geodesic distance, edgewise shared partners, degree distribution, and triad census. (Goodreau et al., 2008; section 6 & 8).
#For directed networks the following parameters have been suggested for GOF (Robins, Pattison & Wang, 2009)
#Goals:
#1) Validation assessment - To identify descriptive patterns (such as degree, geodesic distance) that were not modelled. # Asseses how well the model captures features of data that were not explicitly modelled. 
#2) Calibration assessment - Compare the observed vs. the simulated networks. 
#GOF assumes the model has converged (the each term statistic is smaller than 0.1 absolute value.   

#Question 1: 
#How well doees the model reproduce the terms in the model (via the MLE)?
#insert the “model” term to evaluate the fit of the terms to the model. 
gof.model <- gof(m6 ~ model)
#Step 1: Compare our observed network (obs trendline) to the mean and min/max of simulated networks. 
#Step 2: Use the p-values to determine significant differences between observed/simulated
#where p<0.05 indicates poor model fit for that term
gof.model
summary(gof.model) 
#Observations: 
#1 Means are generably comparable to observed. 
#2 All p-values are large, so GOOD. 
plot(gof.model)
#Do boxplots should look alike: Yes

#Question 2: How well is the model capturing global (structural) processes?
#Step 1: Use structural terms that are NOT in the model. These can be: 
        #Graph counts (# of arcs, recriprocated arcs, etc.)
        #Degree (idegree or odegree) - node level 
        #Closure -triad level
        #Triad census (triadcensus) -triad level
        #Geodesic distance (distance) - dyad level
        #Edgewise shared partners   (esp or espartners) - edge level
#For directed graphs the gof-model default is:  ~ idegree + odegree + espartners + distance + model.

i.sna.fit <- gof(m6, GOF= ~distance + espartners + idegree + odegree, 
                 burnin=1e+5, interval = 1e+5,  control.gof.ergm(seed = 401)) 
# FYI - burn-in is the number of steps in the simulation chain before the simulated network is drawn)
#An output of less than 2 is desired
i.sna.fit

#Step 1: Compare our observed network (obs) to the mean and min/max of simulated networks. Min-max should be within range of observed. 
         # This is good. 
#Step 2: Use pvalues to determine significant differences between observed/simulated
#where p<0.05 indicates poor model fit for that term
        #p-values look good.


par(mfrow=c(2,2))
plot(i.sna.fit)


#Part 3: Simulate networks from our ERGM model to compare to observed
sim <- simulate(m6, nsim=1,burnin = 1e+5, verbose = TRUE, seed=569) 
summary(sim, print.adj=FALSE)
centralization(sim, sna::degree, 
               normalize = TRUE, diag=FALSE,
               cmode="indegree")
#Save simulated network: 
#save(sim, file="C:/Users/nicol/Desktop/Thesis/Thesis Data/SNA_Survey/PR-SSF-SNA/data/Simulated_info_sharing_ERGM_Model6.RData")



#Visualize networks to compare them
library(patchwork) #to output letters on each panel in combination with ggplot2 
library(ggplot2)
library(RColorBrewer)
library(GGally)
#-------II. Save coordinates to standarize vis of plots------------------------ 
#For gplot: 
mycoords.i <- gplot(i.sna, gmode="graph", vertex.cex=1.5) #for gplot

#For ggnet2: (or put set.seed(1)in front of every plot)
x = gplot.layout.fruchtermanreingold(i.sna, NULL)
i.sna %v% "x" = x[, 1]
i.sna %v% "y" = x[, 2]
sim %v% "x" = x[, 1]
sim %v% "y" = x[, 2]
g<- ggnet2(i.sna, color = "landing.site",mode = c("x", "y")) #Trial
g

lvlsim <- sim %v% "degree_centrality"

n <- ggnet2(i.sna, node.color = "membership",
        palette = "Set1", arrow.gap = 0.02,
        size = "degree",
       arrow.size = 5, edge.alpha = 0.25,
       mode = c("x", "y"),
       label.color = "black",
       label.size = 2,
       label = TRUE,
       edge.color = c("color","grey50"))+
  ggtitle("a) Observed info-sharing")+
  theme(legend.title = element_blank())

m <- ggnet2(sim, node.color = "membership",
            size = "degree",
            palette = "Set1", arrow.gap = 0.02,
       arrow.size = 5, edge.alpha = 0.25,
       mode = c("x", "y"),
       label.color = "black",
       label = TRUE,
       label.size = 2,
       edge.color = c("color","grey50"))+
  ggtitle("b) Simulated info-sharing (Model 6)")+
  theme(legend.title = element_blank())+
  guides(size = FALSE, color=FALSE, shape=FALSE)


nm <- (n | m)
nm
#ggsave("figures/sim_vs_obs_Model6.png", width =  12, height = 7, dpi = 300, units = "in", device='png')



#-------Odd ratios-------
exp(coef(m1))
exp(coef(m2))
exp(coef(m3))
exp(coef(m4))
exp(coef(m5))
exp(coef(m6))
