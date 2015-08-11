#---------------------------------------------------------------------------------------------------------------------#
# TITLE:        Seamon_Assignment3.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Assignment #3
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         23 Sept. 2014
#
# COMMENTS:     This file contains code to read in one dataset - random seedling data.  The intent of this script is #to examine the dataset and determine if:
#               1) Does the density of seedlings vary by class? Does this vary by species? 
#               2) Does the height of seedlings vary by class, and does this vary by species? 
#
#
#---------------Setting the working directory and clearing the workspace----------------------------------------------#

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data") # Set working directory
rm(list=ls())   # Clear workspace (remove varialbes)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

#-------Loading the dataset to use for analysis ----------------------------------------------------------------------#

seedlingdata = read.csv("randomSeedlingData.csv", # Read in random seedling data
                        header=TRUE, 
                        skip=1, 
                        col.names=c("Site", "Class", "Spp", "Ht"),)   

#-------Creating variables for each column in dataset-----------------------------------------------------------------#

Site = seedlingdata[,1]   # [site] The first column, sites, in seedlingdata
Class = seedlingdata[,2] # [class]  The second column, class, in seedlingdata
Spp = seedlingdata[,3]   # [Spp] The third column, spp, in seedlingdata
Ht = seedlingdata[,4]   # [Ht] The fourth column, ht (height), in seedlingdata 

#------Creating summary subsets of dataset,  used in Figure 1 Summary barplots----------------------------------------#

#-------for height by class-------------------------------------------------------------------------------------------#

seedlingdata_ht_class <- aggregate(. ~ Class, data=seedlingdata, FUN=sum)
seedlingdata_ht_class_only <- cbind(seedlingdata_ht_class[4])
seedlingdata_ht_class_only_t <- t(seedlingdata_ht_class_only)

#-------for height by species-----------------------------------------------------------------------------------------#

seedlingdata_ht_species <- aggregate(. ~ Spp, data=seedlingdata, FUN=sum)
seedlingdata_ht_species_only <- cbind(seedlingdata_ht_species[4])
seedlingdata_ht_species_only_t <- t(seedlingdata_ht_species_only)


#-------For Loop to iterate thru seedlings by site, class, and species-------------------------------------------------#


#-------For Loop - setting up loop iterators---------------------------------------------------------------------------#

NSites <- (1:30) #---For all sites-------------------------------------------------------------------------------------#
NClasses <- (1:3) #---For all classes----------------------------------------------------------------------------------#
NSpp <- (1:6) #---For all species--------------------------------------------------------------------------------------#

#-------For Looop - prellocating arrays and matrices-------------------------------------------------------------------#

nSeedlings <- array(data=NaN, c(30, 3, 6))  #---array of seedling totals-----------------------------------------------#
nSeedlings_totalsp <- array(data=NaN, c(30, 6, 3)) #---array with total seedlings by class and species-----------------#
density_spp <- array(data=NaN, c(30, 3, 6))   #-----------array containing seedling densities by class and species-----#
density_class <- matrix(data=NA,nrow=3,ncol=1)     #---vector of seedling totals by class------------------------------#
density_species <- matrix(data=NA,nrow=6,ncol=1)     #---vector of seedling totals by species--------------------------#
mean_Ht <- array(data=NaN, c(30, 3, 6))
#std_Ht <- array(data=NaN, c(30, 3, 6))
meanht_class <- matrix(data=NA,nrow=3,ncol=1)  
meanht_species <- matrix(data=NA,nrow=6,ncol=1)

#--------For Loop------------------------------------------------------------------------------------------------------#

for (i in NSites) {
  for (j in NClasses) {
    for (k in NSpp) {
        #sppindex = Spp==k;
        sppindex = Spp==k & Class==j & Site==i;
        classindex = Class==j;
        spppindex = Spp==k;
              
        nSeedlingssum <- sum(sppindex); #------------sums the total number of seedlings per site/class/species---------#
        density_sppvar <- nSeedlingssum / 300; #----------calcs density for each site/class/species across 300m2 area--#
        mean_Ht[i,j,k] <- mean(Ht[sppindex]) #-------------calcs mean height of seedling per site/class/species--------#
        density_spp[i,j,k] <- density_sppvar; #--------populates density of seedlings array from density_sppvar--------#
              
        ht_sppindex <- seedlingdata[[4]];
        #sppindexmean <- lapply(sppindex, mean);
        meanHtvar <- mean(ht_sppindex);
        ht_sppindextotals <- ht_sppindex;
        #mean_Ht <- ht_sppindex;
        #mean_Ht[i,j,k] <- ht_var;
        nSeedlings[i,j,k] <- nSeedlingssum;
              
        density_spp[i,j,6] <- sum(nSeedlings[i,j,1:5] / 300); #----adds the density of all species across the three----#
                                                              #----classes to density_spp array------------------------#
        
        mean_Ht[i,j,6] <- mean(mean_Ht[i,j,1:5]);             #------adds the mean seedling height of all species------#
                                                              #-------across the three classes to mean_Ht array--------#

#-----------Summary variables created for Suumary plots----------------------------------------------------------------#        
                  
        sppsum = sum(spppindex);
        density_species[k] <- sppsum;
        classsum = sum(classindex);
        sppsum = sum(spppindex);
        density_class[j] <- classsum;
              
        meanht_class[j] = (seedlingdata_ht_class_only_t[j] / classsum)  #---Height by Class calculated----------------#
        meanht_species[k] = (seedlingdata_ht_species_only_t[k] / sppsum)  #-----Height by Species calculated----------#
        nSeedlings_totalsp[i,k,j] <- sum(nSeedlings[i,j,k]); #--seedling totals by species and class------------------# 
  
    end}
  end}
end}


#-------------Transposing summary mean height by class and species----------------------------------------------------#

meanht_class_t <- t(meanht_class)
meanht_species_t <- t(meanht_species)

#-------------Creating summary subsets of arrays for class-------------------------------------------------------------#

density_class_matrix <- matrix(density_class)
density_class_matrix <- t(density_class_matrix)
density_species_matrix <- matrix(density_species)
density_species_matrix <- t(density_species_matrix)

#---------subsetting density array for examining distribution by class and species-------------------------------------#

density_spp_den_eachspecies1 <- density_spp[, , 1]
density_spp_den_eachspecies2 <- density_spp[, , 2]
density_spp_den_eachspecies3 <- density_spp[, , 3]
density_spp_den_eachspecies4 <- density_spp[, , 4]
density_spp_den_eachspecies5 <- density_spp[, , 5]
density_spp_den_eachspecies6 <- density_spp[, , 6]

#---------subsetting mean height array for examining distribution by class and species---------------------------------#

density_spp_ht_eachspecies1 <- mean_Ht[, , 1]
density_spp_ht_eachspecies2 <- mean_Ht[, , 2]
density_spp_ht_eachspecies3 <- mean_Ht[, , 3]
density_spp_ht_eachspecies4 <- mean_Ht[, , 4]
density_spp_ht_eachspecies5 <- mean_Ht[, , 5]
density_spp_ht_eachspecies6 <- mean_Ht[, , 6]

#-----------Plotting for Seedling Distribution by Class and Species----------------------------------------------------#

win.graph(11,10) #---Creates the window size---------------------------------------------------------------------------#
bins = seq(0,6000,500) #-----defining the bin sequences----------------------------------------------------------------#
par(mfrow=c(1,4))  #---par function sets number or rows and columns for plotting---------------------------------------#

#-------------Summary Plots of Seedling distribution by Class and Species----------------------------------------------#

barplot(density_class_matrix, main="Seedling Distribution by Class", #----plotting seeding density by class------------#
        col="aquamarine3",
        xlab="Classes",
        ylab="Number of Seedlings",
        names.arg=c("Class 1", "Class 2", "Class 3"))

barplot(density_species_matrix, main="Seedling Distribution by Species",  #----plotting seeding density by species-----#
        col="aquamarine3",
        xlab="Species",
        ylab="Number of Seedlings",
        names.arg=c("Pico", "Pipo", "Psme", "Pien", "Alba", "All Species"))


#---------------Summary Plots of Mean Seedling Height varying by Class and Species-------------------------------------#

barplot(meanht_class_t, main="Mean Seedling Height by Class",  #----plotting mean seeding height by class--------------#
        col="lightgoldenrod",
        xlab="Classes",
        ylab="Mean Seedling Height (cm)",
        names.arg=c("Class 1", "Class 2", "Class 3"))

barplot(meanht_species_t, main="Mean Seedling Height by Species",   #----plotting mean seeding height by species-------#
        col="lightgoldenrod",
        xlab="Species",
        ylab="Mean Seedling Height (cm)",
        names.arg=c("Pico", "Pipo", "Psme", "Pien", "Alba", "All Species"))

cat ("Press [enter] to continue")
line <- readline()

#-----------------Box Plots showing distribution of seedling density and mean seeding height by class and species------# 

#--------------Plots the density per site, for each species, stratified by class --------------------------------------#

win.graph(11,10) #---Creates the window size---------------------------------------------------------------------------#
bins = seq(0,6000,500) #-----defining the bin sequences----------------------------------------------------------------#
par(mfrow=c(1,6))  #---par function sets number or rows and columns for plotting---------------------------------------#
densityrange <- c(.5,60)

boxplot(density_spp_den_eachspecies1,
        at = 1:3 + 1,
        ylim = c(0,.7),
        col="aquamarine3",
        xlab="Class",
        ylab="Seedling Density (seedlings/m2)",
        main="Pico")
boxplot(density_spp_den_eachspecies2, 
        at = 1:3 + 2,
        ylim = c(0,.7), 
        col="aquamarine3",
        xlab="Class",
        ylab="Seedling Density (seedlings/m2))",
        main="Pipo")
boxplot(density_spp_den_eachspecies3, 
        at = 1:3 + 3,
        ylim = c(0,.7),
        col="aquamarine3",
        xlab="Class",
        ylab="Seedling Density (seedlings/m2)",
        main="Psme")
boxplot(density_spp_den_eachspecies4,
        at = 1:3 + 4,
        ylim = c(0,.7), 
        col="aquamarine3",
        xlab="Class",
        ylab="Seedling Density (seedlings/m2)",
        main="Pien")
boxplot(density_spp_den_eachspecies5, 
        at = 1:3 + 5,
        ylim = c(0,.7), 
        col="aquamarine3",
        xlab="Class",
        ylab="Seedling Density (seedlings/m2)",
        main="Abla")
boxplot(density_spp_den_eachspecies6, 
        at = 1:3 + 6,
        ylim = c(0,.7), 
        col="aquamarine3",
        xlab="Class",
        ylab="Seedling Density (seedlings/m2)",
        main="All Species")

cat ("Press [enter] to continue")
line <- readline()

#--------------Plots of mean seedling height per site, for each species, stratified by class -------------------------#

win.graph(11,10) #---Creates the window size---------------------------------------------------------------------------#
bins = seq(0,6000,500) #-----defining the bin sequences----------------------------------------------------------------#
par(mfrow=c(1,6))  #---par function sets number or rows and columns for plotting---------------------------------------#

boxplot(density_spp_ht_eachspecies1,
        at = 1:3 + 1,
        ylim = c(0,60),
        col="lightgoldenrod",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Pico")
boxplot(density_spp_ht_eachspecies2,
        at = 1:3 + 1,
        ylim = c(0,60),
        col="lightgoldenrod",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Pipo")
boxplot(density_spp_ht_eachspecies3,
        at = 1:3 + 1,
        ylim = c(0,60),
        col="lightgoldenrod",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Psme")
boxplot(density_spp_ht_eachspecies4,
        at = 1:3 + 1,
        ylim = c(0,60),
        col="lightgoldenrod",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Pien")
boxplot(density_spp_ht_eachspecies5,
        at = 1:3 + 1,
        ylim = c(0,60),
        col="lightgoldenrod",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Abla")
boxplot(density_spp_ht_eachspecies6,
        at = 1:3 + 1,
        ylim = c(0,60),
        col="lightgoldenrod",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="All Species")