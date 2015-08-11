# Seamon_Assignment3.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Assignment #3
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         18 Sept. 2014
#
# COMMENTS:     This file contains code to read in one dataset - random seedling data.  The intent of this script is #to examine the dataset and determine the if 1) density and 2) height vary by 3) class and 4) species.
#
#
#---------------Setting the working directory and clearing the workspace------------------#

setwd("X:/Dropbox/ES Research/ES Classwork/FOR504/data") # Set working directory
rm(list=ls())   # Clear workspace (remove varialbes)
graphics.off()  # Close current graphing windows
cat("\14")      # Clear command prompt

#-------Loading the dataset to use for analysis - and setting variables for columns-------#

seedlingdata = read.csv("randomSeedlingData.csv", # Read in random seedling data
                        header=TRUE, 
                        skip=1, 
                        col.names=c("Site", "Class", "Spp", "Ht"),)   

#-------Creating variables for each column in seedlingdata--------------------------------#

#seedlingdata_Df <- data.frame(seedlingdata)
Site = seedlingdata[,1]   # [site] The first column, sites, in seedlingdata
Class = seedlingdata[,2] # [class]  The second column, class, in seedlingdata
Spp = seedlingdata[,3]   # [Spp] The third column, spp, in seedlingdata
Ht = seedlingdata[,4]   # [Ht] The fourth column, ht (height), in seedlingdata 

#----------------Creating summary subsets of arrays for height----------------------------#

seedlingdata_ht_class <- aggregate(. ~ Class, data=seedlingdata, FUN=sum)
seedlingdata_ht_class_only <- cbind(seedlingdata_ht_class[4])
seedlingdata_ht_class_only_t <- t(seedlingdata_ht_class_only)

seedlingdata_ht_species <- aggregate(. ~ Spp, data=seedlingdata, FUN=sum)
seedlingdata_ht_species_only <- cbind(seedlingdata_ht_species[4])
seedlingdata_ht_species_only_t <- t(seedlingdata_ht_species_only)

#------------setting up for loop iterators-------------------------------------------------#

NSites <- (1:30)
NClasses <- (1:3)
NSpp <- (1:6)

#-------------prellocating arrays and matrices---------------------------------------------#

nSeedlings <- array(data=NaN, c(30, 3, 6))  #---array of seedling totals-------#
#nSeedlings_totalsp <- array(data=NaN, c(30, 6, 3)) #---array with total seedlings by site, class and species----#
density_spp <- array(data=NaN, c(30, 3, 6))   #-----------array containing seedling densities by site, class and species-----#
density_class <- matrix(data=NA,nrow=3,ncol=1)     #---vector of seedling totals by class-------#
density_species <- matrix(data=NA,nrow=6,ncol=1)     #---vector of seedling totals by species-------#
mean_Ht <- array(data=NaN, c(30, 3, 6)) #array of seedling mean heights by site, class and species--------#
#std_Ht <- array(data=NaN, c(30, 3, 6))
meanht_class <- matrix(data=NA,nrow=3,ncol=1)  
meanht_species <- matrix(data=NA,nrow=6,ncol=1)

#----------------For Loop to iterate thru seedlings by site, class, and species-----------#

for (i in NSites) {
  for (j in NClasses) {
    for (k in NSpp) {
              #sppindex = Spp==k;
              sppindex = Spp==k & Class==j & Site==i;
              classindex = Class==j;
              spppindex = Spp==k;
              
              nSeedlingssum <- sum(sppindex); #------------sums the total number of seedlings per site/class/species----#
              density_sppvar <- nSeedlingssum / 300; #----------calcs density for each site/class/species across 300m2 area---#
              mean_Ht[i,j,k] <- mean(Ht[sppindex]) #-------------calcs mean height of seedling per site/class/species
              density_spp[i,j,k] <- density_sppvar; #--------populates density of seedlings array from density_sppvar
              
              ht_sppindex <- seedlingdata[[4]];
              #sppindexmean <- lapply(sppindex, mean);
              meanHtvar <- mean(ht_sppindex);
              ht_sppindextotals <- ht_sppindex;
              #mean_Ht <- ht_sppindex;
              #mean_Ht[i,j,k] <- ht_var;
              nSeedlings[i,j,k] <- nSeedlingssum; #----iterativly adds seedling totals to nSeedlings array---#
              
              density_spp[i,j,6] <- sum(nSeedlings[i,j,1:5] / 300); #-----adds density of all species across the three classes to density_spp array-----#
              mean_Ht[i,j,6] <- mean(mean_Ht[i,j,1:5]); #--iteratively adds density of all species to density_spp array--#
              
              #density_spp[i,j,k] <- density_sppvar;
              #ht_sppindex = NULL;
              
              sppsum = sum(spppindex);
              density_species[k] <- sppsum;
              classsum = sum(classindex);
              sppsum = sum(spppindex);
              density_class[j] <- classsum;
              
              meanht_class[j] = (seedlingdata_ht_class_only_t[j] / classsum)  #---Height by Class calculated----#
              meanht_species[k] = (seedlingdata_ht_species_only_t[k] / sppsum)  #-----Height by Species calculated----#
              nSeedlings_totalsp[i,k,j] <- sum(nSeedlings[i,j,k]); #--seedling totals by species and class-------# 
              
              seedling_by_class1 <- subset(seedlingdata, Class==1);
              seedling_by_class2 <- subset(seedlingdata, Class==2);
              seedling_by_class3 <- subset(seedlingdata, Class==3);
              seedling_by_species1 <- subset(seedlingdata, Spp==1);
              seedling_by_species2 <- subset(seedlingdata, Spp==2);
              seedling_by_species3 <- subset(seedlingdata, Spp==3);
              seedling_by_species4 <- subset(seedlingdata, Spp==4);
              seedling_by_species5 <- subset(seedlingdata, Spp==5);
              seedling_by_species_all <- subset(seedlingdata, Spp < 6);
    end}
  end}
end}



#nSeedlings_allsp_added <- 

#-------------Transposing mean height by class and species--------------------------------#

meanht_class_t <- t(meanht_class)
meanht_species_t <- t(meanht_species)

#-------------Creating summary subsets of arrays for class--------------------------------#

density_class_matrix <- matrix(density_class)
density_class_matrix <- t(density_class_matrix)
density_species_matrix <- matrix(density_species)
density_species_matrix <- t(density_species_matrix)

#-------------Creating summary subsets of arrays for species and class--------------------#

nseedlings_den_eachspecies <- apply(nSeedlings, c(2,3), sum)
nseedlings_den_eachclassbysite <- apply(nSeedlings, c(1,2), sum)
nSeedling_den_eachclass <- apply(nSeedlings, c(3,2), sum)
nSeedling_den_species <- apply(nSeedling_den_eachclass, 1, sum)
nSeedling_den_class <- apply(nSeedling_den_eachclass, 2, sum)
nSeedling_den_class <- t(nSeedling_den_class)
#nSeedlings_totalsp_sum <- apply(nSeedlings_totalsp, c(3,2), sum) #---summarized array seedling totals by class & species combined-----#
nseedlings_den_eachspecies_site <- apply(nSeedlings, c(1,3), sum)
nseedlings_den_eachspecies_sitesum <- addmargins(nseedlings_den_eachspecies_site, FUN = list(Total = sum), quiet = TRUE)
nSeedlings_totalsp_sum2 <- rowsum(nseedlings_den_eachspecies_site, NSites)
nseedlings_den_eachspecies_sitesum2 <- nseedlings_den_eachspecies_sitesum[-31,]
nseedlings_den_eachspecies_sitesum_final <- data.frame(nseedlings_den_eachspecies_sitesum2)
nseedlings_den_eachspecies_sitesum_final$X6 = NULL

nseedlings_den_eachclass_sitesum<- addmargins(nseedlings_den_eachclassbysite, FUN = list(Total = sum), quiet = TRUE)
nseedlings_den_eachclass_sitesum2 <- nseedlings_den_eachclass_sitesum[-31,]

#---------subsetting density array for examining distribution by class and species----------------#

density_spp_den_eachspecies1 <- density_spp[, , 1]
density_spp_den_eachspecies2 <- density_spp[, , 2]
density_spp_den_eachspecies3 <- density_spp[, , 3]
density_spp_den_eachspecies4 <- density_spp[, , 4]
density_spp_den_eachspecies5 <- density_spp[, , 5]
density_spp_den_eachspecies6 <- density_spp[, , 6]

#---------subsetting mean height array for examining distribution by class and species----------------#

density_spp_ht_eachspecies1 <- mean_Ht[, , 1]
density_spp_ht_eachspecies2 <- mean_Ht[, , 2]
density_spp_ht_eachspecies3 <- mean_Ht[, , 3]
density_spp_ht_eachspecies4 <- mean_Ht[, , 4]
density_spp_ht_eachspecies5 <- mean_Ht[, , 5]
density_spp_ht_eachspecies6 <- mean_Ht[, , 6]

#----------clearing values from some arrays----------#

seedling_by_class1$Site = NULL
seedling_by_class1$Class = NULL
seedling_by_class1$Spp = NULL

seedling_by_class2$Site = NULL
seedling_by_class2$Class = NULL
seedling_by_class2$Spp = NULL

seedling_by_class3$Site = NULL
seedling_by_class3$Class = NULL
seedling_by_class3$Spp = NULL

seedling_by_species1$Site = NULL
seedling_by_species1$Class = NULL
seedling_by_species1$Spp = NULL

seedling_by_species2$Site = NULL
seedling_by_species2$Class = NULL
seedling_by_species2$Spp = NULL

seedling_by_species3$Site = NULL
seedling_by_species3$Class = NULL
seedling_by_species3$Spp = NULL

seedling_by_species4$Site = NULL
seedling_by_species4$Class = NULL
seedling_by_species4$Spp = NULL

seedling_by_species5$Site = NULL
seedling_by_species5$Class = NULL
seedling_by_species5$Spp = NULL

seedling_by_species_all$Site = NULL
seedling_by_species_all$Class = NULL
seedling_by_species_all$Spp = NULL

seedling_by_species5_df <- data.frame(seedling_by_species5)


#--------------plotting-------------------------------------------------------------------#          

#win.graph(7,11) 
#plot(aggmet2007_windspeed_matrix)

#bins = seq(0,6000,100) # defining the bin sequences----#
#par(mfrow=c(4,1))  #---setting the window to display four rows for plotting purposes----#

#seedlingdatamatrix <- as.matrix(seedlingdata, dim="Site")
#names(seedlingdatamatrix) <- c("Site", "Class", "Spp", "Ht")

#hist(nSeedling_den_class, breaks=bins, freq=FALSE,   #---plots aggmet2007 windspeed frequency
     #col = "gray",
     #xlab = "Density",
     #ylab = "Classes",
     #main = "DEnsity of Seedlings Across All Classes (1,2,3)")

#hist(nSeedling_den_species,breaks=bins, freq=FALSE,   #---plots aggmet2007 windspeed frequency
     #col = "gray",
     #xlab = "",
     #ylab = "Count",
     #main = "Random seedling distribution")

#-----------Plotting for Seedling Distribution by Class and Species-----------------------#

win.graph(7,11) 
bins = seq(0,6000,100) # defining the bin sequences
par(mfrow=c(2,1))  #---par function sets number or rows and columns for a matrix for plots

#-------------Figure 1. Plots of Seedling distribution by Class and Species---------------#

barplot(density_class_matrix, main="Seedling Distribution by Class", #----plotting seeding density by class----#
        col="blue",
        xlab="Classes",
        ylab="Number of Seedlings",
        names.arg=c("Class 1", "Class 2", "Class 3"))

barplot(density_species_matrix, main="Seedling Distribution by Species",  #----plotting seeding density by species----#
        col="red",
        xlab="Species",
        ylab="Number of Seedlings",
        names.arg=c("Species 1", "Species 1", "Species 3", "Species 4", "Species 5", "All Species"))

cat ("Press [enter] to continue")
line <- readline()

#---------------Figure 2. Plots of Mean Seedling Height varying by Class and Species--------------#

win.graph(7,11) 
bins = seq(0,6000,100) # defining the bin sequences
par(mfrow=c(2,1))  #---par function sets number or rows and columns for a matrix for plots

barplot(meanht_class_t, main="Mean Seedling Height by Class",  #----plotting mean seeding height by class----#
        col="blue",
        xlab="Classes",
        ylab="Mean Seedling Height (cm)",
        names.arg=c("Class 1", "Class 2", "Class 3"))

barplot(meanht_species_t, main="Mean Seedling Height by Species",   #----plotting mean seeding height by species----#
        col="red",
        xlab="Species",
        ylab="Mean Seedling Height (cm)",
        names.arg=c("Species 1", "Species 1", "Species 3", "Species 4", "Species 5", "All Species"))

cat ("Press [enter] to continue")
line <- readline()

#---------------Figure 3. Boxplot of Seedling Density by Species--------------#

#win.graph(7,11) 
#bins = seq(0,6000,100) # defining the bin sequences
#par(mfrow=c(1,1))  #---par function sets number or rows and columns for a matrix for plots

#boxplot(nseedlings_den_eachspecies_sitesum2) #--------NEEDS FIXED - include all species in column 6
#boxplot(nseedlings_den_eachclass_sitesum2)


#cat ("Press [enter] to continue")
#line <- readline()


#---------------Figure 4. Boxplot of Mean Seedling Height by Species--------------#

#win.graph(7,11) 
#bins = seq(0,6000,100) # defining the bin sequences
#par(mfrow=c(1,3))  #---par function sets number or rows and columns for a matrix for plots

#boxplot(seedling_by_class1) #----NEEDS FIXED----#
#boxplot(seedling_by_class2) #----NEEDS FIXED----#
#boxplot(seedling_by_class3) #----NEEDS FIXED----#

#cat ("Press [enter] to continue")
#line <- readline()

#--------------Plots the density of each species by all classes ------------------#

win.graph(11,10) 
bins = seq(0,6000,500) # defining the bin sequences
par(mfrow=c(1,6))  #---par function sets number or rows and columns for a matrix for plots

boxplot(density_spp_den_eachspecies1,
        col="red",
        xlab="Class",
        ylab="Seedling Density (m2)",
        main="Pico")

boxplot(density_spp_den_eachspecies2, 
        col="red",
        xlab="Class",
        ylab="Seedling Density (m2)",
        main="Pipo")

boxplot(density_spp_den_eachspecies3, 
        col="red",
        xlab="Class",
        ylab="Seedling Density (m2)",
        main="Psme")
boxplot(density_spp_den_eachspecies4,
        col="red",
        xlab="Class",
        ylab="Seedling Density (m2)",
        main="Pien")
boxplot(density_spp_den_eachspecies5, 
        col="red",
        xlab="Class",
        ylab="Seedling Density (m2)",
        main="Abla")
boxplot(density_spp_den_eachspecies6, 
        col="red",
        xlab="Class",
        ylab="Seedling Density (m2)",
        main="All Species")

cat ("Press [enter] to continue")
line <- readline()

#--------------Plots the density of each species by all classes ------------------#


win.graph(11,10) 
bins = seq(0,6000,500) # defining the bin sequences
par(mfrow=c(1,6))  #---par function sets number or rows and columns for a matrix for plots

boxplot(density_spp_ht_eachspecies1,
        col="red",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Pico")
boxplot(density_spp_ht_eachspecies2,
        col="red",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Pipo")
boxplot(density_spp_ht_eachspecies3,
        col="red",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Psme")
boxplot(density_spp_ht_eachspecies4, 
        col="red",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Pien")
boxplot(density_spp_ht_eachspecies5,
        col="red",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="Abla")
boxplot(density_spp_ht_eachspecies6,
        col="red",
        xlab="Class",
        ylab="Mean Seedling Height (cm)",
        main="All Species")