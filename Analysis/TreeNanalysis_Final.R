######################################################################################
###    Tree Number Change Analysis   
###    The code was copied from TreeNanalysis1.R
###    This code processes the plot data for species distribution during the first and second visit.  
###    The code is organized to:
###            (1) Prepare the data and species lists for analysis and subsequent examination
###            (2) Determine the estimated difference in ratio estimators
###            (3) Conduct a bootstrap analysis to arrive at the variance estimates via a different approach
###            Bootstrapping is done for the mean, 5%, and 95% quantiles.  GLS analyses are done for each.
###            (4) An exploratory plot of temperatures of colonized and extirpated plots
###            (5)    Figures and tables for presence/absence range shift 
###            (6) Code to extracting FIA plot points for GIS map
######################################################################################


######################################################################################
### (1) Prepare the data and species lists for analysis and subsequent examination
######################################################################################


### Tree Number Analysis
#--  This code takes the Tree_cleaning_.....R code and prepares it for analysis to categorize plots
#-- as either having more, fewer, or the same number of trees of a given species (-, 0, +).  
###


library(tidyverse)
library(readxl)
library(readr)

############## 
# -- some constants

# The SELECT.VAR constant will be used to create data with only the selected variable and save output with that variable name.
SELECT.VAR <- "annpre" # Variables = "TEMP", "annpre", "decmint", "augmaxt", "smrmnvpd"
                         #   For         Mean Temp, Annual Precip, December Min Temp, August Max Temp, Summer Vapor Pressure Deficit
RESP.TIMING <- 2 # 1 #2  # For which visit (1st or 2nd) do we want the predicted temperature or precipitation values?

RES <- "Results/TreeNum_Results/"   # This will become the results folder for this run. Initially just the root occupancy results folder
RES <- paste0(RES, SELECT.VAR, RESP.TIMING, "/") # Finalizing the results folder location for this run.

LOC <- "Data/"  # Where the data are stored.

#############

resp.vars <- c("TEMP", "annpre")
resp.names <- c("Degrees C", "Precipitation MM")

# Obtaining the response value, whether it be temperature or precipitation values from the first or second visit,
#     as determined by the regression on the previous 10 years.
resp.values <- read_csv(paste0(loc, switch(SELECT.VAR, "TEMP" = "tmp.", "annpre" = "precip."), "1st.2nd.csv")) %>%
  select(c(1:3, RESP.TIMING + 3)) 
colnames(resp.values)[4] <- "response"
v1 <- resp.values[, 4]
v1$response <- ifelse(v1$response == 9999, NA, v1$response)
resp.values[, 4] <- v1$response



# Loading tree data to obtain numbers of trees
Tree5.2 <- readr::read_csv(unzip(paste0(LOC, "Cleaned_Trees_2019.zip"), "Cleaned_Trees_2019.csv"))

## Need to remove trees in the macroplot, otherwise ingrowth of really big trees can occur
Tree5.2 <- Tree5.2 %>% filter(DIST <= 24)

orig <- read_csv(paste0(LOC, "Occ_OriginalVisit.csv"))
revis <- read_csv(paste0(LOC, "Occ_Revisit.csv"))
n.orig <- apply(orig[,14:63], 2, sum)
#orig2 <- orig[, c(1:13, which(n.orig >= 1000)  + 13, 64:66)]

full.spp.codes <- read_xlsx(paste0(LOC, "FullSppNames.xlsx")) %>%
  select(SPCD, COMMON_NAME, SPECIES_SYMBOL, GENUS, SPECIES) %>%
  mutate(SciName = paste(GENUS, SPECIES)) %>%
  select(-GENUS, -SPECIES, -COMMON_NAME)

treecodes <- read_csv(paste0(LOC, "Spp_Codes.csv"))
treecodes[,2] <- apply(treecodes[,2], 1, function(x) gsub("?", " ", x, fixed = T))  # replacing question marks in text with spaces
treecodes$match <- paste0("X", treecodes$SppCode)

ordered.spp <- tibble(spp.codes = colnames(orig)[14:63], n.orig = n.orig)  
ordered.spp <- left_join(ordered.spp, treecodes, by = c("spp.codes" = "match"))
ordered.spp$spp.codes <- as.numeric(substr(ordered.spp$spp.codes, 2, nchar(ordered.spp$spp.codes)))

sppname <- ordered.spp$Common_Name

# Altering input trees to remove trees that appeared but weren't ingrowth
Tree_not.ingrowth <- Tree5.2 %>% filter(Alive_Status == 5, SDN == 1)  # Using the work performed for the occupancy analysis to define whether trees were new or ingrowth.  SDN = 1 = alive-alive,
                                                                            # Alive status = 5 = appeared in second visit
Tree5.3 <- anti_join(Tree5.2, Tree_not.ingrowth)                              #   (in other words, these trees most likely existed previously in the plot)
                                                                             
Analysis1 <- Tree5.3 %>% 
  filter(SDN != 0) %>%                 #removing SDN = 0 values here
  filter(STATUSCD == 1) %>%         # Need all trees to be alive (not dead or removed, 2 & 3)
  group_by(STATECD, PLOT_FIADB,State_Plot, SPCD) %>%
  summarize(n.fv = length(INVYR[INVYR < 2011]),
            n.sv = length(INVYR[INVYR >= 2011]),
            num.code = if(n.fv > n.sv) 1 else if(  # 1 = fewer trees second visit
              n.fv == n.sv) 2 else if(             # 2 = same number of trees both visits
                n.fv < n.sv) 3 else 4)             # 3 = More trees second visit






missed.dead.trees <- read_csv(paste0(LOC, "TreeNum_OddDeadTrees.csv"))  # The summarized tree list from data check from earlier version of analysis

Analysis1.2 <- rbind(Analysis1, missed.dead.trees)

summary_A1 <- Analysis1.2 %>% filter(SPCD %in% ordered.spp$SppCode) %>% select(SPCD, num.code) %>% 
  group_by(SPCD) %>%
  summarize(n_1 = length(num.code[num.code == 1]),
            n_2 = length(num.code[num.code == 2]),
            n_3 = length(num.code[num.code == 3]),
            pct_more = n_3/sum(n_1, n_2, n_3),
            pct_less = n_1/sum(n_1, n_2, n_3),
            total = sum(n_1, n_2, n_3))




#spp_A1 <- summary_A1 %>% filter(sum(n_1, n_2) > 50 & n_3 > 50) %>% select(SPCD)  # Species with > 50 plots with num.code = 1 or 2, > 50 plots with num.code = 3
spp_A1 <- summary_A1 %>% filter(n_1 > 50 & n_3 > 50) %>% select(SPCD)  # Species with > 50 plots with num.code = 1 , > 50 plots with num.code = 3 (NOT INCLUDING num.code 2)
ordered.spp <- ordered.spp[ordered.spp$spp.codes %in% spp_A1$SPCD,]

Analysis2 <- Analysis1 %>% filter(SPCD %in% spp_A1$SPCD)




### Next, to conduct the change-in-numbers analysis we need to prepare the analysis files.  What follows is copied from Dataprep_TreeOcc_Rangeshift3.R

plotall <- read_csv(paste0(LOC, "plot052120.csv"))  # 42540 unique plots
strat <- read_csv(paste0(LOC, "strat_info052120.csv")) %>%
  mutate(W_h = P1POINTCNT/p1pntcnt_eu)             # W_h is the stratum weight
strat2 <- strat %>% select(STRATUM, P1POINTCNT, W_h)  # Reducing the number of columns to those we need 


# creating ownership classes   
#### NOTE: using only the second visit ownership class.  If the second visit info is missing, it is copied from the first visit.  If the 
#### second visit has more than one ownership, we randomly select one WITH THE EXCEPTION that plots containing Forest Service land will be coded 
### as a 10 (Forest Service)  
plotall1.5 <- left_join(plotall, strat2, by = "STRATUM") 

# the below test verifies that no plot has a stratum that changes across years or multiple strata within a plot.  
test1 <- plotall1.5 %>% group_by(STATECD, PLOT_FIADB) %>% summarize(n.strat = length(unique(STRATUM))) %>% filter(n.strat > 1) 


# I see that although the strata are consistent, the values for propfor and other proportions can change between visits, as can PLOT_STATUS_CD.
#  What to do about cases where PLOT_STATUS_CD was or becomes a 3?
test2 <- plotall1.5 %>% group_by(PLOT_FIADB, STATECD) %>% summarize(n.propfor = length(unique(propfor))) %>% filter(n.propfor > 1) 

# There are 1278 plot visits where one of the visits PLOT_STATUS_CD = 3.  I checked and verified that there are no tree data plots in any of the plots where
# 
test3 <- plotall1.5 %>% group_by(PLOT_FIADB, STATECD) %>% summarize(propfor_sum = sum(propfor), 
                                                                    PSCD = 3 %in% PLOT_STATUS_CD) %>%
  filter(propfor_sum > 0, PSCD == T)

## Now we need to reduce the plotall to one value per plot.  We remove the RESERVCD and OWNGRPCD values and use our own classifications.  We
## select the second visit propfor/propnonfor/ etc. values to represent the plot.  
### I assume we remove 2010 plots from the analysis as well??  We have no tree data from INVYR == 2010.
# This selection reduces the plot number to 38236
plotall2 <- plotall1.5 %>% select(-plt_cn) %>% distinct()

# side exercise to estimate area for each ownership stratum, create table

plottmp <- read_csv(paste0(LOC, "anntmp.csv")) %>% mutate(TEMP = TEMP/100)
plotannpre <- read_csv(paste0(LOC, "../DataFiles_Code/New Variables 2021/climate_data110421.csv")) %>% dplyr::select(-PLOT_STATUS_CD, -smrtp) %>%
  mutate(annpre = exp(annpre/100))  # may have already made this transformation??
plot.min.aug.vp <- read_csv(paste0(LOC, "../DataFiles_Code/New Variables 2021/climate_data110421_2.csv")) %>% # December min temp, aug max temp, summer vapor pressure deficit
  mutate(decmint = decmint/100,
         augmaxt = augmaxt/100,
         smrmnvpd = smrmnvpd/100)
plot.vars <- left_join(plottmp, plotannpre, by = c("STATECD", "PLOT_FIADB"))
plot.vars <- left_join(plot.vars,plot.min.aug.vp, by = c("STATECD", "PLOT_FIADB")) %>%
  select(STATECD, PLOT_FIADB, eval(SELECT.VAR))           # The plot.vars table is reduced down the the selected variable SELECT.VAR



Analysis2 <- left_join(plotall2, Analysis2, by = c("STATECD", "PLOT_FIADB")) %>%
  distinct()
Analysis2 <- left_join(Analysis2, plot.vars)   %>%  # All plots are successfully joined to our plots with no propmiss gaps (NA).  
  select(-c(n.fv, n.sv))
  
  
### Back to prepping data for the analysis      
Analysis2[is.na(Analysis2)] <- 0         # replacing all NA values with 0


# now creating the matrix table
Analysis3 <- Analysis2 %>% 
  filter(INVYR > 2010) %>%
  select( -INVYR) %>%
  spread(SPCD, num.code) 
names(Analysis3)[14:ncol(Analysis3)] <- paste0("X", names(Analysis3)[14:ncol(Analysis3)])  # the "tibble" handles having numeric column names, but I can't stand it.  
Analysis3[is.na(Analysis3)] <- 0         # replacing all NA values with 0

# removing columns with fewer than 50 plot associations.  Since this was essentially done above, it removes one column (X0) which did not represent a tree species
spp.num <- rep(NA, length(14:(ncol(Analysis3))))  # blank vector
for(i in 14:(ncol(Analysis3) )) {
  spp.num[i-13] <- length(which(Analysis3[,i] > 0))  # Didn't use an apply(x, 2, sum) function because the values are not all 1 (some 2 and 3s)
}
Analysis3 <- Analysis3[,c(1:13, (which(spp.num > 49)+13))]
n.analysis3 <- ncol(Analysis3)

# Less Than data set
lt <- Analysis3  # lt = "less than / equal to", so plots that remained the same or lost trees

lt.m <-as.matrix(lt) # changing to matrix so that the replacement goes faster (much faster) than it would for a data frame
lt.m[,14:n.analysis3][lt.m[, 14:n.analysis3] == 3] <- 0  # make all 3's (more 2nd visit) = zero
lt.m[,14:n.analysis3][lt.m[, 14:n.analysis3] == 2] <- 0  # set all 2's (equal both visits) = 0 (<< equals 1 if really less than/equal to>>)
lt <- data.frame(lt.m)                 # 1's stay 1s


# Greater Than data set
gt <- Analysis3

gt.m <-as.matrix(gt)
gt.m[,14:n.analysis3][gt.m[, 14:n.analysis3] == 2 | gt.m[, 14:n.analysis3] == 1] <- 0
gt.m[,14:n.analysis3][gt.m[, 14:n.analysis3] == 3] <- 1
gt <- data.frame(gt.m) 

# here are the files to move forward with
write_csv(lt, paste0(dat.loc, "TreeNum_Input/LessThan_", SELECT.VAR, ".csv"))
write_csv(gt, paste0(dat.loc, "TreeNum_Input/GrThan_", SELECT.VAR, ".csv"))



#############################################################################################
###            (2) Determine the estimated difference in ratio estimators
#############################################################################################


#######################  Estimation Analysis ###################################
# Copied / adapted from Analysis2020.3.R

## ANALYSIS PREP


# number of instances for each species by lt(Less Than) and gt (Greater Than)
n.lt <- apply(lt[,14:n.analysis3], 2, sum)
n.gt <- apply(gt[,14:n.analysis3], 2, sum)

# the below file contains information on subgroup codes, softwood/hardwood, etc.
n.lt.df <- data.frame(SPCD = as.numeric(substr(row.names(data.frame(n.lt)), 2,  nchar(row.names(data.frame(n.lt))))), n.lt = n.lt)
n.gt.df <- data.frame(SPCD = as.numeric(substr(row.names(data.frame(n.gt)), 2,  nchar(row.names(data.frame(n.gt))))), n.gt = n.gt)
n.df <- left_join(n.lt.df, n.gt.df)

ordered.spp <- left_join(ordered.spp, full.spp.codes, by = c("spp.codes" = "SPCD")) %>%
  filter(SppCode %in% spp_A1$SPCD) %>%
  left_join(n.df, by = c("spp.codes" = "SPCD")) 


### ANALYSIS FUNCTIONS AND RESULTS ###
actmean <- function(datlt,datgt,resp.lt,resp.gt) {     #resp.lt, resp.gt = response variable less than/greater than
  diff <- Zf <- Zs <- Yf <- Ys <- Rf <- Rs <- rep(0,length(spp_A1$SPCD))                           # Mean difference in response var for a given species
  for (i in 1:length(spp_A1$SPCD)) {                         # i indexes species
    Zf_i <- Zs_i <- Yf_i <- Ys_i <- rep(0, length(strat2$STRATUM))  # Weighted values for each strata
    for(h in 1:length(strat2$STRATUM)){                  # h indexes strata
      Zf_i[h] <- strat2$W_h[h] * sum(datlt[datlt$STRATUM == strat2$STRATUM[h],i + 13])
      Zs_i[h] <- strat2$W_h[h] * sum(datgt[datgt$STRATUM == strat2$STRATUM[h],i + 13])
      Yf_i[h] <- strat2$W_h[h] * sum(datlt[datlt$STRATUM == strat2$STRATUM[h],i + 13] * get(SELECT.VAR, datlt)[datlt$STRATUM == strat2$STRATUM[h]])
      Ys_i[h] <- strat2$W_h[h] * sum(datgt[datgt$STRATUM == strat2$STRATUM[h],i + 13] * get(SELECT.VAR, datgt)[datgt$STRATUM == strat2$STRATUM[h]])
    }
    Zf[i] <- sum(Zf_i)
    Zs[i] <- sum(Zs_i)
    Yf[i] <- sum(Yf_i)
    Ys[i] <- sum(Ys_i)
    Rf[i] <- Yf[i] / Zf[i]
    Rs[i] <- Ys[i] / Zs[i]
    diff[i] <- Rs[i] - Rf[i]
  }
  means <- list(diff, Zf, Zs, Yf, Ys, Rf, Rs)
  names(means) <- c("diff", "Zf", "Zs", "Yf", "Ys", "Rf", "Rs")
  means
}

nn<-length(lt[,1])   # nn = N = total plots including ones not sampled
n.sp <- n.analysis3 - 13  # Number of species examined

wt.varcov.fcn <- function(xy.sum, xsum, ysum, nnh) {(1/nn) * sum(strat2$W_h * nnh * (xy.sum - (1/nnh) * xsum * ysum))}

vardiffsum <- function(ltdat,gtdat,ii, meandat)	{   # ii = column for response variable, meandat = list generated by actmean function
  ltdat_z <- ltdat_y <- ltdat[, c(9, 14:n.analysis3)]           # subsetting data for Z and Y values
  ltdat_y[, 2:(n.sp + 1)] <- ltdat_y[, 2:(n.sp + 1)] * get(SELECT.VAR, ltdat)       # Y values are the indicator * response variable.  
  gtdat_z <- gtdat_y <- gtdat[, c(9, 14:n.analysis3)]           # subsetting data for Z and Y values
  gtdat_y[, 2:(n.sp + 1)] <- gtdat_y[, 2:(n.sp + 1)] * get(SELECT.VAR, gtdat)       # Y values are the indicator * response variable.  
  varDIFF <- rep(0, length(spp_A1$SPCD))
  for (i in 1:length(spp_A1$SPCD))	{
    Zfh <- Zsh <- Yfh <- Ysh <- nn_h <- Zu2fh <- Zu2sh <- Yu2fh <- Yu2sh <- 
      ZYffh <- ZYssh <- ZZfsh <- ZYfsh <- YZfsh <- YYfsh <- rep(0, length(strat2$STRATUM))  # Weighted values for each strata
    for(h in 1:length(strat2$STRATUM)){ 
      # Pieces for calculating stratum-level variances.
      # For each stratum h for each species, first calculate the Z and Y values.
      Zfh[h] <- sum(ltdat_z[ltdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      Zsh[h] <- sum(gtdat_z[gtdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      Yfh[h] <- sum(ltdat_y[ltdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      Ysh[h] <- sum(gtdat_y[gtdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      # Number of plots in each stratum:
      nn_h[h] <- length(gtdat_z$STRATUM[gtdat_z$STRATUM == strat2$STRATUM[h]])
      # Square values for Z and Y (for Step 6, 7, and 8)
      Zu2fh[h] <- sum(ltdat_z[ltdat_z$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      Zu2sh[h] <- sum(gtdat_z[gtdat_z$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      Yu2fh[h] <- sum(ltdat_y[ltdat_y$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      Yu2sh[h] <- sum(gtdat_y[gtdat_y$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      # ZY cross-products between first or second visits(for steps 9, 10)
      ZYffh[h] <- sum(ltdat_z[ltdat_z$STRATUM == strat2$STRATUM[h], i + 1] * ltdat_y[ltdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      ZYssh[h] <- sum(gtdat_z[gtdat_z$STRATUM == strat2$STRATUM[h], i + 1] * gtdat_y[gtdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      # Cross-products between first and second visits, all combinations (for Steps 13, 14, 15)
      ZZfsh[h] <- sum(ltdat_z[ltdat_z$STRATUM == strat2$STRATUM[h], i + 1] * gtdat_z[gtdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      ZYfsh[h] <- sum(ltdat_z[ltdat_z$STRATUM == strat2$STRATUM[h], i + 1] * gtdat_y[gtdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      YZfsh[h] <- sum(ltdat_y[ltdat_y$STRATUM == strat2$STRATUM[h], i + 1] * gtdat_z[gtdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      YYfsh[h] <- sum(ltdat_y[ltdat_y$STRATUM == strat2$STRATUM[h], i + 1] * gtdat_y[gtdat_y$STRATUM == strat2$STRATUM[h], i + 1])
    }
    
    
    nn<-length(ltdat[,1])   # nn = N = total plots including ones not sampled
    
    
    # Weighed variances (Step 8 of algorithm sheet)
    w.varZf <- wt.varcov.fcn(Zu2fh, Zfh, Zfh, nn_h)
    w.varZs <- wt.varcov.fcn(Zu2sh, Zsh, Zsh, nn_h)
    w.varYf <- wt.varcov.fcn(Yu2fh, Yfh, Yfh, nn_h)
    w.varYs <- wt.varcov.fcn(Yu2sh, Ysh, Ysh, nn_h)
    
    # Covariances for visit-pair estimator (Step 11)
    w.covZYff <- wt.varcov.fcn(ZYffh, Zfh, Yfh, nn_h)
    w.covZYss <- wt.varcov.fcn(ZYssh, Zsh, Ysh, nn_h)
    
    # Variance of ratios (Step 12)
    varRf <- (nn/((nn - 1) * meandat$Zf[i]^2)) * (w.varYf + meandat$Rf[i]^2 * w.varZf - 2 * meandat$Rf[i] * w.covZYff)
    varRs <- (nn/((nn - 1) * meandat$Zs[i]^2)) * (w.varYs + meandat$Rs[i]^2 * w.varZs - 2 * meandat$Rs[i] * w.covZYss)
    
    # Weighted crosss-product covariances (Step 15)
    w.covZZfs <- wt.varcov.fcn(ZZfsh, Zfh, Zsh, nn_h)
    w.covZYfs <- wt.varcov.fcn(ZYfsh, Zfh, Ysh, nn_h)
    w.covYZfs <- wt.varcov.fcn(YZfsh, Yfh, Zsh, nn_h)
    w.covYYfs <- wt.varcov.fcn(YYfsh, Yfh, Ysh, nn_h)
    
    # Final variance, variance of the difference between the two ratios
    varDIFF[i] <- varRf + varRs - ((2 * nn)/((nn - 1) * meandat$Zf[i] * meandat$Zs[i])) *
      (w.covYYfs + meandat$Rf[i] * meandat$Rs[i] * w.covZZfs - meandat$Rf[i] * w.covZYfs - meandat$Rs[i] * w.covYZfs)
  }
  varDIFF
}

# Means plus other metrics that are carried over into the calculation of the variance		
temactmean <- actmean(lt, gt, get(SELECT.VAR, lt), get(SELECT.VAR, gt))   
# Standard errors
temSE <- sqrt(vardiffsum(lt, gt, which(names(lt) == SELECT.VAR), temactmean))

sumtaylor<-tibble(SppNames = ordered.spp$Common_Name, spp.codes = ordered.spp$spp.codes, Spp.symbol = ordered.spp$SPECIES_SYMBOL, 
                  SciName = ordered.spp$SciName, n.lt = ordered.spp$n.lt, n.gt = ordered.spp$n.gt, temactmean = temactmean$diff, temSE) %>%
  mutate(LCI = temactmean - 1.96 * temSE,
         UCI = temactmean + 1.96 * temSE)


results.loc <- c(paste0(loc, "TreeNum_Results/"))

write_csv(sumtaylor,paste0(results.loc, "sumtaylor_num_change_", SELECT.VAR, ".csv"))




#################################################################################
###            (3) Conduct a bootstrap analysis to arrive at the variance estimates via a different approach
###            Bootstrapping is done for the mean, 5%, and 95% quantiles.  GLS analyses are done for each.
#################################################################################

strat3 <- as.matrix(strat2)


### First, a bootstrap to estimate the mean and mean CI

actmean2 <- function(datlt,datgt,resp.lt,resp.gt) {     #resp.lt, resp.gt = response less than/greater than
  Zf_i <- Zs_i <- Yf_i <- Ys_i <- matrix(rep(0, length(strat3[, 1]) * n.sp), nrow = length(strat3[, 1]))  # Weighted values for each strata
  datltY <- datlt[, 14:(n.sp + 13)] * datlt[, 13]
  datgtY <- datgt[, 14:(n.sp + 13)] * datgt[, 13]
  for(h in 1:length(strat3[, 1])){                  # h indexes strata
    Zf_i[h, ] <- apply(datlt[datlt[, 9] == strat3[h, 1], 14:(n.sp + 13)], 2, sum) 
    Zs_i[h, ] <- apply(datgt[datgt[, 9] == strat3[h, 1], 14:(n.sp + 13)], 2, sum) 
    Yf_i[h, ] <-   apply(datltY[datlt[, 9] == strat3[h, 1], 1:n.sp], 2, sum) 
    Ys_i[h, ] <-  apply(datgtY[datgt[, 9] == strat3[h, 1], 1:n.sp], 2, sum)
  }
  Zf_i <- Zf_i * strat3[,3]
  Zs_i <- Zs_i * strat3[,3]
  Yf_i <- Yf_i * strat3[,3]
  Ys_i <- Ys_i * strat3[,3]
  
  
  Zf <- apply(Zf_i, 2, sum)
  Zs <- apply(Zs_i, 2, sum)
  Yf <- apply(Yf_i, 2, sum)
  Ys <- apply(Ys_i, 2, sum)
  Rf <- Yf / Zf
  Rs <- Ys / Zs
  diff <- Rs - Rf
  means <- diff*0.01
  means
}




n.iter <- 200

bs.means <- matrix(rep(0, n.iter * n.sp), ncol = n.iter)

nrows <- nrow(lt)
# create new matrices where we don't need to sift through the strata each time
lt1 <- data.matrix(lt)
gt1 <- data.matrix(gt)

y <- Sys.time()
for(j in 1:n.iter){
  row.id <- 1:nrows
  bs.id <- sample(row.id, replace = T)  
  lt2 <- lt1[bs.id, ]
  gt2 <- gt1[bs.id, ]  
  
  bs.means[, j] <- actmean2(lt2, gt2, lt2[,13], gt2[,13])   
}
Sys.time() - y

# 1 hr hrs for 20k iterations

write_csv(data.frame(bs.means), paste0(loc, "TreeNum_Results/BSmeans_", SELECT.VAR, ".csv"))
bs.means <- read_csv(paste0(loc, "TreeNum_Results/BSmeans_", SELECT.VAR, ".csv"))

#apply(bs.means, 1, mean)

bs.results <- data.frame(t(apply(bs.means, 1, quantile, probs = c(0.025, 0.5, 0.975)))) 
colnames(bs.results) <- c("p2.5", "mean", "p97.5")

bs.results2 <- tibble(sppname = ordered.spp$Common_Name, spp.codes = ordered.spp$spp.codes, Spp.symbol = ordered.spp$SPECIES_SYMBOL, 
                      SciName = ordered.spp$SciName, n.lt = ordered.spp$n.lt, n.gt = ordered.spp$n.gt, LCI = bs.results$p2.5, mean = bs.results$mean, UCI = bs.results$p97.5)

#
write_csv(bs.results2,paste0(loc, "TreeNum_Results/bs.results2_", SELECT.VAR, ".csv"))
bs.results2 <- read_csv(paste0(loc, "TreeNum_Results/bs.results2_", SELECT.VAR, ".csv"))


############ GLS Mean ########################



#library(expm)

### Grand mean using GLS  ###

## Species 42, #768, Black Cherry, Prunus emarginata, needs to be removed


gls.fcn <- function(bsresults, deltas){
  #sigma.cor <- as.matrix(cor(t(bsresults)))
  sigma.cov <- as.matrix(cov(t(bsresults)))
  c.ident <- matrix(1, n.sp - 1, 1)
  delta <- matrix(deltas, n.sp - 1, 1)
  
  Vdelta <- solve(t(c.ident) %*% solve(sigma.cov) %*% c.ident)
  #DeltaHat <- Vdelta %*% t(c.ident) %*% solve(sigma.cor) %*% delta   # check
  DeltaHat <- Vdelta %*% t(c.ident) %*% solve(sigma.cov) %*% delta   # check
  deltaUCI <- DeltaHat + 1.96 * sqrt(Vdelta)
  deltaLCI <- DeltaHat - 1.96 * sqrt(Vdelta)
  list(deltaUCI, DeltaHat, deltaLCI)
}

bs.means.gls <- gls.fcn(bs.means[-28,], bs.results2$mean[-28])  

gls.out <- data.frame(means = unlist(bs.means.gls)) 
row.names(gls.out) <- c("UCI", "mean", "LCI")
write_csv(gls.out, paste0(loc, "TreeNum_Results/gls_out", SELECT.VAR, ".csv"))



###########################################################################################
########-----    Skip this section; MC to determine stratified 95%tiles = rough  -------#####
###########################################################################################




###### Now for the bootstrap estimates of the 95th and 5th percentiles ###

library(EnvStats)


lt <- read_csv("LessThan.csv")
gt <- read_csv("GrThan.csv")


quant.fcn <- function(p, data_subset) {
  data_use <- data_subset[data_subset > 0]
  if(length(data_use) == 0 | length(data_use) == 1) 0 else qemp(p, data_use)
}

zero1.fcn <- function(w) {
  w2 <- w/w
  w2[is.nan(w2)] <- 0
  w2
}

actmean3 <- function(datlt,datgt,resp.lt,resp.gt) {     
  #Rprof()
  X95f_i <- X95s_i <- X05f_i <- X05s_i <- matrix(rep(0, length(strat3[, 1]) * n.sp), nrow = length(strat3[, 1]))  # Weighted values for each strata
  datltY <- datlt[, 14:(n.sp + 13)] * datlt[, 13]
  datgtY <- datgt[, 14:(n.sp + 13)] * datgt[, 13]
  for(h in 1:length(strat3[, 1])){                  # h indexes strata
    #    Zf_i[h, ] <- apply(datlt[datlt[, 9] == strat3[h, 1], 14:(n.sp + 13)], 2, sum) 
    #    Zs_i[h, ] <- apply(datgt[datgt[, 9] == strat3[h, 1], 14:(n.sp + 13)], 2, sum) 
    datltY2 <- datltY[datlt[, 9] == strat3[h, 1], ]
    datgtY2 <- datgtY[datgt[, 9] == strat3[h, 1], ]
    for(i in 1:n.sp) {
      X95f_i[h, i] <-  quant.fcn(0.95, datltY2[, i])  
      X95s_i[h, i] <-  quant.fcn(0.95, datgtY2[, i])
      X05f_i[h, i] <-  quant.fcn(0.05, datltY2[, i])
      X05s_i[h, i] <-  quant.fcn(0.05, datgtY2[, i])
    }
    #    X95f_i[h, ] <-  apply(datltY2, 2, quant.fcn, p = 0.95)   # slightly faster not to use apply fcn here
    #    X95s_i[h, ] <-  apply(datgtY2, 2, quant.fcn, p = 0.95)
    #    X05f_i[h, ] <-  apply(datltY2, 2, quant.fcn, p = 0.05)
    #    X05s_i[h, ] <-  apply(datgtY2, 2, quant.fcn, p = 0.05)
  }
  # Multiply the quantile values by weights
  X95f_i <- X95f_i * strat3[,3]
  X95s_i <- X95s_i * strat3[,3]
  X05f_i <- X05f_i * strat3[,3]
  X05s_i <- X05s_i * strat3[,3]
  # Multiply the Z values by weights
  Zx5f_i <-  zero1.fcn(X95f_i) * strat3[,3]
  Zx5s_i <-  zero1.fcn(X95s_i) * strat3[,3]
  # Add both types up by species
  X95f <- apply(X95f_i, 2, sum)
  X95s <- apply(X95s_i, 2, sum)
  X05f <- apply(X05f_i, 2, sum)
  X05s <- apply(X05s_i, 2, sum)
  Zx5f <- apply(Zx5f_i, 2, sum)
  Zx5s <- apply(Zx5s_i, 2, sum)
  # Create the ratios
  Rf95 <- X95f / Zx5f
  Rs95 <- X95s / Zx5s
  Rf05 <- X05f / Zx5f
  Rs05 <- X05s / Zx5s
  # Subtract the ratios and convert to C
  means95 <- (Rs95 - Rf95) * 0.01
  means05 <- (Rs05 - Rf05) * 0.01
  list(means95 = means95, means05 = means05)
  #Rprof(NULL)
  #summaryRprof()
}

 # Setting up parallel processing
library(doParallel)
library(foreach)  # Package "foreach" has parallel capabilities when running for-loops
no_cores <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
cl <- makeCluster(no_cores-1)  
registerDoParallel(cl) 


n.iter <- 20000


nrows <- nrow(lt)
# create new matrices where we don't need to sift through the strata each time
lt1 <- data.matrix(lt)
gt1 <- data.matrix(gt)

y <- Sys.time()  # 3.7 hrs w/ 20k iterations
#for(j in 1:n.iter){
boot.out <- foreach(j = 1:n.iter, .packages = "EnvStats") %dopar% {  # setting up a parallel loop.  Must have associated packages for functions it calls named.
  row.id <- 1:nrows
  bs.id <- sample(row.id, replace = T)  
  lt2 <- lt1[bs.id, ]
  gt2 <- gt1[bs.id, ]  
  
  bs.quant <- actmean3(lt2, gt2, lt2[,13], gt2[,13])   
 # bs.95[, j] <- bs.quant[[1]]
#  bs.05[, j] <- bs.quant[[2]]
list(boot.95 = bs.quant$means95, boot.05 = bs.quant$means05)
  }
Sys.time() - y


bs.95 <- matrix(rep(0, n.iter * n.sp), ncol = n.iter)
bs.05 <- matrix(rep(0, n.iter * n.sp), ncol = n.iter)

for(i in 1:n.iter) {
  bs.95[,i] <- boot.out[[i]]$boot.95
  bs.05[,i] <- boot.out[[i]]$boot.05
}


#saveloc <- "C:/Users/jeremygroom/Google Drive/Consulting Practice/Contracts/ODF_PP_FIA2020/FIA_2020_Analysis/Analysis/Analysis Files/Results/"
write_csv(data.frame(bs.95), "TreeNum_Results/BS.95.csv")
write_csv(data.frame(bs.05), "TreeNum_Results/BS.05.csv")

bs.95 <- read_csv("TreeNum_Results/BS.95.csv")
bs.05 <- read_csv("TreeNum_Results/BS.05.csv")

bs.95.results <- data.frame(t(apply(bs.95, 1, quantile, probs = c(0.025, 0.5, 0.975)))) 
colnames(bs.95.results) <- c("p2.5", "mean", "p97.5")

bs.95.results2 <- tibble(sppname = ordered.spp$Common_Name, spp.codes = ordered.spp$spp.codes, Spp.symbol = ordered.spp$SPECIES_SYMBOL, 
                         SciName = ordered.spp$SciName, n.lt = ordered.spp$n.lt, n.gt = ordered.spp$n.gt, LCI = bs.95.results$p2.5, mean = bs.95.results$mean, UCI = bs.95.results$p97.5)


bs.05.results <- data.frame(t(apply(bs.05, 1, quantile, probs = c(0.025, 0.5, 0.975)))) 
colnames(bs.05.results) <- c("p2.5", "mean", "p97.5")

bs.05.results2 <- tibble(sppname = ordered.spp$Common_Name, spp.codes = ordered.spp$spp.codes, Spp.symbol = ordered.spp$SPECIES_SYMBOL, 
                         SciName = ordered.spp$SciName, n.lt = ordered.spp$n.lt, n.gt = ordered.spp$n.gt, LCI = bs.05.results$p2.5, mean = bs.05.results$mean, UCI = bs.05.results$p97.5)


write_csv(bs.95.results2,"TreeNum_Results/bs.95.results2.csv")
write_csv(bs.05.results2,"TreeNum_Results/bs.05.results2.csv")

 # Again, extracting the Bitter Cherry, which is row 28 of the following tables/vectors
bs.95.gls <- gls.fcn(bs.95[-28, ], bs.95.results2$mean[-28])
bs.05.gls <- gls.fcn(bs.05[-28, ], bs.05.results2$mean[-28])

## GLS output for mean, 95, 05
gls.out <- data.frame(means = unlist(bs.means.gls), p95 = unlist(bs.95.gls), p05 = unlist(bs.05.gls)) 
row.names(gls.out) <- c("UCI", "mean", "LCI")
write_csv(gls.out, "TreeNum_Results/gls_out.csv")

#####################--- End 95% CI bootstrap  --------------########################################





##############################################################################
###                 PLOTTING AND TABLES                                   ####
###            (5)    Figures and tables for presence/absence range shift ####
##############################################################################

#setwd( "TreeNum_Results/")
#install.packages("remotes")
#remotes::install_github("clauswilke/ggtext")
library(ggtext)
library(tidyverse)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplotify) # enables as.grob() function

loadfonts(device='win')

fia.dataprep.fcn <- function(results.file) {
  rX <- read_csv(results.file)
  rX$sig <- ifelse((rX$LCI < 0 & rX$UCI < 0) | (rX$LCI > 0 & rX$UCI > 0), 1, 0)
  rX$SciName2 <- ifelse(rX$sig == 1, paste0("***", rX$SciName, "***" ), paste0("*", rX$SciName, "*" ))
  rX
}

r1 <- fia.dataprep.fcn(paste0(loc, "TreeNum_Results/sumtaylor_num_change_", SELECT.VAR, ".csv")) %>% filter(spp.codes != 768) %>%
  arrange(desc(temactmean))
r1.order <- r1 %>% select(Spp.symbol) %>%
  mutate(order = seq(1:n()))
r2 <- fia.dataprep.fcn(paste0(loc, "TreeNum_Results/bs.results2_", SELECT.VAR, ".csv")) %>% filter(spp.codes != 768) %>%
  left_join(r1.order, by = "Spp.symbol") %>%
  arrange(order)
#r3 <- fia.dataprep.fcn("TreeNum_Results/bs.95.results2.csv") %>% filter(spp.codes != 768) %>%
#  left_join(r1.order, by = "Spp.symbol") %>%
#  arrange(order)
#r4 <- fia.dataprep.fcn("TreeNum_Results/bs.05.results2.csv") %>% filter(spp.codes != 768) %>%
#  left_join(r1.order, by = "Spp.symbol") %>%
#  arrange(order)
gls.vals <- read_csv(paste0(loc, "TreeNum_Results/gls_out", SELECT.VAR, ".csv"))

# In case Lithocarpus densiflorus needs to be altered (code = NODE3)
r1$SciName <- ifelse(r1$SciName == "Lithocarpus densiflorus", "Notholithocarpus densiflorus", r1$SciName)
r1$SciName2 <- ifelse(r1$SciName2 == "*Lithocarpus densiflorus*", "*Notholithocarpus densiflorus*", r1$SciName2)
r2$SciName <- ifelse(r2$SciName == "Lithocarpus densiflorus", "Notholithocarpus densiflorus", r2$SciName)
r2$SciName2 <- ifelse(r2$SciName2 == "*Lithocarpus densiflorus*", "*Notholithocarpus densiflorus*", r2$SciName2)


colnames(r1)[7] <- "temactmean"

bs.plot.fcn <- function(data1, meanvalue, savename, gls.col, titletxt, xaxistxt, ylabtxt, keepy){
  ggplot(data1, aes(factor(xaxistxt, levels = xaxistxt), meanvalue)) +   # fct_reorder allows for reording the factor level (forecats in tidyverse)
    geom_linerange(aes(y = meanvalue, ymin = LCI, ymax = UCI)) +
    geom_hline( yintercept = 0, color = "black", size = 0.5) + 
    geom_point(shape = 21,  aes(fill = factor(sig))) +
    scale_fill_manual(values = c("white", "black")) + 
    theme_classic() + 
    labs(y = ylabtxt, x = NULL, title = titletxt) +  ## can add unicode text (e.g., degree = \u00B0C)
    annotate("rect", xmin = -Inf , xmax = Inf,     # 'annotate' is useful for adding things to plots based on vectors, not tables
             ymin = as.numeric(gls.vals[3, gls.col]), ymax = as.numeric(gls.vals[1, gls.col]), alpha = 0.5) +
    coord_flip() + 
    theme(legend.position = "none", axis.text.y = ggtext::element_markdown(),  # above I add different number of asterisks to achieve italics and bold italics
          text = element_text(family="Times New Roman"),
          plot.title = element_text(hjust = 0.5)) +  # This is the best way to control the font and project it well I've found.
    if(keepy == 0) theme(axis.text.y = element_blank()) 
  # ggsave(savename, device = "png", width = 10, height = 15, units = "cm", dpi = 240, pointsize = 12)
}

individ.x.lab <- "Mean Temperature Difference\n(revisit minus original visit \u00B0C)" 
q1 <- bs.plot.fcn(r1, r1$temactmean, "TreeNum_Results/MeansPlot1.png", 1, "Estimated Mean", r1$SciName2, NULL, 1)# r1$SciName2, NULL, 1 )#
p1 <- bs.plot.fcn(r2, r2$mean, "TreeNum_Results/MeansPlot_BS1.png", 1, "Bootstrap Mean", r2$Spp.symbol, NULL, 0)#1)
#p2 <- bs.plot.fcn(r3, r3$mean, "TreeNum_Results/MeansPlot_BS95.png", 2, "95th Percentile", r3$Spp.symbol, NULL, 0)
#p3 <- bs.plot.fcn(r4, r4$mean, "TreeNum_Results/MeansPlot_BS05.png", 3, "5th Percentile", r4$Spp.symbol, NULL, 0)


#############################################################################################
### WILL NEED TO CORRECT FIGURE LEGENDS AND SELECT AMONG FIGURES FOR NEW ANALYSIS BELOW THIS POINT  11/12/21
################################################################################################


# Plotting p1 through p3 on a single plot.  Estimated mean, BS 05 & 95% CI
q1 <- as.grob(q1)
p2 <- as.grob(p2)
p3 <- as.grob(p3)
p_all <- plot_grid(q1, p2, p3, ncol = 3, rel_widths = c(1, 0.5, 0.5))
group.x.lab <- "Mean Temperature Difference (revisit minus original visit \u00B0C)"
x.grob <- textGrob(group.x.lab, gp = gpar(fontfamily = "Times New Roman"))
# here is the plot, saved to file: 
png("TreeNum_Results/Mean_95_05_results.png", width = 7, height = 6, units = "in", res = 300)
grid.arrange(arrangeGrob(p_all, bottom = x.grob))
dev.off()


# plotting q1 (estimated mean/var) and p1 (bootstrap mean/var)
## Remove "ggsave" call from function (hashmark)
##  Change y-axis of p1 to null
q1 <- as.grob(q1)
p1 <- as.grob(p1)

p_all <- plot_grid(q1, p1, ncol = 2, rel_widths = c(1, 0.65))
group.x.lab <- "Mean Temperature Difference (revisit minus original visit \u00B0C)"
x.grob <- textGrob(group.x.lab, gp = gpar(fontfamily = "Times New Roman"))
# here is the plot, saved to file: 
png("TreeNum_Results/Mean_Comparison.png", width = 7, height = 6, units = "in", res = 300)
grid.arrange(arrangeGrob(p_all, bottom = x.grob))
dev.off()


### Table of species, "symbol", plots inhabited pre-no-post, post-not-pre, both, maybe percentages

table.2 <- left_join(summary_A1, full.spp.codes, by = "SPCD") %>% 
  select(c(1:4, 7:9)) %>%
  relocate(any_of(c("SciName", "SPCD", "SPECIES_SYMBOL", "n_1", "n_2", "n_3", "total"))) %>%
  filter(SPCD != 768)
names(table.2) <- c("Species",	"SppCode",	"Symbol", "Fewer", "Same", "More", "Total")
write_csv(table.2, "TreeNum_Results/Table 2.csv")



### Probability of obtaining 5 (or 8) significant species given 50 spp
p <- 0.5
n <- n.sp

## Brute force:
x_results <- rep(0, 10000000)
for(i in 1:100000) {
  x <- runif(34)
  x_results[i] <- length(x[x <= 0.025 | x >= 0.975])
}
length(x_results[x_results <= 12])
hist(x_results, breaks = 50)

# Same result
1 - pbinom(12, 33, 0.05)
1 - pbinom(15, 33, 0.05)

### Confidence interval comparison between bootstrap & estimated mean
comp.b.e <- tibble(r1$spp.codes, estCIdiff = r1$UCI-r1$LCI, bsCIdiff = r2$UCI - r2$LCI, n_sp = r2$n.gt) %>% 
  mutate(bs_less = ifelse(bsCIdiff < estCIdiff, 1, 0),
         est_less = 1 - bs_less)


mean_est <- mean(comp.b.e$n_sp[comp.b.e$est_less == 1])
se_est <- sd(comp.b.e$n_sp[comp.b.e$est_less == 1])/sqrt(sum(comp.b.e$est_less))
lci_n.est <- mean_est - 1.96*se_est; uci_n.est <- mean_est + 1.96*se_est
n.est <- length(comp.b.e$n_sp[comp.b.e$est_less == 1])
mean_est; lci_n.est; uci_n.est

mean_bs <- mean(comp.b.e$n_sp[comp.b.e$bs_less == 1])
se_bs <- sd(comp.b.e$n_sp[comp.b.e$bs_less == 1])/sqrt(sum(comp.b.e$bs_less))
lci_n.bs <- mean_bs - 1.96*se_bs; uci_n.bs <- mean_bs + 1.96*se_bs
n.bs <- length(comp.b.e$n_sp[comp.b.e$bs_less == 1])
mean_bs; lci_n.bs; uci_n.bs

n.num.plotBS.vs.Est <- tibble(Vals = c("n", "Mean", "LCI", "UCI"), Ests = c(n.est, mean_est, lci_n.est, uci_n.est), BS = c(n.bs, mean_bs, lci_n.bs, uci_n.bs))


write_csv(n.num.plotBS.vs.Est, "TreeNum_Results/n.num.PlotBSvsEst.csv")



############## Histogram figures of plots with more/fewer trees and temperature ##################
orig <- read.csv("OriginalVisit.csv") 
revis <- read.csv("Revisit.csv") 


## Use these files if sidestepping running above code again
#lt <- read_csv("LessThan.csv")
#gt <- read_csv("GrThan.csv")


results.loc <- "TreeNum_Results/"
bs.results2 <- read_csv(paste0(results.loc, 'bs.results2.csv')) %>%
  mutate(sig = ifelse(UCI < 0 | LCI > 0, 1, 0))

sumtaylor <- read_csv(paste0(results.loc, 'sumtaylor_num_change.csv')) %>%
  mutate(sig = ifelse(UCI < 0 | LCI > 0, 1, 0))

   # Currently only includes non-bootstrap signficant species. 
spp.include <- sumtaylor$Spp.symbol[sumtaylor$sig == 1]
#spp.include <- unique(c(bs.results2$Spp.symbol[bs.results2$sig == 1], sumtaylor$Spp.symbol[sumtaylor$sig == 1]))  # This option will plot all sig species, boostrap or estimated


lt2 <- lt[,13:(n.sp + 13)]
lt2 <- data.frame(lt2)
lt2[,2:(n.sp + 1)] <- lt2[,1] * lt2[,2:(n.sp + 1)]

gt2 <- gt[,13:(n.sp + 13)]
gt2 <- data.frame(gt2)
gt2[,2:(n.sp + 1)] <- gt2[,1] * gt2[,2:(n.sp + 1)]

ordered.spp2 <- ordered.spp %>% select(c(3, 4, 5, 6, 7))

lt3 <- lt2 %>% pivot_longer(2:(n.sp + 1), names_to = "spp", values_to = "temps") %>% filter(temps > 0) %>%
  mutate(temps = temps/100)
lt3$spp <- substr(lt3$spp, 2, nchar(lt3$spp))
lt3$spp <- as.numeric(lt3$spp)
lt4 <- left_join(lt3[,2:3], ordered.spp2, by = c("spp" = "SppCode"))

gt3 <- gt2 %>% pivot_longer(2:(n.sp + 1), names_to = "spp", values_to = "temps") %>% filter(temps > 0) %>%
  mutate(temps = temps/100)
gt3$spp <- substr(gt3$spp, 2, nchar(gt3$spp))
gt3$spp <- as.numeric(gt3$spp)
gt4 <- left_join(gt3[,2:3], ordered.spp2, by = c("spp" = "SppCode"))

dat1 <- bind_rows("lt" = lt4, "gt" = gt4, .id = "groups")


# mean lines

sp.codes <- colnames(lt[, 14:(n.sp + 13)])
sp.codes <- substr(sp.codes, 2, nchar(sp.codes))
lt.means <- data.frame(spp = sp.codes, means = temactmean$Rf/100)
gt.means <- data.frame(spp = sp.codes, means = temactmean$Rs/100)
dat.means <- bind_rows("lt" = lt.means, "gt" = gt.means, .id = "groups") %>% 
  mutate(spp = as.numeric(spp)) %>%
  left_join(ordered.spp2, by = c("spp" = "SppCode"))


# plotting

dat.plot <- dat1 %>% filter(SPECIES_SYMBOL %in% spp.include)
dat.means.plot <- dat.means %>% filter(SPECIES_SYMBOL %in% spp.include)

ggplot(dat.plot, aes(temps, linetype = factor(groups, levels = c("lt", "gt")))) +
  geom_freqpoly() + 
  geom_vline(data = dat.means.plot, aes(xintercept = means, linetype = factor(groups, levels = c("lt", "gt")))) +
 # scale_linetype_manual(values = c(1, 1, 2, 2), guide = F) + 
  theme_classic() + 
  geom_hline(aes(yintercept = 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(n.breaks = 6) + 
  facet_wrap(~SciName, scales = "free", ncol = 4) + 
  theme(text=element_text(size=12,  family="Times New Roman"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(face = "italic"),
        legend.position = "none") +
  labs(x = "Mean Degrees C")
ggsave(paste0(results.loc, "TreeNumHistograms.png"), device = "png", width = 15, height = 20, units = "cm", dpi = 240, pointsize = 12)













