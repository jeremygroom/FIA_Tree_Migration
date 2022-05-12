######################################################################################
###    Plot Occupancy Range-shift Analysis   
###    The code was copied from Analysis2020.3_extremes.R
###    This code processes the plot data for species distribution during the first and second visit.  
###    The code is organized to:
###            (1) Prepare the data and species lists for analysis and subsequent examination
###            (2) Determine the estimated difference in ratio estimators
###            (3) Conduct a bootstrap analysis to arrive at the variance estimates via a different approach
###            Bootstrapping is done for the mean and results are funneled through a GLS to arrive at a intra-species mean.
###            (removed) (4) An exploratory plot of temperatures of colonized and extirpated plots
###            (5)    Figures and tables for presence/absence range shift 
###            (6) Figure of range-shift histograms
###            (7) Code to extracting FIA plot points for GIS map
######################################################################################


######################################################################################
### (1) Prepare the data and species lists for analysis and subsequent examination
######################################################################################

library(tidyverse)
library(readxl)


############## 
# -- some constants

# The SELECT.VAR constant will be used to create data with only the selected variable and save output with that variable name.
SELECT.VAR <- "TEMP" # Variables = "TEMP", "annpre"
#   For         Mean Temperature, Annual Precipitation

RESP.TIMING <- 2 # 1 #2  # For which visit (1st or 2nd) do we want the predicted temperature or precipitation values?

RES <- "Results/Occ_Results/"   # This will become the results folder for this run. Initially just the root occupancy results folder
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


## stratification information
strat <- read_csv(paste0(loc, "strat_info052120.csv")) %>%
  mutate(W_h = P1POINTCNT/p1pntcnt_eu)             # W_h is the stratum weight
strat2 <- strat %>% select(STRATUM, P1POINTCNT, W_h)  # Reducing the number of columns to those we need 


orig <- read_csv(paste0(loc, "Occ_OriginalVisit.csv") ) %>% left_join(resp.values[, c(1:2, 4)], by = c("STATECD", "PLOT_FIADB"))
orig[, 13] <- orig[, ncol(orig)]   # Attached the column "response" to the end of the data table, now making the TEMP column have the same values
colnames(orig)[13] <- SELECT.VAR
orig <- orig[, -ncol(orig)]

revis <- read.csv(paste0(loc, "Occ_Revisit.csv") ) 
revis[, 13] <- orig[, 13]  # revis and orig have the same data structure. In this analysis the temp/precip values are the same.
colnames(revis)[13] <- SELECT.VAR

# Test to verify that we have PRISM data for all points with (non-zero) tree data
x1 <- orig[is.na(orig$TEMP) == TRUE,  ]
sum(apply(x1[, 14:63], 1, sum))
which(apply(x1[, 14:63], 1, sum) > 0) # 952 1484
x1[897, ]  # Identified earlier.  STATECD = 53, PLOT_FIADB = 97917
#x1[952, ]  # Identified earlier.  STATECD = 53, PLOT_FIADB = 97917
#x1[1484,]  # STATECD = 53, PLOT_FIADB = 53851  Spp = 263, 351

x2 <- revis[is.na(revis$TEMP) == TRUE,  ] # Same two sites as before
sum(apply(x2[, 14:63], 1, sum))
which(apply(x2[, 14:63], 1, sum) > 0) # only 1484, STATECD = 53 PLOT_FIADB = 53851, spp = 202, 263, 351 

#####--- THESE POINTS MUST HAVE PRISM DATA FOR THEM FOR THE FINAL ANALYSIS --- #######
orig[orig$STATECD == 53 & orig$PLOT_FIADB == 97917, 14:63] <- 0   # Setting species value to zero because it is non-forested (coastal)
orig <- orig %>% filter(orig$State_Plot != 5385153)           # Removing this point from orig, revis because it was only surveyed in 2018
revis <- revis %>% filter(revis$State_Plot != 5385153)


# The PRISM data manipulation introduced NAs.  Changing those to zeros.  
orig[, 13][is.na(orig[, 13]) == T] <- 0
revis[, 13][is.na(revis[, 13]) == T] <- 0


# number of instances for each species by orig and revis
n.orig <- apply(orig[,14:63], 2, sum)
n.revis <- apply(revis[,14:63], 2, sum)

treecodes <- read_csv(paste0(loc, "Spp_Codes.csv"))
treecodes[,2] <- apply(treecodes[,2], 1, function(x) gsub("?", " ", x, fixed = T))  # replacing question marks in text with spaces
treecodes$match <- paste0("X", treecodes$SppCode)

ordered.spp <- tibble(spp.codes = colnames(revis)[14:63], n.orig = n.orig, n.revis = n.revis)
ordered.spp <- left_join(ordered.spp, treecodes, by = c("spp.codes" = "match"))
ordered.spp$spp.codes <- as.numeric(substr(ordered.spp$spp.codes, 2, nchar(ordered.spp$spp.codes)))

sppname <- ordered.spp$Common_Name

# the below file contains information on subgroup codes, softwood/hardwood, etc.
full.spp.codes <- read_xlsx(paste0(loc, "FullSppNames.xlsx")) %>%
  select(SPCD, COMMON_NAME, SPECIES_SYMBOL, GENUS, SPECIES) %>%
  mutate(SciName = paste(GENUS, SPECIES)) %>%
  select(-GENUS, -SPECIES, -COMMON_NAME)

ordered.spp <- left_join(ordered.spp, full.spp.codes, by = c("spp.codes" = "SPCD"))

orig <- data.frame(orig)  # otherwise, as a tibble, it uses way too much memory
revis <- data.frame(revis)


# Checking out temperature data... are any values with trees = 0?  Looks like the answer is no.
orig2 <- orig[,13:63]
orig2$Sum <- apply(orig2[,2:51], 1, sum)
which(orig2$Sum > 0 & orig2[, eval(SELECT.VAR)] == 0)

revis2 <- revis[,13:63]
revis2$Sum <- apply(revis2[,2:51], 1, sum)
which(revis2$Sum > 0 & revis2[, eval(SELECT.VAR)] == 0)



###############################################################################
###  (2)  Estimated Difference in Ratio Estimators          #######################
###############################################################################

# this function calculates the difference in the ratio estimators (change in temp)
# It needs to do so for each of the 25 strata
actmean <- function(datorig,datrevis,resp.orig,resp.revis) {     #resp.orig, resp.revis = response variable for the original and revisit data
  diff <- Zf <- Zs <- Yf <- Ys <- Rf <- Rs <- rep(0,length(sppname))                           # Mean difference in response for a given species
  for (i in 1:length(sppname)) {                         # i indexes species
    Zf_i <- Zs_i <- Yf_i <- Ys_i <- rep(0, length(strat2$STRATUM))  # Weighted values for each strata
    for(h in 1:length(strat2$STRATUM)){                  # h indexes strata
      Zf_i[h] <- strat2$W_h[h] * sum(datorig[datorig$STRATUM == strat2$STRATUM[h],i + 13])
      Zs_i[h] <- strat2$W_h[h] * sum(datrevis[datrevis$STRATUM == strat2$STRATUM[h],i + 13])
      Yf_i[h] <- strat2$W_h[h] * sum(datorig[datorig$STRATUM == strat2$STRATUM[h],i + 13] * get(SELECT.VAR, datorig)[datorig$STRATUM == strat2$STRATUM[h]])
      Ys_i[h] <- strat2$W_h[h] * sum(datrevis[datrevis$STRATUM == strat2$STRATUM[h],i + 13] * get(SELECT.VAR, datrevis)[datrevis$STRATUM == strat2$STRATUM[h]])
    }
    Zf[i] <- sum(Zf_i)
    Zs[i] <- sum(Zs_i)
    Yf[i] <- sum(Yf_i)
    Ys[i] <- sum(Ys_i)
    Rf[i] <- Yf[i] / Zf[i]
    Rs[i] <- Ys[i] / Zs[i]
    diff[i] <- Rs[i] - Rf[i]
  }
  means <- list(diff , Zf, Zs, Yf, Ys, Rf, Rs)
  names(means) <- c("diff", "Zf", "Zs", "Yf", "Ys", "Rf", "Rs")
  means
}

nn<-length(orig[,1])   # nn = N = total plots including ones not sampled

wt.varcov.fcn <- function(xy.sum, xsum, ysum, nnh) {(1/nn) * sum(strat2$W_h * nnh * (xy.sum - (1/nnh) * xsum * ysum))}

vardiffsum <- function(origdat,revisdat,ii, meandat)	{   # ii = column for response variable, meandat = list generated by actmean function
  origdat_z <- origdat_y <- origdat[, c(9, 14:63)]           # subsetting data for Z and Y values
  origdat_y[, 2:51] <- origdat_y[, 2:51] * get(SELECT.VAR, origdat)       # Y values are the indicator * response variable.  
  revisdat_z <- revisdat_y <- revisdat[, c(9, 14:63)]           # subsetting data for Z and Y values
  revisdat_y[, 2:51] <- revisdat_y[, 2:51] * get(SELECT.VAR, revisdat)       # Y values are the indicator * response variable.  
  varDIFF <- rep(0, length(sppname))
  for (i in 1:length(sppname))	{
    Zfh <- Zsh <- Yfh <- Ysh <- nn_h <- Zu2fh <- Zu2sh <- Yu2fh <- Yu2sh <- 
      ZYffh <- ZYssh <- ZZfsh <- ZYfsh <- YZfsh <- YYfsh <- rep(0, length(strat2$STRATUM))  # Weighted values for each strata
    for(h in 1:length(strat2$STRATUM)){ 
      # Pieces for calculating stratum-level variances.
      # For each stratum h for each species, first calculate the Z and Y values.
      Zfh[h] <- sum(origdat_z[origdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      Zsh[h] <- sum(revisdat_z[revisdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      Yfh[h] <- sum(origdat_y[origdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      Ysh[h] <- sum(revisdat_y[revisdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      # Number of plots in each stratum:
      nn_h[h] <- length(revisdat_z$STRATUM[revisdat_z$STRATUM == strat2$STRATUM[h]])
      # Square values for Z and Y (for Step 6, 7, and 8)
      Zu2fh[h] <- sum(origdat_z[origdat_z$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      Zu2sh[h] <- sum(revisdat_z[revisdat_z$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      Yu2fh[h] <- sum(origdat_y[origdat_y$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      Yu2sh[h] <- sum(revisdat_y[revisdat_y$STRATUM == strat2$STRATUM[h], i + 1] ^ 2)
      # ZY cross-products between first or second visits(for steps 9, 10)
      ZYffh[h] <- sum(origdat_z[origdat_z$STRATUM == strat2$STRATUM[h], i + 1] * origdat_y[origdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      ZYssh[h] <- sum(revisdat_z[revisdat_z$STRATUM == strat2$STRATUM[h], i + 1] * revisdat_y[revisdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      # Cross-products between first and second visits, all combinations (for Steps 13, 14, 15)
      ZZfsh[h] <- sum(origdat_z[origdat_z$STRATUM == strat2$STRATUM[h], i + 1] * revisdat_z[revisdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      ZYfsh[h] <- sum(origdat_z[origdat_z$STRATUM == strat2$STRATUM[h], i + 1] * revisdat_y[revisdat_y$STRATUM == strat2$STRATUM[h], i + 1])
      YZfsh[h] <- sum(origdat_y[origdat_y$STRATUM == strat2$STRATUM[h], i + 1] * revisdat_z[revisdat_z$STRATUM == strat2$STRATUM[h], i + 1])
      YYfsh[h] <- sum(origdat_y[origdat_y$STRATUM == strat2$STRATUM[h], i + 1] * revisdat_y[revisdat_y$STRATUM == strat2$STRATUM[h], i + 1])
    }
    
    
    nn<-length(origdat[,1])   # nn = N = total plots including ones not sampled
    
    
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
temactmean <- actmean(orig, revis, get(SELECT.VAR, orig), get(SELECT.VAR, revis))   
# Standard errors
temSE <- sqrt(vardiffsum(orig, revis, which(names(orig) == SELECT.VAR), temactmean))

sumtaylor<-tibble(sppname = ordered.spp$Common_Name, spp.codes = ordered.spp$spp.codes, Spp.symbol = ordered.spp$SPECIES_SYMBOL, 
                  SciName = ordered.spp$SciName, n.orig = ordered.spp$n.orig, n.revis = ordered.spp$n.revis, temactmean = temactmean$diff, temSE) %>%
  mutate(LCI = temactmean - 1.96 * temSE,
         UCI = temactmean + 1.96 * temSE)



write_csv(sumtaylor,paste0(RES, 'sumtaylor_occ_rangeshift_', SELECT.VAR, '_visit_', RESP.TIMING, '.csv'))


#################################################################################

# (3) Bootstrap version for comparison plus construction of GLS confidence intervals#

#################################################################################
library(doParallel)
library(foreach)  # Package "foreach" has parallel capabilities when running for-loops

no_cores <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
cl <- makeCluster(no_cores-1)  
registerDoParallel(cl) 



strat3 <- as.matrix(strat2)


actmean2 <- function(datorig,datrevis,resp.orig,resp.revis) {     #resp.orig, resp.revis = response variable for original or revisit data set.
  Zf_i <- Zs_i <- Yf_i <- Ys_i <- matrix(rep(0, length(strat3[, 1]) * 50), nrow = length(strat3[, 1]))  # Weighted values for each strata
  datorigY <- datorig[, 14:63] * datorig[, 13]
  datrevisY <- datrevis[, 14:63] * datrevis[, 13]
  for(h in 1:length(strat3[, 1])){                  # h indexes strata
    Zf_i[h, ] <- apply(datorig[datorig[, 9] == strat3[h, 1], 14:63], 2, sum) 
    Zs_i[h, ] <- apply(datrevis[datrevis[, 9] == strat3[h, 1], 14:63], 2, sum) 
    Yf_i[h, ] <-   apply(datorigY[datorig[, 9] == strat3[h, 1], 1:50], 2, sum) 
    Ys_i[h, ] <-  apply(datrevisY[datrevis[, 9] == strat3[h, 1], 1:50], 2, sum)
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
#  means <- diff*0.01
  means
}


n.iter <- 20000 #20000

bs.means <- matrix(rep(0, n.iter * 50), ncol = n.iter)

nrows <- nrow(orig)
# create new matrices where we don't need to sift through the strata each time
orig1 <- data.matrix(orig)
revis1 <- data.matrix(revis)


y <- Sys.time()
row.id <- 1:nrows
boot.out <- foreach(j = 1:n.iter) %dopar% {
  bs.id <- sample(row.id, replace = T)  
  orig2 <- orig1[bs.id, ]
  revis2 <- revis1[bs.id, ]  
  
  mean_results <- actmean2(orig2, revis2, orig2[,13], revis2[,13]) 

  list(boot.means = mean_results)
}
Sys.time() - y

for(i in 1:n.iter) {bs.means[, i] <- boot.out[[i]]$boot.means} 

## 25 seconds for 200, should take 42 minutes (took 40)

stopCluster(cl)

write_csv(data.frame(bs.means), paste0(RES, "BSmeans_", SELECT.VAR, "_", RESP.TIMING, ".csv"))
bs.means <- read_csv(paste0(RES, "BSmeans_", SELECT.VAR,  "_", RESP.TIMING, ".csv"))



bs.results <- data.frame(t(apply(bs.means, 1, quantile, probs = c(0.025, 0.5, 0.975)))) 
colnames(bs.results) <- c("p2.5", "mean", "p97.5")

bs.results2 <- tibble(sppname = ordered.spp$Common_Name, spp.codes = ordered.spp$spp.codes, Spp.symbol = ordered.spp$SPECIES_SYMBOL, 
                      SciName = ordered.spp$SciName, n.orig = ordered.spp$n.orig, n.revis = ordered.spp$n.revis, LCI = bs.results$p2.5, mean = bs.results$mean, UCI = bs.results$p97.5)


write_csv(bs.results2,paste0(RES, "bs.results2_", SELECT.VAR,  "_", RESP.TIMING, ".csv"))
bs.results2 <- read_csv(paste0(RES, "bs.results2_", SELECT.VAR,  "_", RESP.TIMING, ".csv"))

library(expm)

### Grand mean using GLS  ###

## Species 42, #768, Black Cherry, Prunus emarginata, needs to be removed


gls.fcn <- function(bsresults, deltas){
  #sigma.cor <- as.matrix(cor(t(bsresults)))
  sigma.cov <- as.matrix(cov(t(bsresults)))
  c.ident <- matrix(1, 49, 1)
  delta <- matrix(deltas, 49, 1)
  
  Vdelta <- solve(t(c.ident) %*% solve(sigma.cov) %*% c.ident)
  #DeltaHat <- Vdelta %*% t(c.ident) %*% solve(sigma.cor) %*% delta   # check
  DeltaHat <- Vdelta %*% t(c.ident) %*% solve(sigma.cov) %*% delta
  deltaUCI <- DeltaHat + 1.96 * sqrt(Vdelta)
  deltaLCI <- DeltaHat - 1.96 * sqrt(Vdelta)
  list(deltaUCI, DeltaHat, deltaLCI)
}

bs.means.gls <- gls.fcn(bs.means[-42,], bs.results2$mean[-42])  # Removing Black Cherry

## GLS output for mean, 95, 05
gls.out <- data.frame(means = unlist(bs.means.gls)) 
row.names(gls.out) <- c("UCI", "mean", "LCI")
write_csv(gls.out, paste0(RES, "gls_out_", SELECT.VAR, "_", RESP.TIMING, ".csv"))
gls.out <- read_csv(paste0(RES, "gls_out_", SELECT.VAR, "_", RESP.TIMING, ".csv"))




#########################################################################
##          (5)    Figures and tables for presence/absence range shift
##########################################################################

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

r1 <- fia.dataprep.fcn(paste0(RES, "sumtaylor_occ_rangeshift_", SELECT.VAR, "_visit_", RESP.TIMING, ".csv")) %>% filter(spp.codes != 768) %>%
  arrange(desc(temactmean))
r1.order <- r1 %>% select(Spp.symbol) %>%
  mutate(order = seq(1:n()))
r2 <- fia.dataprep.fcn(paste0(RES, "bs.results2_", SELECT.VAR, ".csv"))  %>% filter(spp.codes != 768) %>%
  left_join(r1.order, by = "Spp.symbol") %>%
  arrange(order)
#r3 <- fia.dataprep.fcn("bs.95.results2.csv")  %>% filter(spp.codes != 768)  %>%    # REMOVED CI PLOT CODE
#    left_join(r1.order, by = "Spp.symbol") %>%
#    arrange(order)
#r4 <- fia.dataprep.fcn("bs.05.results2.csv")  %>% filter(spp.codes != 768) %>%
#  left_join(r1.order, by = "Spp.symbol") %>%
#  arrange(order)
gls.vals <- read.csv(paste0(RES, "gls_out_", SELECT.VAR, "_", RESP.TIMING, ".csv"))



# In case Lithocarpus densiflorus needs to be altered (code = NODE3)
#r1$SciName <- ifelse(r1$SciName == "Lithocarpus densiflorus", "Notholithocarpus densiflorus", r1$SciName)
#r1$SciName2 <- ifelse(r1$SciName2 == "*Lithocarpus densiflorus*", "*Notholithocarpus densiflorus*", r1$SciName2)
#r2$SciName <- ifelse(r2$SciName == "Lithocarpus densiflorus", "Notholithocarpus densiflorus", r2$SciName)
#r2$SciName2 <- ifelse(r2$SciName2 == "*Lithocarpus densiflorus*", "*Notholithocarpus densiflorus*", r2$SciName2)

colnames(r1)[7] <- "temactmean"

bs.plot.fcn <- function(data1, meanvalue, savename, gls.col, titletxt, xaxistxt, ylabtxt, keepy){
  ggplot(data1, aes(factor(xaxistxt, levels = xaxistxt), meanvalue)) +   # fct_reorder allows for reording the factor level (forecats in tidyverse)
    geom_hline(yintercept = 0, size = 0.1) + 
    geom_linerange(aes(y = meanvalue, ymin = LCI, ymax = UCI)) +
    geom_point(shape = 21,  aes(fill = factor(sig))) +
    scale_fill_manual(values = c("white", "black")) + 
    theme_classic() + 
    labs(y = ylabtxt, x = NULL, title = titletxt) +  ## can add unicode text (e.g., degree = \u00B0C)
    annotate("rect", xmin = -Inf , xmax = Inf,     # 'annotate' is useful for adding things to plots based on vectors, not tables
             ymin = as.numeric(gls.vals[3, gls.col]), ymax = as.numeric(gls.vals[1, gls.col]), alpha = 0.5) +
    coord_flip() + 
    theme(legend.position = "none", axis.text.y = ggtext::element_markdown(),  # above I add different number of asterisks to achieve italics and bold italics
          text = element_text(family="serif"),
          plot.title = element_text(hjust = 0.5)) +  # This is the best way to control the font and project it well I've found.
    if(keepy == 0) theme(axis.text.y = element_blank()) 
  # ggsave(savename, device = "png", width = 10, height = 15, units = "cm", dpi = 240, pointsize = 12)
}

individ.x.lab <- "Mean Temperature Difference\n(revisit minus original visit \u00B0C)" 
q1 <- bs.plot.fcn(r1, r1$temactmean, "MeansPlot1.png", 1, "Estimated Mean", r1$SciName2, NULL, 1)# r1$SciName2, NULL, 1 )#
p1 <- bs.plot.fcn(r2, r2$mean, "MeansPlot_BS1.png", 1, "Bootstrap Mean", r2$Spp.symbol, NULL, 0)#1)
#p2 <- bs.plot.fcn(r3, r3$mean, "MeansPlot_BS95.png", 2, "95th Percentile", r3$Spp.symbol, NULL, 0)  #Removed from analysis
#p3 <- bs.plot.fcn(r4, r4$mean, "MeansPlot_BS05.png", 3, "5th Percentile", r4$Spp.symbol, NULL, 0)   # Removed from analysis



# plotting q1 (estimated mean/var) and p1 (bootstrap mean/var)
## Remove "ggsave" call from function (hashmark)
##  Change y-axis of p1 to null
q1 <- as.grob(q1)
p1 <- as.grob(p1)

p_all <- plot_grid(q1, p1, ncol = 2, rel_widths = c(1, 0.65))
group.x.lab <- "Mean Temperature Difference (revisit minus original visit \u00B0C)"
x.grob <- textGrob(group.x.lab, gp = gpar(fontfamily = "serif"))
# here is the plot, saved to file: 
png(paste0(RES, "Mean_Comparison_", SELECT.VAR, "_", RESP.TIMING, ".png"), width = 7, height = 6, units = "in", res = 300)
grid.arrange(arrangeGrob(p_all, bottom = x.grob))
dev.off()

resp.names[which(resp.vars == SELECT.VAR)]
### Table of species, "symbol", plots inhabited pre-no-post, post-not-pre, both, maybe percentages

## First, housekeeping in loading and preparing the files

orig <- read_csv(paste0(loc, "Occ_OriginalVisit.csv")) %>%
  select(-c(2, 4:13, 64:66)) %>% pivot_longer(cols = -c(1, 2), names_to = "spp", values_to = "presence" )
revis <- read_csv(paste0(loc, "Occ_Revisit.csv"))  %>%
  select(-c(2, 4:13, 64:66)) %>% pivot_longer(cols = -c(1, 2), names_to = "spp", values_to = "presence" )

data.comb <- left_join(orig, revis, by = c("STATECD", "PLOT_FIADB", "spp")) %>% 
  mutate(hist = ifelse(presence.x == 1 & presence.y == 1, 2, 
                       ifelse(presence.x == 1 & presence.y == 0, 1,
                              ifelse(presence.x == 0 & presence.y == 1, 3, 0))))
data.comb2 <- data.comb %>% 
  group_by(spp) %>% 
  summarize(n1 = length(hist[hist == 1]),
            n2 = length(hist[hist == 2]),
            n3 = length(hist[hist == 3]),
            n0 = length(hist[hist == 0])) #%>%
#mutate(tot = sum(n1, n2, n3, n0))
n.plots <- nrow(orig)/50


# the below file contains information on subgroup codes, softwood/hardwood, etc.
full.spp.codes <- read_xlsx(paste0(loc, "FullSppNames.xlsx")) %>%
  select(SPCD, COMMON_NAME, SPECIES_SYMBOL, GENUS, SPECIES) %>%
  mutate(SciName = paste(GENUS, SPECIES),
         spcd  = paste0("X", SPCD)) %>%
  select(-GENUS, -SPECIES, -COMMON_NAME) %>% 
  left_join(data.comb2, by = c("spcd" = "spp"))  %>% 
  mutate(pre = n1 + n2,
         post = n2 + n3)

full.spp.codes <- full.spp.codes[is.na(full.spp.codes$n3) == F,]

table1 <- full.spp.codes[, c(3, 2, 9, 10, 5, 7)]
names(table1) <- c("Species", "Symbol", "First Visit", "Second Visit", "Lost", "Gained")
write_csv(table1, paste0(RES, "Table_OccCount_", SELECT.VAR,  "_", RESP.TIMING, ".csv"))

## WHich species not used but detected?


Analysis1 <- read_csv(paste0(loc, "AnalysisData1_noMicro_2019.csv")) %>% 
  group_by(SPCD) %>%
  summarize(n = n())

all.spp <- read_xlsx(paste0(loc, "FullSppNames.xlsx")) %>%
  select(SPCD, COMMON_NAME, SPECIES_SYMBOL, GENUS, SPECIES) %>%
  mutate(SciName = paste(GENUS, SPECIES)) %>%
  select(-GENUS, -SPECIES, -COMMON_NAME) %>% 
  right_join(Analysis1, by = "SPCD") %>%
  left_join(full.spp.codes[, c(1, 5)], by = "SPCD")

not.used.spp <- all.spp[is.na(all.spp$n1) == T,]

write_csv(not.used.spp, paste0(RES, "NotUsedSpp_", SELECT.VAR,  "_", RESP.TIMING, ".csv"))  # The column n1 is fairly arbitrary - the point is that there were no data, so it receives an NA


### Probability of obtaining 5 (or 8) significant species given 49 spp
p <- 0.5
n <- 49


# Same result
1 - pbinom(2, n, 0.05)
1 - pbinom(7, n, 0.05)

### Confidence interval comparison between bootstrap & estimated mean (already has bitter cherry, 768, removed)
comp.b.e <- tibble(spp.codes = r1$spp.codes, estCIdiff = r1$UCI-r1$LCI, bsCIdiff = r2$UCI - r2$LCI, n_sp = r2$n.revis) %>% 
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

n.plotBS.vs.Est <- tibble(Vals = c("n", "Mean", "LCI", "UCI"), Ests = c(n.est, mean_est, lci_n.est, uci_n.est), BS = c(n.bs, mean_bs, lci_n.bs, uci_n.bs))


write_csv(n.plotBS.vs.Est, paste0(RES, "nPlotBSvsEst_", SELECT.VAR, "_", RESP.TIMING,".csv"))





###########################################
### (7) Extracting points for GIS map
### We want all forested plots, not just those with our species in them
#############################################

#LatLon <- read_csv("PlotLatLon.csv")
LL <- read_csv(paste0(LOC, "plot052120.csv")) %>% filter(PLOT_STATUS_CD != 3) %>% 
  group_by(STATECD, PLOT_FIADB) %>% 
  summarize(sum_propfor = sum(propfor),
            n_plots = n()) %>%
  mutate(State_Plot = as.numeric(paste0(as.character(PLOT_FIADB), as.character(STATECD) ))) 


#%>%
  filter(n_plots == 2,
         sum_propfor > 1)


orig <- read_csv(paste0(LOC, "Occ_OriginalVisit.csv"))

orig_plots <- orig %>% filter(propfor > 0) %>% dplyr::select(1:3) %>% 
  mutate(State_Plot = as.numeric(paste0(PLOT_FIADB, STATECD)))
#%>% 
#  select(4)

ForPlots <- left_join(orig_plots, LL, by = "State_Plot") %>% filter(n_plots > 1)# %>% filter(is.na(LON) == F)

# QGIS can import CSV files as shape files
write_csv(ForPlots, paste0(RES, "GIS_LatLon_", SELECT.VAR,  "_", RESP.TIMING, ".csv"))























