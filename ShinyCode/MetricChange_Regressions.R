#### Metric Change Code ####
## This bit of code determines linear regressions for the change in metric (precipitataion, temperature) 
##   for species' occupancy points.  It does this for the independent variables of visit 1 and 2 temperature
##   and precipitation values.  It takes a while to run (perhaps 6 hours) although it tries to shorten the time
##   with parallel processing.
##
##   This code is not sourced by the Shiny app, only by the RMarkdown file, so its placement in this folder is for convenience.
##



occ.orig <- read_csv(paste0(data.file.loc, "Occ_OriginalVisit.csv")) %>% 
  dplyr::select(State_Plot, W_h,  starts_with("X"))

occ.rev <- read_csv(paste0(data.file.loc, "Occ_Revisit.csv")) %>% 
  dplyr::select(State_Plot, W_h,  starts_with("X"))

delt.t <- read_csv(paste0(data.file.loc, "tmp.20.10.csv")) %>% #"tmp.1st.2nd.csv")) %>% 
  mutate(delta.T = post.temp - pre.temp,
         State_Plot = as.numeric(paste0(PLOT_FIADB, STATECD))) %>%
  dplyr::select(State_Plot, pre.temp, post.temp, delta.T)

delt.p <- read_csv(paste0(data.file.loc, "precip.20.10.csv")) %>% #"precip.1st.2nd.csv")) %>% 
  mutate(delta.P = post.precip - pre.precip,
         State_Plot = as.numeric(paste0(PLOT_FIADB, STATECD))) %>%
  dplyr::select(State_Plot, pre.precip, post.precip, delta.P)

delt.maxv <- read_csv(paste0(data.file.loc, "vpdmax.20.10.csv")) %>% 
  mutate(delta.Vmax = post.vpdmax - pre.vpdmax,
         State_Plot = as.numeric(paste0(PLOT_FIADB, STATECD))) %>%
  dplyr::select(State_Plot, pre.vpdmax, post.vpdmax, delta.Vmax)

delt.minv <- read_csv(paste0(data.file.loc, "vpdmin.20.10.csv")) %>% 
  mutate(delta.Vmin = post.vpdmin - pre.vpdmin,
         State_Plot = as.numeric(paste0(PLOT_FIADB, STATECD))) %>%
  dplyr::select(State_Plot, pre.vpdmin, post.vpdmin, delta.Vmin)


occ.dat <- as.matrix(occ.orig[, 3:ncol(occ.orig)]) + as.matrix(occ.rev[, 3:ncol(occ.rev)])
occ.dat <- cbind(occ.orig[, 1:2], as.data.frame(occ.dat)) 

occ.dat$occ.vals  <- apply(occ.dat[, 3:ncol(occ.dat)], 1, sum)

occ.dat2 <- occ.dat[which(occ.dat$occ.vals > 0), ] %>% 
  left_join(delt.t, by = "State_Plot") %>%   
  left_join(delt.p, by = "State_Plot") %>%
  left_join(delt.maxv, by = "State_Plot") %>%
  left_join(delt.minv, by = "State_Plot") %>%
  dplyr::select(-occ.vals) %>%
  filter(pre.temp < 100) %>% 
  left_join(LL, by = "State_Plot") 


spp.labs <- names(occ.dat2 %>% dplyr::select(starts_with("X")))


timing.metric <-  c("pre.precip", "post.precip", "pre.temp", "post.temp", "pre.vpdmax", "post.vpdmax", "pre.vpdmin", "post.vpdmin")
deltas = c("delta.P", "delta.T", "delta.Vmax", "delta.Vmin")


# Function for examining the occupancy pattern of each species and asking whether the metric (temperature, precipitation) changed significantly from zero, and
#    if so, how.  This regression takes into account spatial nonindependence.
spp.slope.fcn <- function(sppX, prepost, deltaTPV) {
  # Gabbing temperature or precip data for a single species where the species was present.  Adding lat/lon because this will be changed into a spatial data frame.
  sp.X <- occ.dat2 %>% dplyr::select(all_of(prepost), all_of(deltaTPV), all_of(sppX), LAT, LON) %>% filter(get(sppX) > 0) %>%
    mutate(mean_val = get(prepost) - mean(get(prepost)))  # Want to center the independent var so that the intercept may be interpreted.
  
  x.min <- min(sp.X[, 1])   # min and max values for plotting
  x.max <- max(sp.X[, 1])
  test.X <- sp.X %>% filter(get(prepost) == x.min | get(prepost) == x.max) %>% # Setting up a data set for prediction at distribution endpoints (so we can draw a line)
  arrange(mean_val) 
  if (nrow(test.X) > 2) test.X <- test.X[c(1, nrow(test.X)), ]
  
  #Transforming the data frame into a spatial object.
  sp.shp <- sp.X  
  coordinates(sp.shp) = ~ LON+LAT
  proj4string(sp.shp) <- CRS("+proj=longlat +datum=WGS84")
  
  # Linear models
  x.lm1 <- lm(get(deltaTPV) ~ 1, data = sp.shp)        
  x.lm2 <- lm(get(deltaTPV) ~ mean_val, data = sp.shp)
  
  # Spatial error (moving average) model
  nbG <- graph2nb(gabrielneigh(sp.shp), sym = TRUE)   # Setting up a neighborhood of points via a Gabriel graph, a version of Delaunay triangulation 
  #     see: ?gabrielneigh, wikipedia: Gabriel graph
  lw <- nb2listw(nbG)                         # Spatial weights for neighbors list
  
  m1e <- spatialreg::errorsarlm(get(deltaTPV) ~ 1, data = sp.shp, lw, tol.solve = 1.0e-30, Durbin = FALSE)  # First spatial error regression, model of the mean.
  m2e <- spatialreg::errorsarlm(get(deltaTPV) ~ mean_val, data = sp.shp, lw, tol.solve = 1.0e-30)  # Second spatial error regression, has a slope for first/second visit metric
  
  mod.list <- list(x.lm1, x.lm2, m1e, m2e)   # Collect models
  
  
  mod.residuals <- purrr::map(mod.list, residuals)  # List of residuals for each model
  mod.moran <- purrr::map(mod.residuals, moran.mc, listw = lw, nsim = 999)  # Find Moran I statistic test result for each model's residuals.
  
  moran.p <- unlist(lapply(mod.moran, function(x) x$p.value))   # P-value for Moran test.
  mod.aic <- unlist(purrr::map(mod.list, AIC))                  # AIC value for each model
  
  
  if (AIC(m1e) - AIC(m2e) > 3) {      # See which model is better supported.  If the slope version is selected, indicate that with slp.X
    sel.me.X <- m2e; slp.X <- 1     # Selecting spatial error model with a slope, including an indicator of 1 for slope (slp.X).
    var.sig <- c(summary(m2e)$Coef[7], summary(m2e)$Coef[8])  # P-value of the intercept and slope term.
  } else {
    sel.me.X <- m1e; slp.X <- 0  # Selecting mean spatial error model, no slope.  slp.X = 0.   
    var.sig <- c(summary(m1e)$Coef[4], NA)  # p-value of the intercept term
  }
  
  list(Spp = sppX, x.min = x.min, x.max = x.max, slope = slp.X, predict.y = predict(sel.me.X, test.X)[,1], 
       aic = mod.aic - min(mod.aic), moran.p = moran.p, var.sig = var.sig, coeff.model = sel.me.X$coefficients, center.adj = mean(sp.shp@data[,1]),
       int.m1e.est = summary(m1e)$Coef[1], int.m1e.p = summary(m1e)$Coef[4])
  
} 


## Creating loop to find and summarize individual species spatial linear regression models.  Loop may run for many (e.g., 8) hours. 
##    The loop provides values for all visit 1 data and visit 2 data for both changes in temperature and precipitation and saves the summaries as rds files.  
y <- Sys.time() 

for (i in 1:length(deltas)) {       
  for (j in 1:2) {              # pre/post


deltaTPV = deltas[i]    # Don't need combination of delta.T and pre.precip.
prepost <-  timing.metric[(i - 1) * 2 + j] 
    
    
    # Setting up the parallel processing using all computer cores minus one.
    no_cores <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
    cl <- makeCluster(no_cores - 1 )  
    clusterExport(cl, varlist = c("spp.labs", "occ.dat2", "prepost", "deltaTPV"))  # Sending along variables to each cluster.
    clusterEvalQ(cl, c(library(dplyr), library(sp), library(spdep), library(spatialreg))) # each cluster loads the necessary libraries
    
    # pre.temp.out <- as.matrix(sapply(spp.labs, spp.slope.fcn, prepost = "pre.temp", deltaTPV = "delta.T"))
    pre.temp.out <- parSapply(cl, spp.labs, FUN = spp.slope.fcn, prepost = prepost, deltaTPV = deltaTPV)  # This is the cluster call
    stopCluster(cl)
#    pre.temp.out.save <- pre.temp.out

        # Gathering and saving the output.
    pre.temp.out <- as.matrix(pre.temp.out)
    pto1 <- unlist(pre.temp.out[5, ])
    pto1.vals <- substr(names(pto1), nchar(names(pto1)), nchar(names(pto1))) # avoiding .3 names
    pto2 <- tibble(spp = unlist(pre.temp.out[1,]),
                   pred1 = pto1[which(pto1.vals == "1")], 
                   pred2 = pto1[which(pto1.vals != "1")],
                   x.min = unlist(pre.temp.out[2, ]),
                   x.max = unlist(pre.temp.out[3, ]),
                   aic.vals = pre.temp.out[6, ],
                   moran.p = pre.temp.out[7, ],
                   slope.true = unlist(pre.temp.out[4, ]),
                   int.p = unlist(sapply(pre.temp.out[8, ], function(x) x[1])),
                   slp.p = unlist(sapply(pre.temp.out[8, ], function(x) x[2])),
                   int.coef = unlist(sapply(pre.temp.out[9, ], function(x) x[1])),
                   slp.coef = unlist(sapply(pre.temp.out[9, ], function(x) x[2])),
                   center.adj = unlist(pre.temp.out[10, ]),
                   int.alone.est = unlist(pre.temp.out[11, ]),
                   int.alone.p = unlist(pre.temp.out[12, ])) 
    
    write_csv(pto2, paste0(metric.chng.res.loc, "SpatialLM_", prepost, "_", deltaTPV, ".csv"))
    
    
  }
}

Sys.time() - y   # 8 hrs






