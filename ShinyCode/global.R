##

## Some constants:
metric.chng.res.loc <- "Results/MetricChange_Results/"





## Setting up a rather deep loop to gather all estimate and bootstrap outputs plus the GLS CI output
o.n. <- c("occ.", "num.")
p.t.v. <- c("p", "t", "v_x", "v_n")
e.bs <- c("e", "bs")
o.n.folder <- c("Occ_Results/", "TreeNum_Results/")
p.t.v.folder <- c("annpre", "Temp", "maxvpd", "minvpd")
e.bs.file <- c("sumtaylor_", "bs.results2_")  
timing <- 1:2

# Dat1 will include all species CI data.
# gls1 will include the bootstrapped GLS CIs.  Note that there is no similar interval for non-bootstrapped estimates
dat1 <- list(NULL)
names.dat1 <- NULL
gls1 <- list(NULL)
names.gls1 <- NULL
for (i in 1:length(o.n.)) {             # Occupancy vs. tree number analyses
  for (j in 1:length(p.t.v.)) {           # Precip vs. temperature
    for (k in 1:2) {                    # Timing (1 = use initial 10 yrs, 2 = use second 10 yrs)
      for (l in 1:length(e.bs)) {       # Taylor-series expansion estimation vs. bootstrap
        fileX <- paste0("Results/", o.n.folder[i], p.t.v.folder[j], k, "/", e.bs.file[l], p.t.v.folder[j], "_", timing[k], ".csv")
        datX <- read_csv(fileX, show_col_types = FALSE) %>% filter(spp.codes != 768)  # Removing bitter cherry
        dat1 <- append(dat1, list(datX))
        names.dat1 <- c(names.dat1, paste0(o.n.[i], p.t.v.[j], timing[k], ".", e.bs[l]))
        
        if (l == 2) next                   # The result is the same for estimation vs. bootstrap estimation, so skip if already done once.
        fileX.gls <-  paste0("Results/", o.n.folder[i], p.t.v.folder[j], k, "/gls_", p.t.v.folder[j], "_", timing[k], ".csv")
        glsX <- read_csv(fileX.gls, show_col_types = FALSE)
        gls1 <- append(gls1, list(glsX))
        names.gls1 <- c(names.gls1, paste0("gls.", o.n.[i], p.t.v.[j], timing[k]))
      }
    }
  }
}

names(dat1) <- c("NULL", names.dat1)
names(gls1) <- c("NULL", names.gls1)



## Mapping data ##

  # maps
states <- map_data("state")
west_df <- subset(states, region == "california" | region == "oregon" | region == "washington")
counties <- map_data("county")
west_county <- subset(counties, region == "california" | region == "oregon" | region == "washington")

  # Load temp/precip/latlong for all used points.
PR1.2. <- read_csv("Data/precip.20.10.csv", show_col_types = FALSE)   # precip.1st.2nd.csv
TMP1.2. <- read_csv("Data/tmp.20.10.csv", show_col_types = FALSE)   
VPDmax1.2. <- read_csv("Data/vpdmax.20.10.csv", show_col_types = FALSE)   
VPDmin1.2. <- read_csv("Data/vpdmin.20.10.csv", show_col_types = FALSE)   
lst.df <- list(PR1.2. = PR1.2., TMP1.2. = TMP1.2., 
               VPDmax1.2. = VPDmax1.2., VPDmin1.2. = VPDmin1.2.)
LL <- read_csv("Data/PlotLatLon.csv", show_col_types = FALSE) %>% dplyr::select(-n)
TC1.2. <- join_all(dfs = lst.df, by = c("STATECD", "PLOT_FIADB", "INVYR"), type = "left", match = "all") %>%
  mutate(State_Plot = as.numeric(paste0(PLOT_FIADB, STATECD)),
         change.temp = post.temp - pre.temp,
         change.precip = post.precip - pre.precip, 
         change.vpdmax = post.vpdmax - pre.vpdmax, 
         change.vpdmin = post.vpdmin - pre.vpdmin 
  ) %>%
  left_join(LL, by = "State_Plot") %>%
  dplyr::select(-STATECD, -PLOT_FIADB, -INVYR) %>%
  filter(pre.temp <= 8000)

  # Load occupancy and revisit data. 
     ## Note: Verified that all State_Plot numbers correspond with a positive finding (no zeros for State_Plot) for all four files.
occ_orig <- read_csv("Data/Occ_OriginalVisit.csv", show_col_types = FALSE) %>% dplyr::select(State_Plot, TEMP, starts_with("X")) 
occ_revis <- read_csv("Data/Occ_Revisit.csv", show_col_types = FALSE) %>% dplyr::select(State_Plot, TEMP, starts_with("X")) 
num_gt <- read_csv("Data/TreeNum_GrThan_Temp.csv", show_col_types = FALSE) %>% dplyr::select(State_Plot, response, starts_with("X")) 
num_lt <- read_csv("Data/TreeNum_LessThan_Temp.csv", show_col_types = FALSE) %>% dplyr::select(State_Plot, response, starts_with("X")) 


  # Spp codes 
spp.names <- read_csv("Results/Occ_Results/annpre1/bs.results2_annpre_1.csv", show_col_types = FALSE) %>%
  dplyr::select(sppname, spp.codes, SciName) %>%
  mutate(sel.names = paste0(SciName, ", ", sppname)) %>%
  filter(spp.codes != 768)  # removing bitter cherry

spp.names2 <- spp.names
spp.names2$id <- 3:(nrow(spp.names) + 2)

map.choices <-  split(spp.names2$id, spp.names2$sel.names)
map.choices <- c(list(`All Plots` = 1), list(`All Analyzed Plots` = 2), map.choices)

spp.gt <- colnames(num_gt)[grep("X", colnames(num_gt))]
spp.gt <- as.numeric(substr(spp.gt, 2, nchar(spp.gt)))
spp.absent.gt <- spp.names2 %>% mutate(gt.spp = ifelse(spp.codes %in% spp.gt, 1, 0)) %>%
  filter(gt.spp == 0)

  ## 
  ch.TempV1 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_pre.temp_delta.T.csv"), show_col_types = FALSE) 
  ch.TempV2 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_post.temp_delta.T.csv"), show_col_types = FALSE) 
  ch.PrecipV1 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_pre.precip_delta.P.csv"), show_col_types = FALSE) 
  ch.PrecipV2 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_post.precip_delta.P.csv"), show_col_types = FALSE) 
  ch.VPDmaxV1 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_pre.vpdmax_delta.Vmax.csv"), show_col_types = FALSE) 
  ch.VPDmaxV2 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_post.vpdmax_delta.Vmax.csv"), show_col_types = FALSE) 
  ch.VPDminV1 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_pre.vpdmin_delta.Vmin.csv"), show_col_types = FALSE) 
  ch.VPDminV2 <- read_csv(paste0(metric.chng.res.loc, "SpatialLM_post.vpdmin_delta.Vmin.csv"), show_col_types = FALSE) 
  

