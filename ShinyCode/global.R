##



## Setting up a rather deep loop to gather all estimate and bootstrap outputs plus the GLS CI output
o.n. <- c("occ.", "num.")
p.t. <- c("p", "t")
e.bs <- c("e", "bs")
o.n.folder <- c("Occ_Results/", "TreeNum_Results/")
p.t.folder <- c("annpre", "Temp")
e.bs.file <- c("sumtaylor_", "bs.results2_")  
timing <- 1:2

# Dat1 will include all species CI data.
# gls1 will include the bootstrapped GLS CIs.  Note that there is no similar interval for non-bootstrapped estimates
dat1 <- list(NULL)
names.dat1 <- NULL
gls1 <- list(NULL)
names.gls1 <- NULL
for (i in 1:length(o.n.)) {             # Occupancy vs. tree number analyses
  for (j in 1:length(p.t.)) {           # Precip vs. temperature
    for (k in 1:2) {                    # Timing (1 = use initial 10 yrs, 2 = use second 10 yrs)
      for (l in 1:length(e.bs)) {       # Taylor-series expansion estimation vs. bootstrap
        fileX <- paste0("Results/", o.n.folder[i], p.t.folder[j], k, "/", e.bs.file[l], p.t.folder[j], "_", timing[k], ".csv")
        datX <- read_csv(fileX, show_col_types = FALSE)
        dat1 <- append(dat1, list(datX))
        names.dat1 <- c(names.dat1, paste0(o.n.[i], p.t.[j], timing[k], ".", e.bs[l]))
        
        if (l == 2) next                   # The result is the same for estimation vs. bootstrap estimation, so skip if already done once.
        fileX.gls <-  paste0("Results/", o.n.folder[i], p.t.folder[j], k, "/gls_", p.t.folder[j], "_", timing[k], ".csv")
        glsX <- read_csv(fileX.gls, show_col_types = FALSE)
        gls1 <- append(gls1, list(glsX))
        names.gls1 <- c(names.gls1, paste0("gls.", o.n.[i], p.t.[j], timing[k]))
      }
    }
  }
}

names(dat1) <- c("NULL", names.dat1)
names(gls1) <- c("NULL", names.gls1)
