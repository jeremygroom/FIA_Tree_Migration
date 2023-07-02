## Functions used in the code...

# Used in EstComp and CrossedComp figures to extract and prepare tibbles for plotting
fia.dataprep.fcn <- function(o.n.x, p.t.x, t.x, e.bs.x){
  rX <- as_tibble(dat1[[which(names(dat1) == paste0(o.n.x, p.t.x, t.x, ".", e.bs.x))]] )
  rX$sig <- ifelse((rX$LCI < 0 & rX$UCI < 0) | (rX$LCI > 0 & rX$UCI > 0), 1, 0)
  rX$SciName2 <- ifelse(rX$sig == 1, paste0("***", rX$SciName, "***" ), paste0("*", rX$SciName, "*" ))
  rX %>% filter(spp.codes != 768)
  names(rX)[which(names(rX) == "mean" | names(rX) == "temactmean")] <- "response"
  names(rX)[which(names(rX) == "sppname" | names(rX) == "SppNames")] <- "SppNames"
  rX
}



## Plotting function for EstComp
e.c.plot.fcn <- function(data1, meanvalue, gls.val, titletxt, spp, xlabtxt, ylabtxt, keepy, fontsize){
  ggplot(data1, aes(factor(spp, levels = spp), get(meanvalue))) +   # fct_reorder allows for reording the factor level (forecats in tidyverse)
    geom_hline(yintercept = 0, linewidth = 0.1) + 
    geom_linerange(aes(y = get(meanvalue), ymin = LCI, ymax = UCI)) +
    geom_point(shape = 21,  aes(fill = factor(sig))) +
    scale_fill_manual(values = c("white", "black")) + 
    theme_classic() + 
    labs(y = xlabtxt, x = ylabtxt, title = titletxt) +  ## can add unicode text (e.g., degree = \u00B0C)
    annotate("rect", xmin = -Inf , xmax = Inf,     # 'annotate' is useful for adding things to plots based on vectors, not tables
             ymin = as.numeric(gls.val[3, 1]), ymax = as.numeric(gls.val[1, 1]), alpha = 0.5) +
    coord_flip() + 
    theme(legend.position = "none",   
          text = element_text(family = "serif",
                              size = fontsize),
          axis.text.y = ggtext::element_markdown(),  # above I add different number of asterisks to achieve italics and bold italics
          plot.title = element_text(hjust = 0.5)) +  # This is the best way to control the font and project it well I've found.
    if (keepy == 0) theme(axis.text.y = element_blank()) 
  # ggsave(savename, device = "png", width = 10, height = 15, units = "cm", dpi = 240, pointsize = 12)
}

# This function produces the jpeg files for insertion into the manuscript. It relies on the plotting function 'e.c.plot.fcn' above.

comp.plt.all.fcn <- function(densrange, num.plt) {
  
  occnum <- if (densrange == "Range") "occ." else "num."
  # See dataprep function in functions.r
  r1 <- fia.dataprep.fcn(occnum, "t", "1", "e") %>% arrange(desc(response)) %>% 
    mutate(SciName2 = paste0("*", SciName, "*")) 
  
  r1.order <- r1 %>% select(Spp.symbol) %>%
    mutate(order = seq(1:nrow(r1))) %>% data.frame()
  
  r2 <- fia.dataprep.fcn(occnum, "p", "1", "e")  %>% data.frame()
  r3 <- fia.dataprep.fcn(occnum, "v_x", "1", "e")  %>% data.frame()
  r4 <- fia.dataprep.fcn(occnum, "v_n", "1", "e")  %>% data.frame()
  r_list <- list(r1.order, r2, r3, r4)
  
  r5 <- join_all(r_list, type = "left", match = "all") %>% arrange(order)
    
  gls.vals1 <- gls1[[which(names(gls1) == paste0("gls.", occnum, "t", "1"))]]
  gls.vals2 <- gls1[[which(names(gls1) == paste0("gls.", occnum, "p", "1"))]]
  gls.vals3 <- gls1[[which(names(gls1) == paste0("gls.", occnum, "v_x", "1"))]]
  gls.vals4 <- gls1[[which(names(gls1) == paste0("gls.", occnum, "v_n", "1"))]]
  
  plt1.title <- paste0(densrange, " Shift, Temperature")
  plt2.title <- "Precipitation"
  plt3.title <- "Maximum VPD"
  plt4.title <- "Minimum VPD"
  
  plt1.x.ax <- paste0("Difference in\ntemperature (\u00B0C)")
  plt2.x.ax <- paste0("Difference in\nprecipitation (mm)")
  plt3.x.ax <- paste0("Difference in\nmaximum VPD (hPa)")
  plt4.x.ax <- paste0("Difference in\nminimum VPD (hPa)")
  
  
  
  
  # Calling on plotting functions from functions.r
  
  q1 <- e.c.plot.fcn(r1, "response", gls.vals1, plt1.title, r1$SciName2, plt1.x.ax, NULL, 1, 11) 
  p1 <- e.c.plot.fcn(r2, "response", gls.vals2, plt2.title, r2$Spp.symbol, plt2.x.ax, NULL, 0, 11)#1)
  vx1 <- e.c.plot.fcn(r3, "response", gls.vals3, plt3.title, r3$SciName2, plt3.x.ax, NULL, 0, 11) 
  vn1 <- e.c.plot.fcn(r4, "response", gls.vals4, plt4.title, r4$Spp.symbol, plt4.x.ax, NULL, 0, 11)
  
  # plotting q1 (estimated mean/var) and p1 (bootstrap mean/var)
  ##  Change y-axis of p1 to null
  q1 <- as.grob(q1)
  p1 <- as.grob(p1)
  vx1 <- as.grob(vx1)
  vn1 <- as.grob(vn1)
  
  
  p_all <- plot_grid(q1, p1, vx1, vn1, ncol = 4, rel_widths = c(1, 0.6, 0.6, 0.6))
  
  jpeg(paste0(ms.file.loc, densrange, "Shift_All_Fig", num.plt, ".jpg"), width = 10, height = 7, units = "in", res = 600)
  print(p_all)      
  dev.off()
}

## Data summary function, both for Markdown and plotting all species spatial regression lines.
pto.fcn <- function(ch.data) {
   pto_pred <- ch.data %>% dplyr::select(pred1, pred2, spp) %>% 
    pivot_longer(cols = c(pred1, pred2), names_to = "pred_type", values_to = "pred_value") %>%
    mutate(timing = as.numeric(substr(pred_type, 5, 5)))
  pto3 <- ch.data %>% dplyr::select(x.min, x.max, spp) %>%
    pivot_longer(cols = c(x.min, x.max), names_to = "x_type", values_to = "x_value") %>% 
    mutate(timing = ifelse(x_type == "x.min", 1, 2)) %>%
    left_join(pto_pred, by = c("spp", "timing"))
  return(pto3)
}


 ## P-value summary function that translates p-values into asterices. 
sig.astx.fcn <- function(spp.dat, var) {
  sig.num <- get(var, spp.dat )
  sig.txt <- ifelse(is.na(sig.num) == TRUE, "", 
                    ifelse(sig.num >= 0.05, ".", 
                           ifelse(sig.num < 0.05 & sig.num >= 0.01, "*", 
                                  ifelse(sig.num < 0.01 & sig.num >= 0.001, "**", "***"))))
  sig.txt
}

