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
e.c.plot.fcn <- function(data1, meanvalue, gls.val, titletxt, xaxistxt, ylabtxt, keepy){
  ggplot(data1, aes(factor(xaxistxt, levels = xaxistxt), get(meanvalue))) +   # fct_reorder allows for reording the factor level (forecats in tidyverse)
    geom_hline(yintercept = 0, size = 0.1) + 
    geom_linerange(aes(y = get(meanvalue), ymin = LCI, ymax = UCI)) +
    geom_point(shape = 21,  aes(fill = factor(sig))) +
    scale_fill_manual(values = c("white", "black")) + 
    theme_classic() + 
    labs(y = ylabtxt, x = NULL, title = titletxt) +  ## can add unicode text (e.g., degree = \u00B0C)
    annotate("rect", xmin = -Inf , xmax = Inf,     # 'annotate' is useful for adding things to plots based on vectors, not tables
             ymin = as.numeric(gls.val[3, 1]), ymax = as.numeric(gls.val[1, 1]), alpha = 0.5) +
    coord_flip() + 
    theme(legend.position = "none", axis.text.y = ggtext::element_markdown(),  # above I add different number of asterisks to achieve italics and bold italics
          text = element_text(family = "serif",
                              size = 15),
          plot.title = element_text(hjust = 0.5)) +  # This is the best way to control the font and project it well I've found.
    if (keepy == 0) theme(axis.text.y = element_blank()) 
  # ggsave(savename, device = "png", width = 10, height = 15, units = "cm", dpi = 240, pointsize = 12)
}

