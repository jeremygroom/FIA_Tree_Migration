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
    geom_hline(yintercept = 0, size = 0.1) + 
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

