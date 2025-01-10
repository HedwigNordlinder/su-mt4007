
municipal_spearman_rho <- function(df) {
  
  municipalities <- unique(df$municipal_code)
  spearman_rho <- rep(0, length(municipalities))
  for(i in 1:length(municipalities)) {
    
    current_code <- municipalities[i]
    
    current_frame <- df %>% filter(municipal_code == current_code)
    spearman_rho[i] <- cor.test(current_frame$fe_residuals, current_frame$adjacent_case_ratios, method = "spearman")$estimate
    
  }
  return(data.frame(municipal_code = municipalities, srho = spearman_rho))
}

