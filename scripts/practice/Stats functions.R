Stats_matrix <- function(mat){
  S = nrow(mat)
  L = sum(mat)
  
  basal = sum(colSums(mat) == 0)/S
  top   = sum(colSums(t(mat)) == 0)/S
  int   = 1 - basal - top
  gen   = mean(colSums(mat))
  vun   = mean(rowSums(mat))
  gensd = sd(colSums(mat)/(L/S))
  vunsd = sd(rowSums(mat)/(L/S))
  
  return(c(S,L,basal,int,top,gen,gensd,vun,vunsd))
}


# web stats using cheddar load into an LSwebs community file
web_stats <- data.frame(title  = sapply(LS_webs, function(x) x$properties$title),
                        LS    = sapply(LS_webs, function(x) x$properties$LS),
                        S = sapply(LS_webs, NumberOfNodes),
                        L = sapply(LS_webs, NumberOfTrophicLinks),
                        basal = sapply(LS_webs, FractionBasalNodes),
                        int = sapply(LS_webs, FractionIntermediateNodes),
                        top   = sapply(LS_webs, FractionTopLevelNodes),
                        gen   = sapply(LS_webs, function(x) mean(TrophicGenerality(x))),
                        gensd = sapply(LS_webs, function(x) sd(NormalisedTrophicGenerality(x))),
                        vun   = sapply(LS_webs, function(x) mean(TrophicVulnerability(x))),
                        vunsd = sapply(LS_webs, function(x) sd(NormalisedTrophicVulnerability(x))))
