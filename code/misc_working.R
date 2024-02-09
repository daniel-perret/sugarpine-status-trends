x <- growMort_dlp.metric(db = all.fia,
                         byPlot = F,
                         returnSpatial = F,
                         sizeThresh = 2.54,
                         treeDomain = SPCD==117,
                         evals = evals, 
                         stateVar = "TPA", 
                         totals = T,
                         grpBy = ECOSUBCD) %>% 
  group_by(ECOSUBCD) %>% 
  filter(YEAR == max(YEAR))

y <- growMort(db = all.fia,
              byPlot=F,
              treeDomain = SPCD==117,
              totals=T,
              grpBy = ECOSUBCD) %>% 
  group_by(ECOSUBCD) %>% 
  filter(YEAR == max(YEAR))

x <- x %>% filter(ECOSUBCD %in% y$ECOSUBCD)

rf <- growMort(db = all.fia2,
              byPlot=F,
              treeDomain = SPCD==117,
              totals=T,
              grpBy = ECOSUBCD,
              treeList = T) %>% 
  mutate(TRE = paste0(PLT_CN,".",SUBP,".",TREE))# %>% 
  #na.omit()

dp <- growMort_dlp.metric(db = all.fia,
                          byPlot = F,
                          returnSpatial = F,
                          sizeThresh = 2.54,
                          treeDomain = SPCD==117,
                          evals = evals, 
                          stateVar = "TPA", 
                          totals = T,
                          grpBy = ECOSUBCD,
                          treeList = T) %>% 
  mutate(TRE = paste0(PLT_CN,".",SUBP,".",TREE))

