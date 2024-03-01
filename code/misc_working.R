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
              grpBy = MEASYEAR,
              treeList = F)
  
  mutate(TRE = paste0(PLT_CN,".",SUBP,".",TREE))# %>% 
  #na.omit()

dp <- growMort_dlp.metric(db = all.fia,
                          byPlot = F,
                          returnSpatial = F,
                          sizeThresh = 12.7,
                          treeDomain = SPCD==117,
                          evals = evals, 
                          stateVar = "TPA", 
                          totals = T,
                          grpBy = ECOSUBCD) %>% 
  filter(YEAR==2019)



all.fia$TREE <- all.fia$TREE %>% 
  mutate(sizeClass = makeClasses(DIA, interval = 25, numLabs=T))

all.fia$COND <- all.fia$COND %>% 
  mutate(dstrbClass1 = case_when(DSTRBCD1 %in% c(10:12) ~ "insect",
                                DSTRBCD1 %in% c(20:22) ~ "disease",
                                DSTRBCD1 %in% c(30:32) ~ "fire",
                                DSTRBCD1 == 54 ~ "drought",
                                DSTRBCD1 == 0 ~ "none",
                                TRUE ~ "other"),
         dstrbClass2 = case_when(DSTRBCD2 %in% c(10:12) ~ "insect",
                                 DSTRBCD2 %in% c(20:22) ~ "disease",
                                 DSTRBCD2 %in% c(30:32) ~ "fire",
                                 DSTRBCD2 == 54 ~ "drought",
                                 DSTRBCD2 == 0 ~ "none",
                                 TRUE ~ "other"),
         thinned = ifelse(TRTCD1 == 10, 1, 0))

trt <- growMort_dlp.metric(db = all.fia,
                          byPlot = F,
                          #polys = er3.pila,
                          returnSpatial = F,
                          sizeThresh = 2.54,
                          treeDomain = SPCD==117,# & DIA>99,
                          evals = evals, 
                          stateVar = "TPA", 
                          totals = T,
                          grpBy = c(sizeClass,dstrbClass1, OWNGRPCD),
                          treeList = F) %>% 
  filter(YEAR == 2019)

trt %>%
  ggplot(aes(x = sizeClass, y = MORT_PERC, col = factor(dstrbClass1))) + 
  #geom_point(size=3, alpha=0.7) + 
  geom_line(size = 1.5, alpha = 0.8) +
  geom_segment(aes(x = sizeClass, xend = sizeClass, y = MORT_PERC-MORT_PERC_SE,yend=MORT_PERC+MORT_PERC_SE)) +
  scale_color_startrek(name = "Disturbance \n (condition)") +
  labs(x = "Tree diameter (25 cm bins)",
       y = "Decadal mortality (%)") +
  facet_wrap(facets=~factor(OWNGRPCD,labels = c("USFS","Other Fed", "State", "Private")), nrow=2)

trt %>%
  ggplot(aes(x = sizeClass, y = MORT_TPH, col = factor(dstrbClass1))) + 
  #geom_point(size=3, alpha=0.7) + 
  geom_line(size = 1.5, alpha = 0.8) +
  geom_segment(aes(x = sizeClass, xend = sizeClass, y = MORT_TPH-MORT_TPH_SE,yend=MORT_TPH+MORT_TPH_SE)) +
  scale_color_startrek(name = "Disturbance \n (condition)") +
  labs(x = "Tree diameter (25 cm bins)",
       y = "Decadal mortality") +
  facet_wrap(facets=~factor(OWNGRPCD,labels = c("USFS","Other Fed", "State", "Private")), 
             nrow=2, scales="free_y")







own <- growMort_dlp.metric(db = all.fia,
                           byPlot = F,
                           #polys = er3.pila,
                           returnSpatial = F,
                           sizeThresh = 2.54,
                           treeDomain = SPCD==117,# & DIA>99,
                           evals = evals, 
                           stateVar = "BAA", 
                           totals = T,
                           grpBy = c(sizeClass,OWNGRPCD),
                           treeList = F) %>% 
  filter(YEAR == 2019) 

own %>%
  group_by(sizeClass) %>% 
  mutate(CURR_BAH_perc = CURR_BAH/sum(CURR_BAH),
         MORT_BAH_perc = MORT_BAH/sum(MORT_BAH)) %>% 
  ggplot(aes(x = sizeClass, y = CURR_BAH_perc, col = factor(OWNGRPCD,
                                                            labels=c("USFS",
                                                                     "Other Fed",
                                                                     "State",
                                                                     "Private")))) + 
  geom_rect(aes(xmin = 100,ymin=-Inf,xmax = Inf,ymax=Inf),fill="lightgray",col=NA) +
  geom_line(size = 1.5, alpha = 0.8) +
  scale_color_startrek(name = "Ownership") +
  labs(x = "Tree diameter (25 cm bins)",
       y = "Proportion of total live BA")
















