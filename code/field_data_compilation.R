core <- read.csv("FIELD_DATA/S123_092823_CSV/tree_data_092823.csv") %>% 
  select(Core.ID,
         Site.ID,
         Tree.ID,
         Crown.vigor,
         Canopy.position,
         Cones,
         Blister.rust,
         Bark.beetles,
         Live.BA = Live.basal.area..neighborhood.,
         Dead.BA = Dead.basal.area..neighborhood.,
         Core.height = Coring.height..cm.,
         DIA = DCH..cm.,
         Comment = Additional.comment,
         Recorder = )

emlid <- read.csv("FIELD_DATA/emlid_092523.csv") %>% 
  select(Core.ID = Tree,
         LON = Longitude,
         LAT = Latitude,
         Lateral.RMS, 
         NSamples = Samples)

sites <- read.csv("FIELD_DATA/S123_092823_CSV/site_data_092823.csv", header=T) %>% 
  select(Site.ID,
         Niche.zone,
         State,
         SITE.LON.field = x,
         SITE.LAT.field = y,
         National.Forest,
         Site.Locality,
         Forest.Type,
         Species.Present,
         Management,
         Disturbance.Description,
         General.Site.Description,
         Tree.Number, 
         Recorder = Name, 
         Date)

all.dat <- core %>% 
  left_join(emlid, 
            by = "Core.ID") %>% 
  left_join(sites,
            by = "Site.ID") %>% 
  group_by(Site.ID) %>% 
  mutate(SITE.LON.calc = mean(LON,na.rm=T),
         SITE.LAT.calc = mean(LAT,na.rm=T)) %>% 
  ungroup()

sites <- sites %>% 
  left_join(all.dat %>% 
              select(Site.ID, contains("calc")) %>% 
              distinct())

write.csv(all.dat, file = "FIELD_DATA/core_data_complete_092523.csv", row.names = F)
write.csv(sites, file = "FIELD_DATA/site_data_complete_092523.csv", row.names = F)

















