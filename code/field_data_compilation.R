core <- read.csv("data/FIELD_DATA/S123_010924_CSV/tree_data_010924.csv") %>% 
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
         Comment = Additional.comment) %>% 
  bind_rows(read.csv("data/FIELD_DATA/S123_PEFsite_CSV/tree_data_1.csv") %>% 
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
                     Comment = Additional.comment))

emlid <- read.csv("data/FIELD_DATA/emlid_101623.csv") %>% 
  select(Core.ID = Core,
         LON = Longitude,
         LAT = Latitude,
         Lateral.RMS, 
         NSamples = Samples)

sites <- read.csv("data/FIELD_DATA/S123_010924_CSV/site_data_010924.csv", header=T) %>% 
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

write.csv(all.dat, file = "data/FIELD_DATA/core_data_complete_010924.csv", row.names = F)
write.csv(sites, file = "data/FIELD_DATA/site_data_complete_010924.csv", row.names = F)

















