library(tidyverse)
library(tidygraph)
library(sf)
library(sfnetworks)
library(OpenStreetMap)
library(osmdata)
library(OpenStreetMap)

load(file = "networkClean.RData")

startup2016CorpWithConnections # general database about startups
tidyWith2016 # formal graph
graphW  # spatial graph


### Spatial network objects ####################################################


load("osmMapWithDzielniceWarszawy.RData")
####################################

library(sfnetworks)

coords <- cbind(name = startup2016CorpWithConnections$name, 
                lng = startup2016CorpWithConnections$lng, lat = startup2016CorpWithConnections$lat ) %>% as_tibble()

startupGraphFormal <- tidyWith2016 %>% activate(nodes) %>% filter(startup) %>% 
  left_join(., coords, by = "name")

formalNetwork <- as_sfnetwork(startupGraphFormal, coords = c("lng","lat"), crs = "+proj=longlat +datum=NAD83",
                              edges_as_lines = TRUE)
formalNetwork


graphW

startupGraphSpatial <- graphW %>% activate(nodes) %>% 
  left_join(., coords, by = "name")

spatialNetwork <- as_sfnetwork(startupGraphSpatial, coords = c("lng","lat"), crs = "+proj=longlat +datum=NAD83",
                               edges_as_lines = TRUE)
spatialNetwork


################################################################################
### Analysing formal connections database: #####################################

tidyWith2016 # 2447 nodes, 2235 edges

howManyWho <- tidyWith2016 %>% activate(nodes) %>% mutate(otherComp = ifelse(!(startup && osoba), TRUE, FALSE)) %>% 
  select(startup, osoba, otherComp) %>% as_tibble() %>% group_by(startup, osoba, otherComp) %>% 
  count() %>% as.data.frame() 
rownames(howManyWho) <- c("companies", "people", "startups")
howManyWho
#           startup osoba otherComp    n
# companies   FALSE FALSE      TRUE  194
# people      FALSE  TRUE      TRUE 1587
# startups     TRUE FALSE      TRUE  666

tidyWith2016 %>% activate(edges) %>% as_tibble() %>% 
  select(to_startup:connectToStartup) %>% 
  group_by(to_startup, from_startup, connectToStartup) %>% count()
#   to_startup from_startup connectToStartup     n
#   <lgl>      <lgl>        <lgl>            <int>
# 1 FALSE      TRUE         TRUE                66
# 2 TRUE       FALSE        TRUE              2142
# 3 TRUE       TRUE         TRUE                27


tidyWith2016 %>% activate(nodes) %>% as_tibble() %>% 
  select(component_id) %>% summary() 
# 470 components

tidyWith2016 %>% activate(nodes) %>%  as.data.frame() %>% 
  pull(howManyInGroup) %>% 
  as.factor() %>% summary() 
# Most with small number of elements

tidyWith2016 %>% activate(nodes) %>%  as.data.frame() %>% 
  pull(howManyInGroup) %>% hist()


# Calculate what are the components
tidyWith2016 %>% activate(nodes) %>% as_tibble() %>% 
  filter(howManyInGroup == 2) %>% 
  select(component_id) %>% group_by(component_id) %>% count() 

tidyWith2016 %>% activate(nodes) %>% as_tibble() %>% 
  filter(howManyInGroup > 5) %>% 
  select(component_id) %>% group_by(component_id) %>% count() 

tidyWith2016 %>% activate(nodes) %>% as_tibble() %>% 
  filter(howManyInGroup > 10) %>% 
  select(component_id) %>% group_by(component_id) %>% count() 
# only 22 include more than 10 nodes


################################################################################
### Get graph scores - centrality ##############################################
tidyWith2016 %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>% 
  select(degree, startup) %>%
  as_tibble() %>% 
  #group_by(startup) %>% 
  summarise(mean(degree), sd(degree), max(degree))

tidyWith2016 %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>% 
  select(degree, startup) %>%
  as_tibble() %>% 
  group_by(startup) %>% 
  summarise(mean(degree), sd(degree), max(degree))

graphW %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>% 
  select(degree) %>%
  as_tibble() %>% 
  summarise(mean(degree), sd(degree), max(degree))


################################################################################
### VISUALIZATION 1 ############################################################
library(ggraph)

tidyWith2016 <- tidyWith2016 %>% activate(nodes) %>% 
  mutate(type = ifelse(startup, "startup", ifelse(osoba, "person", "firm")))

# visualisation of the total formal graph
ggraph(tidyWith2016, layout = "stress") +
  geom_edge_link(col = "gray70") +
  #geom_edge_loop() +
  geom_node_point(size = 1, aes(colour = as.factor(type))) +
  #geom_node_text(aes(label = id)) +
  theme_graph() +
  labs(colour = "Type")+
  theme(legend.position='bottom',
        legend.text=element_text(size=12),
        legend.title =element_text(size=12))
# 770x530

tidyWith2016 %>% filter(startup)



################################################################################
### Select only subgraphs with at least 2 startups inside ######################

tidyWith2016 <- tidyWith2016 %>% activate(nodes) %>% 
  mutate(startup_n = ifelse(startup,1,0)) %>% 
  group_by(component_id) %>% 
  mutate(countStartups = sum(startup_n)) %>% 
  ungroup()

startupMultiGraph <- tidyWith2016 %>% activate(nodes) %>% filter(countStartups > 1) #%>% select(component_id)
whichInvovesMore <- tidyWith2016 %>% activate(nodes) %>% filter(countStartups > 1) %>% select(component_id) %>% pull(component_id) %>% unique()


ggraph(startupMultiGraph, layout = "stress") +
  geom_edge_link(col = "gray70") +
  #geom_edge_loop() +
  geom_node_point(size = 2, aes(colour = as.factor(type))) +
  #geom_node_text(aes(label = id)) +
  theme_graph() +
  labs(colour = "Type")+
  theme(legend.position='bottom',
        legend.text=element_text(size=12),
        legend.title =element_text(size=12))

howManyWho2 <- startupMultiGraph %>% activate(nodes) %>% mutate(otherComp = ifelse(!(startup && osoba), TRUE, FALSE)) %>% 
  select(startup, osoba, otherComp) %>% as_tibble() %>% group_by(startup, osoba, otherComp) %>% 
  count() %>% as.data.frame() 
rownames(howManyWho2) <- c("companies", "people", "startups")
howManyWho2
#           startup osoba otherComp   n
# companies   FALSE FALSE      TRUE  52
# people      FALSE  TRUE      TRUE 343
# startups     TRUE FALSE      TRUE 238

startupMultiGraph %>% activate(edges) %>% as_tibble() %>% 
  select(to_startup:connectToStartup) %>% 
  group_by(to_startup, from_startup, connectToStartup) %>% count()
#  to_startup from_startup connectToStartup     n
#   <lgl>      <lgl>        <lgl>            <int>
# 1 FALSE      TRUE         TRUE                 8
# 2 TRUE       FALSE        TRUE               794
# 3 TRUE       TRUE         TRUE                27

startupMultiGraph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>% 
  select(degree, startup) %>%
  as_tibble() %>% 
  group_by(startup) %>% 
  summarise(mean(degree), sd(degree))


################################################################################
### the most active agents in the network ######################################

startupMultiGraph %>% activate(nodes) %>% as_tibble() %>% 
  filter(type == "startup") %>% select(name, neighborsOut) %>% 
  arrange(desc(neighborsOut))
# Startups -> max 4 connection, usually at most 2

startupMultiGraph %>% activate(nodes) %>% as_tibble() %>% 
  filter(type == "person") %>% select(name, neighborsOut) %>% 
  arrange(desc(neighborsOut)) %>% View()
# Top 10 investors, the best ones -> 42, 56 outside connections

startupMultiGraph %>% activate(nodes) %>% as_tibble() %>% 
  filter(type == "firm") %>% select(name, neighborsOut) %>% 
  arrange(desc(neighborsOut)) %>% pull(neighborsOut) %>% length() #52 -> 20% is 10.4

startupMultiGraph %>% activate(nodes) %>% as_tibble() %>% 
  filter(type == "firm") %>% select(name, neighborsOut) %>% 
  arrange(desc(neighborsOut)) %>% mutate(top = ifelse(neighborsOut>3, "top", "notop")) %>% 
  group_by(top) %>% summarize(coun = sum(neighborsOut))

# TOP 5 investors, the best one 42 out connection, lower have 14,11,11,7,5,3,3,3,....
92/(92+54)


################################################################################
### Plot with investor activity distribution ###################################
par(mfrow=c(1,2))
startupMultiGraph %>% activate(nodes) %>% as_tibble() %>% 
  #tidyWith2016 %>% activate(nodes) %>% as_tibble() %>% 
  filter(type == "person") %>% select(name, neighborsOut) %>% 
  arrange(desc(neighborsOut)) %>% pull(neighborsOut) %>% ecdf() %>% 
  plot( main = "Private engagement", 
        xlim = c(0,45), ylim = c(0,1),
        xlab = "")
ppareto <- function(x) 1-(1/x)^2
curve(ppareto, col = "navyblue", from = 1,  add = TRUE)

startupMultiGraph %>% activate(nodes) %>% as_tibble() %>% 
  #tidyWith2016 %>% activate(nodes) %>% as_tibble() %>% 
  filter(type == "firm") %>% select(name, neighborsOut) %>% 
  arrange(desc(neighborsOut)) %>% pull(neighborsOut) %>% ecdf() %>% 
  plot( main = "Corporate engagement", 
        xlim = c(0,45), ylim = c(0,1),
        xlab ="")
ppareto <- function(x) 1-(1/x)^2
curve(ppareto, col = "navyblue", from = 1, add = TRUE)
# export: 800x400

par(mfrow=c(1,1))

# COMPANY OR A PERSON at a core
ggraph(startupMultiGraph %>% activate(nodes) %>% 
         filter(howManyInGroup > 50), layout = "stress") +
  geom_edge_link(col = "gray70") +
  #geom_edge_loop() +
  geom_node_point(size = 1, aes(colour = as.factor(type))) +
  #geom_node_text(aes(label = id)) +
  theme_graph() +
  labs(colour = "Type")+
  theme(legend.position='bottom',
        legend.text=element_text(size=12),
        legend.title =element_text(size=12))


################################################################################
### Visualisation with the top connected startups ##############################

tidyWith2016 %>% activate(nodes) %>% 
  select(countStartups, component_id) %>% 
  as_tibble() %>% group_by(component_id) %>% 
  summarize(max = max(countStartups)) %>% arrange(desc(max)) %>% 
  filter(max %in% c(2,3)) %>% pull(component_id)

# component_id -> 1, 2 (największe)
# component_id -> 3, 4, 7 (po 11 i 14 startupów)
# tutaj przynajmniej 4 startupy: 1  2  3  4  7  #  9  6  5 15 24 28
# drobnica 2 i 3:
# 11  17  20  22  26  27  31  71  10  13  21  29  32  36  37  41  51  57  59 123
# 136 152 156 198 204 216 242 258 273 283 375

top1 <- 1 # 56
top2 <- 2 # 46
top3 <- c(3,4,7) # 14, 11, 11
top4 <- c(9,  6,  5, 15, 24, 28) # 7, 6, 5, 4...
top5 <- tidyWith2016 %>% activate(nodes) %>% # drobnica po 2 i 3 
  select(countStartups, component_id) %>% 
  as_tibble() %>% group_by(component_id) %>% 
  summarize(max = max(countStartups)) %>% arrange(desc(max)) %>% 
  filter(max %in% c(2,3)) %>% pull(component_id)
single <- tidyWith2016 %>% activate(nodes) %>% # drobnica po 2 i 3 
  select(countStartups, component_id) %>% 
  as_tibble() %>% group_by(component_id) %>% 
  summarize(max = max(countStartups)) %>% arrange(desc(max)) %>% 
  filter(max == 1) %>% pull(component_id)

formalNetwork %>% activate(nodes) %>% pull(component_id) %>% unique()

formalNetwork <- formalNetwork %>% activate(nodes) %>% 
  mutate(component_cat = ifelse(component_id == 1, "top1",
                                ifelse(component_id == 2, "top2", 
                                       ifelse(component_id %in% top3, "top3",
                                              ifelse(component_id %in% top4, "top4",
                                                     ifelse(component_id %in% top5, "top5",
                                                            "top6"))))))

# If they are localised in the same place??
formalNetwork %>% activate(nodes) %>%  as_tibble() %>% pull(component_cat) %>% as.factor() %>% summary()

formalNetwork %>% activate(nodes) %>%  as_tibble() %>% 
  filter(component_id == 1) %>% st_distance() %>% .[,1] %>% unique() # 4 localised in a different place out of 56

formalNetwork %>% activate(nodes) %>%  as_tibble() %>% 
  filter(component_id %in%  top2) %>% st_distance() %>% .[,1] %>% unique() # 4 localised in a different place out of 46

formalNetwork %>% activate(nodes) %>%  as_tibble() %>% 
  filter(component_id %in%  top3) %>% st_distance() %>% .[,1] %>% unique() # 4 localised in a different place out of 36

formalNetwork %>% activate(nodes) %>%  as_tibble() %>% 
  filter(component_id %in%  top4) %>% st_distance() %>% .[,1] %>% unique() # 5 localised in a different place out of 30

formalNetwork %>% activate(nodes) %>%  as_tibble() %>% 
  filter(component_id %in%  top5) %>% st_distance() %>% .[,1] %>% unique() # 30 localised in a different place out of 70


################################################################################
# Plotting with jittering

library(viridis)

x= st_jitter(st_as_sf(formalNetwork %>% activate(nodes) %>% 
                        filter(!(component_id %in% c(single, top5))), "nodes"), factor =0.02)

# Sparse list with joint coordinates
st_equals(st_as_sf(formalNetwork %>% activate(nodes) , "nodes"), remove_self = T)

# Always jitter coordinates before plotting

# EXPORTED FOR NOW AS THE SPATIAL TOP COMPONENT PLACING VISUALIATION
ggplot() +
  geom_sf(dzielnice.line, mapping = aes(geometry=geometry), inherit.aes = FALSE)+
  geom_sf(data = st_jitter(st_as_sf(formalNetwork %>% activate(nodes) %>% 
                                      filter(!(component_id %in% c(top1, top2))), "nodes"), factor = 0.02),
          col = "gray70", alpha = 0.4) +
  geom_sf(data = st_jitter(st_as_sf(formalNetwork %>% activate(nodes) %>% 
                                      filter(component_id %in% c(top1, top2)), "nodes"), factor=0.02),
          aes(col = component_cat), alpha = 0.3) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="bottom") +
  scale_colour_viridis_d( direction = 1)



################################################################################
### Spatial graph description ##################################################

graphW

# visualisation of the total formal graph
ggraph(graphW, layout = "stress") +
  geom_edge_link(col = "gray70") +
  #geom_edge_loop() +
  geom_node_point(size = 1, col = "blue") +
  #geom_node_text(aes(label = id)) +
  theme_graph() 
# 770x530

spatialNetwork %>% activate(nodes) %>% group_by(spatial_component_id) %>% 
  as_tibble() %>% count() %>% arrange(desc(n)) %>% pull(n) %>% hist()


# Adding info about formal components
spatialNetwork <- spatialNetwork %>% activate(nodes) %>% 
  mutate(component_id = startup2016CorpWithConnections$component_id)

biggestSubgraph <- spatialNetwork %>% activate(nodes) %>% filter(spatial_component_id == 1)

biggestSubgraph <- biggestSubgraph %>% activate(nodes) %>% 
  mutate(formal_group = ifelse(component_id %in%  c(1,2), "main formal", 
                               ifelse(component_id %in% single, "single", "other")))
biggestSubgraph %>% activate(nodes) %>% as_tibble() %>% 
  group_by(formal_group) %>% count()


ggraph(biggestSubgraph, layout = "stress") +
  geom_edge_link(col = "gray70") +
  #geom_edge_loop() +
  geom_node_point(size = 1, aes(col = formal_group)) +
  #geom_node_text(aes(label = id)) +
  theme_graph() 
# 770x530


graphW %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>% 
  select(degree) %>%
  as_tibble() %>% 
  #group_by(startup) %>% 
  summarise(mean(degree), sd(degree))



################################################################################
### Adding ORBIS data ##########################################################

load(file="financialData.RData") 

financialStartup

financialStartup[sapply(financialStartup, is.infinite)] <- NA

# cut by component_id and spatial_component_id or by spatial_howManyInGroup

# Find desired groups:
tidyWith2016 <- tidyWith2016 %>% activate(nodes) %>% 
  mutate(startup_n = ifelse(startup,1,0)) %>% 
  group_by(component_id) %>% 
  mutate(countStartups = sum(startup_n)) %>% 
  ungroup()

whichInvovesMore <- tidyWith2016 %>% activate(nodes) %>% 
  filter(countStartups > 1) %>% select(component_id) %>% pull(component_id) %>% unique() %>% sort()

# find top two formal - components 1 & 2
financialStartup %>% select(howManyInGroup, component_id) %>% arrange(desc(howManyInGroup))

whichInvolvesMoreButNotTwoTop <- whichInvovesMore[-c(1,2)]

# grouping by formal:
# component_id in whichInvolvesMoreButNotTwoTop -> second group
# component_id in c(1,2) -> top, first group
# other component_id -> third, disconnected group

# grouping by spatial:
financialStartup %>% select(spatial_howManyInGroup, spatial_component_id) %>% 
  arrange(desc(spatial_howManyInGroup)) %>% unique()


# spatial_howManyInGroup == 532 -> top cluster
# == 38 -> second cluster
# == 1 -> disconected
# other -> small clusters

financialStartup <- financialStartup %>% 
  mutate(formal = case_when(component_id %in% c(1,2) ~ "topFormal",
                            component_id %in% whichInvolvesMoreButNotTwoTop ~ "connectedToStartup",
                            TRUE ~ "disconnected"),
         informal = case_when(spatial_howManyInGroup == 532 ~ "topCluster",
                              spatial_howManyInGroup == 38 ~ "topCluster",
                              spatial_howManyInGroup == 1 ~ "disconnected",
                              TRUE ~ "smallClusters"), 
         survived = case_when(!is.na(ZawieszenieDzialalnosci) ~0,
                              !is.na(ZakonczenieDzialalnosci) ~0,
                              TRUE ~ 1 )) 

# summary table
financialStartup %>%   group_by(formal, informal) %>% 
  summarise(sol = mean(SolvencyLastYear, na.rm = T),
            ROE = mean(ROELastYear, na.rm = T), 
            profit = mean(profitIncrease, na.rm = T), 
            profitLast = mean(`P/L before tax\nth USD Last avail. yr`, na.rm = T),
            share = mean(shareholderIncrease, na.rm = T), 
            asset = mean(assetIncrease, na.rm = T),
            survived = mean(survived),
            n = n())

financialStartup %>%   group_by(formal) %>% 
  summarise(sol = mean(SolvencyLastYear, na.rm = T),
            ROE = mean(ROELastYear, na.rm = T), 
            profit = mean(profitIncrease, na.rm = T), 
            profitLast = mean(`P/L before tax\nth USD Last avail. yr`, na.rm = T),
            share = mean(shareholderIncrease, na.rm = T), 
            asset = mean(assetIncrease, na.rm = T),
            survived = mean(survived),
            n = n())
# In disconnected group solvency and ROE is better in informal clusters
# it is also easier to survive -> disconnected twice 12% died
# for connected to startups -> being in 
# for di


financialStartup %>%   group_by(formal) %>% 
  summarise(sol = mean(SolvencyLastYear, na.rm = T),
            ROE = mean(ROELastYear, na.rm = T), 
            profit = mean(profitIncrease, na.rm = T),
            profitLast = mean(`P/L before tax\nth USD Last avail. yr`, na.rm = T),
            share = mean(shareholderIncrease, na.rm = T), 
            asset = mean(assetIncrease, na.rm = T),
            survived = mean(survived))

financialStartup %>%   group_by(informal) %>% 
  summarise(sol = mean(SolvencyLastYear, na.rm = T),
            ROE = mean(ROELastYear, na.rm = T), 
            profit = mean(profitIncrease, na.rm = T), 
            share = mean(shareholderIncrease, na.rm = T), 
            asset = mean(assetIncrease, na.rm = T),)

