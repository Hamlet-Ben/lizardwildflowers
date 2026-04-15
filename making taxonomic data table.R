#get all taxa codes

all_taxa <- unique(unlist(strsplit(obs_sf$taxon.ancestry, "/")))

all_taxa_inat_info <- get_taxa_info(all_taxa)

sum(all_taxa_inat_info$rank == "class")

subset(all_taxa_inat_info, rank == "order" )

write.csv(all_taxa_inat_info, "all_taxa_inat_info.csv")

library(dplyr)

family_lookup <- all_taxa_inat_info %>%
  filter(rank == "family") %>%
  select(
    taxon_id = id,
    family = name,
    family_common = preferred_common_name
  )

order_lookup <- all_taxa_inat_info %>%
  filter(rank == "order") %>%
  select(
    taxon_id = id,
    order = name,
    order_common = preferred_common_name
  )

class_lookup <- all_taxa_inat_info %>%
  filter(rank == "class") %>%
  select(
    taxon_id = id,
    class = name,
    class_common = preferred_common_name
  )

library(tidyr)
library(stringr)

obs_long <- obs_sf %>%
  st_drop_geometry() %>%
  select(id, taxon.ancestry) %>%
  mutate(ancestry_list = strsplit(taxon.ancestry, "/")) %>%
  unnest_longer(ancestry_list) %>%
  mutate(ancestry_list = as.numeric(ancestry_list))

obs_taxonomy <- obs_long %>%
  left_join(family_lookup, by = c("ancestry_list" = "taxon_id")) %>%
  left_join(order_lookup,  by = c("ancestry_list" = "taxon_id")) %>%
  left_join(class_lookup,  by = c("ancestry_list" = "taxon_id"))

obs_taxonomy <- obs_taxonomy %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    family = dplyr::first(stats::na.omit(family)),
    family_common = dplyr::first(stats::na.omit(family_common)),
    order = dplyr::first(stats::na.omit(order)),
    order_common = dplyr::first(stats::na.omit(order_common)),
    class = dplyr::first(stats::na.omit(class)),
    class_common = dplyr::first(stats::na.omit(class_common)),
    .groups = "drop"
  )

obs_sf <- obs_sf %>%
  left_join(obs_taxonomy, by = "id")

table(obs_sf$class_common)
table(obs_sf$order_common)
