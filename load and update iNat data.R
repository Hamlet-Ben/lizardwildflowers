# Load your existing saved dataset
load("obs_sf.RData")

# Update it
obs_sf <- update_inat_dataset(
  existing_obs_sf = obs_sf,
  taxon_id = 47125,
  swlat = 49.95,
  swlng = -5.32,
  nelat = 50.10,
  nelng = -5.00,
  max_pages = 100,
  verbose = TRUE
)

# Save the refreshed version
save(obs_sf, file = "obs_sf.RData")
