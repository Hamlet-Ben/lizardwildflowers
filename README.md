#🌼 Lizard Peninsula Wildflower Analysis (iNaturalist + R)
## Overview

This project analyses plant observation data from iNaturalist to explore the distribution and recording patterns of wildflowers across the Lizard Peninsula.

Using R and R Markdown, the project:

* extracts plant observation data for the Lizard region
* identifies well-recorded taxa
* maps species distributions
* generates 1 km grid-based heatmaps
* explores biodiversity patterns relative to recording effort

The outputs are designed to support:

* ecological interpretation
* citizen science engagement (e.g. BioBlitz events)

## Objectives
1. Summarise plant biodiversity
* Total observations, species, observers, temporal coverage
2. Identify well-recorded species
* Based on records, observers, spatial spread, and temporal depth
3. Focus on wildflowers
* Subset of herbaceous flowering plants (see methodology below)
4. Map species distributions
* Including focal species of interest
5. Generate spatial heatmaps (1 km resolution)
* Record density
* Species richness
* Effort-adjusted richness
6. Identify potential biodiversity hotspots and under-recorded areas

## Data Source

Data are retrieved from the iNaturalist API:

Observations endpoint:
https://api.inaturalist.org/v1/observations

Key filters used:

Geographic: Lizard Peninsula (via place_id or spatial polygon)
Taxonomic: Plantae (with further filtering)
Quality: typically research grade

For API documentation:
https://www.inaturalist.org/pages/api+reference

## Methodology
1. Data Extraction

Observations are retrieved via the API using paginated requests and stored locally for reproducibility.

