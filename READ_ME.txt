Here we describe how we obtained and analyzed data for the study: High compositional dissimilarity among small communities is decoupled from environmental variation
(https://doi.org/10.32942/osf.io/vngse)

Running the R codes inside each folder in the order listed below will replicate the whole set of results, including those provided as supporting information:

* 1_data_selection_1
* 2_data_selection_2
* 3_environmental_data_selection
* 4_community_size
* 5_diversity_analysis

- If you want to use the same R package versions that we used you can run the script "renv_environment" first. 

- Brief explanation of each step:

# 1_data_selection_1:
In the first step of the data selection, we added the information of basins from Hydrosheds (level 7 - https://www.hydrosheds.org/page/hydrobasins) to the database 
of fish abundances (RivFishTIME, Comte et al. 2020) and we selected basins with at least 10 sites. 

# 2_data_selection_2:
We snapped the selected sites to the closest hydrography from the HydroRIVERS database (https://www.hydrosheds.org/products/hydrorivers) to attribute a Strahler
stream order to each site. This step was divided into a set of scripts, one for each geographic zone as we needed to use the UTM projection to measure the distance of each point to the hydrography in meters. 

-> SCRIPTS: data_selection_snap_points_to_lines_11t; data_selection_snap_points_to_lines_11u; data_selection_snap_points_to_lines_12t; 
data_selection_snap_points_to_lines_15t; data_selection_snap_points_to_lines_15u; data_selection_snap_points_to_lines_17s;
data_selection_snap_points_to_lines_17t; data_selection_snap_points_to_lines_18s; data_selection_snap_points_to_lines_22j; 
data_selection_snap_points_to_lines_22k; data_selection_snap_points_to_lines_30t; data_selection_snap_points_to_lines_30u;
data_selection_snap_points_to_lines_31t; data_selection_snap_points_to_lines_31u; data_selection_snap_points_to_lines_32t; 
data_selection_snap_points_to_lines_32u; data_selection_snap_points_to_lines_32v; data_selection_snap_points_to_lines_33t;
data_selection_snap_points_to_lines_33u; data_selection_snap_points_to_lines_32v; data_selection_snap_points_to_lines_32w; 
data_selection_snap_points_to_lines_34v; data_selection_snap_points_to_lines_34w; data_selection_snap_points_to_lines_35v;
data_selection_snap_points_to_lines_35w; data_selection_snap_points_to_lines_54t; data_selection_snap_points_to_lines_56j

After snaping all points to the closest hydrography, we joined all data frames together and selected only basins with ten or more sites of orders 1, 2 or 3. We also selected basins with five or more species and we generated one data frame for count data and another for density data. 
-> SCRIPT: data_selection_2

# 3_environmental_data_selection:
We snapped the selected sites to the closest hydrography from RiverATLAS (https://www.hydrosheds.org/hydroatlas) and Free-Flowing Rivers (FFR - https://figshare.com/articles/dataset/Mapping_the_world_s_free-flowing_rivers_data_set_and_technical_documentation/7688801) to select environmental variables. 
As we used the snapPointsToLines function, we also used the UTM projection and because of this, we have a code for each geographic zone.
-> SCRIPTS: snap_11t; snap_11u; snap_12t; snap_15t; snap_15u; snap_17s; snap_17t; snap_18s; snap_22j; snap_22k; snap_30t; snap_30u; snap_31t; snap_31u; snap_32t; 
snap_32u; snap_32v; snap_33t; snap_33u; snap_33v; snap_33w; snap_34v; snap_34w; snap_35v; snap_35w; snap_54t; snap_56j

Then, we joined the snapped sites and exported the environmental data frame of count and density data.
-> SCRIPT: data_selection_3

# 4_community_size:
The median community size was calculated for each metacommunity, using count data.
-> SCRIPT:community_size

# 5_diversity_analysis:
Here we run all analyses described in the paper, which include:

a) Environmental heterogeneity within metacommunities -> SCRIPT: environment_variation
b) Spatial extent of each metacommunity  -> SCRIPT: geographic_distance
c) Simulate metacommunities to measure different beta-diversity metrics (first process-based
simulation) -> SCRIPT: beta_diversity_simulations
d) Rank difference for count data -> SCRIPT: rank_diff
e) Linear models regressing beta-diversity metric (rank difference) against 
community size, environmental heterogeneity and spatial extent -> SCRIPT: lm_beta_div
f) Simulate metacommunities to find a measure of the strength of community-environment
relationships that would not be affected by community size itself (second process-based 
simulation) -> SCRIPT: GLM_HMSC_simulations
g) HMSC for count data -> SCRIPT: HMSC
h) Compute the variance partitioning of the HMSC model -> SCRIPT: variance_partitioning