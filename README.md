# Pollen specialist bee species are accurately predicted from visitation, occurrence and phylogenetic data
Colleen Smith<sup>1</sup>, Nick Bachelder<sup>1</sup>, Avery L. Russell<sup>2</sup>, Vanessa Morales<sup>2</sup>, Abilene R. Mosher<sup>2</sup>, Katja C. Seltmann<sup>1</sup>

<sup>1</sup>Cheadle Center for Biodiversity and Ecological Restoration, University of California, Santa Barbara, Harder South Building 578, Santa Barbara, CA 93106, U.S.A.
<sup>2</sup>Department of Biology, Missouri State University, 910 S John Q Hammons Parkway, Temple Hall, Springfield, MO 65897


This repository contains data and code for the manuscript **Pollen specialist bee species are accurately predicted from visitation, occurrence and phylogenetic data** currently under review in _Oecologia_. All files and figures can be found at [https://zenodo.org/records/13357492](https://zenodo.org/records/13357492)


## Abstract

An animal’s diet breadth is a central aspect of its life history, yet the factors determining why some species have narrow dietary breadths (specialists) and others have broad dietary breadths (generalists) remain poorly understood. This challenge is pronounced in herbivorous insects due to incomplete host plant data across many taxa and regions. Here, we develop and validate machine learning models to predict pollen diet breadth in bees, using a bee phylogeny and occurrence data for 682 bee species native to the United States, aiming to better understand key drivers. We found that pollen specialist bees made an average of 72.9% of their visits to host plants and could be predicted with high accuracy (mean 94%). Our models predicted generalist bee species, which made up a minority of the species in our dataset, with lower accuracy (mean 70%). Models tested on spatially and phylogenetically blocked data revealed that the most informative predictors of diet breadth are plant phylogenetic diversity, bee species' geographic range, and regional abundance. Our findings also confirm that range size is predictive of diet breadth and that both male and female specialist bees mostly visit their host plants. Overall, our results suggest we can use visitation data to predict specialist bee species in regions and for taxonomic groups where diet breadth is unknown, though predicting generalists may be more challenging. These methods can thus enhance our understanding of plant-pollinator interactions, leading to improved conservation outcomes and a better understanding of the pollination services bees provide.

## Description of data 

In [final_data](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/tree/master/final_data)
| File Name  | Description  | 
| :------------ |:---------------| 
| AppendixS2_20Aug2024-revision.csv | List of USA bee species in GloBI dataset, their diet breadth classifications and references.| 
| globi_speciesLevelFinal-12Aug2024_revision2_genera_removed.csv | Species-level data used to predict specialist and generalist bee species with random forest model.|   
| fowler_formatted-15Aug2024.csv | List of bee species in our dataset and their pollen host records from Jarrod Fowler's dataset. Used to assess how often specialist bees visit their host plants.| 
| russell_formatted-15Aug2024.csv | List of bee species in the GloBI dataset and their pollen host records from Avery's Russell's dataset. Used to assess how often specialist bees visit their host plants.| 
| sources_table28nov2023_revision.csv | Sources of bee interaction data found in GloBI visitation dataset|

In Large data files stored on [Zenodo](https://doi.org/10.5281/zenodo.8347145)
| File Name  | Description  | 
| :------------ |:---------------| 
| globi_allNamesUpdated.csv.zip | GloBI dataset with interaction records of bee species from the contiguous United States. Both plant and bee names are updated to the currently accepted species name.|
| globi_allNamesUpdated_Henriquez_Piskulich.csv.zip | GloBI dataset with interaction records of bee species from the contiguous United States. Both plant and bee names are updated to the currently accepted species names with generic changes in bees to match the phylogeny we used in the analysis.|
| globi_speciesLevelFinal-12Aug2024_revision2.csv.zip | Species-level data used to predict specialist and generalist bee species with random forest model with additional genera that were not found in the GloBI visitation dataset.|


## General workflow for running the analyses:

1. Download full GloBI interaction dataset from [Zenodo](https://doi.org/10.5281/zenodo.7348355).
2. Filter the dataset to just include bee data using scripts/format_bee_data.sh

   a. Output: *all_bee_data.tsv* (not included in archive)
   
3. Reformat the data using scripts/GLOBI_bee_update_clean.Rmd

   a. Output: *interactions-14dec2022.csv* (not included in archive)

4. Get list of USA native bee species and calculate extent of occurence using scripts/Chesshire2023-beeArea.R

   a. Output: *modeling_data/Chesshire2023_nameAlignment.csv*
   
   b. Output: *modeling_data/chesshire2023_beeArea-11april2023.csv*

5. Update *Chesshire2023_nameAlignment.csv* to include information about whether name alignment was informed by geography using scripts/name-alignment-chesshire.R

   a. Output: *modeling_data/Chesshire2023_nameAlignment-geoUpdate.csv*
   
6. Update bee names and filter to just be native US bees using scripts/update bee names.R

   a. Output: *modeling_data/large_data_files/globi_american_native_bees_7march2023.csv.zip*
   
7. Make bee phylogeny using scirpts/make bee phylogeny_Henriquez Piskulich.R

   a. Output: *modeling_data/bee_phylogenetic_data_Henriquez_Piskulich_tree.csv*
   
   b. Output: *modeling_data/modified_tree_Henriquez_Piskulich.nwk*
  
8. Update plant names and make plant phylogeny using scripts/make plant phylogeny.R

   a. get plant families from WFO, using scripts/update plant names.R
   
   b. Output: *modeling_data/large_data_files/globi_allNamesUpdated.csv*
   
   c. Output: *modeling_data/globi_phyloDiv.csv*
   
9. Format and update name of list of specialist bees using scripts/format_fowler_hosts.R and scripts/format_russell_hosts.R
	
	a. Output: modeling_data/russell_formatted-30nov2023.csv
	
	b. Output: modeling_data/fowler_formatted-30nov2023.csv
	
10. Update datasets with taxon names in GloBI to match Henriquez Piskulich bee phylogeny scripts/make bee phylogeny_Henriquez Piskulich.R
    
	a. Output: *globi_allNamesUpdated_Henriquez_Piskulich.csv*

11. Make final dataset using scripts/finalDataset.R

    a. Output: modeling_data/large_data_files/globi_speciesLevelFinal-12Aug2024_revision2.csv.zip

12. Run analyses using scripts/main_analyses.R

13. Run analysis to predict specialist host plants using scripts/predict specialist hosts.R

14. Run code to look at how often specialist bees visit their host plants using scripts/specialists-nonhosts.R

## Defintions of column names in *globi_speciesLevelFinal-12Aug2024_revision2.csv*

| Column Name  | Defintion  | 
| :------------ |:---------------| 
| scientificName      | bee species name | 
| bee_genus      | bee species' genus        |   
| bee_family | bee species' family        | 
| diet_breadth_conservative | whether bee is a pollen specialist or generalist using conservative criteria to define generalists        |
| diet_breadth_liberal | whether bee is a pollen specialist or generalist using liberal criteria to define generalists        | 
| phylo_rich | Faith's phylogenetic diversity of plant genera visited       | 
| phylo_simp | phylogenetic Simpson diversity of plant genera visited       | 
| eigen1_plantGenus | first eigenvalue of Morisita-Horn distance-matrix for plant genera visited       | 
| eigen2_plantGenus | second eigenvalue of Morisita-Horn distance-matrix for plant genera visited       | 
| eigen1_plantFam | first eigenvalue of Morisita-Horn distance-matrix for plant families visited       | 
| eigen2_plantFam | second eigenvalue of Morisita-Horn distance-matrix for plant families visited       | 
| rich_genus | number of plant genera visited        | 
| simpson_genus | inverse Simpson diversity of plant genera visited        | 
| rich_fam | number of plant families visited        | 
| simpson_fam | inverse Simpson diversity of plant families visited        | 
| n_globi | sample size in GLOBI        | 
| med_lat | median latitude of bee species in Chesshire et al 2023        | 
| med_long | median longitude of bee species in Chesshire et al 2023        | 
| n_chesshire | sample size in Chesshire et al 2023        | 
| area_m2 | area in m2 of extent of occurrence in Chesshire et al 2023        | 
| area_ha | area in hectares of extent of occurrence in Chesshire et al 2023        | 
| spherical_geometry | was spherical geometry used to calculate area of bee's extent of occurrence?        | 
| mean_doy | mean day of year of collection in Chesshire et al 2023        | 
| median_doy | median day of year of collection in Chesshire et al 2023        | 
| quant10 | 10% quantile of day of year of collection in Chesshire et al 2023        | 
| quant90 | 90% quantile of day of year of collection in Chesshire et al 2023        | 
| flight_season | 'quant90' - 'quant10'        | 
| eigen1 | first eigenvalue of matrix of bee phylogenetic distance        | 
| eigen2 | second eigenvalue of matrix of bee phylogenetic distance        | 
| all other columns | phylogenetic distance of bee genus to bee genus in column name       | 



