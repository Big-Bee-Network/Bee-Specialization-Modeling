# Predicting specialist and generalist bee species
This repository contains data and code for the manuscript *Pollen specialist bee species are accurately predicted from visitation, occurrence and phylogenetic data*.


## Abstract

An animal’s diet breadth is a central aspect of its life history. Yet information about which species have a narrow dietary breadth (specialists) and which have comparatively broad dietary breadths (generalists) is missing for many taxa and regions. One possible way to address this gap is to leverage interaction data found on museum specimens and published in the literature. Here, we use bees as our focal taxon to see if we can predict dietary specialization and generalization using machine learning models and interaction data, along with a genus-level bee phylogeny, and geographic and phenological data of 682 bee species that are native to the United States. To assess whether our models can predict the diet breadths of bee species in different regions or for different taxa, we used spatial and phylogenetic blocking, in which models are cross validated on data that are spatially or phylogenetically independent of the data used to train them. We found that specialist bee species mostly visit their host plants, and that they can be predicted with high accuracy (mean 92% accuracy). Overall model performance was high (mean AUC = 0.84), and our models did a moderate job of predicting generalist bee species, the minority class in our dataset (mean 62% accuracy). Models tested on spatially and phylogenetically blocked data had comparable performance to models tested on randomly blocked data. Our results suggest that it is possible to predict specialist bee species in regions and for taxonomic groups where they are unknown, but may be more challenging to predict generalists, and depends on the class imbalance of the dataset. Researchers looking to identify pollen specialist and generalist species can generate candidate lists of these species by training models on bees from nearby regions or closely related taxa. This type of modeling approach can then enable more targeted data collection.

## Description of data 
In [final_data](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/tree/master/final_data)
| File Name  | Description  | 
| :------------ |:---------------| 
| AppendixS2_5Dec2023.csv      | List of USA bee species in GloBI dataset, their diet breadth classifications and references. | 
| globi_speciesLevelFinal-27nov2023.csv     | Species-level data used to predict specialist and generalist bee species with random forest model.        |   
| fowler_formatted-30nov2023.csv | List of bee species in our dataset and their pollen host records from Jarrod Fowler's dataset. Used to assess how often specialist bees visit their host plants.        | 
| russell_formatted-30nov2023.csv | List of bee species in the GloBI dataset and their pollen host records from Avery's Russell's dataset. Used to assess how often specialist bees visit their host plants.        | 
| globi_allNamesUpdated.csv | GloBI dataset with interaction records of bee species from the contiguous United States. Both plant and bee names are updated to the currently accepted species name.  | 


## General workflow for running the analyses:

1. Download full GloBI interaction dataset from [Zenodo](https://doi.org/10.5281/zenodo.7348355).
2. Filter the dataset to just include bee data using [scripts/format_bee_data.sh](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_bee_data.sh)

   a. Output: *all_bee_data.tsv*
   
3. Reformat the data using [modeling_data/GLOBI_bee_update_clean.Rmd](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/GLOBI_bee_update_clean.Rmd)

   a. Output: *interactions-14dec2022.csv*

4. Get list of USA native bee species and calculate extent of occurence using [scripts/Chesshire2023-beeArea.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/Chesshire2023-beeArea.R)

   a. Output: *Chesshire2023_nameAlignment.csv*
   
   b. Output: *chesshire2023_beeArea-11april2023.csv*

5. Update *Chesshire2023_nameAlignment.csv* to include information about whether name alignment was informed by geography using [scripts/name-alignment-chesshire.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/name-alignment-chesshire.R)

   a. Output: *Chesshire2023_nameAlignment-geoUpdate.csv*
   
6. Update bee names and filter to just be native US bees using [scripts/update bee names.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/update%20bee%20names.R)

   a. Output: *globi_american_native_bees_7march2023.csv*
   
7. Make bee phylogeny using [scirpts/make bee phylogeny.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/make%20bee%20phylogeny.R)

   a. Output: *bee_phylogenetic_data.csv*
  
8. Update plant names and make plant phylogeny using [scripts/make plant phylogeny.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/make%20plant%20phylogeny.R)

   a. get plant families from WFO, using [scripts/update plant names.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/update%20plant%20names.R)
   
   b. Output: *globi_allNamesUpdated.csv*
   
   c. Output: *globi_phyloDiv.csv*
   
9. Format and update name of list of specialist bees using [scripts/format_fowler_hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_fowler_hosts.R) and [scripts/format_russell_hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format%20russell%20hosts.R)

10. Make final dataset using [scripts/finalDataset.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/finalDataset.R)

    a. Output: [globi_speciesLevelFinal-12Aug2024_revision2.csv](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/globi_speciesLevelFinal-12Aug2024_revision2.csv)
   
11. Update datasets with taxon names to match Henriquez Piskulich bee phylogeny [scripts/make bee phylogeny_Henriquez Piskulich.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/bee_name_updates_Henriquez_Piskulich.R)

12. Run analyses using [scripts/main_analyses.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/main%20analyses.R)

13. Run analysis to predict specialist host plants using [scripts/predict specialist hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/predict%20specialist%20hosts.R)

14. Run code to look at how often specialist bees visit their host plants using [scripts/specialists-nonhosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/specialists-nonhosts.R)
## Defintions of column names in *globi_speciesLevelFinal-27nov2023.csv*

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



