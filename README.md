# Predicting Specialist Bee Species

## General workflow for running the analyses:

1. Download full GloBI interaction dataset from [Zenodo](https://zenodo.org/record/7348355#.Y5owy-zMIcQ).
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
   
9. Format and update name of list of specialist bees using [scripts/format_fowler_hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_fowler_hosts.R)

10. Make final dataset using [scripts/finalDataset.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/finalDataset.R)

    a. Output: [globi_speciesLevelFinal.csv](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/globi_speciesLevelFinal.csv)
   
11. Run analyses using [scripts/main_analyses.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/main%20analyses.R)

12. Run analysis to predict specialist host plants using [scripts/predict specialist hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/predict%20specialist%20hosts.R)


## Defintions of column names in *globi_speciesLevelFinal.csv*

| Column Name  | Defintion  | 
| :------------ |:---------------| 
| scientificName      | bee species name | 
| bee_genus      | bee species' genus        |   
| bee_family | bee species' family        | 
| diet_breadth | whether bee is a pollen specialist or generalist according to Fowler lists        |
| diet_breadth_detailed | same as 'diet_breadth' but includes whether specialists are on plant families or genera        | 
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



