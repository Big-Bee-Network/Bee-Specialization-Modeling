# Predicting Specialist Bee Species

General workflow for running the analyses:

1. Download full GloBI interaction dataset from [Zenodo](https://zenodo.org/record/7348355#.Y5owy-zMIcQ).
2. Filter the dataset to just include bee data using [scripts/format_bee_data.sh](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_bee_data.sh)

   a. Output: *all_bee_data.tsv*
   
3. Reformat the data using [modeling_data/GLOBI_bee_update_clean.Rmd](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/GLOBI_bee_update_clean.Rmd)

   a. Output: *interactions-14dec2022.csv*

4. Get list of USA native bee species and calculate extent of occurence using [scripts/Chesshire2023-beeArea.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/Chesshire2023-beeArea.R)

   a. Output: *Chesshire2023_nameAlignment.csv*
   
   b. Output: *chesshire2023_beeArea.csv*

5. Update *Chesshire2023_nameAlignment.csv* to include information about whether name alignment was informed by geography using [scripts/name-alignment-chesshire.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/name-alignment-chesshire.R)

   a. Output: *Chesshire2023_nameAlignment-geoUpdate.csv*
   
5. Update bee names and filter to just be native US bees using [scripts/update bee names.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/update%20bee%20names.R)

   a. Output: *globi_american_native_bees_7march2023.csv*
  
6. Update plant names and make plant phylogeny using [scripts/make plant phylogeny.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/make%20plant%20phylogeny.R)

   a. get plant families from WFO, using [scripts/update plant names.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/update%20plant%20names.R)
   
   b. Output: *globi_allNamesUpdated.csv*
   
   c. Output: *globi_phyloDiv.csv*
   
7. Format and update name of list of specialist bees using [scripts/format_fowler_hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_fowler_hosts.R)
8. Make final dataset using [scripts/finalDataset.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/finalDataset.R)

   a. Output: [globi_speciesLevelFinal.csv](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/globi_speciesLevelFinal.csv)
   
9. Run analyses using [scripts/main_analyses.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/main%20analyses.R)
