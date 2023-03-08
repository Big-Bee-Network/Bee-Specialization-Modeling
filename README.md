# Predicting Specialist Bee Species

General workflow for running the analyses:

1. Download full GloBI interaction dataset from [Zenodo](https://zenodo.org/record/7348355#.Y5owy-zMIcQ).
2. Filter the dataset to just include bee data using [scripts/format_bee_data.sh](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_bee_data.sh)

   a. Output: *all_bee_data.tsv*
   
3. Reformat the data using [modeling_data/GLOBI_bee_update_clean.Rmd](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/GLOBI_bee_update_clean.Rmd)

   a. Output: *interactions-14dec2022.csv*

4. Update bee names and filter to just be native US bees using [scripts/update bee names.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/update%20bee%20names.R)
  
5. Update plant names and make plant phylogeny using [scripts/make plant phylogeny.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/make%20plant%20phylogeny.R)
6. Format and update name of list of specialist bees using [scripts/format_fowler_hosts.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_fowler_hosts.R)
7. Run analyses using [scripts/main_analyses.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/main%20analyses.R)
