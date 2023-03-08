# Predicting Specialist Bee Species

General workflow for running the analyses:

1. Download full GloBI interaction dataset from [Zenodo](https://zenodo.org/record/7348355#.Y5owy-zMIcQ).
2. Filter the dataset to just include bee data using [scripts/format_bee_data.sh](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/format_bee_data.sh)

   a. Output: *all_bee_data.tsv*
   
3. Reformat the data using [modeling_data/GLOBI_bee_update_clean.Rmd](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/modeling_data/GLOBI_bee_update_clean.Rmd)
   a. Output: *interactions-14dec2022.csv*
4. Update plant names with Taxonstand using scripts/update plant names.R
5. Format and update name of list of specialist bees using scripts/format_fowler_hosts.R

   a. bees names are aligned using methods from Chesshire et al 2023 Ecography


6. Update bee names and filter to just be native US bees using [scripts/update bee names.R](https://github.com/Big-Bee-Network/Bee-Specialization-Modeling/blob/master/scripts/update%20bee%20names.R)


7. Run analyses using scripts/main_analyses.R
