# Bee-Specialization-Modeling
Leveraging Large Biological Interaction Data to Quantify Plant Specialization by Bees

General workflow for running the analyses:

1) Download full interaction dataset from GloBI: https://zenodo.org/record/7348355#.Y5owy-zMIcQ
2) Filter the dataset to just include bee data using scripts/format_bee_data.sh
3) Reformat the data using modeling_data/GLOBI_bee_update_clean.Rmd
5) Update plant names using Fowler-GLOBI Modeling/GLOBI-Fowler Data Prep-reformatted.Rmd
6) Run analyses using scripts/main_analyses.R
