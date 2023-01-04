# Predicting Specialist Bee Species

General workflow for running the analyses:

1) Download full interaction dataset from GloBI: https://zenodo.org/record/7348355#.Y5owy-zMIcQ
2) Filter the dataset to just include bee data using scripts/format_bee_data.sh
3) Reformat the data using modeling_data/GLOBI_bee_update_clean.Rmd
5) Update plant names using scripts/update plant names.R
6) Format and update name of list of specialist bees using scripts/format_fowler_hosts.R
7) Update bee names and filter to just be native US bees using scripts/update bee names.R (bees names are aligned using Discover Life, ITIS, and NCBI (in that order) and the name alignment template https://github.com/globalbioticinteractions/name-alignment-template ; JT Miller's tutorial:https://big-bee-network.github.io/name-alignment-workshop/04-name-alignment/index.html)
7) Run analyses using scripts/main_analyses.R
