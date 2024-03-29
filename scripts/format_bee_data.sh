################################## 
# Author: Katja C. Seltmann
# Edited by Colleen Smith December 13, 2022

# Purpose: separate out all bee records from Global Biotic Interactions database
# bee_counts.txt - number of unique records per family, and collection
################################## 


echo Creating headers
head -1  data/interactions.tsv > Andrenidae_data.tsv
head -1  data/interactions.tsv > Apidae_data.tsv
head -1  data/interactions.tsv > Colletidae_data.tsv
head -1  data/interactions.tsv > Halictidae_data.tsv
head -1  data/interactions.tsv > Megachilidae_data.tsv
head -1  data/interactions.tsv > Melittidae_data.tsv
head -1  data/interactions.tsv > Stenotritidae_data.tsv

#find all Andrenidae and write one file with all data and a second file only with unique records

echo Finding all Andrenidae
cat  data/interactions.tsv | grep -w "Andrenidae" >> Andrenidae_data.tsv
wc -l Andrenidae_data.tsv

echo Sorting unique records
sort -r Andrenidae_data.tsv | uniq > Andrenidae_data_unique.tsv
wc -l Andrenidae_data_unique.tsv
wc -l Andrenidae_data_unique.tsv >> bee_counts.txt
#####################################
#find all Apidae and write one file with all data and a second file only with unique records

echo Finding all Apidae
cat  data/interactions.tsv | grep -w "Apidae" >> Apidae_data.tsv
wc -l Apidae_data.tsv

echo Sorting unique records
sort -r Apidae_data.tsv | uniq > Apidae_data_unique.tsv
wc -l Apidae_data_unique.tsv
wc -l Apidae_data_unique.tsv >> bee_counts.txt
#####################################
#find all Colletidae and write one file with all data and a second file only with unique records

echo Finding all Colletidae
cat  data/interactions.tsv | grep -w "Colletidae" >> Colletidae_data.tsv
wc -l Colletidae_data.tsv

echo Sorting unique records
sort -r Colletidae_data.tsv | uniq > Colletidae_data_unique.tsv
wc -l Colletidae_data_unique.tsv
wc -l Colletidae_data_unique.tsv >> bee_counts.txt
#####################################
#find all Halictidae and write one file with all data and a second file only with unique records

echo Finding all Halictidae
cat  data/interactions.tsv | grep -w "Halictidae" >> Halictidae_data.tsv
wc -l Halictidae_data.tsv

echo Sorting unique records
sort -r Halictidae_data.tsv | uniq > Halictidae_data_unique.tsv
wc -l Halictidae_data_unique.tsv
wc -l Halictidae_data_unique.tsv >> bee_counts.txt
#####################################
#find all Megachilidae and write one file with all data and a second file only with unique records

echo Finding all Megachilidae
cat  data/interactions.tsv | grep -w "Megachilidae" >> Megachilidae_data.tsv
wc -l Megachilidae_data.tsv

echo Sorting unique records
sort -r Megachilidae_data.tsv | uniq > Megachilidae_data_unique.tsv
wc -l Megachilidae_data_unique.tsv
wc -l Megachilidae_data_unique.tsv >> bee_counts.txt
#####################################
#find all Melittidae and write one file with all data and a second file only with unique records

echo Finding all Melittidae
cat  data/interactions.tsv | grep -w "Melittidae" >> Melittidae_data.tsv
wc -l Melittidae_data.tsv

echo Sorting unique records
sort -r Melittidae_data.tsv | uniq > Melittidae_data_unique.tsv
wc -l Melittidae_data_unique.tsv
wc -l Melittidae_data_unique.tsv >> bee_counts.txt
#####################################
#find all Stenotritidae and write one file with all data and a second file only with unique records

echo Finding all Stenotritidae
cat  data/interactions.tsv | grep -w "Stenotritidae" >> Stenotritidae_data.tsv
wc -l Stenotritidae_data.tsv

echo Sorting unique records
sort -r Stenotritidae_data.tsv | uniq > Stenotritidae_data_unique.tsv
wc -l Stenotritidae_data_unique.tsv
wc -l Stenotritidae_data_unique.tsv >> bee_counts.txt
#####################################
#create one large bee file
cat *unique.tsv >> all_bee_data.tsv
sort -r all_bee_data.tsv | uniq > indexed_interactions_bees.tsv

