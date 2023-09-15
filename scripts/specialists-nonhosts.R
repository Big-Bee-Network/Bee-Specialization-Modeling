# clear the environment
rm(list=ls()) 

# load packages
library(tidyverse)
library(vroom)

# load the globi data
# this data file has bee and plant names updated, and only american bees
globi = vroom("modeling_data/globi_allNamesUpdated.csv") %>%
  mutate(bee_genus = sub(' .*',"",scientificName))

# load the fowler data
fowler_formatted = read_csv('modeling_data/fowler_formatted-7march2023.csv')
diet_breadth = read_csv('modeling_data/bee_diet_breadth-7march2023.csv')

specialists = diet_breadth[diet_breadth$diet_breadth=='specialist',]$scientificName
globi_u=globi %>% left_join(diet_breadth %>% distinct(scientificName,diet_breadth)) %>%
  mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth))

#double check no increase in the number of rows
nrow(globi) ==nrow(globi_u)


# for bees on the Jarrod Fowler lists, what % visit their host plants?
# for the globi data: categorize interaction partner as host or non-host
# loop through the bee speices in globi_u
# get their host plants
# if the interaciton partner is in the family or genus then categorize it as a host
#fowler_specialists = #fowler[fowler$diet_breadth == 'specialist',]$scientificName

# let's exclude bees with fewer than 20 interactions in the globi dataset
n_threshold = 20
bigN_bees20=globi_u %>% 
  group_by(scientificName) %>% 
  summarize(n=n()) %>% 
  filter(n>=n_threshold)

globi_summ_NoSizeFilter = globi_u %>% 
  group_by(scientificName,bee_family,plant_genus,plant_family) %>% 
  summarize(n_genus=n(),n_citations=n_distinct(sourceCitation)) 

globi_specs = globi_summ_NoSizeFilter %>% 
  filter(scientificName %in% bigN_bees20$scientificName) %>%
  left_join(diet_breadth) %>%
  filter(diet_breadth=='specialist') %>%
  #filter(n_genus >=5) %>% ##let's exclude interactions with fewer than 5 records
  mutate(bee_plant = paste(scientificName,plant_genus))

# double check that specialist bees have either family as host rank or genus
# but not both
which(fowler_formatted %>% split(.$scientificName) %>%
        purrr::map_lgl(function(df) n_distinct(df$host_rank)>1))

my_index = which(globi_specs$scientificName  == 'Dufourea virgata')
a = globi_specs %>% split(1:nrow(globi_specs))
row = a[[my_index]]
empty_list = c()
globi_host = globi_specs %>% split(1:nrow(globi_specs)) %>% 
  purrr::map_dfr(function(row){
    
    (host_plant_df = fowler_formatted %>% 
       filter(scientificName == row$scientificName[1]))
    
    if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='tribe') visiting_host = row$plant_family =='Asteraceae'
    
    return(row %>% mutate(host=visiting_host))
    
  })   #add 0 for bees that haven't visited their host plant
dupes = globi_host[duplicated(globi_host$scientificName),]$scientificName
globi_host %>% filter(scientificName %in% dupes) %>% distinct(diet_breadth_detailed)
globi_host %>% select(scientificName,plant_genus,host)

## what percentage of specialist bees visit their host plants?
host_fidelity=globi_host %>%
  group_by(scientificName) %>%
  summarize(sum_n=sum(n_genus[host])/sum(n_genus),sample_size = sum(n_genus),
            sum_cits = sum(n_citations[host])/sum(n_citations)) 


# plotting params:
point_col = adjustcolor('cadetblue',.5)
cex_lab = 1.5
cex_axis=1.3
cex_main = 1.5

# pdf('figures/histogram_visits_hosts_specialists.pdf')
par(mfrow=c(1,1), mar=c(5,5,4,1))
hist(host_fidelity$sum_n*100,main='Pollen specialists - visits to host plants',
     xlab='% of visits to host plants',col=point_col,cex.lab=cex_lab, 
     cex.axis=cex_axis, cex.main = cex_main)
# dev.off()

paste0('specialist bees visited their host plants on average ',round(mean(host_fidelity$sum_n*100),2),
       '% of the time (median = ',round(median(host_fidelity$sum_n*100),2),'%)')

#what percentage of bees visit their host plants 100% of the time?
paste0(round(mean(host_fidelity$sum_n==1)*100,2),'% of specialist bees visit their host plants 100% of the time')

#get the bees that are least faithful to their host plants
host_fidelity %>% arrange(sum_n)
paste('For',sum(host_fidelity$sum_n==0),'specialist bee species, there were no records of the bee visiting their pollen hosts')
paste('these bees were:')
(unfaithful_bees = host_fidelity[host_fidelity$sum_n==0,]$scientificName)
fowler_formatted %>% filter(scientificName %in% unfaithful_bees) %>% 
  distinct(scientificName, host,host_rank)

globi_host %>% filter(scientificName == "Perdita zebrata")
fowler_formatted %>% filter(scientificName == 'Perdita zebrata') %>% distinct(host)

hist(host_fidelity$sum_cits*100)
host_fidelity %>% filter(sum_n<.3)


#is host fidelity a sampling artificat?
with(host_fidelity,plot(sample_size,sum_n, xlab = "Sample size", ylab = "% of visits to host plants", 
                        cex.lab=cex_lab, cex.axis = cex_axis, pch = 16, col = adjustcolor('cadetblue',.5)))

#what are low host fidelity bees with large sample sizes?
host_fidelity %>% filter(sample_size >200 & sum_n<0.5)


## what about male vs female?
data.frame(globi_u %>% 
             filter(!is.na(sourceSexName)) %>%
             group_by(scientificName,sourceSexName) %>%
             summarize(n=n()))

sexed = globi_u %>% 
  filter(!is.na(sourceSexName)) %>%
  filter(!sourceSexName %in% c('unknown','undetermined',"u", "unknown sex", 'm, f','gyne','male and female')) %>%
  group_by(scientificName,plant_family,plant_genus,sourceSexName) %>%
  summarize(n=n())
unique(sexed$sourceSexName)
sex_codes  = data.frame(sourceSexName = unique(sexed$sourceSexName)) %>%
  mutate(sex = NA) %>%
  mutate(sex = ifelse(sourceSexName %in% c('queen','female (queen)','f','fem','female','femlae',
                                           'female paratype','femaleparatype',
                                           'femaleholotype','worker','female holo',
                                           "1 female","female (worker)", "female queen"),'female',sex)) %>%
  mutate(sex = ifelse(sourceSexName %in% c('male','male paratype','maleparatype','m'),'male',sex)) 

#double check I didn't miss any of the sex codes
sex_codes %>% filter(is.na(sex))
globi_summ_bySex = globi_u %>% 
  filter(!is.na(sourceSexName)) %>%
  filter(!sourceSexName %in% c('unknown','undetermined',"u", "unknown sex", 'm, f','gyne','male and female')) %>%
  left_join(sex_codes) %>%
  group_by(scientificName,bee_family,plant_genus,plant_family,sex) %>% 
  summarize(n_genus=n(),n_citations=n_distinct(sourceCitation)) 



globi_specs_sexed = globi_summ_bySex %>% 
  left_join(diet_breadth) %>%
  filter(diet_breadth=='specialist') %>%
  mutate(bee_plant = paste(scientificName,plant_genus)) %>%
  filter(scientificName %in% bigN_bees20$scientificName) #filter to just bee bees above our threshold sample szie

globi_host_bySex = globi_specs_sexed %>% split(1:nrow(globi_specs_sexed)) %>% 
  purrr::map_dfr(function(row){
    
    (host_plant_df = fowler_formatted %>% 
       filter(scientificName == row$scientificName[1]))
    
    if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='tribe') visiting_host = row$plant_family =='Asteraceae'
    
    return(row %>% mutate(host=visiting_host))
    
  }) 


hosts_by_sex1 = globi_host_bySex %>%
  filter(!is.na(host)) %>%
  group_by(scientificName,sex) %>%
  summarize(sum_n=sum(n_genus[host])/sum(n_genus),sample_size = sum(n_genus)) 


# we'll only include bees with at least 10 observations for both male and feamles
hosts_by_sex = hosts_by_sex1
hosts_by_sex1 %>% split(.$scientificName) %>%
  map_lgl(function(df) df)
#first just plot as box plots
with(hosts_by_sex %>% filter(sex != 'unknown'),boxplot(sum_n~sex))

# then only include bees with pairs of male and female 
# with sample size above some threshold

paired_sexes=names(which(hosts_by_sex  %>% split(.$scientificName) %>% map_lgl(function(df)nrow(df)==2) ))
with(hosts_by_sex %>% 
       filter(scientificName %in% paired_sexes & sex !='unknown'),boxplot(sum_n~sex))

#pair males and females
hosts_by_sex_bigN=hosts_by_sex %>% 
  filter(scientificName %in% paired_sexes & sex !='unknown')

a=hosts_by_sex_bigN %>%
  split(.$scientificName)
df=a[[1]]
sex_differences = hosts_by_sex_bigN %>%
  split(.$scientificName) %>%
  map_dbl(function(df){
    sex_diff = df[df$sex=='female',]$sum_n-df[df$sex=='male',]$sum_n
    return(sex_diff)
  })
hist(sex_differences)

(sexed_wide = hosts_by_sex_bigN %>% select(-sample_size) %>%
    pivot_wider(values_from=sum_n,names_from=sex,values_fill=0))

#make a paired box-plot
loc_gen=1.2; loc_spec=1.8
box_col = adjustcolor('white',0)


# pdf('figures/males_v_females.pdf')
par(mfrow=c(1,1))
with(hosts_by_sex_bigN,stripchart(sum_n~sex,
                                  vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                                  cex.lab = cex_lab, cex.axis=cex_axis,
                                  xlab = 'Bee sex',ylab = '% of visits to host plant'))
for(i in 1:nrow(sexed_wide)){segments(loc_gen, sexed_wide$female[i], loc_spec, sexed_wide$male[i],lty=2,col=adjustcolor('black',.3))}
with(hosts_by_sex_bigN,boxplot(sum_n~sex,col=box_col,add=T,at=c(loc_gen,loc_spec), axes = F,boxwex=c(.35,.35)))
# dev.off()




#combine the two post-hoc figs together
pdf('figures/host_fidelity_posthoc.pdf', width = 12)
par(mfrow=c(1,2), mar =c(4.5,4.5,4.2, 1))
#is host fidelity a sampling artificat?
with(host_fidelity,plot(log10(sample_size),sum_n*100, xlab = expression("Log"[10]*"(Sample size)"), ylab = "% of visits to host plants", 
                        cex.lab=cex_lab, cex = 1.2, cex.axis = cex_axis, pch = 16, col = adjustcolor('cadetblue',.5)))
with(hosts_by_sex_bigN,stripchart(sum_n*100~sex,
                                  vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                                  cex.lab = cex_lab, cex.axis=cex_axis,
                                  xlab = 'Bee sex',ylab = '% of visits to host plant'))
for(i in 1:nrow(sexed_wide)){segments(loc_gen, sexed_wide$female[i]*100, loc_spec, sexed_wide$male[i]*100,lty=2,col=adjustcolor('black',.3))}
with(hosts_by_sex_bigN,boxplot(sum_n*100~sex,col=box_col,add=T,at=c(loc_gen,loc_spec), axes = F,boxwex=c(.35,.35)))

dev.off()



##any statistical diff in sex?
mean(sexed_wide$female)
mean(sexed_wide$male)
(diff=sexed_wide$female-sexed_wide$male)
hist(diff)

library(ggpubr)
ggqqplot(diff)
shapiro.test(diff)
# since the data are not normally distributed 
# we will use a wilcoxin signed rank test
wilcox.test(Pair(female, male) ~ 1, ,alternative='greater',data = sexed_wide)

mean(diff)*100
median(diff)








####
#what about nectarless plants
plants_nectar <- read_excel("modeling_data/plants_nectar.xlsx") %>%
  filter(nectar_status != 'unknown' & !is.na(nectar_status))

host_fidelity %>% 
  filter(host %in% plants_nectar)