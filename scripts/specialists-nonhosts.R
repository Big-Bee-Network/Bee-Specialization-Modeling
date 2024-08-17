# clear the environment
rm(list=ls()) 

# load packages
library(tidyverse)
library(vroom)

# load the globi data
# this data file has bee and plant names updated, and only american bees
globi = vroom("modeling_data/globi_allNamesUpdated_Henriquez_Piskulich.csv") %>%
  mutate(bee_genus = sub(' .*',"",scientificName))




# load the host data
fowler_formatted = read_csv('modeling_data/fowler_formatted-15Aug2024.csv') %>% 
  mutate(ref='fowler')
russell_formatted = read_csv('modeling_data/russell_formatted-15Aug2024.csv')%>% 
  mutate(ref='russell')
hosts = fowler_formatted %>% bind_rows(russell_formatted)

# double check no bees are duplicated
(dupes = hosts[duplicated(hosts$scientificName),]$scientificName)

## both data.frames just contain specialitss. filter th globi data to be just that
globi_u=globi %>% filter(scientificName %in% hosts$scientificName)
globi_u %>% filter(scientificName=="Perdita zebrata")

#load data with discrepancies between fowler and wfo:
discrepancies <- read_csv("modeling_data/discrepancies_fowler_wfo.csv")

#how to deal with this?
#if bee species is visiting one of these families in "wfo_family" it gets counted

# for specialist bees, what % visit their host plants?
# for the globi data: categorize interaction partner as host or non-host
# loop through the bee speices in globi_u
# get their host plants
# if the interaciton partner is in the family or genus then categorize it as a host
#fowler_specialists = #fowler[fowler$diet_breadth == 'specialist',]$scientificName

n_distinct(globi_u$scientificName)
globi_u %>% distinct(scientificName) %>% nrow(.)


# let's exclude bees with fewer than 20 interactions in the globi dataset
n_threshold = 20
bigN_bees20=globi_u %>% 
  group_by(scientificName) %>% 
  summarize(n=n()) %>% 
  filter(n>=n_threshold)


bigN_bees20 %>% filter(scientificName == "Perdita zebrata")
# View(globi_u %>% filter(scientificName == "Perdita zebrata"))
# View(globi_u %>% filter(scientificName == "Perdita zebrata"))
globi_u %>% 
  filter(scientificName == "Perdita zebrata") %>%
  group_by(plant_genus) %>%
  summarize(n=n()) %>% arrange(desc(n))

#get sample sizes:
n_distinct(bigN_bees20$scientificName)
sum(bigN_bees20$n)

with(bigN_bees20, paste0('there are ', n_distinct(scientificName), ' species with ', sum(n), ' records'))

globi_summ_NoSizeFilter = globi_u %>% 
  group_by(scientificName,bee_family,plant_genus,plant_family) %>% 
  summarize(n_genus=n(),n_citations=n_distinct(sourceCitation)) 

globi_specs = globi_summ_NoSizeFilter %>% 
  filter(scientificName %in% bigN_bees20$scientificName) 
 # mutate(bee_plant = paste(scientificName,plant_genus))

#combine hosts with discrepancies
a= hosts %>% filter(scientificName %in% discrepancies$scientificName) %>%
  split(.$scientificName)
row = a[[1]]


hosts_add = hosts %>% filter(scientificName %in% discrepancies$scientificName) %>%
  split(.$scientificName) %>% map_dfr(function(row){
    row2 = row %>% select(-host) %>% 
      left_join(discrepancies %>% rename(host = wfo_family) %>% 
                  distinct(scientificName, host))
    
    return(row %>% bind_rows(row2))
    
  })
hosts = hosts %>% filter(!scientificName %in% discrepancies$scientificName) %>% bind_rows(hosts_add)

# double check that specialist bees have either family as host rank or genus
# but not both
which(hosts %>% split(.$scientificName) %>%
        purrr::map_lgl(function(df) n_distinct(df$host_rank)>1))

my_index = which(globi_specs$scientificName  == 'Dufourea virgata')
a = globi_specs %>% split(1:nrow(globi_specs))
row = a[[my_index]]
empty_list = c()
globi_host = globi_specs %>% split(1:nrow(globi_specs)) %>% 
  purrr::map_dfr(function(row){
    
  
    
    (host_plant_df = hosts %>% 
       filter(scientificName == row$scientificName[1]))
    
    
    
    if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host
    
    

    return(row %>% mutate(host=visiting_host))
    
  })   #add 0 for bees that haven't visited their host plant
dupes = globi_host[duplicated(globi_host$scientificName),]$scientificName
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

#change y-axis to be percentages for histogram:
h= hist(host_fidelity$sum_n*100, plot= F)
h$density = h$counts/sum(h$counts)*100
# plot(h,freq=FALSE)

# pdf('figures/histogram_visits_hosts_specialists.pdf')
# tiff('figures/histogram_visits_hosts_specialists-15july2024.tiff', units="in", width=6, height=6, res=500, compression = 'lzw')

par(mfrow=c(1,1), mar=c(5,5,4,1))
plot(h, freq=F, main='Pollen specialists - visits to host plants', ylab = "% of pollen specialist species",
     xlab='% of visits to known pollen host plants',col=point_col,cex.lab=cex_lab, ylim = c(0,40),
     cex.axis=cex_axis, cex.main = cex_main)
# dev.off()

paste0('specialist bees visited their host plants on average ',round(mean(host_fidelity$sum_n*100),2),
       '% of the time (median = ',round(median(host_fidelity$sum_n*100),2),'%)')

#what percentage of bees visit their host plants 100% of the time?
paste0(round(mean(host_fidelity$sum_n==1)*100,2),'% of specialist bees visit their host plants 100% of the time')

# get summary stats
summary(host_fidelity$sum_n*100)
mean(host_fidelity$sum_n<.10);sum(host_fidelity$sum_n<.10)

mean(host_fidelity$sum_n>=.90); sum(host_fidelity$sum_n>=.90)



#get the bees that are least faithful to their host plants
host_fidelity %>% arrange(sum_n)
paste('For',sum(host_fidelity$sum_n==0),'specialist bee species, there were no records of the bee visiting their pollen hosts')
paste('these bees were:')
(unfaithful_bees = host_fidelity[host_fidelity$sum_n==0,]$scientificName)
hosts %>% 
  filter(scientificName %in% unfaithful_bees) %>% 
  distinct(scientificName, host,host_rank)

globi_host %>% filter(scientificName == "Perdita zebrata")
hosts %>% filter(scientificName == 'Perdita zebrata') %>% distinct(host)

#hist(host_fidelity$sum_cits*100)
need_inspection <- host_fidelity %>% filter(sum_n<.3)

globi_u %>% 
  filter(scientificName %in% need_inspection$scientificName) %>% 
  distinct(scientificName, plant_species, plant_genus, plant_family) %>%
  left_join(hosts %>% select(scientificName, host, host_rank))

# install.packages('remotes')
# remotes::install_github("barnabywalker/kewr")
library("kewr")
search_ipni("Gaillardia")

#is host fidelity a sampling artificat?
with(host_fidelity,plot(sample_size, sum_n, xlab = "Sample size", ylab = "% of visits to host plants", 
                        cex.lab=cex_lab, cex.axis = cex_axis, pch = 16, col = adjustcolor('cadetblue',.5)))

#what are low host fidelity bees with large sample sizes?
unfaithful<-host_fidelity %>% 
  filter(sample_size >200 & sum_n<0.5) %>% 
  arrange(desc(sample_size))

#make a table for the appendix
#need to add hosts
unfaithful_joined = unfaithful %>% left_join(hosts) %>%
  select(scientificName, host, host_rank, sum_n, sample_size) %>%
  rename(Bee=scientificName, 'Proportion of visits to host' = sum_n, 
         Host = host, "Host rank" = host_rank, 'Sample size' = sample_size)

# writexl::write_xlsx(unfaithful_joined,'modeling_data/unfaithful_pollen_specialists-18July2024.xlsx')

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
  mutate(bee_plant = paste(scientificName,plant_genus)) %>%
  filter(scientificName %in% bigN_bees20$scientificName) #%>% #filter to just bee bees above our threshold sample szie
  #left_join(hosts)
nrow(globi_specs_sexed)==nrow(globi_summ_bySex %>% filter(scientificName %in% bigN_bees20$scientificName))
a= globi_specs_sexed %>% split(1:nrow(globi_specs_sexed))
row=a[[198]]
globi_host_bySex = globi_specs_sexed %>% split(1:nrow(globi_specs_sexed)) %>% 
  purrr::map_dfr(function(row){
    
    (host_plant_df = hosts %>% 
       filter(scientificName == row$scientificName[1]))
    
    if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host

    return(row %>% mutate(host=visiting_host))
    
  }) 


hosts_by_sex1 = globi_host_bySex %>%
  filter(!is.na(host)) %>%
  group_by(scientificName,sex) %>%
  summarize(sum_n=sum(n_genus[host])/sum(n_genus),sample_size = sum(n_genus)) 


# we'll only include bees with at least 10 observations for both male and feamles
hosts_by_sex = hosts_by_sex1

#first just plot as box plots
# with(hosts_by_sex %>% filter(sex != 'unknown'),boxplot(sum_n~sex))

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

#get sample sizes
paste0('there are ', n_distinct(hosts_by_sex_bigN$scientificName), ' species with ', sum(hosts_by_sex_bigN$sample_size), ' total records')


#calculate pearson correlation coefficient
pearson <- with(host_fidelity ,cor(log10(sample_size), sum_n*100))
#get slope of relationship
coefs = with(host_fidelity %>% mutate(logN = log10(sample_size),per = 100*sum_n), coef(lm(per~logN)))
my_lm = with(host_fidelity %>% mutate(logN = log10(sample_size),per = 100*sum_n), lm(per~logN))

round_pearson = round(pearson,2)
#combine the two post-hoc figs together
# pdf('figures/host_fidelity_posthoc-1dec2023.pdf', width = 12)
tiff("figures/host_fidelity_posthoc-18july2024.tiff", width = 12, height =8, units = 'in', res = 1000, compression = 'lzw')

par(mfrow=c(1,2), mar =c(4.5,4.5,4.2, 1))
#is host fidelity a sampling artificat?
with(host_fidelity,plot(log10(sample_size),sum_n*100, xlab = expression("Log"[10]*"(Sample size)"), ylab = "% of visits to known pollen hosts", 
                        cex.lab=cex_lab, cex = 1.2, cex.axis = cex_axis, pch = 16, col = adjustcolor('cadetblue',.5)))
abline(coefs[1],coefs[2],lty=2)
text(3.3,10, paste0('r = ', round_pearson), cex =1.6)

with(hosts_by_sex_bigN,stripchart(sum_n*100~sex,
                                  vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                                  cex.lab = cex_lab, cex.axis=cex_axis,
                                  xlab = 'Bee sex',ylab = '% of visits to known pollen hosts'))
for(i in 1:nrow(sexed_wide)){segments(loc_gen, sexed_wide$female[i]*100, loc_spec, sexed_wide$male[i]*100,lty=2,col=adjustcolor('black',.3))}
with(hosts_by_sex_bigN,boxplot(sum_n*100~sex,col=box_col,add=T,at=c(loc_gen,loc_spec),boxlwd=2, border = 'mediumorchid3',axes = F,boxwex=c(.35,.35)))

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





