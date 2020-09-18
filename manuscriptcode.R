library(RCurl)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(UpSetR)

#import datasets for analysis
dataset_diagnosesgen_all <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_diagnosesgen_all.csv"))
dataset_diagnosessampletype <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_diagnosessampletype.csv"))
dataset_clinicalsigns <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_clinicalsigns.csv"))
dataset_samplereason <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_samplereason.csv"))
dataset_sampledecisions_type <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_sampledecision.csv"))
dataset_sampletestoutcome <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_sampletestoutcomes.csv"))
dataset_nuts3_base <-read.csv(text=getURL("https://raw.githubusercontent.com/jdatarsa/sesdata/master/dataset_nuts3_base.csv"))

# Results - Sample and diagnostic information
# Sample and diagnostic information - diagnoses per year
dataset_diagnosesgen_all %>% 
  group_by(year = format(as.Date(dataset_diagnosesgen_all$eventdate),"%Y")) %>% 
  summarise(totaldiagnoses_byyear = n())

#Table 2: Diagnostic test(s) requested
dataset_diagnosesgen_all %>% 
  group_by(result) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>%
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)

#Table 2: Horse signalment
#Sex
dataset_diagnosesgen_all %>% 
  filter(sexgen != "unreported") %>% 
  group_by(sexgen) %>% 
  summarise(total = n()) %>%
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>% 
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)

#Breed
dataset_diagnosesgen_all %>% 
  filter(breedgen != "undefined") %>% 
  group_by(breedgen) %>% 
  summarise(total = n()) %>%
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>% 
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)

#Age (note all in years)
age.avail<-dataset_diagnosesgen_all %>% 
  filter(!is.na(age)) %>% 
  select(age)

median(age.avail$age)
quantile(age.avail$age, c(0.25,0.75))
range(age.avail$age)

#Premises type
dataset_diagnosesgen_all %>% 
  filter( premesiscategory != "undefined") %>% 
  group_by(premesiscategory) %>% 
  summarise(total = n()) %>%
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>% 
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)


#Sample types
# Sample type
dataset_diagnosessampletype %>% 
  group_by(sampletype) %>% 
  summarise(total = n()) %>%
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>% 
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)

#Swab locations
dataset_diagnosessampletype %>% 
  filter(sampletype == 'swab') %>% 
  group_by(samplelocation) %>% 
  dplyr::summarise(total = n(), 
                   perc = as.numeric(round(n()/nrow(dataset_diagnosessampletype %>% 
                                                      filter(sampletype == 'swab'))*100,1))) %>% 
  mutate(percprint = paste(perc,"%",sep ="")) %>% arrange(., desc(total)) %>%
  rbind(c("Total",sum(as.numeric(.$total)),sum(as.numeric(.$perc)),paste(sum(as.numeric(.$perc)),"%",sep=""))) %>% 
  filter(samplelocation == 'nasopharyngeal' | samplelocation == 'nasal' | samplelocation == 'abscess') %>%
  rbind(c("other",nrow(dataset_diagnosessampletype %>% 
                         filter(sampletype == 'swab')) - sum(as.numeric(.$total)),100-sum(as.numeric(.$perc)),paste(100-sum(as.numeric(.$perc)),"%",sep=""))) %>% select(-perc)


# Individual sample results
dataset_sampletestoutcome %>% 
  janitor::tabyl(culture, qPCR) %>% 
  adorn_totals(c("row", "col")) %>%
  adorn_title

#Clinical signs
nrow(dataset_clinicalsigns) #Total number reported
length(unique(dataset_clinicalsigns$strangleslogid)) #representing total number of diagnoses
prop.test(length(unique(dataset_clinicalsigns$strangleslogid)), nrow(dataset_diagnosesgen_all),
        conf.level = 0.95)

(dataset_clinicalsigns.total<-dataset_clinicalsigns %>% group_by(cx) %>% 
  summarise(total = n()) %>%
  arrange(., desc(total)) %>% 
  head(10) %>% 
  rbind(c("other", as.numeric(nrow(dataset_clinicalsigns) - sum(.$total)))))

dataset_clinicalsigns.total$total<-as.integer(dataset_clinicalsigns.total$total)

dataset_clinicalsigns.total %>%  
  mutate(totalN = sum(total)) %>% 
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>% 
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)

#Figure 2
setval<- as.integer(dataset_clinicalsigns %>% group_by(cx) %>% 
                      dplyr::summarise(total = n()) %>% 
                      arrange(desc(total)) %>% 
                      head(1) %>% 
                      .$total*1.5)

cxdata.spread<-spread(as.data.frame(table(dataset_clinicalsigns)), cx, Freq)

upset(cxdata.spread, nsets = 10, nintersects = 20, mb.ratio = c(0.5, 0.5),
                    order.by = c("degree","freq"), decreasing = c(FALSE,TRUE),
                    matrix.color = "gray23", main.bar.color = "#24B4B4",
                    mainbar.y.label = "Frequency of\n reporting combinations (n)", 
                    mainbar.y.max = NULL,
                    sets.bar.color = "gray23", 
                    sets.x.label = "Clinical sign reported (n)",
                    set_size.show = TRUE,
                    text.scale = 1.5, 
                    set_size.scale_max = setval
)

#Reason for sampling
#Table 2
dataset_samplereason %>% 
  filter(samplereason != "undefined") %>% group_by(samplereason) %>% 
  summarise(total = n(), perc = as.numeric(round(n()/nrow(dataset_samplereason%>% 
                                                            filter(samplereason != "undefined"))*100,1))) %>% 
  mutate(percent = total / sum(total)*100) %>%
  mutate (totalreported = sum(total)) %>% rowwise() %>% 
  mutate(lowerciperc = prop.test (total, totalreported)$conf.int[1]*100,
         upperciperc = prop.test(total, totalreported)$conf.int[2]*100)

#Number of diagnoses that had reason for sampling
nrow(dataset_samplereason %>% 
         filter(samplereason != "undefined") %>% 
         select(strangleslogid) %>% distinct())

#Decisions for sampling
#focus on where clinically ill, posit-infection or ELISA were primary and only reasons for sampling
dataset_sampledecisions_type<-dataset_sampledecisions_type %>% mutate(cx = ifelse(grepl('clinically ill',samplereasons2), TRUE, FALSE),
                                               pis = ifelse(grepl('post infection',samplereasons2), TRUE, FALSE),
                                               pselisa = ifelse(grepl('ELISA',samplereasons2), TRUE, FALSE)) %>% 
  filter(cx == TRUE | pis == TRUE | pselisa == TRUE)

# select sample types focusing primarily on GPW and swabs
dataset_sampledecisions_type<-dataset_sampledecisions_type %>% mutate(swab = ifelse(grepl('swab',sampletypes2), TRUE, FALSE),
                      wash = ifelse(grepl('guttural pouch wash',sampletypes2), TRUE, FALSE))

# extract where multiple sample reasons did not occur
dataset_sampledecisions_type<- dataset_sampledecisions_type %>%  mutate(checking = cx + pis + pselisa) %>%  filter(checking == 1)
dataset_sampledecisions_type.summ<-dataset_sampledecisions_type %>%  group_by(cx, pis, pselisa, swab, wash) %>% dplyr::summarise(total = n())

dataset_sampledecisions_type.summ<-dataset_sampledecisions_type.summ %>% mutate(reason = ifelse(cx == TRUE,'cx',ifelse(pis == TRUE,'pis','pselisa')),
                      samptype = ifelse(swab == TRUE & wash == TRUE, 'swab and GPL', (ifelse(swab == TRUE, 'swab', (ifelse(wash == TRUE, 'GPL', 'other')))))) 

dataset_sampledecisions_type.summ<-dataset_sampledecisions_type.summ %>% ungroup() %>% select(reason, samptype, total)

(dataset_sampledecisions_type.summ<-dataset_sampledecisions_type.summ %>% group_by(reason) %>% mutate(totalN = sum(total)) %>%  rowwise() %>% 
  mutate(per=total/totalN*100,
         lowerci = prop.test(total, totalN)$conf.int[1]*100,
         upperci = prop.test(total, totalN)$conf.int[2]*100))

#number of diagnoses that had one of the three prime reasons for sampling but not in combination with another prime reason
#for sampling for the diagnoses as a whole. 
sum(dataset_sampledecisions_type.summ$total) 

#Figure 3
ggplot(data = dataset_sampledecisions_type.summ, aes(x = samptype, y = per, colour = reason)) + 
  geom_pointrange(aes(ymin = lowerci, ymax = upperci),
                       position = position_dodge(width = 0.25), size=1) + 
  geom_errorbar(aes(ymin = lowerci, ymax = upperci),
                position = position_dodge(width = 0.25), size=1, width = 0.2,
                show.legend = FALSE) + 
  scale_color_discrete(name = "Sample reason", labels = c("Clinically ill","Post-infection screening","Positive ELISA follow up")) +
  scale_x_discrete(labels = c("Other","Swab","Swab and GPL","GPL")) + 
  xlab("Sample type") +
  ylab("Percentage") + 
  theme(axis.text.x = element_text(size = rel(2))) +
  theme(axis.text.y = element_text(size = rel(2))) +
  theme(axis.title.x = element_text(size = rel(3))) +
  theme(axis.title.y = element_text(size = rel(3))) + 
  theme(legend.title=element_text(size=rel(2))) +
  theme(legend.text=element_text(size=rel(1.5))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  + 
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

# Spatial information
# Unique veterinarians submitting information
length(unique(dataset_diagnosesgen_all$submittingvet_prim))

# Percentage of nuts 3 areas reported from
length(unique(dataset_diagnosesgen_all$nuts3gid))/nrow(dataset_nuts3_base)

# Total submitting vet practices - Figure 4A
dataset_diagnosesgen_all %>% 
  select(nuts3gid, submittingvet_prim) %>% 
  distinct() %>% 
  group_by(nuts3gid) %>% 
  summarise(totalvetssubmittingperregion = n())

# Total diagnoses per region - Figure 4B
dataset_diagnosesgen_all %>% 
  group_by(nuts3gid) %>% 
  summarise(totaldiagnosesperregion = n())

# Average number of diagnoses per submitting vet practice per region - Figure 4C
dataset_diagnosesgen_all.summ.region.submittingvets<-dataset_diagnosesgen_all %>% 
  group_by(submittingvet_prim, nuts3gid) %>% 
  summarise(diagnosesbyvetpracticebyregion = n()) %>% ungroup() %>% 
  group_by(nuts3gid) %>% summarise(mean(diagnosesbyvetpracticebyregion), sd(diagnosesbyvetpracticebyregion))

mean(dataset_diagnosesgen_all.summ.region.submittingvets$`mean(diagnosesbyvetpracticebyregion)`)
sd(dataset_diagnosesgen_all.summ.region.submittingvets$`mean(diagnosesbyvetpracticebyregion)`)

#Percentage diagnoses by region 
dataset_diagnosesgen_all %>% 
  inner_join(dataset_nuts3_base, by = c("nuts3gid" = "gid")) %>%
  group_by(nuts_name) %>% 
  summarise(totaldiagnoses = n(), 
            percentageraw = as.numeric(round(n()/nrow(dataset_diagnosesgen_all %>% 
                                                        inner_join(dataset_nuts3_base, by = c("nuts3gid" = "gid")))*100,1))) %>% 
  arrange(desc(percentageraw)) %>% #to arrange alphabetically -> arrange(nuts_name)#
  mutate(percentagediagnoses = paste(percentageraw, "%" ,sep =""))

