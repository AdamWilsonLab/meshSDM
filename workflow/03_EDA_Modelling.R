### Save them
readR(datal,"output/datalong.rds")
saveRDS(dataw,"output/datawide.rds")


############
datal %>%
  filter(var%in%c("rough","hole","gcs","aspect","slope","visible")) %>%
  ggplot(aes(x=as.factor(pres_ocr),y=value))+
  geom_violin()+
  facet_grid(var~scale,scales="free")



##


