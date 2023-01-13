

#The raster.breadth function measures the spatial heterogeneity of the distribution of suitability scores from an ENM.
#It returns Levins’ two metrics of niche breadth.

G_breadth_ocr <- raster.breadth(ocr_model3_new) #in G-space
G_breadth_scr <- raster.breadth(scr_model3_new)

# env.breadth() Calculates breadth of a model in E-space using latin hypercube sampling

env.breadth(ocr_model3_new, env2new, tolerance = 1e-04, max.reps = 300, chunk.size = 1e+06)
env.breadth(scr_model3_new, env2new, tolerance = 1e-04, max.reps = 300, chunk.size = 1e+06)

#####################Prepare data for violin plots#################

minidataocr <- data %>%
  dplyr::select (ocr_suit, pres_ocr)%>%
  rename(suitscores = ocr_suit , datatype = pres_ocr) %>%
  mutate(rec ="Octocoral")

minidataocr$datatype <- as.factor(minidataocr$datatype)
minidataocr$datatype <- recode_factor(minidataocr$datatype, "1" = "pres",
                                      "0" = "bcg")

minidatascr <- data %>%
  dplyr::select (scr_suit, pres_scr)%>%
  rename(suitscores = scr_suit , datatype = pres_scr) %>%
  mutate(rec ="Scleractinian")

minidatascr$datatype <- as.factor(minidatascr$datatype)
minidatascr$datatype <- recode_factor(minidatascr$datatype, "1" = "pres",
                                      "0" = "bcg")

recscore <- merge(minidataocr, minidatascr, all.x = TRUE, all.y = TRUE) %>%
  na.omit()

##VIOLIN PLOT COMPARING SUITABILITY FOR EACH TAXA #############################

rec <- c("Octocoral", "Scleractinian")
names(rec) <- c("ocr", "scr")

## Figure 6 in Martinez-Quintana et al. 2023:

Suitbcg <- recscore %>%
  filter (datatype =="bcg")%>%
  ggplot(aes(x=suitscores, y=rec, fill=rec)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.08, color="gainsboro", alpha=0.2) +
  #scale_x_log10()+
  scale_fill_manual(values = c("grey","navy"),
                    breaks = c("Octocoral", "Scleractinian")) +
  #scale_color_viridis_d(option = "inferno") +
  theme_linedraw()+
  labs(x ="Suitability scores", y ="Taxa")+
  theme(legend.position = "none",
        text = element_text(size = 14))

#measuring similarity between ENMs.
#These include Schoener’s D (Schoener 1968), I (Warren et al. 2008),
#and the Spearman rank correlation coefficient between two rasters.
#This function measures similarity in the geographic distribution of suitability scores from two
#ENMs. It returns the metrics, I, D and Spearman rank correlation.
# These metrics are described in Warren et al. 2008.

raster.overlap(ocr_model3_new, scr_model3_new)

#env.overlap Calculates overlap between models in environment space using latin hypercube sampling

env.overlap(ocr_model3_new, scr_model3_new,
            env2new,
            tolerance = 0.001,
            max.reps = 1000,
            cor.method = "spearman",
            chunk.size = 1e+06,
            verbose = FALSE)


#Hypothesis testing################################################

##Niche identity or equivalency test
#To run an identity test, we need to decide what type of models we will build,
#how many replicates we will run, and (in the case of GLM and GAM) a model formula to use for empirical models
#and the Monte Carlo replicates.
#The resulting object contains the replicate models, p values, and plots of the results.
#Typically identity tests are run with at least 99 replicates

##Figure 5A in Martinez-Quintana et al., 2023:

identity.test <- identity.test(species.1 = ocr, species.2 = scr, env = env2new, nback = 20000,
                               bg.source = "points",
                               type = "mx", nreps = 100)

#Background test (symmetric):
#These test compare the empirical overlap to the overlap expected when points
#are drawn randomly from the background of both species
#(species.1 background vs. species.2 background)keeping sample sizes for each species constant

####Figure 5A in Martinez-Quintana et al., 2023:

bg.bc.sym = background.test(species.1 = ocr,
                            species.2 = scr,
                            env = env2new,
                            type = "mx",
                            nreps = 100,
                            test.type = "symmetric")

###Test whether the amount of suitable habitat on the reef is simialr between taxa:

##### First, test for data normality

# Shapiro-Wilk normality test:
my_data<- recscore %>%
  filter (datatype =="bcg")%>%
  sample_(5000)#maximum sample size for Shapiro

head(my_data)
with(my_data, shapiro.test(suitscores[rec == "Octocoral"]))# p << 0.05 NOT NORMAL
with(my_data, shapiro.test(suitscores[rec == "Scleractinian"])) #  p << 0.05 NOT NORMAL

#Samples not normal distributed. Thus,we use the non parametric two-samples Wilcoxon rank test (Mann-Withey U):

my_data<- recscore %>%
  filter (datatype =="bcg") #all samples

#Is the median suitability scores different between taxa?

wilcox.test(suitscores ~ rec, data = my_data,   #p-value < 2.2e-16
            exact = TRUE, conf.int = TRUE, conf.level = 0.95)


#I the median suitability lower for octo recruits than for scleractinian recruits?

wilcox.test(suitscores ~ rec, data = my_data,
            exact = FALSE, alternative = "less")

