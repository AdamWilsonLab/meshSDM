
# meshSDM

A collection of functions and the workflow used to fit and project Species Distribution Models (SDMs) on 3D meshes in **"Three-dimensional species distribution modeling reveals the realized spatial niche for coral recruitment on contemporary Caribbean reefs"(Martínez-Quintana et al., 2023)**.

## Abstract 

The 3-dimensional structure of habitats is a critical component of species' niches driving coexistence in species-rich ecosystems. However, its influence on structuring and partitioning recruitment niches has not been widely addressed. We developed a new method to combine Species Distribution Modeling and Structure from Motion and characterized three-dimensional recruitment niches of two ecosystem engineers on Caribbean coral reefs, scleractinian corals and gorgonians. Fine-scale roughness was the most important predictor of suitable habitat for both taxa, and their niches largely overlapped, primarily due to scleractinians broader niche breadth. Crevices and holes at mm-scales on calcareous rock with low coral cover were more suitable for octocorals than for scleractinian recruits, suggesting the decline of scleractinian corals is facilitating the recruitment of octocorals on contemporary Caribbean reefs. However, the relative abundances of the taxa were independent of the amount of suitable habitat on the reef, emphasizing niche-processes solely do not predict recruitment rates.

## Installing from GitHub
```
devtools::install_github("https://github.com/AdamWilsonLab/meshSDM.git")
library(meshSDM)
```
- Functions used in the library meshSDM can be found in the `R` folder. 
- Data are found in [Zenodo public repository](https://doi.org/10.5281/zenodo.7487349)
- Scripts to process and reshape the data prior to analysis, to fit and project Maxent on 3D meshes,and to compare habitat suitability between taxa are available in the `workflow` folder and scripts can be run in the following order:

`workflow/01_Data_Processing_mesh.R`

- Find all dense point clouds, meshes and shape files (recruits) and align each point cloud to each mesh. 
- Import and process the mesh to return it with attached attributes.
- For the occurrences, calculate the median of continuous variables and percentages of categorical predictors within 1-cm around each recruit (i.e., within 1-cm diameter torus).
- Link the attributes calculated for the occurrences with the mesh and update the attribute table.

`workflow/01b_Data_Processing_mesh.R`

- For each quadrat, merge the continuoes variables calculated at five scales into one mesh, and drop the variables (columns) that are not needed for the analysis.
- Build file list combining the data set for all quadrats and scales.
- Add a column in the data frame with unique identifiers for each face of the polygon mesh (fid).

`workflow/02_Data_Processing_mesh_combined.R`

- Read in the individual quadrat data, rbind it, and reshape it to the format needed for EDA and modeling.

`workflow/03_Variable_Processing_ENMTools.R`

- Quadrats are grouped in 8 groups to avoid spatial autocorrelation.
- Sample 30000 background points from each group of quadrats and all the occurrences (recruits).
- Extract the information needed for model fitting.
- Reformat the recruit occurrences to be used with ENMTools into an enmtools.species object.

`workflow/04_ENMTools_Analyses.R`

- Select variables for model fitting (Both taxa):
    * Select variables that did not generate NAs in the occurrences.
    * Calculate variable importance.
    * Test for variable correlation and select the most important from the pairs of correlated variables (|r|>0.6).
- For each taxon: 
    * Test the performance of models built with different regularization multipliers.
    * Selected the model with the best performance based on AUC and Bohl tests.
    * Train the model with 70% of the occurrences and validate in the remaining 30%.
    * Plot predicted habitat suitability on the mesh.
- Code for the following figures:
    * **Figure 3** and **Figure 4**.
    * **Supplementary Figure 2**, **Supplementary Figure 3**, **Supplementary Figure 4**.

`workflow/05_Comparison_Suitability_Between_Taxa.R`

- Niche breadth calculations.
- Violin Plots comparing suitability scores between taxa (**Figure 6**).
- Hypotheses testing:
    * Niche overlap (similarity) between models in E-space using latin hypercube sampling.
    * Schoener’s D (Schoener 1968), I (Warren et al. 2008), and  Spearman's rank correlation coefficient.
    * Niche identity or equivalency test (**Figure 5A**).
    * Symmetric Background test (**Figure 5B**).
    * Shapiro-Wilk normality test and Mann-Withey U test.

To uninstall the package, we suggest `remove.packages("meshSDM")`.


