##########################################

# visualize sampled quads

mesh=readRDS("rawdata/20190415/output/ect110r_10.rds")
shade3d_var(mesh,mesh$data$pres_ocr,palette = "magma")


