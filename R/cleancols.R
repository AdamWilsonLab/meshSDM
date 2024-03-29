#' @import plotly
#' @import tidyverse

cleancols <- function(x){
  dplyr::select(x,
                x="Coord. X",
                y="Coord. Y",
                z="Coord. Z",
                r=one_of(c("Rf","R")),
                g=one_of(c("Gf","G")),
                b=one_of(c("Bf","B")),
                class=Classification,
                x_smooth=contains("Coord. X.smooth"),
                y_smooth=contains("Coord. Y.smooth"),
                z_smooth=contains("Coord. Z.smooth"),
                Ny=Ny,
                Nx=Nx,
                Nz=Nz,
                gc=contains("Gaussian curvature"),
                rough=contains("Roughness"),
                aspect=contains("Dip direction (degrees)"),
                density=contains("Volume density"),
                slope="Dip (degrees)")
}
