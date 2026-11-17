# script to check what the file structure of an FVCOM NCDF4 file is to devlop extraction code.
rm(list = ls())

library(tidyverse)
library(ncdf4)

nc <- nc_open("./Objects/Example/nsea_v09_restart_0001_cut2.nc")

# Overview: dimensions, variables, global attributes
print(nc)

# Just the names
names(nc$dim)   # dimensions
names(nc$var)   # variables

# Compact table: variable, dimensions, units, long name
var_info <- tibble(
  name  = names(nc$var),
  dims  = map_chr(nc$var, ~ paste(map_chr(.x$dim, "name"), collapse = ", ")),
  units = map_chr(nc$var, ~ .x$units),
  long  = map_chr(nc$var, ~ .x$longname)
)

# save variable table
write.csv(var_info,"./Objects/FVCOM_variables.csv",row.names = F)


lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
zeta <- ncvar_get(nc, "zeta")      # dims: node x time

# check the shape before indexing
dim(zeta)

df <- tibble(lon = lon, lat = lat, zeta = zeta[, 2])  # first time step

temp <- ncvar_get(nc, "temp")
dim(temp)
df$temp_surf <- temp[, 1, 1]   # surface layer, first time step (layer 1 is the surface in FVCOM)

ggplot(df, aes(lon, lat, colour = temp_surf)) +
  geom_point(size = 0.4) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_minimal()

nv  <- ncvar_get(nc, "nv")      # check dim(nv): should be nele x 3
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
# if lon/lat are missing in a restart file, use x and y (projected) instead

dim(nv)
if (dim(nv)[1] == 3) nv <- t(nv)   # make it nele x 3 if it came the other way round

# each triangle has 3 edges: 1-2, 2-3, 3-1
edges <- bind_rows(
  tibble(a = nv[, 1], b = nv[, 2]),
  tibble(a = nv[, 2], b = nv[, 3]),
  tibble(a = nv[, 3], b = nv[, 1])
) %>%
  mutate(lo = pmin(a, b), hi = pmax(a, b)) %>%
  distinct(lo, hi)                # drop shared edges so each is drawn once

seg <- edges %>%
  transmute(x = lon[lo], y = lat[lo], xend = lon[hi], yend = lat[hi])

ggplot(seg) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 0.1, colour = "grey30") +
  coord_quickmap() +
  theme_minimal()
ggsave("./Figures/domain/FVCOM_Grid.png")
