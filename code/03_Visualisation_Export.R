library("magrittr")
library("viridis")

# Options ----------------------------------------------------------------------

# Early Holocene Norway

#peak1 <- 8.5
#peak2 <- 12.5

#tac_shp_layer <- "Norway_cropped"
#tac_epsg <- "+init=epsg:25832"

# Late Glacial S Scandinavia


peak1 <- 13.5
peak2 <- 20


tac_shp_layer <-"Southern_scandinavia_25832"
tac_epsg <- "+init=epsg:25832"


# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

options("scipen" = 0)

# Load Total Area of Calculation Polygon
# Early Holocene Norway:
tac <- rgdal::readOGR(dsn = "C:/Users/Victor/Desktop/skrivbord/Lundström et al 2020_PhilTransB_Final results/Data sets & Results/Late Glacial Denmark & Scania/shp-data", layer = "Southern_scandinavia_25832")
sp::proj4string(tac) <- sp::CRS(tac_epsg)

# Plot of archaeological sites and Thiessen polygons ------------------------


plot_sites <- function(){
  plot(vertices_spdf@coords[,1], vertices_spdf@coords[,2],
       type = "n",
       asp = 1,
       main = "Sites",
       xlab = "",
       cex.axis = 1.2,
       ylab = NA,
       axes=FALSE,
       col.axis="transparent"
       )
  x.axis = as.numeric(axis(1,col="transparent",col.axis="transparent")/1)
  y.axis = as.numeric(axis(2,col="transparent",col.axis="transparent")/1)
  #axis(1, at=x.axis, labels = x.axis/100000)
  #axis(2, at=y.axis, labels = y.axis/100000)
  sp::plot(tac, add=TRUE, col="grey")
  points(sites,
         pch = 21,
         col = "white",
         bg = "red",
         lwd = 0.8,
         cex = 0.7)
  legend(x = "topleft",
         legend = c("Archaeological sites", "TAC"),
         pch = c(20, NA),
         #lty = c(NA, NA),
         col = c("red", "black"),
         fill = c(NA, "grey"),
         border = c(NA,"black"),
        # merge = TRUE,
         bty = "n",
         bg = "transparent")
}
plot_sites()


plot_sites_voronoi <- function(){
plot(vertices_spdf@coords[,1], vertices_spdf@coords[,2],
     type = "n",
     asp = 1,
     main = "Sites and corresponding voronoi diagram",
     xlab = "",
     cex.axis = 1.2,
     axes = F,
     ylab = NA,
     col.axis="transparent"
)
  sp::plot(tac, add=T)
  #x.axis = as.numeric(axis(1,col="transparent",col.axis="transparent")/1)
  #y.axis = as.numeric(axis(2,col="transparent",col.axis="transparent")/1)
  #axis(1, at=x.axis, labels = x.axis/100000)
  #axis(2, at=y.axis, labels = y.axis/100000)
#  sp::plot(tac, add=TRUE, col="grey")
  plot(deldir::deldir(sites@coords[,1], sites@coords[,2],
       rw=c(t(sites@bbox)[1,1],
            t(sites@bbox)[2,1],
            t(sites@bbox)[1,2],
            t(sites@bbox)[2,2])),
       wlines = "tess",
       wpoints = "none",
       number = FALSE,
       lty = 1,
       add = TRUE,
       col = "grey")
  points(vertices_spdf,
         pch = 22,
         col = "white",
         bg = "black",
         lwd = 0.8,
         cex = 0.5)
  points(sites,
         pch = 21,
         col = "white",
         bg = "red",
         lwd = 0.8,
         cex = 0.7)
legend(x = "topleft",
       legend = c("Archaeo-/nlogical sites/n", "Voronoi/nvertices/n", "Voronoi/npolygons/n", "Bounding/nBox"),
       pch = c(19, 15, NA, 22),
       lty = c(NA, NA, 1, NA),
       pt.cex = c(1,1,1,2),
       col = c("red", "black", "grey", "black"),
       merge = TRUE,
       bty = "n",
       bg = "transparent",
       xpd = TRUE,
       cex=0.8,
       inset=c(-0.00,0))
rect(xleft = vertices_spdf@bbox[1],xright = vertices_spdf@bbox[3], ybottom = vertices_spdf@bbox[2], ytop= vertices_spdf@bbox[4], bg="grey")
}
plot_sites_voronoi()



# Variogram / semivariogram ----------------------------------------------------
plot_vario <-
plot(vertices_vario,
     vertices_vario_fit,
     cex.axis = 1.2)
plot_vario


model_values <- gstat::variogramLine(vertices_vario_fit, max(vertices_vario$dist))

ggplot_vario <-
  ggplot2::ggplot(vertices_vario, ggplot2::aes(x=dist, y=gamma)) +
  ggplot2::geom_hline(yintercept = sill.plateau, linetype="dotdash", col="grey") +
  ggplot2::geom_vline(xintercept = range.plateau, linetype="dashed", col="grey") +
  ggplot2::geom_point(colour="dodgerblue3", shape=21, cex=2) +
  ggplot2::geom_line(data = model_values, col="dodgerblue3") + 
  #geom_line(data = model_values, col="red") + 
  ggplot2::labs(x="Distance (m)", y = "Semivariance") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text=ggplot2::element_text(size=13),
        axis.title=ggplot2::element_text(size=14))
ggplot_vario

ggplot_vario + 
  ggplot2::xlim(c(0, range.plateau+(range.plateau/100)*20)) + 
  ggplot2::ylim(c(0, sill.plateau+(sill.plateau/100)*20))



# Kriging results --------------------------------------------------------------
plot_kriging <- function(){
LEC_kriged %>% as.data.frame %>%
  ggplot2::ggplot(ggplot2::aes(x=x, y=y)) +  
  ggplot2::geom_tile(ggplot2::aes(fill=var1.pred))+
  ggplot2::coord_equal() +
  #ggplot2::scale_fill_gradientn(colours = terrain.colors(255)) +
  #ggplot2::scale_fill_gradientn(colours = rainbow(255)) +
  #scale_fill_viridis() +
  #scale_fill_viridis(direction = -1) +
  #scale_fill_viridis(direction = -1, alpha=0.8) +
  #scale_fill_viridis(option = "A") + # magma
  #scale_fill_viridis(option = "A", direction=-1) + # magma
  #scale_fill_viridis(option = "B") + # inferno
  #scale_fill_viridis(option = "B", direction=-1) + # inferno
  #scale_fill_viridis(option = "C") + # plasma
  #ggplot2::scale_fill_gradient(low = "yellow", high="red") +
  ggplot2::labs(x="", y="") +
  viridis::scale_fill_viridis(direction = -1, name="LEC radii") +
  ggplot2::theme_linedraw() +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                  # axis.text.x = ggplot2::element_text(angle = 90),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
 #                  axis.text=ggplot2::element_text(size=13),
#                   axis.title=ggplot2::element_text(size=14)
) +
ggplot2::geom_point(data = as.data.frame(sites),
                    ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                   colour = "white", fill= "black", alpha=1/2)
}
plot_kriging()


# please install package "ggspatial" first:
# install.packages("ggspatial")
r <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                      y = sp::coordinates(LEC_kriged)[,2],
                                      z = LEC_kriged$var1.pred),
                           crs = sp::CRS(tac_epsg))

plot_kriging_map <-
  ggplot2::ggplot() + ggspatial::layer_spatial(r) +
  scale_fill_viridis(direction = -1, name="LEC radii") +
  ggspatial::annotation_spatial(tac, size = 0.5, col = "red", alpha=0) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank()) +
  ggplot2::geom_point(data = as.data.frame(sites),
                      ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                      colour = "white", fill= "black", alpha=1/2) +
  ggplot2::theme_void()

plot_kriging_map
  
# Variance of kriging results --------------------------------------------------
plot_variance <- function(){
LEC_kriged %>% as.data.frame %>%
  ggplot2::ggplot(ggplot2::aes(x=x, y=y)) + 
  ggplot2::geom_tile(ggplot2::aes(fill=var1.var)) + 
  ggplot2::coord_equal() +
  #ggplot2::scale_fill_gradient(low = "yellow", high="red", name = "Variance") +
  #viridis::scale_fill_viridis(option = "C", name="Variance") + # plasma
  viridis::scale_fill_viridis(option = "C", direction=-1, name="Variance") + # plasma (inverted)
  ggplot2::geom_point(data = as.data.frame(sites),
                        ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                        colour = "white", fill= "black", alpha=1/2) +
  ggplot2::labs(x="", y="") +
  #ggplot2::theme_linedraw() +
  ggplot2::theme_void() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank())

}
plot_variance()



# Change units of all data.frames ----------------------------------------------
Isolines_stats[, 1] <- Isolines_stats[, 1] / 1000 # for easy reading


# Plot of descriptive properties of isolines -----------------------------------

# Number of distinct areas per isoline
plot_n_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of areas with a specific site density",
                x = "Isoline [km]",
                y = "Number of distinct areas") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14))

plot_n_area
  
if(merge_polygons == TRUE){
# EXPERIMENTAL:
# Number of distinct areas per isoline MERGED
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Area_merged)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of areas with a specific site density MERGED",
                x = "Isoline [km]",
                y = "Number of distinct areas") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))
}

# Number of sites per isoline
plot_n_sites <- 
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of sites per Isoline",
                x = "Isoline [km]",
                y = "Number of sites") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14))

plot_n_sites

# Percent of sites per isoline
plot_perc_sites <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = percent_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Percentage of sites per Isoline",
                x = "Isoline (km)",
                y = "Sites (%)") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::ylim(0,100) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
               axis.text.x = ggplot2::element_text(angle = 90),
               panel.grid.major = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.text=ggplot2::element_text(size=13),
               axis.title=ggplot2::element_text(size=14))

plot_perc_sites

# Area per isoline
plot_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Enclosed area per isoline",
                x = "Isoline [km]",
                y = expression(paste("Area (km"^"2",")", sep=""))) +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14))

plot_area
  
# Plot of increase of number of sites and area per equidistance ----------------

# Increase of number of sites
plot_incr_sites <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = increase_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Increase of number of sites per equidistance",
                x = "[km]",
                y = "number of sites") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14))

plot_incr_sites

# Increase of area
plot_incr_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = increase_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Increase of area per equidistance",
                x = "Isoline (km)",
                y = expression(paste("Area (km"^"2",")", sep=""))) +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14)) +
  # Draw vertical line to mark the two peaks
  ggplot2::geom_vline(xintercept = c(peak1, peak2), linetype="dotdash", col="grey") +
  # Draw red circle around the two peaks
  ggplot2::geom_point(data = Isolines_stats[Isolines_stats$km_isoline%in%c(peak1, peak2),], pch=21, size=8, fill=NA, colour="red", stroke=0.5)


plot_incr_area

# Plot of difference in increase of number of sites and area per equidistance --

# Difference of increase of number of sites
plot_diff_sites <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = diff_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Difference of increase of number of sites per equidistance",
                x = "[km]",
                y = "Difference") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14))

plot_diff_sites

# Difference increase of area
plot_diff_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = diff_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Difference of increase of area per equidistance",
                x = "[km]",
                y = expression(paste("Area (km"^"2",")", sep=""))) +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text=ggplot2::element_text(size=13),
                 axis.title=ggplot2::element_text(size=14))

plot_diff_area

# Save plots as images ---------------------------------------------------------

# export raster maps as tiffs
tiff("output/01_kriging.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_kriging()
dev.off()

# export raster maps as tiffs
tiff("output/01b_kriging_map.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_kriging_map
dev.off()

tiff("output/02_variance.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_variance()
dev.off()


# export plots to svg
im_width = 7

tiff("output/00_sites.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_sites()
dev.off()

tiff("output/00_sites_voronoi.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_sites_voronoi()
dev.off()

#svg("output/00_sites_voronoi.svg", width=im_width)
#plot_sites_voronoi()
#dev.off()

svg("output/03_vario.svg", width=im_width)
ggplot_vario
dev.off()

svg("output/04_vario_zoom.svg", width=im_width)
ggplot_vario + 
  ggplot2::xlim(c(0, range.plateau+(range.plateau/100)*20)) + 
  ggplot2::ylim(c(0, sill.plateau+(sill.plateau/100)*20))
dev.off()

svg("output/05_n_area.svg", width=im_width)
plot_n_area
dev.off()

svg("output/06_n_sites.svg", width=im_width)
plot_n_sites
dev.off()

svg("output/07_perc_sites.svg", width=im_width)
plot_perc_sites
dev.off()

svg("output/08_area.svg", width=im_width)
plot_area
dev.off()

svg("output/09_incr_sites.svg", width=im_width)
plot_incr_sites
dev.off()

svg("output/10_incr_area.svg", width=im_width)
plot_incr_area
dev.off()

svg("output/11_diff_sites.svg", width=im_width)
plot_diff_sites
dev.off()

svg("output/12_diff_area.svg", width=im_width)
plot_diff_area
dev.off()

# Save data --------------------------------------------------------------------

# Isoline_stats
write.table(Isolines_stats,
            "output/Isolines_stats.csv",
            sep = ";",
            dec = ",",
            row.names = FALSE)

# Polygons of Isolines and raster of Kriging
if(export_raster == TRUE){
  
  # Polygons of isolines as shape file
  rgdal::writeOGR(isoline_polygons,
                  dsn = "output",
                  layer = "isoline_polygons",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/isoline_polygons.prj")
  
  if(merge_polygons == TRUE){
  # Merged Polygons of isolines as shape file
  rgdal::writeOGR(isoline_merged,
                  dsn = "output",
                  layer = "isoline_merged",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/isoline_merged.prj")
  
  # export smoothed Isolines
  
  rgdal::writeOGR(iso_merged_lines_smooth,
  dsn = "output",
  layer = "iso_merged_lines_smooth",
  driver = "ESRI Shapefile",
  check_exists = TRUE,
  overwrite_layer = TRUE)

  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/iso_merged_lines_smooth.prj")
  
  }
  
  # Vornoi diagrams as shape file
  rgdal::writeOGR(voronoi_tiles,
                  dsn = "output",
                  layer = "voronoi_tiles",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/voronoi_tiles.prj")
  
  
  # Vertices as shape file
  rgdal::writeOGR(vertices_spdf,
                  dsn = "output",
                  layer = "vertices",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/vertices.prj")
  
  # Polygons of isolines as shape file
  rgdal::writeOGR(sites,
                  dsn = "output",
                  layer = "sites",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/sites.prj")
  
  # Write raster files as GeoTiff and grd-File for use in GIS-Programms like QGIS
  
  # Kriging-Results
  r <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                        y = sp::coordinates(LEC_kriged)[,2],
                                        z = LEC_kriged$var1.pred),
                             crs = sp::CRS(your_projection))
  
  
  
  raster::writeRaster(r, "output/Kriging_raster.tif", format="GTiff", overwrite=T)
  raster::writeRaster(r, "output/Kriging_raster.grd",format="raster", overwrite=T)
  
  
  # Variance (Quality Measure)
  v <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                        y = sp::coordinates(LEC_kriged)[,2],
                                        z = LEC_kriged$var1.var),
                             crs = sp::CRS(your_projection))
  
  
  
  raster::writeRaster(v, "output/Variance_raster.tif", format="GTiff", overwrite=T, prj=T)
  raster::writeRaster(v, "output/Variance_raster.grd",format="raster", overwrite=T, prj=T)
  
}

