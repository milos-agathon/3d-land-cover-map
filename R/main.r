# 1. PACKAGES

libs <- c(
    "terra",
    "giscoR",
    "sf",
    "tidyverse",
    "ggtern",
    "elevatr",
    "png",
    "rayshader",
    "magick"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(
    lapply(
        libs, library, character.only = T
    )
)

# 2. COUNTRY BORDERS

country_sf <- giscoR::gisco_get_countries(
    country = "BA",
    resolution = "1"
)

plot(sf::st_geometry(country_sf))

png("bih-borders.png")
plot(sf::st_geometry(country_sf))
dev.off()

# 3 DOWNLOAD ESRI LAND COVER TILES

urls <- c(
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/33T_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/34T_20220101-20230101.tif"
)

for(url in urls){
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

# 4 LOAD TILES

raster_files <- list.files(
    path = getwd(),
    pattern = "20230101.tif$",
    full.names = T
)

crs <- "EPSG:4326"

for(raster in raster_files){
    rasters <- terra::rast(raster)

    country <- country_sf |>
        sf::st_transform(
            crs = terra::crs(
                rasters
            )
        )

    land_cover <- terra::crop(
        rasters,
        terra::vect(
            country
        ),
        snap = "in",
        mask = T
    ) |>
    terra::aggregate(
        fact = 5,
        fun = "modal"
    ) |>
    terra::project(crs)

    terra::writeRaster(
        land_cover,
        paste0(
            raster,
            "_bosnia",
            ".tif"
        )
    )
}

# 5 LOAD VIRTUAL LAYER

r_list <- list.files(
    path = getwd(),
    pattern = "_bosnia",
    full.names = T
)

land_cover_vrt <- terra::vrt(
    r_list,
    "bosnia_land_cover_vrt.vrt",
    overwrite = T
)

# 6 FETCH ORIGINAL COLORS

ras <- terra::rast(
    raster_files[[1]]
)

raster_color_table <- do.call(
    data.frame,
    terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
    r = raster_color_table[,2],
    g = raster_color_table[,3],
    b = raster_color_table[,4]
)

# 7 ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_bosnia <- terra::subst(
    land_cover_vrt,
    from = from,
    to = to,
    names = cols
)

terra::plotRGB(land_cover_bosnia)

# 8 DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 9, clip = "locations"
)

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

land_cover_bosnia_resampled <- terra::resample(
    x = land_cover_bosnia,
    y = terra::rast(elev),
    method = "near"
) |>
terra::project(crs_lambert)

terra::plotRGB(land_cover_bosnia_resampled)

img_file <- "land_cover_bosnia.png"

terra::writeRaster(
    land_cover_bosnia_resampled,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            cols[9]
        )(256)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 12,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .5,
        phi = 85,
        theta = 0
    )

rayshader::render_camera(
    zoom = .58
)

# 10. RENDER OBJECT
#-----------------

filename <- "3d_land_cover_bosnia-dark.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = 1,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w * 1.5,
    height = h * 1.5
)

# 11. PUT EVERYTHING TOGETHER

c(
    "#419bdf", "#397d49", "#7a87c6", 
    "#e49635", "#c4281b", "#a59b8f", 
    "#a8ebff", "#616161", "#e3e2c3"
)

legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "mono")

plot(
    NULL,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    ylab = "",
    xlab = "",
    xlim = 0:1,
    ylim = 0:1,
    xaxs = "i",
    yaxs = "i"
)
legend(
    "center",
    legend = c(
        "Water",
        "Trees",
        "Crops",
        "Built area",
        "Rangeland"
    ),
    pch = 15,
    cex = 2,
    pt.cex = 1,
    bty = "n",
    col = c(cols[c(1:2, 4:5, 9)]),
    fill = c(cols[c(1:2, 4:5, 9)]),
    border = "grey20"
)
dev.off()

# filename <- "land-cover-bih-3d-b.png"

lc_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    legend_name
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
        my_legend, "none"
    ), 2500
)

p <- magick::image_composite(
    magick::image_scale(
        lc_img, "x7000" 
    ),
    my_legend_scaled,
    gravity = "southwest",
    offset = "+100+0"
)

magick::image_write(
    p, "3d_bosnia_land_cover_final.png"
)
