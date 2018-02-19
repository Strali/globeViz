library(data.table)
library(dplyr)
library(geosphere)
library(ggplot2)
library(gridExtra)
library(purrr)
library(mapproj)
library(maps)
library(parallel)
library(raster)
library(sp)

source('mclapply.hack.R')

get_paths <- function(x, idx, ...) {
  gcInt <- function(x, x1, x2) {
    x <- gcIntermediate(x[x1, ], x[x2, ], ...)
    if (is.list(x)) {
      x <- x %>% purrr::map2(c(x1, x1 + 0.5), ~data.frame(.x, .y)) %>% 
        bind_rows %>% setnames(c("long", "lat", "group"))
    } else x <- data.frame(x, x1) %>% setNames(c("long", "lat", "group"))
    x
  }
  purrr::map(setdiff(1:length(x), idx), ~gcInt(x, .x, idx)) %>% bind_rows
}

df_segs <- function(d, seg.size, n.frames, replicates = 1, direction = "fixed") {
  n <- nrow(d)
  if (n < 3) 
    stop("Data not appropriate for this operation.")
  if (seg.size < 3) 
    stop("Segment size too small.")
  z <- round(runif(2, 2, seg.size))
  z[z > n] <- n
  n1 <- ceiling(diff(c((z[1] - z[2]), n))/z[1])
  if (n.frames - n1 < 100) 
    stop("Insufficient frames")
  offset <- sample(0:(n.frames - n1), replicates)
  
  f <- function(k, d, n, n1, z, offset) {
    ind2 <- z[1] * k
    ind1 <- max(ind2 - z[2], 1)
    if (ind2 > n) 
      ind2 <- n
    d <- slice(d, ind1:ind2)
    purrr::map(offset, ~mutate(d, group = ifelse(replicates == 1, group, 
                                                 group + as.numeric(sprintf(".%d", k))), frameID = .x + k)) %>% bind_rows
  }
  
  if (direction == "reverse") 
    d <- mutate(d, long = rev(long), lat = rev(lat))
  if (direction == "random" && rnorm(1) < 0) 
    d <- mutate(d, long = rev(long), lat = rev(lat))
  d <- purrr::map(1:n1, ~f(.x, d, n, n1, z, offset)) %>% bind_rows %>% arrange(group, 
                                                                               frameID)
  d
}

project_to_hemisphere <- function(lat, long, lat0, long0) {
  hold <- cbind(lat, long)
  x <- purrr::map(list(lat, lat0, long - long0), ~.x * pi/180)
  inview <- sin(x[[1]]) * sin(x[[2]]) + cos(x[[1]]) * cos(x[[2]]) * cos(x[[3]]) > 0
  data.table(long = hold[, 2], lat = hold[, 1], inview = inview)
}

save_maps <- function(x, lon_seq, lat_seq, col = NULL, type = "network", z.range = NULL) {
  if (is.null(col))
    col <- switch(type, network = c("#FFFFFF25", "#1E90FF25", "#FFFFFF",
                                    "#1E90FF50"), maptiles = c("black", "steelblue4"), maplines = "white")
  i <- x$frameID[1]
  if (type == "network")
    x.lead <- group_by(x, group) %>% slice(n())
  g <- ggplot(x, aes(long, lat))
  if (type == "maptiles") {
    if (is.null(z.range))
      z.range <- range(x$z, na.rm = TRUE)
    g <- ggplot(x, aes(long, lat, fill = z)) + geom_tile() + scale_fill_gradientn(colors = col,
                                                                                  limits = z.range)
  } else {
    g <- ggplot(x, aes(long, lat, group = group))
    if (type == "maplines")
      g <- g + geom_path(colour = col)
    if (type == "network")
      g <- g + geom_path(colour = col[2]) + geom_path(colour = col[1]) + 
        geom_point(data = x.lead, colour = col[3], size = 0.6) + geom_point(data = x.lead,
                                                                            colour = col[4], size = 0.3)
  }
  g <- g + theme_blank + coord_map("ortho", orientation = c(lat_seq[i], lon_seq[i],
                                                            23.4))
  dir.create(outDir <- file.path("frames", type), recursive = TRUE, showWarnings = FALSE)
  png(sprintf(paste0(outDir, "/", type, "_%03d.png"), i), width = 2*1920,
      height = 2*1080, res = 300, bg = "transparent")
  print(g)
  dev.off()
  NULL
}

eb <- element_blank()
theme_blank <- theme(axis.line = eb, axis.text.x = eb, axis.text.y = eb, axis.ticks = eb, 
                     axis.title.x = eb, axis.title.y = eb, legend.position = "none", panel.background = eb, 
                     panel.border = eb, panel.grid.major = eb, panel.grid.minor = eb, plot.background = element_rect(colour = "transparent",                                                                                                                 
                                                                                                                     fill = "transparent"))
world <- map_data("world")
# Locations = (Gothenburg, Oslo, Stockholm, Tyres�, Dubrovnik, Copenhagen, Londonm, Boom, Prague,
#              Berlin, Budapest, Bratislava, Split, Salzburg, Serre Chevalier, Helsinki, Ume�,
#              �re, S�len, Lund, Visby, Karlstad, Athens, Side, Dubai, Singapore, Darwin, Cairns,
#              Brisbane, Sydney, Canberra, Melbourne, Adelaide, Perth, Alice Springs, Christchurch,
#              Queenstown, Bankok, New York, Atlantic City, Washington DC, Amsterdam, Gran Canaria,
#              Barcelona, Andorra la Vella, Bad Gastein, Trysil, Zagreb, Stenberga)
lats = c(57.71, 59.91, 59.32, 59.23, 42.65, 55.68, 51.51, 51.09, 50.08,
         52.52, 47.50, 48.15, 42.51, 47.81, 44.95, 60.17, 63.83,
         63.40, 61.16, 55.70, 57.63, 59.40, 37.98, 36.78, 25.20, 1.35, -12.46, -16.92,
         -27.47, -33.87, -35.28, -37.81, -34.93, -31.95, -23.70, -43.53,
         -45.03, 13.76, 40.71, 39.36, 38.91, 52.37, 27.92,
         41.39, 42.51, 47.11, 61.32, 45.82, 57.32)
long = c(11.97, 10.75, 18.07, 18.30, 18.09, 12.57, 0.13, 4.37, 14.44,
         13.41, 19.04, 17.11, 16.44, 13.05, 6.56, 24.94, 20.26,
         13.08, 13.26, 13.19, 18.29, 13.51, 23.73, 31.40, 55.27,103.82, 130.85, 145.78,
         153.03, 151.21, 149.13, 144.96, 138.60, 115.86, 133.88, 172.63,
         168.66, 100.50,-74.00, -74.42, -77.04, 4.90, -15.55,
         2.17, 1.52, 13.13, 12.26, 15.98, 15.42)
d = data.frame(lats, long)

set.seed(1)
p <- SpatialPoints(cbind(network$lon, network$lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
idx1 <- 1
paths <- get_paths(p, idx1, addStartEnd = TRUE)

n.frames <- 900
n.period <- 120

paths <- paths %>% split(.$group) %>% purrr::map(~df_segs(.x, 3, n.frames, replicates = 1, 
                                                          direction = "reverse")) %>% bind_rows

lon_seq <- rep(seq(0, 360, length.out = n.period + 1)[-(n.period + 1)], length = n.frames)
lat_seq <- rep(41, length(lon_seq))
paths <- paths %>% split(.$frameID)

d.bath <- read.csv("data/marmap_coord_-180;-90;180;90_res_10.csv") %>% data.table %>% 
  setnames(c("long", "lat", "z"))
r <- raster(extent(-180, 180, -90, 90), res = 1/6)
projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r <- setValues(r, d.bath$z)
d.bath.agg <- r %>% aggregate(2) %>% rasterToPoints %>% data.table %>% setnames(c("long",
                                                                                  "lat", "z"))
d.tiles <- mclapply(1:n.period, function(i, dat, lon, lat)
  {
  left_join(dat, project_to_hemisphere(dat$lat, dat$long, lat[i], lon[i])) %>% filter(inview) %>% 
    dplyr::select(-inview) %>% mutate(frameID = i)
  },
  dat = d.bath.agg, lat = lat_seq, lon = lon_seq)

z.range <- purrr::map(d.tiles, ~range(.x$z, na.rm = TRUE)) %>% unlist %>% range
d.world <- purrr::map(1:n.period, ~mutate(world, frameID = .x))

mclapply(paths, save_maps, lon_seq, lat_seq, type = "network")
mclapply(d.world, save_maps, lon_seq, lat_seq, type = "maplines")
mclapply(d.tiles, save_maps, lon_seq, lat_seq, type = "maptiles", z.range = z.range)
