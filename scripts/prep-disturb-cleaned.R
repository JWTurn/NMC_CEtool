# Prepare and clean disturbance data
# Peter Baker
# October 15, 2026

library(arcgislayers)
library(sf)
library(readxl)
library(dplyr)

raw     <- file.path("data", "raw")
derived <- file.path("data", "derived")
crs     <- st_crs(3154)$wkt

###### YUKON ###########

#### Inputs ####
reclass_xlsx <- "C:/Users/peteb/Downloads/NMC_Surface_disturbance_Reclass_2026-10-13 (1).xlsx"
upd_mines    <- "C:/Users/peteb/Downloads/Past_Present_Quartz_Mines-20261015T205046Z-1-001/Past_Present_Quartz_Mines/Past_Present_Quartz_Mines.shp"
wp_dir       <- "C:/Users/peteb/Downloads/sd_data_oct2026_cleaned-20261015T182444Z-1-001/sd_data_oct2026_cleaned"
gdb          <- file.path(wp_dir, "sd_data_oct2026.gdb")

gy_srv    <- "https://mapservices.gov.yk.ca/arcgis/rest/services/GeoYukon/GY_EnvironmentalMonitoring/MapServer/"
pull_date <- format(Sys.Date(), "%Y-%m-%d")
this_year <- as.integer(format(Sys.Date(), "%Y"))

#### Lookup tables ####
# normalize text for matching (case, spacing, en/em dashes)
norm <- function(x) {
  x <- gsub("[\u2013\u2014]", "-", x)
  tolower(gsub("\\s+", " ", trimws(x)))
}

# read one schema version ("v4" or "v5") from the reclass sheet
read_lut <- function(sheet, v) {
  d  <- read_excel(reclass_xlsx, sheet = sheet, .name_repair = "minimal")
  nm <- names(d)
  col <- function(pat) d[[grep(paste0(pat, " ", v, "$"), nm, ignore.case = TRUE)[1]]]
  
  lut <- tibble(
    k_ind       = norm(col("^(Industry|Land use) type")),
    k_dist      = norm(col("^Disturbance type")),
    industry_rc = col("^(Industry|Land use) reclassification")
  )
  if (sheet == "Linear") {
    lut$disturbance_rc <- col("^Disturbance reclassification")
    lut$lf_type        <- col("^Linear feature type")
  } else {
    lut$disturbance_rc <- col("^Linear feature type") 
  }
  lut <- filter(lut, !is.na(k_ind))
  stopifnot(!anyDuplicated(lut[c("k_ind", "k_dist")]))
  lut
}

# add or replace rows for classes missing from the sheet
patch_lut <- function(lut, add) {
  add <- mutate(add, k_ind = norm(k_ind), k_dist = norm(k_dist))
  lut |> anti_join(add, by = c("k_ind", "k_dist")) |> bind_rows(add)
}

luts <- list(
  "lines v4" = read_lut("Linear", "v4") |> patch_lut(tribble(
    ~k_ind,    ~k_dist,             ~industry_rc, ~disturbance_rc,    ~lf_type,
    "Unknown", "Survey / Cutline",  "Unknown",    "Survey / Cutline", "Other LF",
    "Mining",  "Diversion Channel", "Mining",     "Trench",           "Other LF",
    "Utility", "Uknown",            "Utility",    "Trail",            "Other LF",      
    "Utility", "Access Road",       "Utility",    "Unpaved Road",     "Unpaved Road"
  )),
  "polys v4" = read_lut("Areal", "v4") |> patch_lut(tribble(
    ~k_ind,           ~k_dist,             ~industry_rc,     ~disturbance_rc,
    "Mining",         "Mining",            "Mining",         "Clearing",
    "Unknown",        "Unknown",           "Transportation", "Clearing",
    "Oil and Gas",    "Well Pad",          "Transportation", "Clearing",
    "Mining",         "Road Cut and Fill", "Transportation", "Clearing",
    "Oil and Gas",    "Unknown",           "Transportation", "Clearing",
    "Mining",         "Tailing Pond",      "Mining",         "Pond",
    "Transportation", "Pond",              "Transportation", "Pond",
    "Urban",          "Golf Course",       "Urban",          "Recreation Area",
    "Mining",         "Layover Area",      "Quartz Mining",  "Clearing"         
  )),
  "lines v5" = read_lut("Linear", "v5") |> patch_lut(tribble(
    ~k_ind,           ~k_dist,            ~industry_rc,     ~disturbance_rc,    ~lf_type,
    "Transportation", "Survey / cutline", "Transportation", "Survey / Cutline", "Other LF",
    "Utility",        "Unpaved road",     "Utility",        "Unpaved Road",     "Unpaved Road"
  )),
  "polys v5" = read_lut("Areal", "v5") |> patch_lut(tribble(
    ~k_ind,  ~k_dist,       ~industry_rc,     ~disturbance_rc,
    "Mining", "Placer mine", "Placer mining",  "Placer mining",
    "Urban",  "Clearing",    "Transportation", "Clearing"
  ))
)

#### Standardize + reclass (GeoYukon and WPs) ####
std_cols <- c("REF_ID", "DATABASE", "FEATURE_VISIBILITY", "TYPE_INDUSTRY", "TYPE_DISTURBANCE",
              "industry_rc", "disturbance_rc", "lf_type", "image_year", "schema", "source")

prep <- function(x, kind, src) {
  nm <- names(x)
  schema <- if ("TYPE_LANDUSE" %in% nm) "v5" else "v4"
  nm[nm %in% c("TYPE_LANDUSE", "TYPE_INDUS")] <- "TYPE_INDUSTRY"  
  nm[nm == "TYPE_DISTU"] <- "TYPE_DISTURBANCE"
  nm[nm == "FEATURE_VI"] <- "FEATURE_VISIBILITY"
  names(x) <- nm
  st_geometry(x) <- "geometry"
  
  x <- x |>
    st_zm() |>                                                  
    st_transform(crs) |>
    mutate(
      TYPE_INDUSTRY    = coalesce(TYPE_INDUSTRY, "Unknown"),    
      TYPE_DISTURBANCE = coalesce(TYPE_DISTURBANCE, "Unknown"),
      k_ind = norm(TYPE_INDUSTRY), k_dist = norm(TYPE_DISTURBANCE)
    ) |>
    left_join(luts[[paste(kind, schema)]], by = c("k_ind", "k_dist")) |>
    mutate(
      REF_ID     = as.character(REF_ID),
      image_year = as.integer(format(IMAGE_DATE, "%Y")),
      schema     = schema,
      source     = src
    )
  for (cl in setdiff(std_cols, names(x))) x[[cl]] <- NA  
  select(x, all_of(std_cols))
}

#### GeoYukon pull ####
gy_lin  <- arc_select(arc_open(paste0(gy_srv, 29))) |> prep("lines", "GeoYukon")
gy_poly <- arc_select(arc_open(paste0(gy_srv, 30))) |> prep("polys", "GeoYukon") |> st_make_valid()

### Work packages ####
wp_tab <- tibble(layer = st_layers(gdb)$name) |>
  filter(layer != "test_metadata") |>
  mutate(
    type = sub(".*_", "", layer),                       # lines/polys/aoi
    wp   = sub("(_sd)?_(lines|polys|aoi)$", "", layer)  # work package name
  )

read_wp <- function(t) {
  ly <- filter(wp_tab, type == t)
  gtype <- if (t == "polys") 6 else 5  # force MULTIPOLYGON / MULTILINESTRING (linearizes curves)
  do.call(rbind, unname(Map(\(l, w) prep(st_read(gdb, l, type = gtype, quiet = TRUE), t, w), ly$layer, ly$wp)))
}

wp_lin  <- read_wp("lines")
wp_poly <- read_wp("polys") |> st_make_valid()

table(st_geometry_type(wp_lin))   
table(st_geometry_type(wp_poly))  

# Bring in the AOIs
read_aoi <- function(w) {
  l <- wp_tab$layer[wp_tab$wp == w & wp_tab$type == "aoi"]
  st_read(gdb, l, type = 6, quiet = TRUE) |> st_zm() |> st_transform(crs) |> st_make_valid() |> st_union()
}
aois <- st_sf(wp = unique(wp_tab$wp), geometry = do.call(c, lapply(unique(wp_tab$wp), read_aoi)))

#### Replace WP areas from GeoYukon ####

aoi_lines <- st_union(aois[aois$wp %in% wp_tab$wp[wp_tab$type == "lines"], ])
aoi_polys <- st_union(aois[aois$wp %in% wp_tab$wp[wp_tab$type == "polys"], ])

gy_lin  <- st_difference(gy_lin,  aoi_lines)
gy_poly <- st_difference(gy_poly, aoi_polys)

#### Combine ####
lin  <- rbind(gy_lin,  wp_lin)  |> mutate(fid = row_number())
poly <- rbind(gy_poly, wp_poly) |> mutate(fid = row_number())

# checks
unmatched <- function(x) {
  x |>
    st_drop_geometry() |>
    filter(is.na(disturbance_rc) | is.na(industry_rc)) |>
    count(schema, TYPE_INDUSTRY, TYPE_DISTURBANCE, sort = TRUE)
}
unmatched(lin)
unmatched(poly)

table(lin$DATABASE,            lin$schema,  useNA = "ifany")
table(poly$DATABASE,           poly$schema, useNA = "ifany")
table(lin$FEATURE_VISIBILITY,  lin$schema,  useNA = "ifany")
table(poly$FEATURE_VISIBILITY, poly$schema, useNA = "ifany")

#### Manual year updates from Gov YT ####
lin  <- mutate(lin,  year_start = NA_integer_, year_end = NA_integer_, year_source = NA_character_)
poly <- mutate(poly, year_start = NA_integer_, year_end = NA_integer_, year_source = NA_character_)

# Quartz mine production years
mines <- st_read(upd_mines, quiet = TRUE) |> st_transform(crs)

mine_yrs <- st_join(poly["fid"], mines[c("Production", "Producti_1")], left = FALSE) |>
  st_drop_geometry() |>
  distinct(fid, .keep_all = TRUE)

poly <- poly |>
  left_join(mine_yrs, by = "fid") |>
  mutate(
    hit         = !is.na(Production),
    year_start  = if_else(hit, as.integer(Production), year_start),
    year_end    = if_else(hit, as.integer(Producti_1), year_end),
    year_end    = if_else(year_end == 0, this_year, year_end),  
    year_source = if_else(hit, "Meghan Hornseth", year_source)
  ) |>
  select(-Production, -Producti_1, -hit)

# find features at a point
fid_at <- function(x, lon, lat, dist = 2) {
  pt <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), st_crs(x))
  x$fid[st_is_within_distance(x, pt, dist = dist, sparse = FALSE)[, 1]]
}

# Coffee exploration footprint: 2007 to 2019
coffee <- fid_at(poly, -139.34367, 62.88583)
st_drop_geometry(poly[poly$fid %in% coffee, c("source", "TYPE_DISTURBANCE")])  
poly <- poly |>
  mutate(
    year_start  = if_else(fid %in% coffee, 2007L, year_start),
    year_end    = if_else(fid %in% coffee, 2019L, year_end),
    year_source = if_else(fid %in% coffee, "Meghan Hornseth", year_source)
  )

# Clear Creek Road: built 2015
clear_ck <- fid_at(lin, -137.37961, 63.83136)
st_drop_geometry(lin[lin$fid %in% clear_ck, c("source", "TYPE_DISTURBANCE")])  
lin <- lin |>
  mutate(
    year_start  = if_else(fid %in% clear_ck, 2015L, year_start),
    year_source = if_else(fid %in% clear_ck, "Meghan Hornseth", year_source)
  )

table(poly$year_source, useNA = "ifany")

# Analysis year: start year where known, floor at 2012
lin  <- mutate(lin,  year = pmax(coalesce(year_start, 2012L), 2012L))
poly <- mutate(poly, year = pmax(coalesce(year_start, 2012L), 2012L))

#### Feature types ####
lin <- lin |>
  mutate(feature_type = case_when(
    disturbance_rc %in% c("Highway", "Railway", "Paved Road") ~ "paved",
    lf_type == "Unpaved Road"                                ~ "unpaved",
    .default = "otherlf"
  ))

poly <- poly |>
  mutate(feature_type = case_when(
    schema == "v4" & TYPE_DISTURBANCE == "Mining"                                  ~ "othermines",
    schema == "v5" & TYPE_INDUSTRY == "Mining" & norm(TYPE_DISTURBANCE) == "unknown" ~ "othermines",
    tolower(disturbance_rc) == "placer mining" ~ "placer",
    tolower(disturbance_rc) == "quartz mining" ~ "quartz",
    tolower(disturbance_rc) == "harvest"       ~ NA_character_,  # dropped
    .default = "otherpolys"
  )) |>
  filter(!is.na(feature_type))

table(lin$feature_type,  lin$source)
table(poly$feature_type, poly$source)

#### Save ####
out <- file.path(derived, paste0("YT_disturbance_", pull_date, ".gpkg"))
st_write(select(lin,  -fid), out, layer = "linear", delete_layer = TRUE)
st_write(select(poly, -fid), out, layer = "areal",  delete_layer = TRUE)



####### BRITISH COLUMBIA ###########
## Shared reader for all casyls data (BC and NWT)

# year fields, in priority order (first non-NA)
year_cols <- c("RoadYear", "EstYearDist", "ASRC_YEAR", "COMMISSIONED_YEAR",
               "OPERATIONS_EFFECTIVE_DATE", "PROJECT_ESTABLISHED_DATE")
to_year <- function(v) suppressWarnings(as.integer(substr(as.character(v), 1, 4)))  

read_dist <- function(f, kind, ftype = NA_character_) {
  d <- if (kind == "polys") {
    st_read(f, type = 6, quiet = TRUE) |> st_make_valid() 
  } else {
    st_read(f, quiet = TRUE)
  }
  d  <- st_transform(st_zm(d), crs)
  nm <- names(d)
  ind <- grep("^industry$", nm, ignore.case = TRUE, value = TRUE)[1]
  yc  <- unique(c(nm[match(tolower(year_cols), tolower(nm), nomatch = 0)],
                  grep("work_?date_?first", nm, ignore.case = TRUE, value = TRUE)))  
  
  st_sf(
    layer        = sub("\\.gpkg$", "", basename(f)),
    industry     = if (!is.na(ind)) as.character(d[[ind]]) else NA_character_,
    year_raw     = if (length(yc)) do.call(coalesce, lapply(yc, \(cl) to_year(d[[cl]]))) else NA_integer_,
    feature_type = ftype,
    geometry     = st_geometry(d)
  )
}

# no date = present by 2012
set_year <- function(x) mutate(x, year = pmax(coalesce(year_raw, 2012L), 2012L))


##### BRITISH COLUMBIA ####

# Castlys layers (20250501), extracted from FGDB
bc_dir  <- "C:/Users/peteb/Downloads/BC_disturb-20261015T225037Z-1-001/BC_disturb"
bc_file <- function(x) file.path(bc_dir, paste0("20250501_finallayers__", x, ".gpkg"))

# paved = Industry Transportation or Urban/Rural (per metadata)
bc_roads <- read_dist(bc_file("transportation_roads"), "lines") |>
  mutate(feature_type = if_else(industry %in% c("Transportation", "Urban/Rural"), "paved", "unpaved"))

bc_lin <- rbind(
  bc_roads,
  read_dist(bc_file("transportation_rail"),   "lines", "paved"),
  read_dist(bc_file("utilities_corridor"),    "lines", "otherlf"),
  read_dist(bc_file("oilgas_pipelines"),      "lines", "otherlf"),
  read_dist(bc_file("oilgas_seismic"),        "lines", "otherlf"),
  read_dist(bc_file("transportation_trails"), "lines", "otherlf"),
  read_dist(bc_file("urbanrural_dams"),       "lines", "otherlf")
) |> set_year()

bc_poly <- rbind(
  read_dist(bc_file("mining_mining"),             "polys", "othermines"),
  read_dist(bc_file("placermining_placermining"), "polys", "placer"),
  read_dist(bc_file("transportation_sandgravel"), "polys", "othermines"),
  read_dist(bc_file("agriculture_agriculture"),   "polys", "otherpoly"),
  read_dist(bc_file("oilgas_building"),           "polys", "otherpoly"),
  read_dist(bc_file("oilgas_wellsites"),          "polys", "otherpoly"),
  read_dist(bc_file("transportation_airstrip"),   "polys", "otherpoly"),
  read_dist(bc_file("urbanrural_buildings"),      "polys", "otherpoly"),
  read_dist(bc_file("urbanrural_landfills"),      "polys", "otherpoly"),
  read_dist(bc_file("urbanrural_recreation"),     "polys", "otherpoly"),
  read_dist(bc_file("urbanrural_residential"),    "polys", "otherpoly")
) |> set_year()

# checks: which layers have dates, and geometry types
table(bc_lin$layer,  is.na(bc_lin$year_raw))
table(bc_poly$layer, is.na(bc_poly$year_raw))
table(st_geometry_type(bc_lin))

## NWT ====

nt_dir <- "C:/Users/peteb/Downloads/NWT_disturb-20261015T230636Z-1-001/NWT_disturb"
nt_file <- function(x) file.path(nt_dir, paste0(x, ".gpkg"))

nt_lin <- rbind(
  read_dist(nt_file("transportation_roads"),  "lines", "paved"),  # all NWT roads paved
  read_dist(nt_file("oilgas_piplines"),       "lines", "otherlf"),  # filename typo is in source
  read_dist(nt_file("oilgas_seismic"),        "lines", "otherlf"),
  read_dist(nt_file("transportation_trails"), "lines", "otherlf")
) |> set_year()

nt_poly <- rbind(
  read_dist(nt_file("mining_mining"),           "polys", "othermines"),
  read_dist(nt_file("mining_buildings"),        "polys", "otherpoly"),
  read_dist(nt_file("mining_clearings"),        "polys", "otherpoly"),
  read_dist(nt_file("oilgas_wellsites"),        "polys", "otherpoly"),
  read_dist(nt_file("transportation_airstrip"), "polys", "otherpoly"),
  read_dist(nt_file("urbanrural_buildings"),    "polys", "otherpoly"),
  read_dist(nt_file("urbanrural_recreation"),   "polys", "otherpoly")
) |> set_year()

table(nt_lin$layer,  is.na(nt_lin$year_raw))
table(nt_poly$layer, is.na(nt_poly$year_raw))

## Combine YT, BC, NWT ====

std <- function(x, region) {
  x |>
    mutate(region = region, source = if ("source" %in% names(x)) source else region) |>
    select(region, source, feature_type, year)
}

all_lin <- rbind(std(lin, "YT"), std(bc_lin, "BC"), std(nt_lin, "NWT")) |>
  st_collection_extract("LINESTRING")  # drop stray points from erase

all_poly <- rbind(std(poly, "YT"), std(bc_poly, "BC"), std(nt_poly, "NWT")) |>
  mutate(feature_type = if_else(feature_type == "otherpolys", "otherpoly", feature_type)) |>  # YT naming
  st_collection_extract("POLYGON")

table(all_lin$feature_type,  all_lin$region)
table(all_poly$feature_type, all_poly$region)
table(all_lin$year,  all_lin$region)
table(all_poly$year, all_poly$region)

### Save ----

st_write(all_lin,  file.path(derived, "NMC_linear_combined.gpkg"), delete_dsn = TRUE)
st_write(all_poly, file.path(derived, "NMC_areal_combined.gpkg"),  delete_dsn = TRUE)


### Save shapefiles by feature type ----
shp_dir <- file.path(derived, "feature_types")
dir.create(shp_dir, showWarnings = FALSE)

write_ftype <- function(x, cast_to) {
  x <- st_cast(x, cast_to)  # shapefiles need one geometry type
  for (ft in unique(x$feature_type)) {
    st_write(
      rename(x[x$feature_type == ft, ], ftype = feature_type),  # avoid 10-char name truncation
      file.path(shp_dir, paste0(ft, ".shp")),
      delete_dsn = TRUE, quiet = TRUE
    )
  }
}

write_ftype(all_lin,  "MULTILINESTRING")
write_ftype(all_poly, "MULTIPOLYGON")

list.files(shp_dir, pattern = "\\.shp$")

