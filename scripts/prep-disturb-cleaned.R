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
    "Utility", "Uknown",            "Utility",    "Trail",            "Other LF",      # typo in WP data
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
    "Mining",         "Layover Area",      "Quartz Mining",  "Clearing"         # as Laydown area
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
  for (cl in setdiff(std_cols, names(x))) x[[cl]] <- NA  # fill missing columns
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
    year_end    = if_else(year_end == 0, this_year, year_end),  # 0 = ongoing
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
st_drop_geometry(poly[poly$fid %in% coffee, c("source", "TYPE_DISTURBANCE")])  # check
poly <- poly |>
  mutate(
    year_start  = if_else(fid %in% coffee, 2007L, year_start),
    year_end    = if_else(fid %in% coffee, 2019L, year_end),
    year_source = if_else(fid %in% coffee, "Meghan Hornseth", year_source)
  )

# Clear Creek Road: built 2015
clear_ck <- fid_at(lin, -137.37961, 63.83136)
st_drop_geometry(lin[lin$fid %in% clear_ck, c("source", "TYPE_DISTURBANCE")])  # check
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