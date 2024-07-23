# see https://github.com/r-spatial/sf/issues/2131

# Known namespaces that need sticky columns. This is set to the
# packages that are already depending on sf-awareness at the time
# the feature was introduced (Jul 23, 2024). 
# New packages wanting to opt-in should use `make_sf_aware()` in their .onLoad().
known_sf_aware <- c(".globalenv", "sf",
					"alcyon", "animalEKF", "arcpullr", "basf", "bcmaps", "bispdep", "canadamaps", "CCAMLRGIS", "cffdrs", "chilemapas", "dggridR", "ecochange", "geotopbricks", "ggmapinset", "habCluster", "hgwrr", "LabourMarketAreas", "M3", "MazamaSpatialUtils", "micromap", "nngeo", "onmaRg", "PointedSDMs", "ppgm", "raptr", "RCzechia", "rgplates", "rKIN", "rLFT", "rts2", "segmetric", "sftime", "siland", "spatialreg", "spdep", "spMaps", "spsurvey", "stars", "starsExtra", "stcos", "surveyvoi", "tilegramsR", "transfR", "waver", "wdnr.gis", "wdpar", "windAC", "abmR", "abstr", "adw", "agricolaeplotr", "alarmdata", "amt", "anipaths", "aopdata", "appeears", "arcgisgeocode", "arcgislayers", "arcgisutils", "areal", "arealDB", "ARPALData", "ARUtools", "ascotraceR", "atakrig", "atpolR", "automap", "bangladesh", "basemaps", "bayesmove", "BayesX", "bcdata", "bcputility", "bdc", "bdl", "bdvis", "BeeBDC", "BFS", "bfsMaps", "BIEN", "bigDM", "BIOMASS", "bioregion", "blackmarbler", "blockCV", "bluebike", "BoundaryStats", "bRacatus", "btb", "camtrapR", "canadianmaps", "capm", "CARBayes", "CARBayesdata", "CARBayesST", "card", "cartograflow", "cartogram", "cartogramR", "cartographer", "cartographr", "cartography", "CAST", "CatastRo", "CDCPLACES", "cdrcR", "CDSE", "censable", "centr", "cft", "changeRangeR", "chessboard", "chirps", "cleangeo", "clhs", "cmsafvis", "cnmap", "CoastlineFD", "comorosmaps", "concaveman", "conleyreg", "constrainedKriging", "CopernicusDEM", "CopernicusMarine", "covid19br", "covid19sf", "covidcast", "crawl", "crimedata", "cropDemand", "CropScapeR", "cropZoning", "crsuggest", "CRTspat", "cshapes", "CSHShydRology", "csodata", "csquares", "ctmm", "cubble", "CvmortalityMult", "cyclestreets", "dafishr", "damAOI", "datazoom.amazonia", "daymetr", "densityarea", "DEPONS2R", "Directional", "disaggregation", "dispeRse", "distanceto", "divseg", "divvy", "dots", "downscale", "dsims", "dsmSearch", "dssd", "dwp", "dynamicSDM", "ebirdst", "echor", "edbuildmapr", "ediblecity", "EEAaq", "eiExpand", "eixport", "eks", "elevatr", "EmissV", "emstreeR", "enmSdmX", "envi", "epikit", "epiR", "epm", "eSDM", "evolMap", "exactextractr", "expowo", "extRatum", "FedData", "feltr", "fgdr", "FIESTA", "FIESTAutils", "fisheye", "fishRman", "fitbitViz", "flexpolyline", "flightplot", "FLightR", "fmesher", "forestecology", "ForestTools", "FORTLS", "fsr", "fude", "galah", "gbm.auto", "gdalUtilities", "geneHapR", "GeNetIt", "GeoAdjust", "geoAr", "geobr", "geocausal", "geocmeans", "geodimension", "geodiv", "geodrawr", "geofi", "geogenr", "geogrid", "geojsonio", "geomander", "GEOmap", "geomaroc", "geomerge", "geomultistar", "geonetwork", "geoperu", "geostan", "geouy", "gfcanalysis", "ggautomap", "ggfields", "ggOceanMaps", "GGoutlieR", "ggseg", "ggspatial", "GIFT", "giscoR", "GISINTEGRATION", "GISSB", "glottospace", "googletraffic", "gps.track", "GPSeqClus", "grainscape", "graph4lg", "GREENeR", "gridpattern", "gstat", "gtfs2emis", "gtfs2gps", "gtfstools", "gwavr", "GWmodel", "GWnnegPCA", "GWpcor", "gwpcormapper", "GWSDAT", "h3jsr", "happign", "HDSpatialScan", "helsinki", "hemispheR", "hereR", "hero", "himach", "hosm", "hwsdr", "hydroloom", "hyfo", "hypsoLoop", "IceSat2R", "icosa", "idbr", "imcRtools", "importinegi", "inlabru", "INLAspacetime", "inldata", "intamap", "intamapInteractive", "ipdw", "IRexamples", "ispdata", "ISRaD", "itsdm", "jmastats", "jpgrid", "jpmesh", "klexdatr", "KMLtoSHAPE", "kokudosuuchi", "LAGOSNE", "lakemorpho", "LandComp", "landsepi", "lazysf", "lconnect", "leafem", "leafgl", "leafpm", "leafpop", "leastcostpath", "letsR", "lgcp", "lidaRtRee", "lidR", "link2GI", "LMMsolver", "LMoFit", "lwgeom", "macleish", "macroBiome", "MainExistingDatasets", "malariaAtlas", "mapboxapi", "mapchina", "mapedit", "MapGAM", "mapi", "mapiso", "mapme.biodiversity", "mapmixture", "mapping", "mapsapi", "mapscanner", "mapsf", "mapSpain", "mapStats", "maptiles", "mapview", "MassWateR", "MazamaSpatialPlots", "medfateland", "meteo", "meteoland", "meteospain", "metR", "MetricGraph", "mgwrhw", "mgwrsar", "micromapST", "MigConnectivity", "misuvi", "mkde", "mlr3spatial", "modisfast", "MODISTools", "Momocs", "monographaR", "Morpho", "motif", "move2", "movecost", "movegroup", "MTA", "naijR", "naturaList", "ncdfgeom", "ndi", "neonPlantEcology", "netmap", "nhdplusTools", "nhdR", "NipponMap", "njgeo", "nlrx", "nominatimlite", "nswgeo", "oceanexplorer", "oceanic", "oceanis", "oceanmap", "odbr", "ofpetrial", "ohsome", "ohun", "openairmaps", "opendatatoronto", "openeo", "opentripplanner", "Orcs", "osmextract", "OSMscale", "osrm", "otpr", "overturemapsr", "ows4R", "ozmaps", "palaeoverse", "PAMscapes", "pargasite", "pastclim", "patternize", "pavo", "pct", "pgirmess", "PL94171", "PlanetNICFI", "planscorer", "plantTracker", "pliman", "plotdap", "plusCode2", "populR", "potential", "povmap", "pRecipe", "PReMiuM", "pressuRe", "prevR", "prioritizr", "prioritizrdata", "prisonbrief", "pseudohouseholds", "pspatreg", "ptools", "PublicWorksFinanceIT", "pycno", "qlcVisualize", "qualmap", "r5r", "raceland", "rangeBuilder", "rangeMapper", "rasterbc", "rasterDT", "rasterpic", "raybevel", "raytracing", "rcarbon", "RchivalTag", "rdhs", "rdwplus", "readwritesqlite", "redist", "redistmetrics", "redistverse", "redlistr", "ref.ICAR", "Relectoral", "remap", "rerddapXtracto", "rfishnet2", "rflexscan", "rgeoboundaries", "rgeoda", "rgeopat2", "rgugik", "ripc", "RiskMap", "riverdist", "rivnet", "rlandfire", "Rlibkdv", "rmapshaper", "rmapzen", "rnaturalearth", "rnrfa", "roads", "robis", "rolap", "rosmium", "roughsf", "rpostgis", "RPyGeo", "Rsagacmd", "rsat", "rsi", "rsocialwatcher", "rSPARCS", "rstac", "RStoolbox", "rTLsDeep", "rtop", "rWCVP", "RWmisc", "sabre", "sampbias", "satres", "SchoolDataIT", "scider", "SDLfilter", "SDPDmod", "secr", "secrdesign", "secrlinear", "seedreg", "sfarrow", "sfcentral", "sfdct", "sfdep", "sfhotspot", "sfislands", "sfnetworks", "sftrack", "sgapi", "sgsR", "shoredate", "simodels", "SimSurvey", "sits", "smartmap", "smile", "SMITIDstruct", "smoothr", "soilassessment", "sorvi", "spacejamr", "SpatFD", "SpatGC", "spatgeom", "SpatGRID", "spatialEco", "SpatialFeatureExperiment", "SpatialGraph", "SpatialKDE", "SpatialPosition", "SpatialRDD", "spatialrisk", "spatialsample", "SpaTopic", "spatsoc", "spatsurv", "spbal", "spectator", "spectralR", "sphet", "spmodel", "spmoran", "spnaf", "spNetwork", "sqlhelper", "SRTsim", "SSDM", "SSIMmap", "SSN2", "SSNbayes", "stampr", "starsTileServer", "stats19", "stelfi", "StormR", "stplanr", "stppSim", "streamDepletr", "streetscape", "sugarbag", "SUMMER", "SUNGEO", "suntools", "supercells", "SurfaceTortoise", "surveyPrev", "swfscDAS", "swfscMisc", "SWMPrExtension", "SWTools", "tanaka", "TDLM", "tectonicr", "tenm", "terrainr", "tidycensus", "tidygeoRSS", "tidyrgee", "tidysdm", "tidyterra", "tidytransit", "tidyUSDA", "tigris", "tilemaps", "tinytiger", "tmap", "tmaptools", "tongfen", "track2KBA", "trackdf", "trackeRapp", "transformr", "treePlotArea", "TreeRingShape", "treesliceR", "trigpoints", "TUFLOWR", "uavRmp", "uci", "ulex", "UnalR", "upstartr", "ursa", "usmapdata", "valhallr", "valuemap", "VancouvR", "vein", "velociraptr", "versioning", "VicmapR", "vietnameseConverter", "viewscape", "voluModel", "Voyager", "walkboutr", "wallace", "waywiser", "weed", "WEGE", "WeightedTreemaps", "WES", "wflo", "wildlifeDI", "wingen", "WorldMapR", "zippeR", "zonebuilder")

# Register environment as sf-aware. We use a lexical flag instead of
# inspecting the the name of the calling namespace and add it to
# `the$sf_aware` because the flag is more flexible. It makes it
# possible for arbitrary environments to opt into (or disable) the sf
# API like this:
#
# ```
# local({ make_sf_aware(); sf[...] })
# ```
#
# Packages should call it like this in their `.onLoad()` hook (and
# thus before their namespace is sealed):
#
# ```
# make_sf_aware(topenv(environment()))
# ```

#' for packages: use the sticky geometry [ behaviour of sf in package code
#'
#' for packages: use the sticky geometry [ behaviour of sf in package code
#' @param env environment
#' @param value logical; default `TRUE`
#' @export
make_sf_aware <- function(env = parent.frame(), value = TRUE) {
	env$.__sf_aware__. <- value
}

is_sf_aware <- function(env = parent.frame(2)) {
	top <- topenv(env)

	# Check for overrides
	top_name <- env_name(top)
	if (!is.null(top_name) && top_name %in% known_sf_aware)
		TRUE
	else {
		if (!requireNamespace("rlang", quietly = TRUE))
			stop("rlang is not installed: install first?")
		# Now check for the lexical flag. This could be rewritten
		# without rlang as a loop over parents that stops at `topenv()`.
		flag <- rlang::env_get(
			env,
			".__sf_aware__.",
			default = FALSE,
			inherit = TRUE,
			last = top
		)
		stopifnot(is.logical(flag) && length(flag) == 1 && !is.na(flag))
		flag
	}
}

env_name <- function(env) {
	ns <- topenv(env)

	if (isNamespace(ns))
		getNamespaceName(ns)
	else if (identical(ns, globalenv()))
		".globalenv"
	else
		NULL
}
