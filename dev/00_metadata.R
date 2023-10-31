# Settings
parent <- "NYIS"
offset <- 24 * 30 * 3
api_key <- Sys.getenv("EIA_API_KEY")
api_path <- "electricity/rto/region-sub-ba-data/data"
api_meta_path <- "electricity/rto/region-sub-ba-data/"
api_meta_parent <- "electricity/rto/region-sub-ba-data/facet/parent"


meta <- EIAapi::eia_metadata(api_path = api_meta_path,
                             api_key = api_key)

meta$startPeriod
meta$endPeriod

meta_parent <- EIAapi::eia_metadata(api_path = api_meta_parent,
                                    api_key = api_key)

meta_parent$facets
meta_parent$totalFacets     
meta_parent$command

nyis_sample <- EIAapi::eia_get(api_key = api_key,
                api_path = api_path,
                facets = list(parent = parent))


head(nyis_sample)
unique(nyis_sample$subba)
unique(nyis_sample$parent)

ny_grid_meta <- nyis_sample |>
    dplyr::select(subba, subba_name = `subba-name`, parent, parent_name = `parent-name`, value_units = `value-units`) |>
    dplyr::distinct()


ny_grid_meta

usethis::use_data(ny_grid_meta, overwrite = TRUE)
