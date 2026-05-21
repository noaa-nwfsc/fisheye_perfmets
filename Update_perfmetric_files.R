library(here)
# Display list of available date stamp folders for performance metrics
pm_dir <- file.path(
    "G:",
    "Shared drives",
    "NMFS NWC FRAM EDC CE (contains MSA Confidential Data)",
    "FISHEyE",
    "data",
    "PerformanceMetrics"
)
dir(pm_dir)

# Copy files over for Performance Metrics
# Note from 6/25/2024 - I only copied over the CP data file
perfmetricsdir <- file.path(pm_dir, "2026-05-21")
#perfmetricsfiles <- list.files(perfmetricsdir)[grepl('RData', list.files(perfmetricsdir))]

perfmetricsfiles <- list.files(perfmetricsdir)[grepl(
    'perfmetrics.rds',
    list.files(perfmetricsdir)
)]

try(
    if (length(perfmetricsfiles) == 0) {
        stop("There aren't any files to move in that folder")
    }
)

destination <- ifelse(
    grepl('fisheye_perfmets', getwd()),
    "data",
    "Performancemetrics/data"
)

for (pfiles in perfmetricsfiles) {
    file.copy(
        from = file.path(perfmetricsdir, pfiles),
        to = here(destination),
        overwrite = T
    )
}

datvarsfiles <- list.files(perfmetricsdir)[grepl(
    'datvars',
    list.files(perfmetricsdir)
)]

try(
    if (length(datvarsfiles) == 0) {
        stop("There aren't any datvars files to move in that folder")
    }
)

for (dfiles in datvarsfiles) {
    file.copy(
        from = file.path(perfmetricsdir, dfiles),
        to = destination,
        overwrite = T
    )
}
