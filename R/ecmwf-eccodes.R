#' @include conditions.R http.R
NULL

# Post-implementation audit follow-up (IMPLEMENTER_PROMPT.md item 5's "lateral
# options" discussion) -- a CLI-only, decode-only fallback for the CCSDS/AEC-
# compressed pixel data GDAL's GRIB driver cannot always decode (see
# R/grib-read.R's `.grib_check_ccsds_support()` header note: many GDAL builds,
# including the CRAN macOS binary `terra` bundles, lack libaec support).
#
# eccodes (ECMWF's own GRIB library) decodes every GRIB2 packing template,
# including CCSDS/AEC, reliably and identically across platforms -- but it is
# an external system binary, not an R package, so it cannot be declared as an
# Imports/Suggests dependency the usual way (there is no CRAN mechanism that
# auto-installs a system library for a user). This file provides:
#
#  - a capability probe (`.have_eccodes()`) mirroring `.have_terra()`'s
#    pattern: pure, mockable, no side effects;
#  - an explicit, user-invoked provisioning helper (`ecmwf_install_eccodes()`)
#    that downloads a tiny standalone package manager (`micromamba`) once and
#    uses it to install the plain `eccodes` conda-forge package (the C
#    library + CLI tools only -- no Python) into a self-contained, per-user
#    cache directory. This is never triggered automatically by any fetch --
#    a normal package load or `fetch_forecast()` call never touches the
#    network for this; the user (or a deployment's own setup step) calls it
#    once, and every subsequent call is a no-op (idempotent, and persists
#    across R sessions via `tools::R_user_dir(..., "cache")`);
#  - the actual decode-only extraction seam (`.eccodes_extract_point()`),
#    which shells out to eccodes' `grib_ls -l lat,lon,1` nearest-neighbour
#    query -- a first-class, documented eccodes CLI feature purpose-built for
#    exactly "value nearest this point", not a workaround. This deliberately
#    avoids `grib_get_data`, which dumps every gridpoint in the file (ECMWF's
#    global 0.25 deg grid is ~1.04M points per message) -- wildly wasteful
#    for a single-site point query.
#
# Why CLI, not eccodes' Python bindings (`python-eccodes`): this package
# already treats one heavy optional binary (terra/GDAL) as Suggests-gated;
# adding a second optional *binary* (eccodes CLI) is a smaller footprint than
# also requiring a managed Python interpreter + numpy + python-eccodes, and
# needs no interpreter bridge from R.
#
# Why not brew/apt/choco: three different code paths with three different
# flag/package-naming conventions, each needing a system package manager
# that may not be installed or may need elevated privileges -- and, per real
# experience during this investigation, an *unpinned* system package manager
# is exactly what caused the original problem this fallback exists to route
# around (Homebrew's current `gdal` pulled in a newer GDAL than the CRAN
# binary's, which broke terra/GDAL's band-metadata parsing even after
# fixing the CCSDS decode). `micromamba` + a pinned conda-forge channel gives
# a reproducible, fully self-contained install, identical across macOS,
# Linux, and Windows, that lives in one deletable folder.

# Where the provisioned micromamba binary + eccodes environment are cached.
# `tools::R_user_dir(..., "cache")` is the CRAN-sanctioned per-user cache
# location: persists across sessions, never the package library or a
# `tempdir()`, and is what a user deletes to fully undo `ecmwf_install_eccodes()`.
.eccodes_cache_root <- function() {
  tools::R_user_dir("meteoTidy", which = "cache")
}

.micromamba_bin_path <- function() {
  ext <- if (identical(.Platform$OS.type, "windows")) ".exe" else ""
  file.path(.eccodes_cache_root(), "bin", paste0("micromamba", ext))
}

.eccodes_env_dir <- function() {
  file.path(.eccodes_cache_root(), "eccodes-env")
}

# Candidate paths for a CLI tool inside the provisioned env, matching conda's
# standard per-OS package layout (`bin/` on macOS/Linux, `Library/bin/` on
# Windows -- VERIFIED for macOS via a real `micromamba create -c conda-forge
# eccodes`, 2026-07-06; the Windows path follows conda's documented packaging
# convention for compiled C/C++ tools but was not locally verified, since
# this sandbox has no Windows target to test against).
.eccodes_env_bin <- function(tool) {
  ext <- if (identical(.Platform$OS.type, "windows")) ".exe" else ""
  tool_file <- paste0(tool, ext)
  c(
    file.path(.eccodes_env_dir(), "bin", tool_file),
    file.path(.eccodes_env_dir(), "Library", "bin", tool_file)
  )
}

# Resolve a usable `grib_ls`: the provisioned env first, then whatever's
# already on PATH -- so a user/CI with its own eccodes install (from any
# source: brew, apt, an existing conda env, whatever) is picked up for free,
# no provisioning needed.
.eccodes_grib_ls_path <- function() {
  env_candidates <- .eccodes_env_bin("grib_ls")
  found <- env_candidates[file.exists(env_candidates)]
  if (length(found) > 0) {
    return(found[[1]])
  }
  on_path <- Sys.which("grib_ls")
  if (nzchar(on_path)) {
    return(unname(on_path))
  }
  NA_character_
}

#' Is a usable eccodes install available?
#'
#' Checks the provisioned cache environment first ([ecmwf_install_eccodes()]),
#' then the system `PATH`. A separate, mockable function (rather than inlining
#' the check at call sites) so tests can simulate eccodes' presence or
#' absence without needing it actually installed.
#'
#' @return Single logical.
#' @keywords internal
#' @noRd
.have_eccodes <- function() {
  path <- .eccodes_grib_ls_path()
  if (is.na(path)) {
    return(FALSE)
  }
  status <- tryCatch(
    system2(path, "-V", stdout = FALSE, stderr = FALSE),
    error = function(e) 1L, warning = function(w) 1L
  )
  identical(status, 0L)
}

# Map the running machine to micromamba's release platform tag. Takes
# sysname/machine explicitly (rather than reading Sys.info()/R.version
# internally) so tests can exercise every branch without needing to run on
# every actual platform. VERIFIED (2026-07-06): all five tags below resolve
# to real, current micromamba release assets via
# https://micro.mamba.pm/api/micromamba/<tag>/latest.
.micromamba_platform_tag <- function(sysname = Sys.info()[["sysname"]],
                                     machine = R.version$arch) {
  is_arm <- grepl("arm|aarch64", machine, ignore.case = TRUE)
  switch(sysname,
    Darwin = if (is_arm) "osx-arm64" else "osx-64",
    Linux = if (is_arm) "linux-aarch64" else "linux-64",
    Windows = "win-64",
    abort_meteo(
      c(
        "No known micromamba build for platform {.val {sysname}}/{.val {machine}}.",
        "i" = "Install eccodes yourself (any source) and ensure {.code grib_ls} is on PATH; ecmwf_install_eccodes() only automates the common platforms." # nolint: line_length_linter.
      ),
      class = "eccodes_unsupported_platform"
    )
  )
}

.micromamba_download_url <- function(platform = .micromamba_platform_tag()) {
  sprintf("https://micro.mamba.pm/api/micromamba/%s/latest", platform)
}

# Download (if not already cached) and extract the micromamba binary via the
# package's single HTTP seam (`.http_get()`), so this respects
# `METEOTIDY_NO_NET` and the same mocking seam as every other network call in
# the package -- there is no second, uncontrolled network code path here.
# VERIFIED (2026-07-06): the resolved download is a `.tar.bz2` containing
# `bin/micromamba` (macOS); Windows/Linux follow the same archive layout per
# micromamba's own release packaging (not locally re-verified per-platform,
# since this sandbox can only run macOS/Linux binaries).
.eccodes_download_micromamba <- function() {
  bin_path <- .micromamba_bin_path()
  if (file.exists(bin_path)) {
    return(invisible(bin_path))
  }

  raw <- .http_get(.micromamba_download_url(), parse = "raw")

  archive <- tempfile(fileext = ".tar.bz2")
  on.exit(unlink(archive), add = TRUE)
  writeBin(raw, archive)

  extract_dir <- tempfile("micromamba-extract-")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  utils::untar(archive, exdir = extract_dir)

  found <- list.files(extract_dir, pattern = "^micromamba(\\.exe)?$",
                      recursive = TRUE, full.names = TRUE)
  if (length(found) == 0) {
    abort_meteo(
      "The downloaded micromamba archive did not contain a micromamba executable.", # nolint: line_length_linter.
      class = "eccodes_install_failed"
    )
  }

  dir.create(dirname(bin_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(found[[1]], bin_path, overwrite = TRUE)
  Sys.chmod(bin_path, mode = "0755")
  invisible(bin_path)
}

# Run `micromamba create` to provision the eccodes environment. VERIFIED
# (2026-07-06): this exact command, against a real micromamba binary,
# resolved and installed `eccodes` (+ its real transitive deps: libaec,
# libpng, openjpeg, netcdf, ...) from conda-forge, and the resulting
# `bin/grib_ls` correctly decoded a real CCSDS-compressed ECMWF message.
.eccodes_create_env <- function(micromamba_bin) {
  env_dir <- .eccodes_env_dir()
  status <- system2(
    micromamba_bin,
    c(
      "create", "-y",
      "-r", .eccodes_cache_root(),
      "-p", env_dir,
      "-c", "conda-forge", "eccodes"
    ),
    stdout = TRUE, stderr = TRUE
  )
  exit_status <- attr(status, "status") %||% 0L
  if (!identical(exit_status, 0L)) {
    abort_meteo(
      c(
        "micromamba failed to install eccodes.",
        "x" = paste(utils::tail(status, 5), collapse = "\n")
      ),
      class = "eccodes_install_failed"
    )
  }
  invisible(env_dir)
}

#' Provision a CLI-only eccodes install for reading ECMWF GRIB2
#'
#' [source_ecmwf()] reads ECMWF Open Data GRIB2 through **eccodes** (ECMWF's own
#' library; plan 18), which decodes the CCSDS/AEC-compressed messages ECMWF ships
#' and reports their native `shortName`/`step`/`perturbationNumber`/`units`
#' directly. eccodes is a hard requirement for that adapter but an external
#' system binary, not an R package -- so this function provisions a small,
#' self-contained, CLI-only install (no other part of the package needs it): it
#' downloads `micromamba` (a tiny, dependency-free package manager; downloaded
#' once, cached) and uses it to install the plain `eccodes` conda-forge package
#' (the C library + CLI tools, no Python) into a per-user cache directory
#' (`tools::R_user_dir("meteoTidy", "cache")`). If a usable `grib_ls` is already
#' on `PATH` (e.g. an OS package like `libeccodes-tools`), this is a no-op.
#'
#' This is **never called automatically**: a normal package load or
#' `fetch_forecast()` call never touches the network for this. Call it once
#' (e.g. as part of a deployment's setup step); every subsequent call, and
#' every subsequent R session, reuses the cached install with no further
#' network activity -- until `force = TRUE` is passed, which redoes both the
#' `micromamba` download and the `eccodes` environment from scratch.
#'
#' @param force Logical. If `TRUE`, redo the install even if a usable
#'   `grib_ls` is already cached or on `PATH`. Default `FALSE`.
#' @return Invisibly, `TRUE` if a usable eccodes install is available after
#'   this call (whether newly provisioned or already present).
#' @family adapter
#' @export
#' @examples
#' \dontrun{
#' ecmwf_install_eccodes()
#' }
ecmwf_install_eccodes <- function(force = FALSE) {
  if (!isTRUE(force) && .have_eccodes()) {
    inform_meteo("eccodes is already available; nothing to do (pass `force = TRUE` to reinstall).") # nolint: line_length_linter.
    return(invisible(TRUE))
  }

  # Only checked once we know a network operation is actually about to
  # happen (rather than unconditionally on entry), so a call that turns out
  # to be a no-op never trips the guard.
  if (identical(Sys.getenv("METEOTIDY_NO_NET"), "1")) {
    abort_meteo(
      c(
        "Network access is disabled ({.envvar METEOTIDY_NO_NET} = {.val 1}).",
        "i" = "This guard exists so tests never make a live HTTP request."
      ),
      class = "network_disabled"
    )
  }

  if (isTRUE(force)) {
    unlink(.micromamba_bin_path())
    unlink(.eccodes_env_dir(), recursive = TRUE)
  }

  inform_meteo("Downloading micromamba (once; cached under tools::R_user_dir(\"meteoTidy\", \"cache\"))...") # nolint: line_length_linter.
  micromamba_bin <- .eccodes_download_micromamba()

  inform_meteo("Installing eccodes from conda-forge (this may take a minute)...")
  .eccodes_create_env(micromamba_bin)

  if (!.have_eccodes()) {
    abort_meteo(
      "eccodes was installed but grib_ls could not be found or run afterwards.", # nolint: line_length_linter.
      class = "eccodes_install_failed"
    )
  }
  inform_meteo("eccodes is ready.")
  invisible(TRUE)
}

# GRIB2 unit strings eccodes reports natively (e.g. "K", the ungimmicked
# Kelvin GRIB2 actually stores temperature in -- unlike GDAL's GRIB driver,
# which auto-converts to Celsius on read, see R/grib-read.R). VERIFIED
# (2026-07-06): eccodes reports "K" for 2t against the real committed
# fixture. Wind components' exact reported unit string was not locally
# verified (no CCSDS-compressed u/v fixture is committed) -- "K" is the only
# empirically confirmed entry; anything else falls through with GRIB2's
# "**"-exponent notation normalised to plain udunits syntax (e.g.
# "m s**-1" -> "m s-1"), which `units`/udunits2 accepts.
.eccodes_unit_to_udunits <- function(raw) {
  if (is.na(raw)) {
    return(NA_character_)
  }
  if (identical(raw, "K")) {
    return("K")
  }
  gsub("\\*\\*", "", raw)
}

#' Extract nearest-gridpoint values from every message of a GRIB2 file via eccodes
#'
#' The decode-only fallback for `grib_extract_point()` (R/grib-read.R): shells
#' out to eccodes' `grib_ls -l lat,lon,1` nearest-neighbour query (a
#' first-class eccodes CLI feature, not a workaround), which decodes CCSDS/AEC
#' packing GDAL's GRIB driver may not support. Deliberately avoids
#' `grib_get_data`, which dumps every gridpoint (ECMWF's 0.25 deg global grid
#' is ~1.04M points per message) -- wasteful for a single-site query.
#'
#' @param path Single string, path to a local `.grib2` file.
#' @param lat,lon Single doubles, the site's latitude/longitude in degrees.
#' @return A tibble with one row per GRIB message, in file order: `member`
#'   (integer, `perturbationNumber`, `NA` if not applicable), `value`
#'   (double, in eccodes' natively-decoded unit -- e.g. Kelvin for
#'   temperature, *not* GDAL's auto-converted Celsius), `unit` (character,
#'   the raw GRIB2 unit string eccodes reports; see
#'   `.eccodes_unit_to_udunits()`).
#' @keywords internal
#' @noRd
.eccodes_extract_point <- function(path, lat, lon) {
  grib_ls <- .eccodes_grib_ls_path()
  if (is.na(grib_ls)) {
    abort_meteo(
      "eccodes is not available ({.code grib_ls} not found).",
      class = "eccodes_required"
    )
  }

  args <- c(
    "-j", "-p", "perturbationNumber",
    "-l", sprintf("%.6f,%.6f,1", lat, lon),
    path
  )
  out <- system2(grib_ls, args, stdout = TRUE, stderr = TRUE)
  exit_status <- attr(out, "status") %||% 0L
  if (!identical(exit_status, 0L)) {
    abort_meteo(
      c(
        "eccodes ({.code grib_ls}) failed to decode {.file {path}}.",
        "x" = paste(utils::tail(out, 5), collapse = "\n")
      ),
      class = "eccodes_decode_failed"
    )
  }

  .eccodes_parse_nearest_json(paste(out, collapse = "\n"))
}

# Parse `grib_ls -j -l lat,lon,1`'s JSON output (one array element per GRIB
# message) into a tibble. Split out from `.eccodes_extract_point()` so the
# parsing logic is unit-testable against canned JSON text, without needing a
# real eccodes install.
.eccodes_parse_nearest_json <- function(json_text) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  rows <- lapply(parsed, function(msg) {
    neighbour <- msg$neighbours[[1]]
    member <- suppressWarnings(as.integer(msg$keys$perturbationNumber %||% NA_integer_))
    tibble::tibble(
      member = member,
      value = as.numeric(neighbour$value),
      unit = as.character(neighbour$unit)
    )
  })
  vctrs::vec_rbind(!!!rows)
}
