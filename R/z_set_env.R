## Set up pkg environment to manage variables related to:
# 2. Comtrade database of countries, stored as pkg data.
# 3. Comtrade database of commodity descriptions and codes, stored as pkg data.
cr_env <- new.env()

assign(
  "ua",
  paste(
    Sys.info()[["user"]], R.version$version.str, version$platform, sep = ", "
  ), envir = cr_env
)

# Initialize placeholders for package data within cr_env.
assign('freq', NULL, envir = cr_env)
assign('flow', NULL, envir = cr_env)
assign('product', NULL, envir = cr_env)
assign('indicators', NULL, envir = cr_env)
assign('stat_procedure', NULL, envir = cr_env)
assign('time', NULL, envir = cr_env)
assign('reporter', NULL, envir = cr_env)
assign('partner', NULL, envir = cr_env)
assign('list_of_datasets', NULL, envir = cr_env)
assign('updated', 'init', envir = cr_env)
