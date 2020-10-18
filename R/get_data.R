WORKOUT <- "f2b13bc9f24f4fcaa0419df049d47999"
SAVE_DATA <- fs::path_expand("~/peloton_data")


#' Initialize the local data store directory
#'
#' This package will save peloton data locally to the desired location.
#' By default this is ~/peloton_data.
#'
#' This initializes that directory.
#'
#' @param save_to directory to create local storage folders
#'
#' @return NULL
#'
pelo_init_local_data_store <- function(save_to = pelo_local_dir()) {
  fs::dir_create(save_to)
  fs::dir_create(fs::path(save_to, "performance_graph"))
  fs::dir_create(fs::path(save_to, "workout_list"))
  message(glue::glue("Local storage initialized at {save_to}. Run pelo_user_id to get and save your user ID. "))
}

pelo_local_storage_is_initialized <- function() {
  fs::dir_exists(pelo_local_dir())
}

#' Fetch the local directory
#'
#' @return the path to the local data directory
#' @export
#'
pelo_local_dir <- function() {
  getOption("pelo_data_dir", default = fs::path_expand("~/peloton_data"))
}

pelo_set_local_dir <- function(dirname) {
  options(pelo_data_dir = dirname)
}

#' Return a local storage path
#'
#' Performance graphs, workout lists, and your local user ID are stored
#' locally. This function returns the directory where these things are stored.
#'
#' @param dir_type "pg" for performance graphs, "wl" for workout list, and uid
#' for "user_id"
#'
#' @return character with file path
#' @export
#'
#' @examples
#' data_dir("pg")
data_dir <- function(dir_type = c("pg", "wl", "uid")) {
  dir_type <- match.arg(dir_type)
  dir_name_map <- c(
    "pg" = "performance_graph",
    "wl" = "workout_list",
    "uid" = ""
  )


  head_dir <- pelo_local_dir()
  out <- fs::path(head_dir, dir_name_map[dir_type])
  if (!fs::dir_exists(out)) {
    warning("Path does not exist! Run pelo_init_local_data_store to initialize.")
  }

  out
}

#' Check if a performance graph exists locally
#'
#' @param workout_id the ID to check.
#'
#' @return lgl
#'
check_for_local_pg <- function(workout_id) {
  path <- workout_id_to_performance_graph_path(workout_id = workout_id)
  fs::file_exists(path)
}

workout_id_to_performance_graph_path <- function(workout_id) {
  fs::path(data_dir("pg"), workout_id, ext = "rds")
}

pelo_api_endpoints <- function(endpoint = c("pg", "login", "wl"), ...) {
  endpoint <- match.arg(endpoint)
  endpoint_map <- c(
    "pg" = "https://api.onepeloton.com/api/workout/{workout_id}/performance_graph?every_n=1",
    "login" = "https://api.onepeloton.com/auth/login",
    "wl" = "https://api.onepeloton.com/api/user/{user_id}/workouts?limit={n_to_get}"
  )

  glue::glue(endpoint_map[endpoint], .envir = list(...))
}


#' Get performance graph for a workout ID
#'
#' @param workout_id The workout ID to get
#' @param save should this be saved locally?
#'
#' @return performance graph result
#' @export
#'
get_performance_graph <- function(workout_id, save = TRUE) {
  response <- httr::GET(pelo_api_endpoints(endpoint = "pg", workout_id = workout_id))
  out <- content(response)
  if (save) {
    local_path <- fs::path(data_dir("pg"), workout_id = workout_id, ext = "rds")
    readr::write_rds(out, local_path)
  }
  out
}

#' Load the performance graph for a workout ID
#'
#' @param workout_id the workout ID to get the PG for
#' @param save whether to save locally
#'
#' @return a performance graph response
#' @export
#'
pelo_performance_graph <- function(workout_id, save = TRUE) {
  if (check_for_local_pg(workout_id)) {
    return(readr::read_rds(workout_id_to_performance_graph_path(workout_id)))
  } else {
    message(glue::glue("No local data found for workout {workout_id}. Fetching from peloton..."))
    pg <- get_performance_graph(workout_id, save = save)
    return(pg)
  }
}

#' Log in to your peloton account in order to access privileged data, e.g. workout details
#'
#' @return a response
#' @export
#'
pelo_login <- function() {
  uid <- readline("enter Peloton email: ")
  pwd <- rstudioapi::askForSecret("peloton_data", message = c("Enter Peloton account password"), title = "login")
  payload <- jsonlite::toJSON(list(username_or_email = jsonlite::unbox(uid), password = jsonlite::unbox(pwd)))
  response <- httr::POST(pelo_api_endpoints("login"), body = payload)
  stop_for_status(response)
  response
}

#' Get Peloton user_id
#'
#' Note that the user_id is not the same as your login credentials. This will
#' check for a locally stored user ID, and if there's not one, it will fetch
#' the user_id by signing into Peloton.
#'
#' @return the Peloton User ID
#' @export
#' @importFrom readr read_file
#'
pelo_user_id <- function() {
  if (!pelo_local_storage_is_initialized()) {
    stop("Local storage is not initialized. Run `pelo_init_local_data_store()`")
  }
  uid_file <- fs::path(data_dir("uid"), "peloton_user_id", ext = "txt")
  if (fs::file_exists(uid_file)) {
    return(read_file(uid_file))
  } else {
    message("No user_id stored locally. Fetching from peloton...")
    pelo_info <- pelo_login()
    uid <- content(pelo_info)$user_id
    readr::write_file(uid, uid_file)
    return(uid)
  }
}

#' Get a list of all workouts from Peloton
#'
#' @param user_id The user_id
#' @param n_to_get Max number of workouts to get
#'
#' @return tibble with list of all workouts
#' @export
#' @import purrr httr tibble dplyr
#'
pelo_get_workout_list <- function(user_id = pelo_user_id(), n_to_get = 10000) {
  response <- httr::GET(pelo_api_endpoints("wl", user_id = user_id, n_to_get = n_to_get))
  stop_for_status(response, "fetch workout list")
  out <- content(response) %>%
    pluck("data") %>%
    map_depth(2, ~ ifelse(is.null(.x), NA, .x)) %>%
    map_df(as_tibble) %>%
    mutate_at(vars("start_time", "end_time"), ~ lubridate::as_datetime(.)) %>%
    mutate(
      workout_mins = round(.data$end_time - .data$start_time),
      date = lubridate::as_date(lubridate::as_datetime(.data$created_at))
    )
  out
}

#' Get the list of all workouts
#'
#' @param force_refresh force remote fetch rather than local
#' @param user_id user ID
#' @param n_to_get how many workouts to get
#'
#' @return a tibble with workouts
#' @export
#' @importFrom rlang .data
#'
pelo_workout_list <- function(force_refresh = FALSE, user_id = pelo_user_id(), n_to_get = 10000) {
  wl_path <- data_dir("wl")
  wls <- fs::dir_ls(wl_path)
  if (force_refresh | (length(wls) == 0)) {
    wl <- pelo_get_workout_list(user_id = user_id, n_to_get = n_to_get)
    readr::write_rds(wl, fs::path(wl_path, glue::glue('wl-{strftime(Sys.time(), "%s")}.rds')))
  } else {
    file <- wls %>%
      fs::file_info() %>%
      filter(.data$birth_time == max(.data$birth_time)) %>%
      select(.data$path, .data$birth_time)

    message(glue::glue("Most recent workout list as of {file$birth_time}"))
    wl <- readr::read_rds(file$path)
  }
  wl
}

seconds_since_pedaling_start <- function(performance_data) {
  unlist(performance_data$seconds_since_pedaling_start)
}

extract_by_slug <- function(performance_data, slug) {
  slugs <- map(performance_data[["metrics"]], "slug")
  if (!(slug %in% slugs)) {
    return(NA)
  }

  performance_data[["metrics"]] %>%
    purrr::keep(~ .x[["slug"]] == slug) %>%
    .[[1]] %>%
    .[["values"]] %>%
    unlist()
}

#' Update performance graphs
#'
#' @param sleep how long to sleep between API calls
#' @param force_refresh refresh workout list?
#'
#' @return NULL
#' @export
#'
pelo_update_performance_graphs <- function(sleep = 2, force_refresh = FALSE) {
  workout_list <- pelo_workout_list(force_refresh = force_refresh)
  missing_workouts <- !check_for_local_pg(workout_list$id)
  workout_ids <- workout_list$id[missing_workouts]
  if (length(workout_ids) > 0) {
    message(glue::glue("Getting {length(workout_ids)} workouts"))
  }
  for (id in workout_ids) {
    message(glue::glue("Getting workout {id}"))
    get_performance_graph(id)
    Sys.sleep(sleep)
  }
}

workout_tbl <- function(workout_id, metrics = c("output", "cadence", "resistance", "speed", "heart_rate")) {
  pd <- pelo_performance_graph(workout_id)

  metrics_out <- metrics %>%
    map(~ extract_by_slug(pd, .x)) %>%
    set_names(metrics) %>%
    as_tibble()

  metrics_out$seconds_since_pedaling_start <- seconds_since_pedaling_start(pd)
  metrics_out$workout_id <- workout_id

  metrics_out
}
