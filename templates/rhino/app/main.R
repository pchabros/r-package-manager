box::use(
  fst[read_fst],
  sf[st_read],
  shiny.semantic[grid, grid_template, semanticPage],
  shiny[moduleServer, NS],
  waiter[use_waiter],
)
box::use(
  app/logic/utils[loading_screen],
  app/view/dropdowns,
  app/view/info_plot,
  app/view/info_text,
  app/view/map,
  app/view/podium,
  app/view/timeline,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    use_waiter(),
    loading_screen("Citius, Altius, Fortius"),
    title = "Olympics History Map",
    grid(
      grid_template(default = list(
        areas = rbind(
          c("timeline", "info"),
          c("map", "medals"),
          c("map", "dropdowns"),
          c("map", "podium")
        ),
        rows_height = c("30%", "25%", "15%", "30%"),
        cols_width = c("72%", "28%")
      )),
      timeline = timeline$ui(ns("timeline")),
      map = map$ui(ns("map")),
      info = info_text$ui(ns("info_text")),
      medals = info_plot$ui(ns("info_plot")),
      dropdowns = dropdowns$ui(ns("dropdowns")),
      podium = podium$ui(ns("podium"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    game_data <- read_fst("app/data/game_data.fst", as.data.table = TRUE)
    medal_data <- read_fst("app/data/medal_data.fst", as.data.table = TRUE)
    events_data <- read_fst("app/data/events_data.fst", as.data.table = TRUE)
    timeline_data <- read_fst("app/data/timeline_data.fst", as.data.table = TRUE)
    sports_by_year <- read_fst("app/data/sports_by_year.fst", as.data.table = TRUE)
    map_data <- st_read(dsn = "app/data/map_data.shp", quiet = TRUE)
    year <- timeline$server("timeline", timeline_data)
    event_sport <- dropdowns$server("dropdowns", sports_by_year, year)
    event_podium <- podium$server("podium", events_data, year, event_sport)
    map$server("map", map_data, medal_data, event_podium, year)
    info_text$server("info_text", year, game_data)
    info_plot$server("info_plot", year, medal_data)
  })
}
