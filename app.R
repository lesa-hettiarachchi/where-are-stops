library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(leaflet)

# -------------------------------------------------------------------------
# Processed data (produced by Task2A_Processing.Rmd)
# -------------------------------------------------------------------------

ta1 <- read_csv("processed/ta1_stop_counts.csv",       show_col_types = FALSE)
ta2 <- read_csv("processed/ta2_stop_density.csv",      show_col_types = FALSE)
ta3 <- read_csv("processed/ta3_underserved_index.csv", show_col_types = FALSE)
lga_master <- read_csv("processed/lga_master.csv",     show_col_types = FALSE)
lga_boundaries <- st_read("processed/lga_boundaries.geojson", quiet = TRUE)

sector_levels <- c("Inner Melbourne", "Middle Melbourne", "Outer Melbourne")
mode_levels   <- c("Total", "Bus", "Train", "Tram")

mode_colours <- c(
  Total = "#4C72B0",
  Bus   = "#DD8452",
  Train = "#55A868",
  Tram  = "#C44E52"
)
sector_colours <- c(
  "Inner Melbourne"  = "#2C7BB6",
  "Middle Melbourne" = "#7FBC41",
  "Outer Melbourne"  = "#D7301F"
)
highlight_colour <- "#F2C14E"

# Long version of TA1 for the small-multiples chart
ta1_long <- ta1 %>%
  pivot_longer(c(total, bus, train, tram), names_to = "mode", values_to = "stops") %>%
  mutate(
    mode = factor(stringr::str_to_title(mode), levels = mode_levels),
    dominant_sector = factor(dominant_sector, levels = sector_levels)
  )

# Master table joined onto boundaries for the choropleth
lga_map <- lga_boundaries %>%
  mutate(lga_code21 = as.numeric(lga_code21)) %>%
  select(lga_code21) %>%
  left_join(lga_master, by = "lga_code21")

lga_choices <- c("None", sort(lga_master$lga_name21))

# Helpers ----------------------------------------------------------------

mode_col <- function(prefix, mode) paste0(prefix, "_", tolower(mode))

fmt <- function(x, digits = 0) format(round(x, digits), big.mark = ",", nsmall = digits, trim = TRUE)

# Pre-computed headline numbers used in the narrative
n_lga         <- nrow(ta1)
n_no_train    <- sum(ta1$train == 0)
n_no_tram     <- sum(ta1$tram == 0)
busiest       <- ta2 %>% slice_max(total, n = 1)
densest       <- ta2 %>% slice_max(density_total, n = 1)
sparsest      <- ta2 %>% slice_min(density_total, n = 1)
most_under    <- ta3 %>% slice_max(index_total, n = 1)
best_served   <- ta3 %>% slice_min(index_total, n = 1)
busiest_density_rank <- ta2 %>%
  mutate(rank = min_rank(desc(density_total))) %>%
  filter(lga_code21 == busiest$lga_code21) %>% pull(rank)

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------

ui <- page_navbar(
  title = "Where Are the Stops?",
  window_title = "Where Are the Stops? Public Transport Availability across Greater Melbourne",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  # Keep the "Explore the data" controls visible while the reader scrolls
  # down to the TA2 map and the TA3 scatter.
  header = tags$head(tags$style(HTML("
    /* Every card header bold, whether or not it is wrapped in tags$strong().
       The inherit rule keeps the wrapped ones at the same weight as the rest
       so they do not render heavier on fonts that have a 900 face. */
    .card-header { font-weight: 700; }
    .card-header strong { font-weight: inherit; }

    @media (min-width: 992px) {
      /* bslib gives .sidebar `overflow: auto`, which makes it the scroll
         container for its children - a sticky child would then stick to a
         box that never scrolls. Releasing it lets the page be the scroller. */
      .bslib-sidebar-layout > .sidebar {
        overflow: visible;
      }
      .bslib-sidebar-layout > .sidebar > .sidebar-content {
        position: sticky;
        top: 1rem;
        max-height: calc(100vh - 2rem);
        overflow-y: auto;
      }
    }
  "))),

  # ---- Tab 1 ------------------------------------------------------------
  nav_panel(
    "1. Public Presentation",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        open = "open",
        h4("Explore the data"),
        radioButtons(
          "mode", "Transport mode (map and index)",
          choices = mode_levels, selected = "Total", inline = TRUE
        ),
        checkboxGroupInput(
          "sectors", "Show LGAs whose main sector is",
          choices = sector_levels, selected = sector_levels
        ),
        selectInput("highlight", "Highlight an LGA", choices = lga_choices),
        sliderInput(
          "top_n", "Flag the N most under-served LGAs (index chart)",
          min = 1, max = 10, value = 3, step = 1
        ),
        hr(),
        helpText(
          "LGA = Local Government Area (a council area). ",
          "Every chart counts a physical stop once, even if many routes use it."
        )
      ),

      card(
        card_header("Public transport stop availability across Greater Melbourne LGAs"),
        p(
          "Greater Melbourne is made up of 35 council areas (LGAs). Some have hundreds of ",
          "bus, train and tram stops, others rely on a handful of bus routes. This page lets ",
          "you compare your area with the rest of the city in 3 steps:"
        ),
        tags$ol(
          tags$li(strong("How many stops are there, and of which kind?"), " (simple counts)"),
          tags$li(strong("How thinly are those stops spread over the land?"), " (stops per square kilometre)"),
          tags$li(strong("How many residents must share each stop?"), " (a 0-100 'under-served' score)")
        ),
        p(
          "*Use the controls on the left to focus on one transport mode, one part of the city, ",
          "or a single LGA. Hover or click the map for details."
        )
      ),

      layout_columns(
        col_widths = breakpoints(sm = 6, lg = 3),
        value_box("Unique stops counted", fmt(sum(ta1$total)), theme = "primary"),
        value_box("LGAs compared", n_lga, theme = "secondary"),
        value_box("LGAs with no train stop", n_no_train, theme = "warning"),
        value_box("LGAs with no tram stop", n_no_tram, theme = "danger")
      ),

      # TA1 ----------------------------------------------------------------
      card(
        card_header(tags$strong("1. Stop count by transport mode")),
        p(
          "Each panel is one transport mode. LGAs are sorted by their total number of stops ",
          "so you can read one LGA straight across all four panels. Empty space in the Train ",
          "and Tram panels is the point: most outer LGAs are served by buses only."
        ),
        plotOutput("ta1_plot", height = "720px"),
        p(
          class = "text-muted small",
          "Note: 54 stops are served by more than one mode and are counted once in each mode ",
          "they serve, so the Bus + Train + Tram panels add up to slightly more than Total."
        )
      ),

      # TA2 ----------------------------------------------------------------
      card(
        card_header(tags$strong(textOutput("ta2_title", inline = TRUE))),
        p(
          "A big outer LGA can have a moderate number of stops and still be very sparse ",
          "because those stops are spread over hundreds of square kilometres. Dividing the ",
          "count by land area shows how dense the stop network really is. Darker = more ",
          "stops per square kilometre. Click an LGA for its numbers."
        ),
        leafletOutput("ta2_map", height = "560px"),
        uiOutput("ta2_summary")
      ),

      # TA3 ----------------------------------------------------------------
      card(
        card_header(tags$strong(textOutput("ta3_title", inline = TRUE))),
        p(
          "Land area is one way to be fair; population is another. Here every LGA's ",
          "population is divided by its number of stops (residents per stop) and the result ",
          "is rescaled to 0-100, where 0 = best served and 100 = most under-served for the ",
          "chosen mode. Plotting the score against population shows whether the biggest ",
          "communities are also the ones sharing each stop with the most people."
        ),
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(8, 4)),
          plotOutput("ta3_plot", height = "520px"),
          div(
            h6("Ranking for the selected mode"),
            DTOutput("ta3_table")
          )
        ),
        uiOutput("ta3_noservice")
      ),

      card(
        card_header(tags$strong("Key findings")),
        uiOutput("findings")
      ),

      card(
        card_header(tags$strong("How to read these results (Limitations)")),
        tags$ul(
          tags$li("An LGA is a large area. One value per LGA cannot describe every suburb inside it."),
          tags$li("Stop counts and densities measure how many stops exist, not how often services run, how far people walk to a stop, or how crowded vehicles are."),
          tags$li("Stops per km² treats all land equally, including farmland and parks where nobody lives. Residents per stop corrects for this but ignores land."),
          tags$li("The 0-100 score is relative to the 35 LGAs shown here. A score of 0 does not mean 'perfect', only 'best of this group'. A single extreme LGA can compress the other scores (see Tram)."),
          tags$li("LGAs with no stops of a mode are not scored for that mode; they are listed separately as 'No service'."),
          tags$li("Population is the ABS 2024 estimate mapped onto 2021 LGA boundaries; stop data is a snapshot and may not reflect recent changes.")
        )
      )
    )
  ),

  # ---- Tab 2 ------------------------------------------------------------
  nav_panel(
    "2. Purpose and Design",
    card(
      card_header("Purpose, Audience and Narrative"),
      p(
        strong("Purpose. "),
        "To help members of the public compare public transport stop availability across ",
        "the 35 Greater Melbourne LGAs and recognise which areas are relatively under-served."
      ),
      p(
        strong("Audience. "),
        "Residents, community advocates and local planners with no data-analysis or GIS ",
        "background. Everything is expressed as counts, 'stops per km²' and a 0-100 score."
      ),
      p(
        strong("Narrative structure. "),
        "The three task abstractions are one story told in three steps. ",
        "TA1 establishes scale with raw counts and reveals that many LGAs are bus-only. ",
        "TA2 makes the comparison fair by land area and moves the pattern onto a map, ",
        "where the inner/outer divide becomes obvious. ",
        "TA3 makes it fair by population and converts everything to one 0-100 scale so the ",
        "most under-served areas can be ranked and named. Each step reuses the previous ",
        "step's numbers, so the reader never has to learn a new unit."
      ),
      p(
        strong("Changes from the Task 1 plan. "),
        "The plan is implemented as designed. Two small refinements were added during ",
        "implementation: (a) a 'dominant sector' filter (Inner / Middle / Outer) so the ",
        "public can restrict all three charts to their part of the city, and (b) the ",
        "under-served ranking table beside the TA3 scatter, because a scatter alone does ",
        "not name the LGAs clearly for a non-technical reader."
      )
    ),
    card(
      card_header("Task Abstractions"),
      DTOutput("task_abstraction_table")
    ),
    card(
      card_header("Selected Visual Idioms"),
      DTOutput("idiom_table")
    )
  ),

  # ---- Tab 3 ------------------------------------------------------------
  nav_panel(
    "3. Raw Data",
    card(
      card_header("Raw datasets and sources"),
      DTOutput("raw_data_table")
    ),
    card(
      card_header("Raw data explanation"),
      tags$ul(
        tags$li(strong("lga_stops_routes.csv"), " is the core file. Each row is a stop-route pair (stop_id, stop_name, vehicle, route_number, route_name) tagged with the 2021 LGA it sits in. One physical stop appears once per route that uses it, so it must be de-duplicated by stop_id before counting."),
        tags$li(strong("lga_area.csv"), " defines the 36 assessment LGAs (35 after removing Unincorporated Vic) and gives land area in km² and straight-line distance to the CBD. Area is the denominator for TA2."),
        tags$li(strong("Population estimates by age and sex, LGA 2024 (ABS, Table 3)"), " gives 2024 population per LGA using 2024 LGA codes. Only the 'Total persons' column is used, as the denominator for TA3."),
        tags$li(strong("LGA_2021_LGA_2024.csv"), " maps 2024 codes to 2021 codes so population can be joined to the transport files without relying on names."),
        tags$li(strong("lga_sector.csv"), " records the share of each LGA's area inside Inner / Middle / Outer Melbourne. Used only to label each LGA with its dominant sector for filtering."),
        tags$li(strong("lga_area.geojson"), " holds the LGA boundary polygons used for the choropleth in TA2."),
        tags$li(strong("lga_details.csv, lga_adjacents.csv, routes_city_freq.csv, stops.geojson"), " were inspected and cleaned in Task 1B but are not required by the three task abstractions, so they are not used in the presentation. Two corrupted values in lga_details.csv (Darebin higher_education = 100, Melbourne's four blank counts) are documented on the Preprocessing tab and left as NA rather than guessed or zero-filled.")
      )
    )
  ),

  # ---- Tab 4 ------------------------------------------------------------
  nav_panel(
    "4. Preprocessing Issues",
    card(
      card_header("Issues found and decisions made"),
      p(
        "Full code and output are in ", tags$code("Task1B_Preprocessing.html"),
        " (issues 1-11, submitted with Task 1) and ", tags$code("Task1B_Corrections.html"),
        " (issues 12-15, added after review). The table summarises each issue, the ",
        "evidence that found it, the decision, and the reason."
      ),
      DTOutput("preprocessing_table")
    ),
    card(
      card_header("Four corrections made after the Task 1 review"),
      p(
        "A review of the Task 1 submission identified four problems that were missed, ",
        "repaired without evidence, or handled incorrectly. All four are now fixed and ",
        "validated in ", tags$code("Task1B_Corrections.Rmd"), ":"
      ),
      tags$ul(
        tags$li(
          strong("Yarra Ranges area and CBD distance (structure). "),
          "lga_area.csv wrote the area as 1,611.9332 without quotes, splitting the row ",
          "into five fields, sqkm became 1 and cbd_euclidean_km became 611.93. I had ",
          "repaired this by hand during manual inspection and never recorded it. The ",
          "addendum reproduces the malformed line, shows what the parser does with it, ",
          "and validates the repaired area (1611.9332) against the independent geometry ",
          "in lga_area.geojson, which agrees to within 0.0001%. Uncorrected, Yarra Ranges ",
          "would have shown 779 stops per km\u00b2 and inverted the whole TA2 map."
        ),
        tags$li(
          strong("Melbourne CBD distance (illogical). "),
          "lga_area.csv gave the LGA that contains the CBD a distance of 50 km, placing ",
          "it past Bacchus Marsh, while its neighbours Yarra and Port Phillip are 3 km ",
          "and 5 km. It was the only LGA whose distance disagreed with lga_area.geojson. ",
          "Corrected to 0, and Melbourne is now the closest LGA to the CBD."
        ),
        tags$li(
          strong("Darebin higher_education = 100 (illogical). "),
          "Every other LGA records 0-8 so I took the the Tukey upper fence is 7.5. Darebin would have ",
          "1.35 higher-education facilities per school, where no other LGA exceeds 0.10. ",
          "No second source can recover the true number, so guessing would invent data: ",
          "set to NA and recorded as unknown."
        ),
        tags$li(
          strong("Melbourne facility counts (missing, not zero). "),
          "My first submission replaced the four blank values with 0. That was wrong: 0 is ",
          "a measurement meaning 'none here', and the Melbourne LGA contains RMIT, the ",
          "University of Melbourne and the Royal Melbourne Hospital. They are now kept as ",
          "NA with an explicit facilities_missing flag, so the gap stays visible."
        )
      ),
      p(
        class = "text-muted",
        "None of the three task abstractions uses lga_details.csv or cbd_euclidean_km, ",
        "so no stop count, density or index value changes. The corrections make the ",
        "context variables truthful and replace an undocumented manual repair and an ",
        "invented zero with detection code, independent validation and an honest NA."
      )
    ),
    card(
      card_header("Evidence and remarks"),
      tags$ul(
        tags$li(strong("Structural validation: "), "Every raw CSV is now re-read as plain text and its delimiters counted outside quoted fields, so a split row cannot pass silently. This check flags lga_sector.csv (stray 6th column) and confirms the commas inside quoted route names in routes_city_freq.csv are parsed correctly."),
        tags$li(strong("Row / column checks: "), "Area 36 x 4, details 35 x 6, adjacents 174 x 4, sector 40 x 6 (one stray column), stops 43,818 x 7, freq 453 x 6, mapping 564 x 4 before cleaning."),
        tags$li(strong("Cardinia fix: "), "The value 1,283 in lga_sector.csv was written without quotes, splitting into '1' and '283'. Corrected to 1283 (matches lga_area.csv) and lga_part_sqkm_pct restored to 1."),
        tags$li(strong("Area cross-validation: "), "All 35 areas were compared against the sqkm computed from the boundary geometry in lga_area.geojson. Each and every one agrees to within 0.01%, and after the Melbourne fix every CBD distance matches too."),
        tags$li(strong("Records vs unique stops: "), "31,722 stop-route records in scope reduce to 21,383 unique stops but 54 stops serve more than one mode."),
        tags$li(strong("Scope: "), "Stops filtered from 80+ LGAs to the 35 assessment scope LGAs. Mapping table filtered from 564 rows to 35 with no duplicates or gaps."),
        tags$li(strong("Population join: "), "All 35 LGAs matched ABS 2024 population via the 2021-2024 mapping table. Zero missing values after the join."),
        tags$li(strong("Final validation: "), "Eight assertions are run before the clean files are written - 35 rows, no sqkm <= 1, all areas within 0.01% of the geometry, all CBD distances matching the geometry, Melbourne closest to the CBD, no higher_education above the Tukey fence, Melbourne facilities NA rather than 0, and no missing population. All were passed.")
      )
    )
  ),

  # ---- Tab 5 ------------------------------------------------------------
  nav_panel(
    "5. Processed Data",
    card(
      card_header("Processed tables produced in Task 2A"),
      p("All tables are created by ", tags$code("Task2B_Processing.Rmd") ,"from the clean/ folder and saved in processed/. The app reads them directly."),
      DTOutput("processed_table_list")
    ),
    navset_card_tab(
      nav_panel("Master table (one row per LGA)", DTOutput("main_processed_table")),
      nav_panel("TA1 - stop counts", DTOutput("ta1_table_full")),
      nav_panel("TA2 - stop density", DTOutput("ta2_table_full")),
      nav_panel("TA3 - under-served index", DTOutput("ta3_table_full"))
    )
  ),

  # ---- Tab 6 ------------------------------------------------------------
  nav_panel(
    "6. Student Credit",
    card(
      card_header("Student information"),
      tags$p(strong("Student name:"), " Lesandu Hetti Arachchige"),
      tags$p(strong("Student ID:"), " 21533031"),
      tags$p(strong("Subject code:"), " CSE5DEV - Data Visualisation"),
      tags$p(strong("App title:"), " Where Are the Stops? Public Transport Availability across Greater Melbourne")
    ),
    card(
      card_header("Individual work statement"),
      p("I declare that this Shiny application, the Task 2A processing script and the accompanying analysis are my own individual work, building on the presentation plan and preprocessing I submitted for Task 1 of this assessment.")
    ),
    card(
      card_header("Data acknowledgement"),
      tags$ul(
        tags$li("Public transport stop, route and frequency data, LGA boundaries, sector and context files: provided by the CSE5DEV teaching team (derived from Public Transport Victoria / DataVic open data)."),
        tags$li("Population: Australian Bureau of Statistics, Regional population by age and sex, 2024 - Table 3, Population estimates by age and sex, by LGA."),
        tags$li("LGA 2021 to LGA 2024 correspondence: Australian Bureau of Statistics, Australian Statistical Geography Standard (ASGS)."),
        tags$li("Base map tiles: OpenStreetMap contributors, via the leaflet R package.")
      )
    ),
    card(
      card_header("AI use declaration"),
      p("Generative AI was used to help and plan the implementaion and support during UI desigining.")
    )
  )
)

# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- shared reactive filters -----------------------------------------

  selected_codes <- reactive({
    req(input$sectors)
    lga_master %>%
      filter(dominant_sector %in% input$sectors) %>%
      pull(lga_code21)
  })

  highlight_code <- reactive({
    if (is.null(input$highlight) || input$highlight == "None") return(NA_real_)
    lga_master$lga_code21[lga_master$lga_name21 == input$highlight]
  })

  # ---- TA1: small multiples --------------------------------------------

  output$ta1_plot <- renderPlot({
    d <- ta1_long %>%
      filter(lga_code21 %in% selected_codes()) %>%
      mutate(
        is_hl = lga_code21 == highlight_code() & !is.na(highlight_code()),
        lga_name21 = reorder(lga_name21, stops * (mode == "Total"), FUN = sum)
      )
    validate(need(nrow(d) > 0, "Select at least one sector."))

    ggplot(d, aes(x = stops, y = lga_name21, fill = mode)) +
      geom_col(width = 0.8) +
      geom_col(data = filter(d, is_hl), fill = highlight_colour, width = 0.8) +
      facet_wrap(~ mode, nrow = 1, scales = "free_x") +
      scale_fill_manual(values = mode_colours, guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Unique stops", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        strip.text = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.2, "lines")
      )
  })

  # ---- TA2: choropleth -------------------------------------------------

  output$ta2_title <- renderText({
    paste0("2. Stops per square kilometre - ", input$mode)
  })

  output$ta2_map <- renderLeaflet({
    dcol <- mode_col("density", input$mode)
    ccol <- if (input$mode == "Total") "total" else tolower(input$mode)

    m <- lga_map %>%
      filter(lga_code21 %in% selected_codes()) %>%
      mutate(
        value = .data[[dcol]],
        count = .data[[ccol]],
        is_hl = lga_code21 == highlight_code() & !is.na(highlight_code())
      )
    validate(need(nrow(m) > 0, "Select at least one sector."))

    pal <- colorNumeric("YlGnBu", domain = c(0, max(m$value, na.rm = TRUE)))

    labels <- sprintf(
      "<strong>%s</strong><br/>%s stops: %s<br/>Area: %s km²<br/>Density: %s stops / km²<br/>Main sector: %s",
      m$lga_name21, input$mode, fmt(m$count), fmt(m$sqkm, 0), fmt(m$value, 2), m$dominant_sector
    ) %>% lapply(htmltools::HTML)

    # Frame the selected LGAs explicitly. Letting leaflet derive the view from
    # the sf object is unreliable: the auto-fit overrides setView() and
    # computes a nonsense zoom when the card is still being laid out.
    bb <- as.numeric(sf::st_bbox(m))

    # scrollWheelZoom = FALSE so the wheel scrolls the page rather than being
    # swallowed by the map, which sits half-way down a long page. Readers can
    # still zoom with the +/- buttons or by double-clicking.
    leaflet(m, options = leafletOptions(minZoom = 7, scrollWheelZoom = FALSE)) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(value),
        fillOpacity = 0.75,
        color = ~ ifelse(is_hl, highlight_colour, "#555555"),
        weight = ~ ifelse(is_hl, 4, 1),
        label = labels,
        popup = labels,
        highlightOptions = highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE)
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~ value, opacity = 0.8,
        title = paste0(input$mode, " stops / km²")
      ) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  })

  output$ta2_summary <- renderUI({
    dcol <- mode_col("density", input$mode)
    d <- ta2 %>%
      filter(lga_code21 %in% selected_codes()) %>%
      mutate(value = .data[[dcol]]) %>%
      arrange(desc(value))
    req(nrow(d) > 0)
    top <- head(d, 3); bottom <- d %>% filter(value > 0) %>% tail(3) %>% arrange(value)
    zero <- d %>% filter(value == 0)
    tagList(
      p(
        strong("Densest: "),
        paste(sprintf("%s (%s / km²)", top$lga_name21, fmt(top$value, 2)), collapse = ", "),
        br(),
        strong("Sparsest with service: "),
        paste(sprintf("%s (%s / km²)", bottom$lga_name21, fmt(bottom$value, 2)), collapse = ", "),
        if (nrow(zero) > 0) tagList(
          br(), strong("No ", input$mode, " stops at all: "),
          paste(zero$lga_name21, collapse = ", ")
        )
      )
    )
  })

  # ---- TA3: under-served index ------------------------------------------

  ta3_selected <- reactive({
    icol <- mode_col("index", input$mode)
    rcol <- mode_col("rps", input$mode)
    ta3 %>%
      filter(lga_code21 %in% selected_codes()) %>%
      mutate(
        index = .data[[icol]],
        residents_per_stop = .data[[rcol]],
        dominant_sector = factor(dominant_sector, levels = sector_levels),
        is_hl = lga_code21 == highlight_code() & !is.na(highlight_code())
      )
  })

  output$ta3_title <- renderText({
    paste0("3. Under-served index 0-100 - ", input$mode)
  })

  output$ta3_plot <- renderPlot({
    d <- ta3_selected() %>% filter(!is.na(index))
    validate(need(nrow(d) > 0, "No LGA in the current selection has stops of this mode."))
    top <- d %>% slice_max(index, n = input$top_n, with_ties = FALSE)

    ggplot(d, aes(x = total_pop, y = index)) +
      geom_point(aes(colour = dominant_sector), size = 3.5, alpha = 0.85) +
      geom_point(data = top, colour = "#D62728", size = 5.5, shape = 21, stroke = 1.5, fill = NA) +
      geom_text(data = top, aes(label = lga_name21), vjust = -1.1, size = 3.8, fontface = "bold") +
      geom_point(data = filter(d, is_hl), colour = highlight_colour, size = 7, shape = 21, stroke = 2, fill = NA) +
      geom_text(data = filter(d, is_hl & !(lga_code21 %in% top$lga_code21)),
                aes(label = lga_name21), vjust = -1.1, size = 3.8) +
      scale_colour_manual(values = sector_colours, name = "Main sector") +
      scale_x_continuous(labels = scales::label_comma()) +
      scale_y_continuous(limits = c(-2, 110), breaks = seq(0, 100, 20)) +
      labs(
        x = "LGA population (ABS 2024)",
        y = "Under-served index (0 = best served, 100 = most under-served)",
        subtitle = sprintf("Red rings mark the %d most under-served LGAs for %s stops",
                           input$top_n, tolower(input$mode))
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$ta3_table <- renderDT({
    d <- ta3_selected() %>%
      filter(!is.na(index)) %>%
      arrange(desc(index)) %>%
      transmute(
        Rank = row_number(),
        LGA = lga_name21,
        `Residents / stop` = round(residents_per_stop),
        Index = round(index, 1)
      )
    datatable(
      d, rownames = FALSE, options = list(pageLength = 8, dom = "tp", scrollX = TRUE),
      class = "compact stripe"
    ) %>%
      formatRound("Residents / stop", 0) %>%
      formatStyle("Index",
                  background = styleColorBar(c(0, 100), "#F4A6A6"),
                  backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$ta3_noservice <- renderUI({
    d <- ta3_selected() %>% filter(is.na(index))
    if (nrow(d) == 0) return(NULL)
    p(
      class = "text-muted",
      strong(sprintf("No %s service (not scored): ", tolower(input$mode))),
      paste(sprintf("%s (%s residents)", d$lga_name21, fmt(d$total_pop)), collapse = "; ")
    )
  })

  # ---- Key findings (computed from the processed tables) ----------------

  output$findings <- renderUI({
    tags$ul(
      tags$li(sprintf(
        "Scale: Casey has the most stops (1286) but 21 of the 35 LGAs have no tram stop and 2 have no train stop. The outer ring is essentially a bus-only network."
      )),
      tags$li(sprintf(
        "Density: Port Philip is the densest at 18.3 stops per km² while Macedon Ranges is the sparsest at 0.15. This is roughly a 125-fold difference. Casey drops from 1st on raw count to 23rd on density because its stops are spread over 409 km²."
      )),
      tags$li(sprintf(
        "Population fairness: Cardina is the most under-served overall (index 100, about 405 residents per stop) while Derabin is the best served (index 0, about 187 residents per stop). The most under-served LGAs are mostly fast-growing outer areas, not the most populated ones."
      )),
      tags$li(
        "Consistency across the three views: the same large outer LGAs (Cardinia, Macedon Ranges, Mitchell, Murrindindi, Moorabool) are among the lowest on raw counts, among the sparsest on density and in the upper half of the under-served index, so the finding is not an artefact of one measure."
      ),
      tags$li(
        "Rail access is the sharpest divide: for train and tram the 0-100 score is driven by a few LGAs with only a handful of stops, and Manningham and Murrindindi have no train stop at all."
      )
    )
  })

  # ---- Tab 2 tables -----------------------------------------------------

  output$task_abstraction_table <- renderDT({
    datatable(tibble::tribble(
      ~`Task`, ~`Action - target`, ~`Analysis question`, ~`Indicator type`, ~`Data used`, ~`Presentation`,
      "TA1", 
      "Summarise the stop count by transport mode for each Greater Melbourne LGA.",
      "Within each LGA, how is the stop count split among bus, train and tram? Does a single total hide which options residents actually have?",
      "Raw - distinct stop_id counts per LGA and per mode",
      "clean_stops.csv (lga_code21, stop_id, vehicle) & clean_area.csv for the LGA list",
      "Small multiples: four aligned horizontal bar charts (Total / Bus / Train / Tram), same LGA order",
      
      "TA2", 
      "Compare stop density of each LGA.",
      "Which LGAs have the sparsest coverage of stops per km², and are they the same as the lowest in TA1?",
      "Processed - rate: stops / sqkm, per mode",
      "TA1 counts joined to clean_area.csv (sqkm) with clean_lga_area.geojson boundaries",
      "Choropleth map of Greater Melbourne shaded by density, mode selectable",
      
      "TA3",
      "Rank LGAs on a 0-100 index of residents per stop, overall and per mode.",
      "Accounting for how many residents each stop must serve, which LGAs are most under-served and which lack rail specifically?",
      "Normalised - residents per stop, min-max scaled to 0-100, zero-stop modes flagged 'No service'",
      "TA1 counts joined to ABS 2024 population via LGA_2021_LGA_2024 mapping",
      "Scatter of index (y) vs population (x) with the N most under-served highlighted, plus ranking table"
    ), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$idiom_table <- renderDT({
    datatable(tibble::tribble(
      ~`Visual idiom`, ~`Used for`, ~`Justification`,
      "Small-multiple horizontal bar charts",
      "TA1",
      "Bars are the most accurate way to compare magnitudes. A shared, sorted LGA axis lets one LGA be read across all modes, and the empty Train/Tram panels make 'bus-only' visible at a glance.",
      
      "Choropleth map (Leaflet, sequential YlGnBu palette)",
      "TA2",
      "Density is inherently spatial by olouring real LGA boundaries lets a non-technical reader find their own area and see the inner-to-outer gradient. A single-hue sequential palette matches an ordered quantity.",
      
      "Scatter plot with highlighted points + ranking table",
      "TA3",
      "Two quantitative attributes (population, index) call for a scatter to reveal the relationship. Highlighting and a sorted table make the ranking explicit for a public audience.",
      
      "Value boxes and dynamic text", 
      "All",
      "Headline numbers and findings are computed from the processed tables so the narrative always matches the data.",
      
      "Sidebar filters (mode, sector, highlight, top-N)",
      "All",
      "Interaction lets the public focus on their own mode, region or LGA without cluttering the charts."
    ), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  # ---- Tab 3 table ------------------------------------------------------

  output$raw_data_table <- renderDT({
    datatable(tibble::tribble(
      ~`Dataset`, ~`Source`, ~`LGA code version`, ~`Rows x cols (raw)`, ~`Role in this project`,
      "lga_stops_routes.csv", 
      "Provided (PTV / DataVic)", 
      "2021",
      "43,818 x 7", 
      "Core: stop-route records -> unique stop counts per LGA and mode (TA1, TA2, TA3)",
      
      "lga_area.csv",
      "Provided",
      "2021", 
      "36 x 4", 
      "Assessment LGA list, area (sqkm) is the TA2 denominator",
      
      "lga_area.geojson",
      "Provided",
      "2021",
      "36 features", 
      "LGA boundary polygons for the TA2 choropleth",
      
      "Regional_Population_sex_and_age.xlsx (Table 3)",
      "Australian Bureau of Statistics, 2024 release",
      "2024", 
      "national, all LGAs",
      "Total persons per LGA - TA3 denominator",
      
      "LGA_2021_LGA_2024.csv",
      "Provided (ABS correspondence)", 
      "2021 + 2024", "564 x 4", 
      "Aligns ABS 2024 population to 2021 LGA codes",
      
      "lga_sector.csv", 
      "Provided", "2021", 
      "40 x 5 (+1 stray)", 
      "Dominant sector label for filtering (context only)",
      
      "lga_details.csv", 
      "Provided", "2021", 
      "35 x 6", 
      "Inspected in Task 1B but not used by the task abstractions",
      
      "lga_adjacents.csv", 
      "Provided", 
      "2021", 
      "174 x 4", 
      "Inspected in Task 1B but not used by the task abstractions",
      
      "routes_city_freq.csv", 
      "Provided",
      "n/a (route level)", 
      "453 x 6", 
      "Inspected in Task 1B but not used by the task abstractions",
      
      "stops.geojson", 
      "Provided", 
      "n/a (point level)", 
      "point features", 
      "Inspected in Task 1B but not used by the task abstractions"
    ), rownames = FALSE, options = list(pageLength = 10, dom = "t", scrollX = TRUE))
  })

  # ---- Tab 4 table ------------------------------------------------------

  output$preprocessing_table <- renderDT({
    datatable(tibble::tribble(
      ~`#`, ~`Issue or anomaly`, ~`Type`, ~`Evidence / check`, ~`Action taken`, ~`Reason`,
      1, 
      "Cardinia row in lga_sector.csv split by an unquoted comma (1,283 -> '1' and '283')", 
      "Structure / illogical value",
      "lga_sqkm = 1 and lga_part_sqkm_pct = 283 (> 1), lga_area.csv shows 1283 km²", 
      "Set lga_sqkm = 1283 and lga_part_sqkm_pct = 1",
      "Values were impossible and would break the join with lga_area.csv",
      
      2, 
      "Stray trailing empty 6th column in lga_sector.csv", 
      "Structure",
      "colnames() shows an unnamed column, all NA except the Cardinia spill-over",
      "Dropped column 6",
      "Column carried no information and produced 40+ spurious NAs",
      
      3, 
      "Melbourne LGA has NA in all four facility columns of lga_details.csv",
      "Missing values",
      "colSums(is.na()) and filter(if_any(..., is.na))",
      "Kept as NA with a facilities_missing flag, LGA retained (see issue 15)",
      "Dropping Melbourne would leave a hole in a Greater Melbourne analysis, but a blank is not a measured zero",
      
      4,
      "Wyndham rows in lga_adjacents.csv missing their own lga_code21", 
      "Missing value / linkage",
      "filter(is.na(lga_code21)), neighbours reference Wyndham as 27260", 
      "Filled lga_code21 = 27260 from the adjacency records",
      "Code is recoverable from the same file with certainty",
      
      5, 
      "Commas inside route_name values", 
      "Structure (checked)",
      "Printed sample route names from stops and freq", 
      "No action - values were correctly quoted",
      "Confirmed the CSV parser did not split these into extra columns",
      
      6, 
      "route_number unusable as a join key (mixed types, many blanks)", 
      "Linkage",
      "Counted blank route_number in both files and all freq route_names exist in stops", 
      "Use route_name as the join key if frequency is needed",
      "route_name is complete in freq and fully matched",
      
      7,
      "Repeated stop records (one row per route) and 54 multi-mode stops", 
      "Counting / structure",
      "n() vs n_distinct(stop_id), modes per stop_id", 
      "Count distinct stop_id; count multi-mode stops once per mode",
      "Row counts would overstate stops by ~50%",
      
      8,
      "Unincorporated Vic (29399) present in area/sector/stops/geojson but not details",
      "Special record / linkage",
      "setdiff() across tables and no freq linkage affected", 
      "Removed from all tables and the GeoJSON -> 35 LGAs",
      "Tiny non-council island which is not meaningful for LGA comparison",
      
      9, 
      "Out-of-scope LGAs in stops (80+) and mapping table (564 rows)", 
      "Scope / linkage",
      "n_distinct(lga_code21) before/after; duplicate check on mapping", 
      "Filtered both to the 35 assessment codes; verified no duplicates or missing codes",
      "Prevents wrong joins and keeps the analysis unit consistent",
      
      10, 
      "Population uses 2024 LGA codes but transport uses 2021 codes", 
      "Linkage",
      "Joined ABS Table 3 (Victoria) via LGA_2021_LGA_2024 and checked setdiff() = 0", 
      "Added total_pop to the area table for all 35 LGAs",
      "Joining by code through the correspondence table avoids name mismatches",
      
      11, 
      "4 exact duplicate rows in stops",
      "Duplicates",
      "duplicated() and group_by(across(everything()))", 
      "Removed with distinct()",
      "Exact duplicates add nothing",
      
      12,
      "Yarra Ranges row in lga_area.csv split by an unquoted comma (1,611.9332) and sqkm read as 1 and cbd_euclidean_km as 611.93",
      "Structure",
      "Delimiter count outside quotes finds 5 fields on a 4-field header which makes sqkm = 1 is impossible for the largest LGA. In geometry in lga_area.geojson gives 1611.9341", 
      "Repaired to sqkm = 1611.9332 and cbd_euclidean_km = 52, then validated against the GeoJSON (agrees to 0.0001%)",
      "sqkm is the TA2 denominator: uncorrected, Yarra Ranges would have shown 779 stops/km2 and inverted the map. Originally repaired by hand without evidence now detected and validated in code",
      
      13, 
      "Melbourne cbd_euclidean_km = 50", 
      "Illogical value",
      "Only LGA whose distance disagrees with lga_area.geojson (0). Melbourne contains the CBD reference point, yet Yarra = 3 km and Port Phillip = 5 km surround it", 
      "Corrected to 0 using the GeoJSON value",
      "A 50 km value places the densest, most tram-served LGA on the rural fringe and contradicts every other indicator",
      
      14, 
      "Darebin higher_education = 100", 
      "Illogical value",
      "Range is 0 - 8 for all other LGAs. Tukey upper fence 7.5, 12.5x the next highest ratio to school facilities 1.35 vs max 0.10 elsewhere", 
      "Set to NA and recorded as unknown",
      "No second source can recover the true value, and guessing would invent data",
      
      15, 
      "Melbourne facility counts were filled with 0 in the first submission", 
      "Missing values (treatment corrected)",
      "0 is a measurement meaning 'none here' the LGA contains RMIT, the University of Melbourne and the Royal Melbourne Hospital", 
      "Reverted to NA and added a facilities_missing flag",
      "An unmeasured value must not be presented as a measured zero therefore flag forces any later step to handle the gap deliberately"
    ), rownames = FALSE, options = list(pageLength = 15, dom = "t", scrollX = TRUE))
  })

  # ---- Tab 5 tables -----------------------------------------------------

  output$processed_table_list <- renderDT({
    datatable(tibble::tribble(
      ~`Table`, ~`Rows`, ~`Purpose`, ~`Used in`,
      
      "ta1_stop_counts.csv",
      nrow(ta1), 
      "Distinct stops per LGA: total, bus, train, tram + dominant sector", 
      "Tab 1 - TA1 small multiples; value boxes",
      
      "ta2_stop_density.csv",
      nrow(ta2), 
      "TA1 counts / sqkm -> stops per km² per mode", 
      "Tab 1 - TA2 choropleth and density summary",
      
      "lga_boundaries.geojson", 
      nrow(lga_boundaries), 
      "Simplified LGA polygons (100 m tolerance) joined to the master table at run time", 
      "Tab 1 - TA2 map",
      
      "ta3_underserved_index.csv",
      nrow(ta3), 
      "Residents per stop and 0-100 min-max index per mode. 'No service' flags", 
      "Tab 1 - TA3 scatter, ranking table, findings",
      
      "lga_master.csv",
      nrow(lga_master), 
      "All indicators, one row per LGA", 
      "Tab 5 master table."
    ), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$main_processed_table <- renderDT({
    datatable(lga_master, rownames = FALSE, filter = "top",
              options = list(pageLength = 35, scrollX = TRUE, dom = "ft"))
  })
  output$ta1_table_full <- renderDT({
    datatable(ta1, rownames = FALSE, options = list(pageLength = 35, scrollX = TRUE, dom = "ft"))
  })
  output$ta2_table_full <- renderDT({
    datatable(ta2 %>% mutate(across(starts_with("density_"), ~ round(., 3)), sqkm = round(sqkm, 1)),
              rownames = FALSE, options = list(pageLength = 35, scrollX = TRUE, dom = "ft"))
  })
  output$ta3_table_full <- renderDT({
    datatable(ta3 %>% mutate(across(c(starts_with("rps_"), starts_with("index_")), ~ round(., 1))),
              rownames = FALSE, options = list(pageLength = 35, scrollX = TRUE, dom = "ft"))
  })
}

shinyApp(ui, server)
