# App UI =======================================================================

page_fluid(
  
  # bootstrap theme
  theme = bs_theme(
      bootswatch = "zephyr", 
      fg = PALETTE$WHITE,
      bg = PALETTE$DARK_BLUE, 
      primary = PALETTE$PINK, 
      secondary = PALETTE$BLACK,
      base_font = font_google("Saira Condensed", local = TRUE)
    ) %>% 
    bs_add_rules(
      list(
        # custom class styles
        sass_file("www/assets/scss/styles.scss"),
        # style inline code
        "code { background-color: transparent; color: $secondary; }",
        "section#about a { color: $secondary; }", # text-decoration: none;
        "section#about a:hover { color: $primary }",
        # remove default padding from page body
        "body>.container-fluid { @extend .p-0 }",
        # add buffer for navbar
        # add same buffer after for symmetry
        "section:before, section:after {
          height: 62px;
          content: '';
          display:block;
        }"
      )
    ),
  # enable shinyjs package
  useShinyjs(),
  
  
  # Page Loader ----------------------------------------------------------------
  
  conditionalPanel(
    # show loader while data loads
    condition = "$('html').hasClass('shiny-busy')", 
    # container
    div(
      style = "position: fixed; z-index: 9999; background: #ffffff90; bottom: 0; top: 0; left: 0; right: 0;",
      # spinner icon
      div(
        class = "spinner-border text-primary top-50 start-50",
        style = "position: absolute;",
        role = "status",
        span(
          class = "visually-hidden",
          "Loading..."
        )
      )
    )
  ),
  
  
  # Page Navbar ----------------------------------------------------------------
  
  tags$nav(
    class = "navbar navbar-expand-lg sticky-top py-2 px-3 text-uppercase",
    style = sprintf("background-color: %s;", PALETTE$DARK_BLUE),
    # container
    div(
      class = "container-fluid",
      # LHS - Brand
      a(
        class = "navbar-brand mr-auto p-2", 
        href = "#",
        "Ultry"
      ),
      # RHS - Hamburger Menu Button
      tags$button(
        class = "navbar-toggler ms-auto",
        type="button",
        `data-bs-toggle`="collapse",
        `data-bs-target`="#collapseNavbar",
        tags$span(
          class="navbar-toggler-icon"
        )
      ),
      # RHS - Menu. Show items for wide screens; collapse for small
      div(
        class = "collapse navbar-collapse flex-grow-1 text-right",
        id = "collapseNavbar",
        # navbar items
        tags$ul(
          class = "navbar-nav ms-auto flex-nowrap",
          # home
          tags$li(
            class = "nav-item",
            a(
              class = "nav-link",
              "Home",
              href = "#"
            )
          ),
          # app
          tags$li(
            class = "nav-item",
            a(
              class = "nav-link",
                  "App",
                  href = "#app"
            )
          ),
          # about
          tags$li(
            class = "nav-item",
            a(
              class = "nav-link",
              "About",
              href = "#about"
            )
          )
        )
      )
    )
  ),
  
  
  # Section - Above the Fold ---------------------------------------------------
  
  mod_above_the_fold_ui("above_the_fold"),
  
  
  # Section - App --------------------------------------------------------------
  
  tags$section(
    class = "px-3 py-4",
    id = "app",
    # Row 1 - Map and Race Time Cards
    layout_columns(
      # id = "app",
      col_widths  = c(8, 4),
        # LHS - Course Details: Map, Elevation, Leg Cards
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center py-2",
            "Course",
            # zoom reset button
            div(
              actionButton(
                "reset_zoom_course",
                class = "btn btn-outline-primary border-0",
                label = "",
                icon = icon("zoom-out", lib = "glyphicon"),
                title = "Reset Zoom"
              )
            )
          ),
          card_body(
            # Cards w/ leg details - name; distance; elevation gain; elevation loss
            mod_leg_info_ui("leg_info"),
            layout_columns(
              col_widths  = c(6, 6),
              # Table - All Leg Details
              mod_leg_table_ui("leg_table"),
              div(
                # Map - Course
                div(
                  style = "height: 360px",
                  mod_route_map_ui("map")#,
                ),
                # Area Chart - Course Elevation
                div(
                  style = "height: 240px",
                  mod_route_elevation_ui("elevation")
                )
              )
            )
          )
        ),
        # RHS - Goal Projection: Goal Cards; Race Times Graph
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center py-2",
            "Results Projection",
            # zoom reset button
            div(
              actionButton(
                "reset_zoom_results", 
                class = "btn btn-outline-primary border-0",
                label = "",
                icon = icon("zoom-out", lib = "glyphicon"),
                title = "Reset Zoom"
              )
            )
          ),
          card_body(
            # Cards w/ goal details - goal time; goal pace
            mod_leg_info_goal_ui("leg_info_goal"),
            # Line chart - historic race times & goal projection
            div(
              class = "route-times",
              style = "height: 560px;",
              mod_route_times_ui("times")
            )
          )
        )
      ),
    # Row 2 - Results table Card
    div(
      card(
        class = "m-0",
        card_header(
          class = "d-flex justify-content-between align-items-center py-2",
          "Results Table"
        ),
        card_body(
          # Table - Results
          mod_results_table_ui("results_table")
        )
      )
    )
  ),
  
  
  # Section - About ------------------------------------------------------------
  
  tags$section(
    class = "container-fluid text-secondary",
    id = "about",
    style = sprintf("background-color: %s", PALETTE$WHITE),
    div(
      class = "row justify-content-center",
      div(
        class = "col-8 gap-4",
        # section header
        div(
          class = "mb-5",
          # Header - H1
          h1(
            class = "text-center text-primary",
            "About Ultry"
          ),
          # Header - H2
          h2(
            class = "text-center display-6 text-secondary",
            "Plan Your Next Ultra Like You’re Trying"
          )
        ),
        # Proof of Contact
        div(
          class = "mb-5",
          h3(
            class = "text-secondary",
            "Proof of Concept"
          ),
          p(
            "Ultrarunning presents unique challenges. Terrain, elevation, 
            weather, and night sections all play a cumulative role in how 
            long it takes to finish a race. 100-miler ultras can see finish 
            times vary from 25 to over 50 hours, even for runners with similar 
            fitness levels. With such a wide range, proper preparation 
            becomes crucial."
          ),
          p(
            "This is where Ultry comes in. Ultry helps you leverage historical 
            race data to better plan for your next ultra. By analyzing past 
            results and course elevation profiles, Ultry helps you understand 
            the difficulty of different course segments and more accurately 
            estimate your leg times. This allows you to make informed decisions 
            about sleep strategies, nutrition, and pacing throughout the race."
          )
        ),
        # current state & future work
        div(
          class = "mb-5",
          h3(
            class = "text-secondary",
            "Current State & Future Work"
          ),
          p(
            "At present, Ultry is a proof of concept, showcasing its purpose 
            using UTCT’s 100 mile event as an example. Further development is 
            required to turn it into a more versatile tool. Once it is adapted 
            to receive data from other races, the user will be prompted to 
            upload three files:"
          ),
          tags$ul(
            tags$li(
              tags$span(
                style = "font-weight: bold",
                "GPX Route: "
              ),
              "Central to calculating and visualizing the route statistics, 
              such as leg distances, elevation gain and elevation loss."
            ),
            tags$li(
              tags$span(
                style = "font-weight: bold",
                "Checkpoints: "
              ),
              "A table containing checkpoint names and at which distance on the 
              course they are located. This aids in dividing the route into 
              course legs."
            ),
            tags$li(
              tags$span(
                style = "font-weight: bold",
                "Race Times: "
              ),
              "Historic race times of runners. Each runner should have their 
              race time recorded at each checkpoint and provided as such in 
              the race times file. The tool works without this level of detail, 
              but fewer insights will be gained."
            )
          )
        ),
        # tech stack
        div(
          class = "mb-5",
          h3(
            class = "text-secondary",
            "Tech Stack"
          ),
          p(
            "Part of this project’s motivation is to explore R Shiny’s viability 
            as a web application framework for applications with a need for 
            highly custom UI/UX. At PlumeVue we equip organizations to better 
            leverage their data. Oftentimes, the companies we work with have 
            employees with a certain level of analytical skills. R is 
            considered one of the most popular coding languages for data 
            analytics and data science, and often already used internally. 
            Rather than introducing new technologies or paying for no-code 
            platforms, it becomes an attractive option for data tool development 
            to ensure maintainability and enable in-house modifications 
            post-project handover."
          ),
          p(
            "This project serves as an exploration of R Shiny’s frontend 
            capabilities and constraints. R Shiny describes itself as an 
            easy-to-use framework to build web applications without requiring 
            HTML, CSS or JavaScript knowledge. This may be misconstrued as a 
            lack of frontend flexibility. This exploration showcases that there 
            are very few restrictions on what’s possible frontend-wise. The 
            biggest “constraint” is that Shiny is meant for developing Single 
            Page Applications (SPAs), typically meant for highly interactive, 
            dynamic applications, such as a Power BI dashboard or Google Maps. 
            This means Shiny is a poor fit for developing e.g. a website with 
            many web pages. As with Ultry, you could have multiple page 
            sections, or you could have multiple tabs, but everything still 
            lives on the same web page."
          )
        ),
        # theming
        div(
          class = "mb-5",
          h4(
            class = "text-secondary",
            "Theming"
          ),
          p(
            "Bootstrap is a frontend framework that comes with a bunch of 
            modifiable component design templates which allows you to build 
            web apps in no time. Bootswatch’s ", 
            tags$a("Zephyr", href = "https://bootswatch.com/zephyr/"), 
            " Bootstrap theme serves as the theming foundation."
          ),
          p(
            "Ultry’s colours and typography were chosen based off of UTCT’s 
            100 miler ", 
            tags$a("event page", href = "https://www.ultratrailcapetown.com/100miles"),
            ". They do an excellent job of balancing vibrancy and simplicity."
          ),
          p(
            tags$code(
              tags$a(
                "bslib",
                href = "https://rstudio.github.io/bslib/"
              )
            ),
            " is the R package used to integrate bootstrap with Shiny. Most 
            Bootstrap Sass variables, such as theme colours and fonts can be set 
            directly in R. More custom styling may be imported from a CSS or 
            Sass file. An example of this is the custom positioning and styling 
            of the tooltip styling when hovering over a course leg on the map."
          )
        ),
        # data visualization
        div(
          class = "mb-5",
          h4(
            class = "text-secondary",
            "Data Visualization"
          ),
          h5(
            class = "text-secondary",
            "Map"
          ),
          p(
            tags$a(
              "Mapbox",
              href = "https://www.mapbox.com/"
            ),
            " serves as the interactive map and its course visualization. ",
            tags$code(
              tags$a(
                "mapboxer",
                href = "https://github.com/crazycapivara/mapboxer"
              )
            ),
            " is the R package that allows for interfacing with 
            mapbox in R."
          ),
          h5(
            class = "text-secondary",
            "Charts"
          ),
          p(
            tags$a(
              "Echarts",
              href = "https://echarts.apache.org/en/index.html"
            ),
            " is used for the race time and course elevation plots. Echarts is 
            a powerful JS interactive data visualization library with efficient 
            rendering capabilities for visualizing large amount of data. The ",
            tags$code(
              tags$a(
                "echarts4r",
                href = "https://echarts4r.john-coene.com/"
              )
            ),
            " is the R wrapper for echarts, which 
            does a great job of exposing all of the Echarts API features."
          ),
          h5(
            class = "text-secondary",
            "Tables"
          ),
          p(
            tags$code(
              tags$a(
                "reactable",
                href = "https://glin.github.io/reactable/"
              )
            ),
            " supports Ultry’s results table. It is based off of the ",
            tags$a(
              "React table",
              href = "https://tanstack.com/table/latest"
            ),
            " which makes it highly flexible and interactive. 
            Pagination and search are easily enabled."
          ),
          p(
            "The bar charts in the results table is made possible by the ",
            tags$a(
              tags$code(
                "sparklines",
                href = "https://bart6114.github.io/sparklines/"
              )
            ),
            " R package, based off of jQuery’s ",
            tags$a(
              "sparklines plugin",
              href = "https://omnipotent.net/jquery.sparkline/#s-about"
            ),
            "."
          )
        )
      )
    )
  ),
  # Footer
  tags$footer(
    tags$section(
      class = "d-flex align-items-center justify-content-between p-4 border-bottom",
      # LHS
      div(
        class = "me-5",
        div(
          class = "d-block",
          # div(
            h6(
              class = "text-uppercase fw-bold mb-0",
              "Developed By"
            ),
          # ),
          p(
            class = "m-0",
            "Johannes Harmse"
          )
        )
      ),
      # Center
      div(
        class = "d-flex flex-column align-items-center",
        a(
          class = "navbar-brand p-2 pt-0", 
          href = "https://plumevue.com/",
          tags$image(
            src = "assets/images/plumevue-logo-white.png",
            width = "32px"
          )
        ),
        p(
          class = "m-0",
          sprintf("© %s PlumeVue Inc.", format(Sys.Date(), "%Y"))
        )
      ),
      # RHS
      div(
        tags$a(
          class = "me-4 text-reset",
          href = "https://www.linkedin.com/in/johannes-harmse/",
          tags$i(
            class = "fab fa-linkedin",
            style = "font-size: 2rem;"
          )
        ),
        tags$a(
          class = "me-4 text-reset",
          href = "https://github.com/johannesharmse",
          tags$i(
            class = "fab fa-github",
            style = "font-size: 2rem;"
          )
        )
      )
    )
  )
)

