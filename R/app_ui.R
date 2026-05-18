# Dev version of app #----------------------------------------------------------

#' The application User-Interface (development version - additional functionality)
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui_dev <- function(request) {
  # this finds the root location of the package and sets to wd. Not ideal but only
  # way can get the app to run when saved as a package. Find another solution later
  start_wd <- getwd()
  on.exit(setwd(start_wd)) # to reset location afterwards
  setwd(paste0(app_sys('R'), '/..'))

  sectors <- c(
    'All',
    "Cement",
    "Ceramics",
    "Chemicals",
    "Construction",
    "Electrical engineering",
    "Food & drink",
    "Glass",
    "Iron & steel",
    "Lime",
    "Mechanical engineering",
    "Non-ferrous metals",
    "Other",
    "Paper",
    "Refineries",
    "Textiles",
    "Vehicles",
    "CO2 Infrastructure",
    "H2 Infrastructure"
  )

  clusters_vec <- c(
    'All',
    'Grangemouth',
    'Humberside',
    'Humberside2',
    'Londonderry',
    'Medway',
    'Merseyside',
    'Peterhead',
    'South Wales',
    'Southampton',
    'Teeside'
  )

  cluster_category <- c(
    '< 25km' = '<25km',
    '25 to 30km' = '25-30km',
    '> 30km' = '>30km',
    'CO2_C2S' = 'CO2_C2S'
  )

  flow_category <- c('fuel', 'commodity')

  technology_category <- c(
    'All',
    'BECCS',
    'CCS',
    'Coal',
    'Dry kiln',
    'Electricity',
    'Heat pump',
    'Hydrogen',
    'Natural gas',
    'Oil',
    'Standard_FF',
    'Steam'
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    useShinyjs(),
    useWaiter(),

    # UI logic
    navbarPage(
      theme = shinytheme('spacelab'),
      title = 'COMIT',

      tabPanel('Model', mod_model_ui("model_1")),

      tabPanel('Upload', mod_upload_ui("upload_1"), style = "margin: auto; width: 40%;"),

      tabPanel(
        'Outputs',
        mod_outputs_ui(
          "outputs_1",
          sectors,
          technology_category,
          clusters_vec,
          cluster_category
        )
      ),
      tabPanel(
        'Attribution',
        mod_attribution_ui("attribution_1", sectors)
      ),
      tabPanel(
        'Processes',
        mod_processes_ui("processes_1", flow_category, sectors)
      ),
      tabPanel(
        'About',
        tags$div(htmltools::includeMarkdown(app_sys(
          "app/about_page.md"
        )), style = 'max-width: 800px; margin-left: 5%; margin-right: 5%; margin-bottom: 5%;')
      ),

      footer = tagList(
        tags$div(
          class = 'footer',
          style = 'position: relative;  bottom: 0%; width: 100%; height: 100px;',

          tags$div(id = 'footer_box',
                   style = 'postion: absolute; bottom: 0px; height: 100px; width: 100%; border-top: 2px solid #e7e7e7; background-color: #f8f8f8;'),
          tags$img(src = 'www/DESNZ_Colour_main.png',
                   style = 'position: absolute; bottom: 20px; left: 2%; width: 150px'),
          tags$img(src = 'www/logo.png',
                   style = 'position: absolute; bottom: 20px; right: 2%; width: 100px; padding-left: 21px; padding-right: 21px'),
          # note: keep below to same total width to be aligned with above
          tags$div(as.character(desc_get_version()),
                   style = "position: absolute; bottom: 0; right: 2%; text-align: center; width: 100px")
        )
      )
    )

  )

}


# Main version #----------------------------------------------------------------

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # this finds the root location of the package and sets to wd. Not ideal but only
  # way can get the app to run when saved as a package. Find another solution later
  start_wd <- getwd()
  on.exit(setwd(start_wd)) # to reset location afterwards
  setwd(paste0(app_sys('R'), '/..'))

  sectors <- c('All',
               "Cement",
               "Ceramics",
               "Chemicals",
               "Construction",
               "Electrical engineering",
               "Food & drink",
               "Glass",
               "Iron & steel",
               "Lime",
               "Mechanical engineering",
               "Non-ferrous metals",
               "Other",
               "Paper",
               "Refineries",
               "Textiles",
               "Vehicles",
               "CO2 Infrastructure",
               "H2 Infrastructure")

  clusters_vec <- c('All',
                    'Grangemouth',
                    'Humberside',
                    'Humberside2',
                    'Londonderry',
                    'Medway',
                    'Merseyside',
                    'Peterhead',
                    'South Wales',
                    'Southampton',
                    'Teeside')

  cluster_category <- c('< 25km' = '<25km',
                        '25 to 30km' = '25-30km',
                        '> 30km' = '>30km',
                        'CO2_C2S' = 'CO2_C2S')

  flow_category <- c('fuel', 'commodity')

  technology_category <- c('All',
                           'BECCS',
                           'CCS',
                           'Coal',
                           'Dry kiln',
                           'Electricity',
                           'Heat pump',
                           'Hydrogen',
                           'Natural gas',
                           'Oil',
                           'Standard_FF',
                           'Steam')

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    useShinyjs(),
    useWaiter(),

    # UI logic
    navbarPage(
      theme = shinytheme('spacelab'),
      title = 'COMIT',

      tabPanel('Model', mod_model_ui("model_1")),

      tabPanel('Upload', mod_upload_ui_lite("upload_1"),
               style = "margin: auto; width: 40%;"),

      tabPanel('Outputs',
               mod_outputs_ui("outputs_1",
                              sectors,
                              technology_category,
                              clusters_vec,
                              cluster_category)
      ),
      tabPanel('About',
               tags$div(htmltools::includeMarkdown(app_sys("app/about_page_lite.md")),
                        style = 'max-width: 800px; margin-left: 5%; margin-right: 5%; margin-bottom: 5%;')),

      footer = tagList(

        tags$div(class = 'footer', style = 'position: relative;  bottom: 0%; width: 100%; height: 100px;',

                 tags$div(id = 'footer_box',
                          style = 'postion: absolute; bottom: 0px; height: 100px; width: 100%; border-top: 2px solid #e7e7e7; background-color: #f8f8f8;'),
                 tags$img(src = 'www/DESNZ_Colour_main.png',
                          style = 'position: absolute; bottom: 20px; left: 2%; width: 150px'),
                 tags$img(src = 'www/logo.png',
                          style = 'position: absolute; bottom: 20px; right: 2%; width: 100px; padding-left: 21px; padding-right: 21px'),
                 # note: keep below to same total width to be aligned with above
                 tags$div(as.character(desc_get_version()),
                          style = "position: absolute; bottom: 0; right: 2%; text-align: center; width: 100px")
        )
      )
    )

  )

}


#-------------------------------------------------------------------------------


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "COMIT"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
