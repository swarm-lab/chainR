shinyUI(
  navbarPage(
    title = "chainR",
    theme = shinytheme("cosmo"),
    fluid = FALSE,
    collapsible = TRUE,

    tabPanel("-",

             includeCSS("www/custom.css"),

             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 width = 3,

                 div(style = "text-align:center;",
                     shinyFilesButton("the_video", "Select video file",
                                      "Please select a video file", FALSE)
                 ),

                 tags$hr(),

                 numericInput("the_framerate", "Time between images", value = 5),

                 tags$hr(),

                 radioButtons("the_action", "Action",
                              c("Add" = "add", "Remove" = "rmv", "Modify" = "mod"),
                              selected = "add", inline = TRUE),

                 tags$hr(),

                 div(id = "the_save_div",
                     actionButton("the_save", "Save data"),
                     textOutput("the_confirmation")
                 )
               ),

               # Main panel
               mainPanel(
                 width = 9,

                 wellPanel(
                   div(id = "the_frame_div",
                       plotOutput("the_frame", height = "100%",  width = "100%",
                                  dblclick = "the_frame_click",
                                  brush = brushOpts(
                                    id = "the_frame_brush",
                                    resetOnNew = TRUE
                                  )),
                       uiOutput("the_unzoom_button"),
                       textOutput("the_timer")
                   )
                 ),

                 wellPanel(
                   HTML("<table width='100%'> <tr> <td width='10%' align='left'>"),
                   actionButton("the_previous_frame", "<<<"),
                   HTML("</td> <td align='center'>"),
                   uiOutput("the_frame_slider"),
                   HTML("</td> <td width='10%' align='right'>"),
                   actionButton("the_next_frame", ">>>"),
                   HTML("</td> </tr> </table>")
                 )
               )
             )
    ),

    tabPanel(tagList(tags$html("Powered by"),
                     tags$img(src = "white-rstudio-logo.png",
                              height = "20")),
             value = "RStudio",
             tags$head(tags$script(src = "actions.js"))
    )
  )
)

