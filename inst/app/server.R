the_frames <- {}
the_point <- 1
the_next_id <- 1
the_mod <- 0
the_mod_row <- NA
the_last_success <- 0
the_dat <- data.frame(frame = numeric(0), id = numeric(0), x1 = numeric(0),
                      y1 = numeric(0), x2 = numeric(0), y2 = numeric(0),
                      start = numeric(0), end = numeric(0))

shinyServer(function(input, output, session) {

  react <- reactiveValues(the_success = 0,
                          the_range = list(x = NULL, y = NULL, zoom = FALSE))

  shinyFileChoose(input, "the_video", session = session,
                  roots = c(wd = normalizePath("~")))

  # Load video
  observe({
    if (!is.null(input$the_video)) {
      path <- parseFilePaths(roots = c(wd = normalizePath("~")), input$the_video)

      if (file.exists(paste0(as.character(path$datapath), ".csv"))) {
        the_dat <<- read.csv(paste0(as.character(path$datapath), ".csv"))
      }

      react$the_video <- video(as.character(path$datapath))
    }
  })

  # Render frame slider
  output$the_frame_slider <- renderUI({
    if (!is.null(react$the_video) & !is.null(input$the_framerate)) {
      the_frames <<- round(seq(1, nframes(react$the_video), fps(react$the_video) * input$the_framerate))
      sliderInput("the_frame_slider", NULL, min = 1, max = length(the_frames),
                  value = 1, ticks = FALSE, step = 1)
    } else {
      sliderInput("the_frame_slider", NULL, min = 1, max = 1,
                  value = 1, ticks = FALSE, step = 1)
    }
  })

  observe({
    if (!is.null(input$the_previous_frame)) {
      isolate({
        updateSliderInput(session, "the_frame_slider",
                          value = input$the_frame_slider - 1)
      })
    }
  })

  observe({
    if (!is.null(input$the_next_frame)) {
      isolate({
        updateSliderInput(session, "the_frame_slider",
                          value = input$the_frame_slider + 1)
      })
    }
  })

  # Render plot output
  observe({
    if (!is.null(react$the_video) & !is.null(input$the_frame_slider)) {
      isolate({
        react$the_frame <- readFrame(react$the_video, the_frames[input$the_frame_slider])

        if (input$the_frame_slider == 1) {
          # react$the_points <- filter(the_dat, frame == the_frames[input$the_frame_slider])
          react$the_points <- filter(the_dat, (start <= the_frames[input$the_frame_slider]) &
                                       (end > the_frames[input$the_frame_slider]))
        } else {
          # tmp <- filter(the_dat, frame == the_frames[input$the_frame_slider])
          tmp <- filter(the_dat, (start <= the_frames[input$the_frame_slider]) &
                          (end > the_frames[input$the_frame_slider]))
          if (nrow(tmp) > 0) {
            react$the_points <- tmp
          } else {
            react$the_points <- NULL
            # react$the_points <- filter(the_dat, frame == the_frames[input$the_frame_slider - 1])
            # if (nrow(react$the_points) > 0) {
            #   react$the_points$frame <- the_frames[input$the_frame_slider]
            # }
          }
        }
      })
    }
  })

  observeEvent(input$the_frame_brush, {
    isolate({
      brush <- input$the_frame_brush
      if (!is.null(brush)) {
        react$the_range$x <- c(brush$xmin, brush$xmax)
        react$the_range$y <- c(brush$ymin, brush$ymax)
        react$the_range$zoom <- TRUE
      }
    })
  })

  output$the_unzoom_button <- renderUI({
    if (react$the_range$zoom) {
      actionButton("the_unzoom", "Unzoom")
    }
  })

  observe({
    if (!is.null(input$the_unzoom) && (input$the_unzoom > 0)) {
      isolate({
        react$the_range$x <- NULL
        react$the_range$y <- NULL
        react$the_range$zoom <- FALSE
      })
    }
  })

  output$the_timer <- renderText({
    if (!is.null(react$the_video) & !is.null(input$the_frame_slider)) {
      the_time <- the_frames[input$the_frame_slider] / fps(react$the_video)
      the_floor_time <- floor(the_time)
      the_img <- round(fps(react$the_video) * (the_time - the_floor_time))
      the_period <- seconds_to_period(the_floor_time)
      the_second <- sprintf("%02d", second(the_period))
      the_minute <- sprintf("%02d", minute(the_period))
      the_hour <- sprintf("%02d", hour(the_period))

      paste0(the_hour, ":", the_minute, ":", the_second, ".", the_img)
    }
  })

  output$the_frame <- renderPlot({
    if (!is.null(react$the_frame)) {
      plot(react$the_frame, xlim = react$the_range$x, ylim = react$the_range$y)

      print(the_dat)

      if (!is.null(react$the_points)) {
        points(c(react$the_points$x1, react$the_points$x2),
               c(react$the_points$y1, react$the_points$y2),
               col = "red", pch = 19)
        segments(react$the_points$x1, react$the_points$y1,
                 react$the_points$x2, react$the_points$y2,
                 col = "white", lwd = 2)
      }
    }
  })

  # Add, remove, modify
  observe({
    if (!is.null(input$the_frame_click)) {
      isolate({
        if (input$the_action == "add") {
          if (the_point == 1) {
            react$the_points <- rbind(react$the_points,
                                      data.frame(frame = the_frames[input$the_frame_slider],
                                                 id = the_next_id,
                                                 x1 = input$the_frame_click$x,
                                                 y1 = input$the_frame_click$y,
                                                 x2 = NA, y2 = NA,
                                                 start = the_frames[input$the_frame_slider],
                                                 end = Inf))
            the_next_id <<- the_next_id + 1
            the_point <<- 2
          } else {
            react$the_points[nrow(react$the_points), ]$x2 <- input$the_frame_click$x
            react$the_points[nrow(react$the_points), ]$y2 <- input$the_frame_click$y
            the_point <<- 1
          }
        } else if (input$the_action == "mod") {
          if (the_mod == 0) {
            the_distance <- data.frame(d1 = sqrt((input$the_frame_click$x - react$the_points$x1) ^ 2 +
                                                   (input$the_frame_click$y - react$the_points$y1) ^ 2),
                                       d2 = sqrt((input$the_frame_click$x - react$the_points$x2) ^ 2 +
                                                   (input$the_frame_click$y - react$the_points$y2) ^ 2))
            the_column <- which.min(c(min(the_distance$d1, na.rm = TRUE),
                                      min(the_distance$d2, na.rm = TRUE)))
            the_row <- which.min(the_distance[, the_column])

            react$the_points <- rbind(react$the_points, react$the_points[the_row, ])
            react$the_points$start[the_row] <- the_frames[input$the_frame_slider]
            react$the_points$end[nrow(react$the_points)] <- the_frames[input$the_frame_slider]

            if (the_column == 1) {
              react$the_points[the_row, ]$x1 <- NA
              react$the_points[the_row, ]$y1 <- NA
              the_mod <<- 1
            } else {
              react$the_points[the_row, ]$x2 <- NA
              react$the_points[the_row, ]$y2 <- NA
              the_mod <<- 2
            }
            the_mod_row <<- the_row
          } else {
            if (the_mod == 1) {
              react$the_points[the_mod_row, ]$x1 <- input$the_frame_click$x
              react$the_points[the_mod_row, ]$y1 <- input$the_frame_click$y
              the_mod <<- 0
            } else {
              react$the_points[the_mod_row, ]$x2 <- input$the_frame_click$x
              react$the_points[the_mod_row, ]$y2 <- input$the_frame_click$y
              the_mod <<- 0
            }
          }
        } else if (input$the_action == "rmv") {
          the_distance <- data.frame(d1 = sqrt((input$the_frame_click$x - react$the_points$x1) ^ 2 +
                                                 (input$the_frame_click$y - react$the_points$y1) ^ 2),
                                     d2 = sqrt((input$the_frame_click$x - react$the_points$x2) ^ 2 +
                                                 (input$the_frame_click$y - react$the_points$y2) ^ 2))
          the_column <- which.min(c(min(the_distance$d1, na.rm = TRUE),
                                    min(the_distance$d2, na.rm = TRUE)))
          the_row <- which.min(the_distance[, the_column])
          # react$the_points <- react$the_points[-the_row, ]
          react$the_points$end[the_row] <- the_frames[input$the_frame_slider]
          the_point <<- 1
        }

        the_dat <<- filter(the_dat, (start > the_frames[input$the_frame_slider]) |
                             (end <= the_frames[input$the_frame_slider])) %>%
          rbind(react$the_points)

        react$the_points <- filter(the_dat, (start <= the_frames[input$the_frame_slider]) &
                                     (end > the_frames[input$the_frame_slider]))
      })
    }
  })

  # Save data
  observe({
    if (input$the_save > 0) {
      path <- parseFilePaths(roots = c(wd = normalizePath("~")), input$the_video)
      write.csv(the_dat, paste0(as.character(path$datapath), ".csv"), row.names = FALSE)
      isolate({
        react$the_success <- react$the_success + 1
      })
    }
  })

  output$the_confirmation <- renderText({
    if (react$the_success > 0 & react$the_success != the_last_success) {
      invalidateLater(1000, session)
      the_last_success <<- react$the_success
      "Success!"
    } else {
      ""
    }
  })
})











