doPlotDownload <- function(dat, x, y) {
    if (PermitPlot()) {
        dat4plot <- subset(dat, is.na(dat$VALUE) == FALSE)
        ##Prepping data for plotting by converting to more "plot friendly" values
        dat4plot <- mutate(
            dat4plot,
            VARIANCE = case_when(
                unit == '' ~ VARIANCE,
                unit == 'thousands' ~ VARIANCE / 1e3,
                unit == 'millions' ~ VARIANCE / 1e6,
                unit == 'billions' ~ VARIANCE / 1e9,
                T ~ -999
            ),
            q25 = case_when(
                unit == '' ~ q25,
                unit == 'thousands' ~ q25 / 1e3,
                unit == 'millions' ~ q25 / 1e6,
                unit == 'billions' ~ q25 / 1e9,
                T ~ -999
            ),
            q75 = case_when(
                unit == '' ~ q75,
                unit == 'thousands' ~ q75 / 1e3,
                unit == 'millions' ~ q75 / 1e6,
                unit == 'billions' ~ q75 / 1e9,
                T ~ -999
            ),
            VALUE = case_when(
                unit == '' ~ VALUE,
                unit == 'thousands' ~ VALUE / 1e3,
                unit == 'millions' ~ VALUE / 1e6,
                unit == 'billions' ~ VALUE / 1e9,
                T ~ -999
            )
        )

        dat4plot$sort2 <- if (!input$LayoutSelect) {
            reorder(dat4plot$VARIABLE, dat4plot$sort)
        } else {
            reorder(dat4plot$ylab, dat4plot$sort)
        }
        #
        # dat4plot$thresh <-  if(input$Ind_sel=="Economic"){
        #   length(unique(dat4plot$YEAR<=2010))
        # } else if(input$Ind_sel!="Economic"){
        #   if(input$LayoutSelect){
        #     if(is.na(max(dat4plot$VARIANCE))) {
        #       data.frame(dat %>%
        #                    group_by(METRIC) %>%
        #                    mutate(threshold=max(VALUE, na.rm=T)+max(VARIANCE, na.rm=T)+max(VALUE, na.rm=T)/10))%>%
        #         subset(select=c(threshold))
        #     } else {
        #       data.frame(dat %>%
        #                    group_by(METRIC) %>%
        #                    mutate(threshold=max(VALUE, na.rm=T)+max(VALUE, na.rm=T)/10))%>%
        #         subset(select=c(threshold))
        #     }
        #   } else {
        #     0
        #   }
        #  }

        dat4plot$upper <-
            if (input$Ind_sel == "Economic") {
                if (input$AVE_MED == 'A') {
                    dat4plot$VALUE + dat4plot$VARIANCE
                } else if (input$AVE_MED == 'T') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q75
                }
            } else if (input$Ind_sel == 'Cost') {
                if (input$AVE_MED_COSTS == 'A') {
                    dat4plot$VALUE + dat4plot$VARIANCE
                } else if (input$AVE_MED_COSTS == 'T') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q75
                }
            } else if (input$Ind_sel == 'Other') {
                if (input$otherStats == 'Mean') {
                    dat4plot$VALUE + dat4plot$VARIANCE
                } else if (input$otherStats == 'Total') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q75
                }
            } else if (input$Ind_sel == 'Labor') {
                if (input$crewStats == 'Mean') {
                    dat4plot$VALUE + dat4plot$VARIANCE
                } else if (input$crewStats == 'Total') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q75
                }
            } else if (
                input$Ind_sel == 'Vessel characteristics' ||
                    input$Ind_sel == 'Processor characteristics'
            ) {
                if (input$demStats == 'Mean') {
                    dat4plot$VALUE + dat4plot$VARIANCE
                } else if (input$demStats == 'Total') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q75
                }
            }

        dat4plot$lower <-
            if (input$Ind_sel == "Economic") {
                if (input$AVE_MED == 'A') {
                    dat4plot$VALUE - dat4plot$VARIANCE
                } else if (input$AVE_MED == 'T') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q25
                }
            } else if (input$Ind_sel == 'Cost') {
                if (input$AVE_MED_COSTS == 'A') {
                    dat4plot$VALUE - dat4plot$VARIANCE
                } else if (input$AVE_MED_COSTS == 'T') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q25
                }
            } else if (input$Ind_sel == 'Other') {
                if (input$otherStats == 'Mean') {
                    dat4plot$VALUE - dat4plot$VARIANCE
                } else if (input$otherStats == 'Total') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q25
                }
            } else if (input$Ind_sel == 'Labor') {
                if (input$crewStats == 'Mean') {
                    dat4plot$VALUE - dat4plot$VARIANCE
                } else if (input$crewStats == 'Total') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q25
                }
            } else if (
                input$Ind_sel == 'Vessel characteristics' ||
                    input$Ind_sel == 'Processor characteristics'
            ) {
                if (input$demStats == 'Mean') {
                    dat4plot$VALUE - dat4plot$VARIANCE
                } else if (input$demStats == 'Total') {
                    dat4plot$VALUE
                } else {
                    dat4plot$q25
                }
            }

        upper <- function() {
            if (input$Ind_sel == "Economic") {
                if (input$AVE_MED == 'A') {
                    max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
                } else if (input$AVE_MED == 'M') {
                    max(dat4plot$q75, na.rm = T)
                } else {
                    max(dat4plot$VALUE, na.rm = T)
                }
            } else if (input$Ind_sel == 'Cost') {
                if (input$AVE_MED_COSTS == 'A') {
                    max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
                } else if (input$AVE_MED_COSTS == 'M') {
                    max(dat4plot$q75, na.rm = T)
                } else {
                    max(dat4plot$VALUE, na.rm = T)
                }
            } else if (input$Ind_sel == 'Other') {
                if (input$otherStats == 'Mean') {
                    max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
                } else if (input$otherStats == 'Median') {
                    max(dat4plot$q75, na.rm = T)
                } else {
                    max(dat4plot$VALUE, na.rm = T)
                }
            } else if (input$Ind_sel == 'Labor') {
                if (input$crewStats == 'Mean') {
                    max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
                } else if (input$crewStats == 'Median') {
                    max(dat4plot$q75, na.rm = T)
                } else {
                    max(dat4plot$VALUE, na.rm = T)
                }
            } else if (
                input$Ind_sel == 'Vessel characteristics' ||
                    input$Ind_sel == 'Processor characteristics'
            ) {
                if (input$demStats == 'Mean') {
                    max(dat4plot$VALUE + dat4plot$VARIANCE, na.rm = T)
                } else if (input$demStats == 'Median') {
                    max(dat4plot$q75, na.rm = T)
                } else {
                    max(dat4plot$VALUE, na.rm = T)
                }
            }
        }

        yaxislabel <- function() {
            if (input$LayoutSelect) {
                if (input$Ind_sel %in% c('Economic', 'Cost')) {
                    paste(dat4plot$STAT, "(see plot title for units)")
                } else {
                    paste(dat4plot$SUMSTAT, "(see plot title for units)")
                }
            } else {
                dat4plot$ylab
            }
        }

        yr <- function() {
            return(unique(as.numeric(dat4plot$YEAR)))
        }

        groupVar <- "whitingv"

        allcolourThirds <-
            c(
                # set colors for whiting/non-whiting/all vessels
                'Non-whiting vessels' = "#E69F00",
                'Whiting vessels' = "#0072B2",
                'All vessels' = "#000000",
                # set colors for whiting/non-whiting/all processors
                'Non-whiting processors' = "#E69F00",
                'Whiting processors' = "#0072B2",
                'All processors' = "#000000",
                # set black for mothership vessels
                'Mothership vessels' = "#000000",
                # set black for catcher-procesor vessels
                'Catcher-processor vessels' = "#000000"
            )

        colourThirds <- allcolourThirds[unique(dat4plot$whitingv)]

        sect <- function() {
            if (input$Sect_sel == "CV") {
                return("Catcher Vessels")
            } else if (input$Sect_sel == "M") {
                return("Motherships")
            } else if (input$Sect_sel == "CP") {
                return("Catcher-Processors")
            } else if (input$Sect_sel == "FR") {
                return("Shorebased Processors")
            }
        }

        # Plot header construction ####
        # title
        plot.title <- function() {
            if (input$Sect_sel == "CV") {
                return("West Coast Catcher Vessels")
            } else if (input$Sect_sel == "M") {
                return("West Coast Mothership Vessels")
            } else if (input$Sect_sel == "CP") {
                return("West Coast Catcher-Processor Vessels")
            } else if (input$Sect_sel == "FR") {
                return("Shorebased Processors")
            }
        }

        gv <- function() {
            if (input$Sect_sel == 'CV') {
                if (!input$LayoutSelect) {
                    if (input$CategorySelect != 'Fisheries') {
                        sprintf(paste(
                            "Vessels in",
                            input$inSelect
                        ))
                    } else {
                        sprintf(paste(
                            ""
                        ))
                    }
                } else if (
                    input$CategorySelect == 'Homeport' |
                        input$CategorySelect == 'State'
                ) {
                    sprintf(paste(
                        input$VariableSelect,
                        "vessels in",
                        input$inSelect
                    ))
                } else if (input$CategorySelect == 'Vessel length class') {
                    sprintf(paste(
                        "Vessels in",
                        input$inSelect,
                        "for",
                        input$VariableSelect
                    ))
                } else {
                    sprintf(paste(
                        "Vessels in",
                        input$VariableSelect
                    ))
                }
            } else {
                sprintf(paste(
                    ""
                ))
            }
        }

        main <- function() {
            bquote(atop(.(plot.title()), .(gv())))
        }
        ##gv <- function() {

        source_lab <- function() {
            paste(
                "\n
        \nSourced from the FISHEyE application (https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheries NWFSC on ",
                format(Sys.Date(), format = "%B %d %Y")
            )
        }
        # confidentiality messages ####
        supp_obs <- function() {
            "\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. \nPlease see the About tab for more information."
        }

        # x-axis label ####
        # xlab is actually "notes"
        xlab <- function() {
            #   if (max(dat4plot$conf) == 0) {
            #     if(max(dat4plot$flag) == 0) {
            #       paste(source_lab())
            #     }
            #     else {
            #       paste(supp_obs(), source_lab())
            #     }
            #   } else {
            if (max(dat4plot$flag) == 0) {
                paste(supp_obs(), source_lab())
            } else {
                paste(supp_obs(), source_lab())
            }
        }
        #}

        scale_text <- function() {
            if (input$Ind_sel != "Economic") {
                return(1.1)
            } else {
                return(1.2)
            }
        }

        # scaling factor for geom text size ####
        scale_geom_text <- function() {
            if (any(dat4plot$VALUE > 0, na.rm = T)) {
                return(max(dat4plot$VALUE, na.rm = T))
            } else {
                return(0)
            }
        }

        #----- Define ggplot ------#
        # format data for graph and add to graph ####
        # special data for seasonality plot
        #  print(paste0(seasonality, 1))
        if (input$Ind_sel == 'Other' & !input$LayoutSelect) {
            # and seasonality is selected
            if (input$otherSelect == "Seasonality") {
                ssn <- mutate(
                    dat,
                    VALUE = as.Date(
                        VALUE,
                        origin = "2014-01-01",
                        format = "%Y-%m-%d"
                    ),
                    sort2 = reorder(VARIABLE, sort)
                )
                g <- ggplot(
                    ssn,
                    aes(
                        x = .data[[x]],
                        y = .data[[y]],
                        group = .data[[groupVar]]
                    ),
                    environment = environment()
                ) +
                    coord_cartesian(xlim = c(NA, max(2025, currentyear)))
                # otherwise normal plot:
            } else {
                dat4plot <- dat4plot[order(dat4plot$sort), ]
                dat4plot$bystategrp <- paste0(dat4plot$AGID, dat4plot$whitingv)
                g <-
                    # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
                    ggplot(
                        dat4plot,
                        aes(
                            x = .data[[x]],
                            y = .data[[y]],
                            group = .data[[groupVar]]
                        ),
                        environment = environment()
                    ) #+coord_cartesian(xlim = c(0, length(table(dat4plot$YEAR))+1))
            }
        } else {
            #dat <- dat[order(dat4plot$sort), ]
            g <-
                # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
                ggplot(
                    dat4plot,
                    aes(
                        x = .data[[x]],
                        y = .data[[y]],
                        group = .data[[groupVar]]
                    ),
                    environment = environment()
                ) #+coord_cartesian(xlim = c(0, length(table(dat4plot$YEAR))+1))
        }

        # add lines and points to the plot ####
        if (!input$LayoutSelect) {
            if (input$Ind_sel == 'Other') {
                if (input$otherSelect == 'Share of landings by state') {
                    g <-
                        g +
                        geom_line(
                            aes(
                                colour = .data[[groupVar]],
                                group = 'bystategrp'
                            ),
                            size = 2
                        ) +
                        geom_point(
                            aes_string(
                                colour = groupVar,
                                shape = 'AGID',
                                group = 'bystategrp'
                            ),
                            size = 2
                        )
                } else {
                    g <- g +
                        geom_line(
                            aes(colour = .data[[groupVar]]),
                            linewidth = .75
                        ) +
                        geom_point(aes(colour = .data[[groupVar]]), size = 2)
                }
            } else {
                g <- g +
                    geom_line(
                        aes(colour = .data[[groupVar]]),
                        linewidth = .75
                    ) +
                    geom_point(aes(colour = .data[[groupVar]]), size = 2)
            }
        }
        g <- g +
            geom_line(aes(colour = .data[[groupVar]]), linewidth = 0.75) +
            geom_point(aes(colour = .data[[groupVar]]), size = 2)

        #------ Add variance ------#
        if (!exists('ssn')) {
            g <- g +
                geom_ribbon(
                    aes(ymax = upper, ymin = lower, fill = whitingv),
                    alpha = .25
                ) #show.legend = FALSE,
        } else {
            g <- g
        }

        #----- define facet -----#
        if (!input$LayoutSelect) {
            g <- g + facet_wrap(~sort2, ncol = 2)
        } else {
            g <- g + facet_wrap(~sort2, scales = 'free_y', ncol = 2)
        }

        #----- Define rectangles and labels -----#
        #----- Define grey shading and Non-CS/CS labels ------####
        # choose label text size ####
        labeltext <- ifelse(input$LayoutSelect & input$tabs == 'Panel1', 5, 7)

        # geom_rect (define the grey boxes for pre-catch shares) ####
        geom_rect_fun <- function(ymin_val = -Inf, ymax_val = Inf) {
            geom_vline(
                xintercept = 2010.5,
                linewidth = .75,
                color = "darkgray"
            )
        }

        # geom_text function ####

        geom_text_fun <- function(x_val, y_val, label_val, vjust_val = .5) {
            geom_text(
                aes(
                    x = x_val,
                    y = y_val,
                    vjust = vjust_val,
                    label = label_val,
                    family = "serif",
                    fontface = "italic"
                ),
                hjust = 0,
                color = "grey20",
                size = labeltext / scale_text()
            )
        }

        # set rect and text for plots with both CS and non-CS years ####
        # otherwise no rect or text
        # the original code for the geom_text* are commented out at the bottom of the doc
        # if there are years shown before and after implementation of catch shares
        if (length(yr()) > 1 & min(yr()) < 2011 & max(yr()) > 2010) {
            # if the "Group by vessels" display is chosen
            if (!input$LayoutSelect) {
                # if seasonality is clicked
                if (input$Ind_sel == 'Other') {
                    # and seasonality is selected
                    if (input$otherSelect == "Seasonality") {
                        g <- g +
                            geom_rect_fun(
                                ymin_val = structure(-Inf, class = "Date"),
                                ymax_val = structure(Inf, class = "Date")
                            )

                        # for all other variables
                    }
                }
            }
        } else {
            # end of rect/text for cs/non-cs, no CS box required for plots with only one "kind" of year
            g <- g
        }

        #----- define scale -----#
        g <- g +
            scale_fill_manual(values = colourThirds) +
            scale_colour_manual(values = colourThirds)

        #----- define solid line y=0 ------#
        g <- g + geom_hline(yintercept = 0)

        #----- define labels ------#
        g <- g + labs(y = yaxislabel(), x = xlab(), title = main())

        if (input$LayoutSelect) {
            if (input$tabs == 'Panel1') {
                strptextsize <- 8
            } else {
                strptextsize <- 12
            }
        } else {
            strptextsize <- 12
        }

        if (input$LayoutSelect) {
            if (length(unique(dat4plot$YEAR)) > 9) {
                xaxissize <- 6
            } else {
                xaxissize <- 11
            }
        } else {
            xaxissize <- 11
        }

        #----- define theme ------#
        g <- g +
            theme(
                plot.title = element_text(
                    size = rel(1.2),
                    colour = "grey25",
                    family = "sans",
                    face = "bold",
                    vjust = 1,
                    hjust = .5
                ),
                plot.margin = unit(c(0.25, 0.25, 1, 0.25), "cm"),
                panel.background = element_rect(fill = "white"),
                #panel.spacing = unit(1, "lines"),
                panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major.y = element_line(
                    color = "#656C70",
                    linetype = "dotted"
                ),
                strip.text = element_text(
                    family = "sans",
                    size = strptextsize,
                    color = "grey25",
                    vjust = 1
                ),
                strip.background = element_rect(fill = "lightgrey"),
                panel.grid.minor.x = element_line(
                    color = "#E5E5E5",
                    linewidth = 0.5
                ),
                panel.grid.major.x = element_line(
                    color = "#E5E5E5",
                    linewidth = 0.5
                ),
                axis.title.x = element_text(
                    size = rel(.7),
                    face = "italic",
                    hjust = 0,
                    vjust = 0,
                    colour = "grey25"
                ),
                axis.title.y = element_text(
                    size = rel(1.2),
                    vjust = 2,
                    colour = "grey25"
                ),
                axis.line.x = element_line(
                    size = 2,
                    colour = "black",
                    linetype = "solid"
                ),
                axis.text = element_text(size = 11),
                axis.text.x = element_text(size = xaxissize),
                legend.position = "top",
                legend.key = element_rect(fill = "white"),
                legend.text = element_text(
                    family = "sans",
                    color = "grey25",
                    face = "bold",
                    size = 10
                ),
                legend.title = element_blank()
            )
        g <- g +
            scale_x_continuous(
                breaks = scales::pretty_breaks(),
                minor_breaks = function(x) {
                    seq(floor(min(x)), ceiling(max(x)), by = 1)
                }
            )
        print(g)
    }
}
