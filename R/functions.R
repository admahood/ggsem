#' Create path model plot
#'
#' This function creates a plot from a structural equation model created by either
#' the lavaan::sem or piecewiseSEM::psem functions
#'
#' @param fit The lavaan::sem - created object
#' @param title Character string with the plot title
#' @param layout_df optional data frame with the x and y coordinates for each variable in the plot
#' @param rename_nodes will you be supplying a look up table to rename the nodes
#' @param new_node_names Named vector to rename the variables
#' @param cols vector of colors for the arrows
#' @param layout select from a variety of layout algorithms. can be "auto",
#' "igraph", "manual", "dendrogram", "linear", "matrix", "treemap", "circlepack",
#' "partition", or "hive". see ?ggraph::create_layout() for more details.
#' @param alpha threshold for significance. Defaults to 0.05
#' @param exclude variables to exclude from the plot
#' @param filename filename to write the figure to
#' @param labels display standardized coefficients along the arrows?
#' @export
#'
#' @examples
#'
#'
#' fake_data <- data.frame(x =stats::runif(100, -10, 10)) |>
#'    dplyr::mutate(noise1 = rnorm(100),
#'          y = 2*x + noise1,
#'           noise2= rnorm(100),
#'           z = x + y + noise2)
#'
#' mod <- 'y ~ x
#'          z ~ x + y'
#'
#' fit <- lavaan::sem(mod, data = fake_data)
#' ggsem(fit)
#'
#' # example from `lavaan`
#' ## The industrialization and Political Democracy Example
#' ## Bollen (1989), page 332
#' library(lavaan)
#' model <- '
#'  # latent variable definitions
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'     dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'  # regressions
#'    dem60 ~ ind60
#'    dem65 ~ ind60 + dem60
#'
#'  # residual correlations
#'    y1 ~~ y5
#'   y2 ~~ y4 + y6
#'    y3 ~~ y7
#'    y4 ~~ y8
#'    y6 ~~ y8
#' '
#'
#' fit <- lavaan::sem(model, data = PoliticalDemocracy)
#' summary(fit, fit.measures = TRUE)
#' ggsem(fit)
#'
#' # example from piecewiseSEM
#' library(piecewiseSEM)
#' mod <- psem(
#' lm(rich ~ cover, data = keeley),
#' lm(cover ~ firesev, data = keeley),
#' lm(firesev ~ age, data = keeley),
#' data = keeley
#' )
#'
#' summary(mod)
#' plot(mod)
#' ggsem(mod)

ggsem <- function(fit,
                  filename,
                  title = class(fit)[1],
                  layout_df = NA,
                  rename_nodes =F,
                  cols =  c("#E41A1C", "#377EB8", "grey80"),
                  new_node_names = NA,
                  layout = "auto",
                  labels = TRUE,
                  alpha = 0.05,
                  exclude = "none") {
  requireNamespace("tidygraph")
  requireNamespace("cowplot")
  requireNamespace("ggraph")
  requireNamespace("lavaan")
  requireNamespace("ggpubr")
  # Extract standardized parameters
  if(as.character(class(fit)) == "lavaan"){
  params <- lavaan::standardizedSolution(fit) |>
    dplyr::filter(!lhs %in% "exclude",
                  !rhs %in% "exclude")
  # Edge properties

  param_edges <- params |>
    dplyr::filter(op %in% c("=~", "~", "~~"), lhs != rhs) |> #, pvalue < .10) |>
    dplyr::transmute(to = lhs,
              from = rhs,
              pvalue=pvalue,
              # sig=sig,
              val = est.std,
              type = dplyr::case_when(
                op == "~"  ~ "regression",
                op == "~~" ~ "correlation",
                TRUE ~ NA_character_)) |>
    dplyr::mutate(val = ifelse(pvalue >= alpha, 0, val))

  param_edges <- param_edges |>
    dplyr::mutate(class = val) |>
    dplyr::mutate(sign = ifelse(val>0, "+","-")) |>
    dplyr::mutate(sign = replace(sign, val == 0, "ns"))|>
    dplyr::mutate(sign = replace(sign, is.na(val), "ns")) |>
    dplyr::mutate(hlab = round(val,2)) |>
    dplyr::mutate(hlab = ifelse(hlab ==0, "", hlab)) |>
    tidyr::replace_na(list(hlab = ""))

  # Node properties
  param_nodes <- params |>
    dplyr::filter(lhs == rhs) |>
    dplyr::transmute(metric = lhs, e = est.std)}

  if(as.character(class(fit)) == "psem"){
    sum <- summary(fit)$coefficients
    param_edges <- sum |>
      dplyr::select(to = Response, from = Predictor, val = Std.Estimate,
                    pvalue = P.Value)|>
      dplyr::mutate(class = val) |>
      dplyr::mutate(sign = ifelse(val>0, "+","-")) |>
      dplyr::mutate(sign = replace(sign, val == 0, "ns"))|>
      dplyr::mutate(sign = replace(sign, is.na(val), "ns")) |>
      dplyr::mutate(hlab = round(val,2)) |>
      dplyr::mutate(hlab = ifelse(hlab ==0, "", hlab)) |>
      tidyr::replace_na(list(hlab = ""))

    param_nodes <- data.frame(metric = unique(c(sum$Response, sum$Predictor)),
                              e = NA)

  }

  # Complete Graph Object
  if(!labels) param_edges <- dplyr::mutate(param_edges, hlab = NA)

  param_graph1 <- tidygraph::tbl_graph(param_nodes,
                                       param_edges)


  # setting up the manual layout

  if(layout == "manual"){
    lut_x <- layout_df$x; names(lut_x) <- layout_df$metric
    lut_y <- layout_df$y; names(lut_y) <- layout_df$metric

    layout_man <- ggraph::create_layout(param_graph1, layout = "linear") |>
      dplyr::mutate(x = lut_x[metric],
                    y=lut_y[metric]) |>
      dplyr::select(x,y) |>
      as.data.frame()

    # applying the manual layout to the graph objects, one for each group
    layout1 <- ggraph::create_layout(param_graph1, layout = layout_man)
  }else{
    layout1 <- ggraph::create_layout(param_graph1, layout=layout)
  }

  p1_title <- title[1]

  if(rename_nodes){
    new_node_names_df <- data.frame(x = layout1$metric) |>
      dplyr::mutate(new_names = new_node_names[x])
  }


  # Plot
  p1 <- ggraph::ggraph(layout1) +
    ggraph::geom_edge_arc(ggplot2::aes(color=as.factor(sign),
                      width = abs(val),
                      label = hlab
                      #linetype = as.factor(sign)
    ),
    strength = 0.1,
    angle_calc = "along",
    vjust = -.75,
    check_overlap = FALSE,
    arrow = ggplot2::arrow(15, length = ggplot2::unit(0.15, "inches"), type = "closed"),
    label_colour = "grey20",
    end_cap = ggraph::circle(0.5, "inches"),
    start_cap = ggraph::circle(0.5, "inches")
    ) +
    ggraph::scale_edge_color_manual(name = "Direction",
                            values = cols)+
    ggraph::scale_edge_width(guide = "none", range = c(.75,2)) +
    ggraph::theme_graph(fg_text_colour = 'white',
                base_family = 'sans'
    ) +
    ggplot2::scale_x_continuous(expand = c(0.095, 0.095))+
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(color="black", fill="transparent")
    )
  if(length(title) ==1)p1 <- p1 + ggplot2::ggtitle(title[1]) else p1 <- p1 + ggplot2::ggtitle(title[1], title[2])
  if(rename_nodes){ p1 <- p1 + ggraph::geom_node_text(ggplot2::aes(label = new_node_names_df$new_names),
                                              fontface = "bold"#,
                                              #nudge_y = 0.05
                                              )}else{
                                                p1<- p1+
                                                  ggraph::geom_node_text(
                                                    ggplot2::aes(label = metric),
                                                    fontface = "bold"#,#family = 'Times',  size = 10, #node names
                                                                       # nudge_y = 0.05
                                                    )
                                              }
  return(p1)
}

#' Create a random layout
#'
#' This is the first step to creating a manual layout. Change the x and y values
#' as appropriate (using dplyr::mutate(), for example) to make things look just
#' right.
#' @param fit output of lavaan::sem()
#' @export
#' @returns a data frame with columns x and y for positions on the plot, and metric for the name of the variable
random_layout <- function(fit){
  requireNamespace("dplyr")
  requireNamespace("lavaan")
  requireNamespace("tibble")

  prep <- lavaan::standardizedsolution(fit) |>
    dplyr::filter(lhs == rhs) |>
    dplyr::transmute(metric = lhs, e = est.std)

  return(prep |>
    dplyr::mutate(x=stats::runif(nrow(prep), min=-1, max=1),
                  y=stats::runif(nrow(prep), min=-1, max=1)) |>
    dplyr::select(-e) |>
    tibble::as_tibble())
}

#' Make a nice-looking legend
#'
#' You can add the legend to a path model plot using ggpubr::ggarrange(), for
#' example.
#' @param scols the colors of thes
#' @param signs the names associated with the colors
make_legend <- function(scols = c("#377EB8", "#E41A1C", "grey80"),
                        signs = c("positive", "negative", "neutral")){
  requireNamespace("cowplot")
  requireNamespace("tibble")
  requireNamespace("ggplot2")
  dummy <- tibble::tibble(val = c(0.9, 0, -0.8), class = scols, Relationship = signs)
  names(scols) = signs
  cowplot::get_legend(
    ggplot2::ggplot(dummy, ggplot2::aes(x=val, y=val)) +
      ggplot2::geom_line(ggplot2::aes(color = Relationship), lwd=3) +
      ggplot2::scale_color_manual(values = scols)+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.key.size = ggplot2::unit(1.5,"cm"),
            legend.text = ggplot2::element_text(size=15),
            legend.title = ggplot2::element_text(size=15),
            legend.background = ggplot2::element_rect(fill="transparent"))+
      ggplot2::guides(color=ggplot2::guide_legend(ncol=1))
  )
}
