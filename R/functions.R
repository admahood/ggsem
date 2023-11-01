#' Create path model plot
#'
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
#' @export
#'
#' @examples
#'
#' # library(tidyverse)
#' # library(lavaan)
#' # library(gglavaan)
#' # library(ggraph)
#'
#' #fake_data <- data.frame(x = runif(100, -10, 10)) %>%
#' #   mutate(noise1 = rnorm(100),
#' #         y = 2*x + noise1,
#' #          noise2= rnorm(100),
#' #          z = x + y + noise2)
#'
#' #mod <- 'y ~ x
#' #         z ~ x + y'
#'
#' #fit <- sem(mod, data = fake_data)
#'
#' #ggsem1(fit = fit)

ggsem <- function(fit, filename,
                  title="Path Model",
                  layout_df = NA,
                  rename_nodes =F,
                  cols =  c("#E41A1C", "#377EB8", "grey80"),
                  new_node_names = NA,
                  layout = "auto",
                  alpha = 0.05,
                  exclude = "none") {
  requireNamespace("tidygraph")
  requireNamespace("cowplot")
  requireNamespace("ggraph")
  requireNamespace("lavaan")
  requireNamespace("ggpubr")
  requireNamespace("classInt")
  requireNamespace("ggtext")
  # Extract standardized parameters
  params <- lavaan::standardizedSolution(fit) %>%
    dplyr::filter(!lhs %in% "exclude",
                  !rhs %in% "exclude")
  # Edge properties

  param_edges <- params %>%
    dplyr::filter(op %in% c("=~", "~", "~~"), lhs != rhs) %>% #, pvalue < .10) %>%
    dplyr::transmute(to = lhs,
              from = rhs,
              pvalue=pvalue,
              # sig=sig,
              val = est.std,
              type = dplyr::case_when(
                op == "~"  ~ "regression",
                op == "~~" ~ "correlation",
                TRUE ~ NA_character_)) %>%
    dplyr::mutate(val = ifelse(pvalue >= alpha, 0, val))

  param_edges <- param_edges %>%
    dplyr::mutate(class = val) %>%
    dplyr::mutate(sign = ifelse(val>0, "+","-")) %>%
    dplyr::mutate(sign = replace(sign, val == 0, "ns"))%>%
    dplyr::mutate(sign = replace(sign, is.na(val), "ns")) %>%
    dplyr::mutate(hlab = round(val,2)) %>%
    dplyr::mutate(hlab = ifelse(hlab ==0, "", hlab)) %>%
    tidyr::replace_na(list(hlab = ""))

  # Node properties
  param_nodes <- params %>%
    dplyr::filter(lhs == rhs) %>%
    dplyr::transmute(metric = lhs, e = est.std)

  # Complete Graph Object
  param_graph1 <- tidygraph::tbl_graph(param_nodes,
                                       param_edges)
  # setting up the manual layout

  if(layout == "manual"){
    lut_x <- layout_df$x; names(lut_x) <- layout_df$metric
    lut_y <- layout_df$y; names(lut_y) <- layout_df$metric

    layout_man <- ggraph::create_layout(param_graph1, layout = "linear") %>%
      dplyr::mutate(x = lut_x[metric],
                    y=lut_y[metric]) %>%
      dplyr::select(x,y) %>%
      as.data.frame()

    # applying the manual layout to the graph objects, one for each group
    layout1 <- ggraph::create_layout(param_graph1, layout = layout_man)
  }else{
    layout1 <- ggraph::create_layout(param_graph1, layout=layout)
  }

  p1_title <- title[1]

  if(rename_nodes){
    new_node_names_df <- data.frame(x = layout1$metric) %>%
      dplyr::mutate(new_names = new_node_names[x])
  }

  # Plot
  p1 <- ggraph(layout1) +
    geom_edge_arc(aes(color=as.factor(sign),
                      width = abs(val),
                      label= hlab
                      #linetype = as.factor(sign)
    ),
    strength = 0.1,
    angle_calc = "along",
    vjust = -.75,
    check_overlap = FALSE,
    arrow = arrow(25, length = unit(0.3, "inches"), type = "open"),
    label_colour = "grey20",
    end_cap = circle(0.5, "inches"),
    start_cap = circle(0.5, "inches")
    )+
    scale_edge_color_manual(name = "Direction",
                            values = cols)+
    scale_edge_width(guide = "none", range = c(.5,2)) +
    theme_graph(fg_text_colour = 'white',
                base_family = 'Arial'#,base_family = 'Times'
    ) +
    scale_x_continuous(expand = c(0.095, 0.095))+
    theme(#plot.title = element_text(size = 30),
      legend.position = "none",
      plot.background = element_rect(color="black", fill="transparent")
    )
  if(length(title) ==1)p1 <- p1 + ggtitle(title[1]) else p1 <- p1 + ggtitle(title[1], title[2])
  if(rename_nodes){ p1 <- p1 + geom_node_text(aes(label = new_node_names_df$new_names),
                                              fontface = "bold",
                                              nudge_y = 0.05)}else{
                                                p1<- p1+ geom_node_text(aes(label = metric),fontface = "bold",#family = 'Times',  size = 10, #node names
                                                                        nudge_y = 0.05)
                                              }
  return(p1)
}

#' Get Nodes
#'
#' Helper function for random_layout
#'
#' @export
get_nodes <- function(fit){
  lavaan::standardizedsolution(fit) %>%
    filter(lhs == rhs) %>%
    transmute(metric = lhs, e = est.std)
}

#' Create a random layout
#'
#' This is the first step to creating a manual layout. Change the x and y values
#' as appropriate (using dplyr::mutate(), for example) to make things look just
#' right.
random_layout <- function(fit){
  get_nodes(fit) %>%
    dplyr::mutate(x=runif(nrow(.), min=-1, max=1),
                  y=runif(nrow(.), min=-1, max=1)) %>%
    dplyr::select(-e) %>%
    as_tibble()
}

#' Make a nice-looking legend
#'
#' You can add the legend to a path model plot using ggpubr::ggarrange(), for
#' example.
make_legend <- function(scols = c("#377EB8", "#E41A1C", "grey80"),
                        signs = c("positive", "negative", "neutral")){
  dummy <- tibble(val = c(0.9, 0, -0.8), class = scols, Relationship = signs)
  names(scols) = signs
  get_legend(
    ggplot(dummy, aes(x=val, y=val)) +
      geom_line(aes(color = Relationship), lwd=3) +
      scale_color_manual(values = scols)+
      theme_classic()+
      theme(legend.key.size = unit(1.5,"cm"),
            legend.text = element_text(size=15),
            legend.title = element_text(size=15),
            legend.background = element_rect(fill="transparent"))+
      guides(color=guide_legend(ncol=1))
  )
}
