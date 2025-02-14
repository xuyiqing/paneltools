#' paneltools
#'
#' A Toolkit for Panel Data Analysis
#'
#'
#' @name paneltools-package
#' @aliases paneltools-package 
#'
#' @author Hongyi Jiang hyjiang2017@nsd.pku.edu.cn, Peking University
#' Ziyi Liu ziyiliu2020@gmail.com, University of Chicago 
#' Yiqing Xu yiqingxu@stanford.edu, Stanford University 
#' Zhongyu Yin zhongyuy@stanford.edu, Stanford University
#' 
#' @details This package provides a set of commonly used functions for panel data analysis, 
#' including data preprocessing, obtaining relative period to treatment, event study plot visualization,
#' match and its estimates as well as inference
#' 
#' @keywords package
#' 
#' @importFrom tidyr complete
#' @importFrom dplyr %>% group_by mutate arrange syms
#' @importFrom stats na.omit quantile sd var cov pchisq lm as.formula median pnorm predict qnorm reshape dnorm pf rbinom loess
#' @importFrom ggplot2 ggplot_build geom_boxplot geom_density geom_tile
#' geom_point labs theme_bw scale_fill_manual
#' geom_hline geom_line geom_ribbon geom_vline geom_text
#' ggplot coord_cartesian coord_flip element_text
#' element_blank scale_x_discrete scale_x_continuous scale_y_continuous
#' scale_linetype_manual scale_size_manual theme geom_area
#' scale_colour_manual aes annotate ggtitle geom_rect
#' scale_color_discrete scale_color_manual ggplotGrob
#' guide_legend margin geom_jitter geom_pointrange
#' guides xlab ylab element_rect geom_errorbar theme_minimal element_line coord_fixed
NULL