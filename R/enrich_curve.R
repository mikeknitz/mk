# Updated 2024-11-27
# Depends on mk::thm()
# Depends on mk::thm_bg()
# TODO: Add more customizations to placement of grobs, etc.

#' @title Enrichment curve for GSEA results
#' @description Take "gseaResult" and return enrichment curve plot
#' @returns A ggplot object
#' @examples
#' \dontrun{
#' mk::enrich_curve(gene.set.id, gsea.res) +
#'  mk::size(5, 2.5)
#' mk::enrich_curve(gene.set.id, gsea.res, plot.stats = FALSE) +
#'  mk::size(5, 2.5)
#' }
#' @param id one gene set id, from `ID` column of gsea.res
#' @param gsea.res GSEA result object of class "gseaResult", e.g., output of `clusterProfiler::GSEA()`
#' @param title,subtitle,caption ggplot titles, etc.
#' @param plot.stats Whether to add NES and test statistic to plot, default TRUE
#' @param sig For plot.stats = TRUE, which significance to use? default "qvalue"
#' @param base.size pass argument to mk::thm() to change default sizes of text elements in plot, default NULL to take the default value in mk::thm()
#' @param ... Arguments merged to enrichplot::gseaplot()
#' @export

enrich_curve <- function(

  # One gene set id, from `ID` column of gsea.res
  id,

  # GSEA results, e.g., output of clusterProfiler::GSEA()
  # Should be of class "gseaResult"
  gsea.res,
  
  # Titles, etc.
  title = id,
  subtitle = NULL,
  caption = NULL,

  # Add statistics to the plot?
  plot.stats = TRUE,

  # For plot.stats = TRUE, which significance to use?
  sig = c("qvalue", "p.adjust", "pvalue"),
  
  # Pass argument to mk::thm(),
  base.size = 16,

  # Arguments merged to enrichplot::gseaplot()
  ...

) {


  # ----------------------------------------------------------------------------
  # Checks
  # ----------------------------------------------------------------------------

  if (!inherits(gsea.res, "gseaResult")) {
    stop("\"gsea.res\" should be of class \"gseaResult\"")
  }

  # ----------------------------------------------------------------------------
  # Get single entry according to argument `id`
  # ----------------------------------------------------------------------------
  
  entry <- subset(gsea.res@result, ID %in% id)
  if (nrow(entry) != 1) stop('"id" column not found in "gsea.res@result"')

  # ----------------------------------------------------------------------------
  # Get stats if plot.stats = TRUE
  # ----------------------------------------------------------------------------
  
  if (plot.stats) {

    nes <- scales::label_number(0.0001)(entry$NES)
    nes <- paste0("NES = ", nes)
    es <- scales::label_number(0.0001)(entry$enrichmentScore)
    es <- paste0("ES = ", es)

    if (sig[1] %in% "qvalue") {
      
      if (entry$qvalue < 0.0001) {
        sig.val <- "q < 0.0001"
      } else {
        sig.val <- scales::label_number(0.0001)(entry$qvalue)
        sig.val <- paste0("q = ", sig.val)
      }

    } else if (sig[1] %in% "p.adjust" ) {

      if (entry$p.adjust < 0.0001) {
        sig.val <- "p.adj < 0.0001"
      } else {
        sig.val <- scales::label_number(0.0001)(entry$p.adjust)
        sig.val <- paste0("p.adj = ", sig.val)
      }

    } else if (sig[1] %in% "pvalue") {
      
      if (entry$pvalue < 0.0001) {
        sig.val <- "p < 0.0001"
      } else {
        sig.val <- scales::label_number(0.0001)(entry$pvalue)
        sig.val <- paste0("p = ", sig.val)
      }
      
    } else {
      stop(paste0(
        '"sig" must be one of: "qvalue", "p.adjust", "pvalue"'
      ))
    }

  }

  # ----------------------------------------------------------------------------
  # Define grob objects if plot.stats = TRUE
  # ----------------------------------------------------------------------------
  # Goes in upper right if NES >= 0
  # Goes in lower left if NES < 0
  
  if (plot.stats) {
    if (entry$NES >= 0) {
      anno <- paste(nes, sig.val, sep = "\n")
      anno.grob <- grid::textGrob(anno, x = 0.985, y = 0.96,
        just = c("right", "top"))
      anno.width <- grid::grobWidth(anno.grob)
      anno.height <- grid::grobHeight(anno.grob)
      anno.back <- grid::roundrectGrob(
        x = 0.99, y = 0.975, just = c("right", "top"),
        width = anno.width + grid::unit(5, units = "points"),
        height = anno.height + grid::unit(9, units = "points"),
        gp = grid::gpar(fill = "#A6BDDB")
        # gp = grid::gpar(fill = "#C6DBEF")
      )
    } else if (entry$NES < 0) {
      anno <- paste(nes, sig.val, sep = "\n")
      anno.grob <- grid::textGrob(anno, x = 0.015, y = 0.045,
        just = c("left", "bottom"))
      anno.width <- grid::grobWidth(anno.grob)
      anno.height <- grid::grobHeight(anno.grob)
      anno.back <- grid::roundrectGrob(
        x = 0.01, y = 0.02, just = c("left", "bottom"),
        width = anno.width + grid::unit(5, units = "points"),
        height = anno.height + grid::unit(9, units = "points"),
        gp = grid::gpar(fill = "#A6BDDB")
        # gp = grid::gpar(fill = "#C6DBEF")
      )
    } else {
      stop("Internal error 2411332105245")
    }
  }

  # ----------------------------------------------------------------------------
  # Plot
  # ----------------------------------------------------------------------------
  
  # Call enrichplot::gseaplot() and merge arguments from ...
  args.gseaplot <- list(
    x = gsea.res,
    geneSetID = id,
    by = "runningScore",
    title = id
  )
  p <- do.call(enrichplot::gseaplot, utils::modifyList(args.gseaplot, list(...)))

  # Add grobs if plot.stats = TRUE
  if (plot.stats) {
    p <- p + 
      ggplot2::annotation_custom(anno.back) +
      ggplot2::annotation_custom(anno.grob)
  }
  
  # Themes, etc.
  p <- p +
    thm(base.size = base.size) +
    thm_bg(panel = "white") +
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "lightgrey")) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # ----------------------------------------------------------------------------

  return(p)
  
  # avoid R CMD CHECK "no visible binding" messages
  ID <- NULL

}