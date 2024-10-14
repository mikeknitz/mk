#' @name save_plot
#' @rdname save_plot
#' @title Plot save wrapper function
#' @description Quickly save plots
#' @examples
#' \dontrun{
#' # See capabilities available
#' save_plot(print.capabilities = TRUE)
#' 
#' # sp() is an alias for save_plot()
#' sp(print.capabilties = TRUE)
#' 
#' # Quick plot to default file = "Rplot.png"
#' sp(p, 5, 3)
#' 
#' # Type is inferred by file extension
#' sp(p, 5, 3, file = "Rplot.pdf")
#' 
#' # Explicitly define dpi with dpi argument to
#'   # avoid auto.dpi argument behavior
#' save_plot(
#'   p      = p,
#'   width  = 7,
#'   height = 5,
#'   units  = "in",
#'   file   = "myplot.tiff",
#'   dpi    = 800
#' )
#' 
#' # If multiple devices capable of a file type, can
#'   # explicitly define device, e.g., cairo_pdf instead
#'   # of default Cairo::Cairo(type = "pdf")
#' sp(p, 5, 3, file = "Rplot.pdf", device = "cairo_pdf")
#' 
#' # Save with an interesting file extension
#' sp(p, 5, 3, file = "Rplot.hello", type = "png")
#' 
#' }
#' @details ...
#' @returns invisible(NULL)
#' @param p plot object invoked with plot()
#' @param width width (inches by default), default = 7
#' @param height height (inches by default), default = 5
#' @param file path to file WITH extension, default = "Rplot.png"
#' @param type image type output. default NULL = infer from `file` extension. Or one of: bmp, jpeg, pdf, png, svg, tiff
#' @param units units for width and height, ONLY current option is inches (default units = "in")
#' @param device default NULL = choose device based on those available. Use `print.capabilities = TRUE` to see valid options. Depending on installation, one of: bmp, Cairo, cairo_pdf, jpeg, png, svg, tiff. Cairo device (Cairo package) supports multiple types
#' @param bg background color <chr>, default "transparent", or a R color or hex value <chr>
#' @param dpi <numeric> dots per inch, default 300
#' @param auto.dpi <numeric> default 300. If argument `dpi` not supplied, set dpi such that an image scaled to 13.5 inch width would be approx this many dpi. Ignored if not using a rasterized format (png, jpeg, tiff, bmp). NULL = turn off this functionality.
#' @param print.capabilities TRUE = Don't plot, just return dataframe of supported and installed types/devices on this system. default = FALSE.
#' @param dpi.CairoPDF,dpi.CairoSVG dpi arguments when device = "Cairo" and type = "pdf" or "svg". default = "auto". See details.
#' @param compression compression used for device = "tiff". default = "lzw". Otherwise file types are very large for compression = "none".
#' @param quality quality used for device = "jpeg". default = 75 <numeric>.
#' @param silent silence messages from this function. Does not silences messages, warnings, or errors from `plot()`.
#' @param ... Arguments passed to selected device 
#' @export

save_plot <- function(
  # plot object
  p,

  # Width and height
  width = 7, height = 5,

  # Path with extension
  file = "Rplot.png",

  # File type (png, pdf, tiff, etc.)
  type = NULL,

  # Units (only inches supported)
  units = "in",

  # Plotting device from list
  device = NULL,

  # Background
  bg = "transparent",

  # dots per inch
  dpi = 300,

  # Set dpi such that scaled to 13.5 inches, apparent dpi is 300 (approx)
  auto.dpi = 300,

  # Just return dataframe of supported devices / types
  print.capabilities = FALSE,

  # dpi arguments for device = Cairo and type = "pdf" or "svg"
  dpi.CairoPDF = "auto",
  dpi.CairoSVG = "auto",

  # compression argument used for device = "tiff"
  compression = "lzw",

  # quality argument used for device = "jpeg"
  quality = 75,

  # Silence function messages
  # Does not silence messages/warnings/errors associated with plot(p)
  silent = FALSE,

  # TODO:
  # Override silent and also silence messages/warnings/errors
  # from calling plot() on supplied p
  # silent.all = FALSE,

  # Arguments passed to selected device
  ...

) {

  if (units != "in") stop("Only units = \"", "in", "\" currently supported")
  missing.dpi <- missing(dpi)

  if (missing(p) && !print.capabilities) stop("No plot object `p` supplied")

  # ----------------------------------------------------------------------------
  # Check capabilities of system
  # ----------------------------------------------------------------------------
  # `caps` dataframe here used to select a device if device == NULL

  r.packages <- utils::installed.packages()

  # caps dataframe to store devices installed and their capabilities
  caps <- data.frame(device = character(), type = character())

  # Add supported Cairo types
  if ("Cairo" %in% r.packages) {
    cairo.caps <- Cairo::Cairo.capabilities()
    cairo.caps <- names(cairo.caps[cairo.caps])
    cairo.caps <- data.frame(device = "Cairo", type = cairo.caps)
    cairo.caps <- subset(
      cairo.caps,
      type %in% c("png", "pdf", "svg")
    )
    caps <- rbind(caps, cairo.caps)
  }

  # Add other support
  other.caps <- capabilities(c(
    "jpeg", # jpeg() function
    "png",  # png() function
    "tiff"  # tiff() function
  ))
  other.caps <- names(other.caps[other.caps])
  other.caps <- data.frame(device = other.caps, type = other.caps)
  caps <- rbind(caps, other.caps)

  # Support for svg(), cairo_pdf(), bmp()
  # Support for type = "cairo" in jpeg(), png(), tiff(), bmp()
  if (capabilities("cairo")) {
    cairo.type.cap <- TRUE
    caps <- rbind(caps, data.frame(
      device = c("svg", "cairo_pdf", "bmp"),
      type   = c("svg", "pdf",       "bmp")
    ))
  } else {
    cairo.type.cap <- FALSE
  }

  # Add column for function used
  caps$`function` <- ifelse(caps$device %in% "Cairo", "Cairo::Cairo()", "NA")
  caps$`function` <- ifelse(caps$device %in% "jpeg", "grDevices::jpeg()", caps$`function`)
  caps$`function` <- ifelse(caps$device %in% "png", "grDevices::png()", caps$`function`)
  caps$`function` <- ifelse(caps$device %in% "tiff", "grDevices::tiff()", caps$`function`)
  caps$`function` <- ifelse(caps$device %in% "svg", "grDevices::svg()", caps$`function`)
  caps$`function` <- ifelse(caps$device %in% "cairo_pdf", "grDevices::cairo_pdf()", caps$`function`)
  caps$`function` <- ifelse(caps$device %in% "bmp", "grDevices::bmp()", caps$`function`)
  caps <- caps[, c("type", "device", "function")]

  # Arrange caps by (type,device) with Cairo device preferred
  caps$device <- factor(caps$device, levels = c(
    "Cairo",
    base::setdiff(caps$device, "Cairo")
  ))
  caps <- caps[order(caps$type, caps$device), ]
  caps$device <- as.character(caps$device)
  row.names(caps) <- NULL

  # caps$priority <- seq_len(nrow(caps))

  if (print.capabilities) {
    return(caps)
  }

  # ----------------------------------------------------------------------------
  # Check `type`, infer if needed, compare to supported types
  # ----------------------------------------------------------------------------

  supported.types <- c("png", "pdf", "svg", "tiff", "jpeg", "bmp")
  has.extension <- grepl("\\.", file)

  if (is.null(type)) {
    if (!has.extension) {
      stop("No extension found in `file`, and no `type` provided")
    }
    type <- sub("^.+\\.(.+?)$", "\\1", file)
    type <- tolower(type)
    if (!(type %in% supported.types)) {
      stop(paste0(
        "`type` not provided, and type parsed from filename could not ",
        "be found in supported filetypes"
      ))
    }
  } else {
    type <- tolower(type)
  }
  if (!(type %in% supported.types)) {
    stop("Provided `type` could not be found in supported filetypes")
  }

  # ----------------------------------------------------------------------------
  # Set dpi according to auto.dpi logic
  # ----------------------------------------------------------------------------

  if (!is.null(auto.dpi)) {
    if (missing.dpi) {
      dpi <- round((13.5 / width) * auto.dpi)
    }
  }

  # ----------------------------------------------------------------------------
  # If user selects device, see if valid
  # ----------------------------------------------------------------------------

  # if device chosen, see if capable
  if (!is.null(device)) {
    if (!(device %in% caps$device)) {
      stop(paste0("device: \"", device, "\" either not supported or not installed"))
    }
    if (!(type %in% caps[caps$device %in% device, "type"])) {
      stop(paste0(
        "type: \"", type, "\" not supported for ",
        "device: \"", device, "\""
      ))
    }
  }

  # ----------------------------------------------------------------------------
  # If device = NULL, select the first device capable for type chosen
  # ----------------------------------------------------------------------------

  if (is.null(device)) {
    which.row <- which(caps$type %in% type)
    if (length(which.row) == 0) {
      stop(paste0(
        "No supported and installed device has support for type: \"", type, "\""
      ))
    }
    device <- caps[which.row[1], "device"]
  }

  # ----------------------------------------------------------------------------
  # Summary info function
  # ----------------------------------------------------------------------------

  func.using <- caps[
    which((caps$type %in% type) & (caps$device %in% device)),
  ]$`function`
  if (length(func.using) != 1) warning("internal error in messaging")

  summary_info <- function(file, type, device, func.using, width, units, height, dpi) {
    message(paste0(
      "\n",
      "Plotting...\n\n",
      "file     =  ", file, "\n",
      "type     =  ", type, "\n",
      "device   =  ", device, "\n",
      "function =  ", func.using, "\n",
      "width    =  ", width, " ", units, "\n",
      "height   =  ", height, " ", units, "\n",
      "dpi      =  ", dpi, "\n"
    ))
    return(invisible(NULL))
  }

  # ----------------------------------------------------------------------------
  # device "Cairo"
  # ----------------------------------------------------------------------------

  if (device == "Cairo") {

      args.dev <- list(
        width = width,
        height = height,
        units = units,
        dpi = dpi,
        file = file,
        type = type,
        bg = bg
      )

      # For pdf, use dpi.CairoPDF
      if (type == "pdf") {
        args.dev$dpi <- dpi.CairoPDF
        # if (!silent) {
        #   message(paste0(
        #     "For pdf output, using dpi = ", dpi.CairoPDF
        #   ))
        # }
      }

      # For svg, use dpi.CairoSVG
      if (type == "svg") {
        args.dev$dpi <- dpi.CairoSVG
        # if (!silent) {
        #   message(paste0(
        #     "For svg output, using dpi = ", dpi.CairoSVG
        #   ))
        # }
      }

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      # Summary info
      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, args.dev$dpi)
      }

      do.call(Cairo::Cairo, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  # ----------------------------------------------------------------------------
  # device "tiff"
  # ----------------------------------------------------------------------------

  if (device == "tiff") {

      args.dev <- list(
        filename = file,
        width = width,
        height = height,
        units = units,
        bg = bg,
        res = dpi,
        compression = compression
      )

      # if (cairo.type.cap) {
      #   args.dev$type <- "cairo"
      # }

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      # Summary info
      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, args.dev$res)
      }

      do.call(grDevices::tiff, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  # ----------------------------------------------------------------------------
  # device "jpeg"
  # ----------------------------------------------------------------------------

  if (device == "jpeg") {

      args.dev <- list(
        filename = file,
        width = width,
        height = height,
        units = units,
        quality = quality,
        bg = bg,
        res = dpi
      )

      # if (cairo.type.cap) {
      #   args.dev$type <- "cairo"
      # }

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      # Summary info
      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, args.dev$res)
      }

      do.call(grDevices::jpeg, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  # ----------------------------------------------------------------------------
  # device "png"
  # ----------------------------------------------------------------------------

  if (device == "png") {

      args.dev <- list(
        filename = file,
        width = width,
        height = height,
        units = units,
        bg = bg,
        res = dpi
      )

      # if (cairo.type.cap) {
      #   args.dev$type <- "cairo"
      # }

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      # Summary info
      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, args.dev$res)
      }

      do.call(grDevices::png, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  # ----------------------------------------------------------------------------
  # device "svg"
  # ----------------------------------------------------------------------------

  if (device == "svg") {

      args.dev <- list(
        filename = file,
        width = width,
        height = height,
        bg = bg
      )

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      # Summary info
      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, "NA")
      }

      do.call(grDevices::svg, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  # ----------------------------------------------------------------------------
  # device "cairo_pdf"
  # ----------------------------------------------------------------------------

  if (device == "cairo_pdf") {

      args.dev <- list(
        filename = file,
        width = width,
        height = height,
        bg = bg
      )

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, "NA")
      }

      do.call(grDevices::cairo_pdf, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  # ----------------------------------------------------------------------------
  # device "bmp"
  # ----------------------------------------------------------------------------

  if (device == "bmp") {

      args.dev <- list(
        filename = file,
        width = width,
        height = height,
        units = units,
        bg = bg,
        res = dpi
      )

      # if (cairo.type.cap) {
      #   args.dev$type <- "cairo"
      # }

      # User argument overrides
      args.dev <- utils::modifyList(args.dev, as.list(list(...)))

      if (!silent) {
        summary_info(file, type, device, func.using, width, units, height, args.dev$res)
      }

      do.call(grDevices::bmp, args.dev)
      try(plot(p))
      grDevices::dev.off()
      return(invisible(NULL))

  }

  stop("internal error, no plot output")

}

#' @rdname save_plot
#' @export

sp <- save_plot
