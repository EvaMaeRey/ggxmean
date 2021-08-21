#' Title
#'
#' @param trials
#' @param n
#' @param nrow
#' @param ncol
#' @param scales
#' @param shrink
#' @param strip.position
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1323)
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point(color = "olivedrab4") +
#'   geom_lm() +
#'   facet_sample(n_sampled = 8) +
#'   labs(title = "Random Sample of 8 from population")
facet_sample <- function(n = 16, n_sampled = 5, nrow = NULL, ncol = NULL,
                            scales = "fixed", shrink = TRUE, strip.position = "top",
                         seed = sample(2000:3000, 1)) {

  facet <- facet_wrap(~.bootstrap, nrow = nrow, ncol = ncol, scales = scales,
                      shrink = shrink, strip.position = strip.position)
  facet$params$n <- n
  facet$params$n_sampled <- n_sampled
  facet$params$seed <- seed

  ggproto(NULL, FacetSample,
          shrink = shrink,
          params = facet$params
  )
}

FacetSample <- ggproto("FacetSample", FacetWrap,
                          compute_layout = function(data, params) {
                            id <- seq_len(params$n)

                            dims <- wrap_dims(params$n, params$nrow, params$ncol)
                            layout <- data.frame(PANEL = factor(id))

                            if (params$as.table) {
                              layout$ROW <- as.integer((id - 1L) %/% dims[2] + 1L)
                            } else {
                              layout$ROW <- as.integer(dims[1] - (id - 1L) %/% dims[2])
                            }
                            layout$COL <- as.integer((id - 1L) %% dims[2] + 1L)

                            layout <- layout[order(layout$PANEL), , drop = FALSE]
                            rownames(layout) <- NULL

                            # Add scale identification
                            layout$SCALE_X <- if (params$free$x) id else 1L
                            layout$SCALE_Y <- if (params$free$y) id else 1L

                            cbind(layout, .bootstrap = id)
                          },
                          map_data = function(data, layout, params) {
                            if (is.null(data) || nrow(data) == 0) {
                              return(cbind(data, PANEL = integer(0)))
                            }
                            set.seed(params$seed)
                            # n_samples <- round(nrow(data) * params$prop)
                            n_sampled <- params$n_sampled
                            new_data <- lapply(seq_len(params$n), function(i) {
                              cbind(data[sample(nrow(data), n_sampled), , drop = FALSE], PANEL = i)
                            })
                            do.call(rbind, new_data)
                          }
)
