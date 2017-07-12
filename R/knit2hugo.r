##' knitr output hook for hugo highlight shortcode.
##'
##' Output hook suitable for use with hugo's syntax highlighter shortcode
##'
##' Put render_hugo() in the first chunk of your document to make
##' knitr output hugo-friendly {{< highlight >}} shortcode code fences
##' instead of markdown's default triple-backticks.
##'
##' @title render hugo-friendly markdown from knitr
##' @return Output hook
##' @author Kieran Healy
##' @export
render_hugo <- function(extra="") {
    require(knitr)

    render_markdown(TRUE)
    hook.r <- function(x, options) {
        paste0("\n\n{{< highlight ", tolower(options$engine),
               if (extra !="") " ", extra, " >}}\n", x, "\n{{< /highlight >}}\n\n")
    }

    hook.t <- function(x, options) { paste0("\n\n{{< highlight text >}}\n",
                                            x, "{{< /highlight >}}\n\n")
    }

    knit_hooks$set(source = function(x, options) {
        x <- paste(knitr:::hilight_source(x, "markdown", options), collapse = "\n")
        hook.r(x, options)
    }, output = hook.t, warning = hook.t, error = hook.t, message = hook.t)
}
