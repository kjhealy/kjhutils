##' Update installed github packages
##'
##' Update installed github packages
##' @title update_github
##' @return none
##' @author https://stackoverflow.com/questions/32538052/update-all-packages-from-github
##' @export
update_github <- function() {
  # check/load necessary packages
  # devtools package
  if (!("package:devtools" %in% search())) {
    tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
    on.exit(detach("package:devtools", unload=TRUE))
  }

  pkgs <- installed.packages(fields = "RemoteType")
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]

  print(github_pkgs)
  lapply(github_pkgs, function(pac) {
    message("Updating ", pac, " from GitHub...")

    repo = packageDescription(pac, fields = "GithubRepo")
    username = packageDescription(pac, fields = "GithubUsername")

    install_github(repo = paste0(username, "/", repo))
  })
}
