#' ParseArguments
#'
#' Parse command-line arguments
#'
#' @param accepted.switches Named list of switches. The names of
#'   accepted.switches will be used as the names of the parsed arguments.
#' @param command.args Character vector of command arguments. Defaults to
#'   \code{commandArgs(trailingOnly = TRUE)}, \emph{i.e.} the trailing
#'   command-line arguments.
#'
#' @return Returns a named list of command-line arguments. The names are taken
#'   from \code{names(accepted.switches)}.
#'
#' @examples
#'
#' ParseArguments(list(output = "-o", input = "-i"),
#'   c("-i", "output/covar_analysis/post_recal_data.table",
#'     "-i", "output/covar_analysis/recal_data.table",
#'     "-o", "output/covar_analysis/recalibration_plots.pdf")))
#'
#' @export
#'

ParseArguments <- function(
  accepted.switches,
  command.args = commandArgs(trailingOnly = TRUE)) {

  # check for unexpected options
  for (command.argument in command.args) {
    if (grepl("^-", command.argument) &
        ! command.argument %in% accepted.switches) {
      stop(paste("Invalid option:", command.argument))
    }
  }

  # initialise the output list
  parsed.arguments <- accepted.switches
  for (argument in names(accepted.switches)) {
    parsed.arguments[[argument]] <- list()
  }

  # get the positions of the flags
  switch.i <- which(command.args %in% accepted.switches)

  # parse the expected options
  for (i in switch.i){

    # check for empty argument
    if (i + 1 > length(command.args)) {
      stop(paste("Missing argument to option ", command.args[[i]]))
    } else if (grepl("^-", command.args[[i + 1]])) {
      stop(paste("Missing argument to option ", command.args[[i]]))
    } else if (command.args[[i + 1]] == "") {
      stop(paste("Missing argument to option ", command.args[[i]]))

    # add argument to parsed.arguments
    } else {
      switch <- command.args[[i]]
      argument.name <- names(accepted.switches[accepted.switches == switch])
      parsed.arguments[[argument.name]] <- unlist(list(
        parsed.arguments[[argument.name]],
        command.args[[i + 1]]))
    }
  }

  # return parsed arguments
  parsed.arguments
}
