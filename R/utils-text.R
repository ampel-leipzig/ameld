#' Format enumeration
#'
#' Helper function to format an enumeration.
#'
#' @param ... one or more objects, converted to characters
#' @param conjunction `character`, final conjunction
#' @param oxford `logical`, oxford comma?
#' @importFrom utils head tail
#' @export
#' @examples
#' enum(letters[1:2])
#' enum(letters[1:3])
enum <- function(..., conjunction = "and", oxford = TRUE) {
    words <- vapply(..., as.character, NA_character_)

    if (length(words) == 1L)
        return(words)

    if (oxford && length(words) > 2L)
        conjunction <- paste0(", ", conjunction, " ")
    else
        conjunction <- paste0(" ", conjunction, " ")

    paste0(
        paste0(head(words, -1L), collapse = ", "), conjunction, tail(words, 1L)
    )
}
