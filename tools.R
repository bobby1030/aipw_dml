build_formula <- function(dependent, independent) {
    return(
        reformulate(
            independent, response = dependent
        )
    )
}