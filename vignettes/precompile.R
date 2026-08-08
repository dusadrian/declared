package_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
description <- file.path(package_root, "DESCRIPTION")

if (!file.exists(description)) {
    stop("Run this script from the declared package root.", call. = FALSE)
}

package_name <- unname(read.dcf(description, fields = "Package")[1, 1])
if (!identical(package_name, "declared")) {
    stop("The current directory is not the declared package root.", call. = FALSE)
}

vignette_dir <- file.path(package_root, "vignettes")
names <- c("a_Declared_solution", "b_Added_value", "c_Weighting")
sources <- file.path(vignette_dir, paste0(names, ".Rmd.orig"))
support <- file.path(vignette_dir, c("declared.css", "declared.theme"))

missing <- c(sources, support)[!file.exists(c(sources, support))]
if (length(missing) > 0) {
    stop(
        "Missing vignette input: ",
        paste(basename(missing), collapse = ", "),
        call. = FALSE
    )
}

build_root <- tempfile(pattern = "declared_vignettes_")
build_vignettes <- file.path(build_root, "vignettes")
package_library <- file.path(build_root, "library")
dir.create(build_vignettes, recursive = TRUE)
dir.create(package_library)
on.exit(unlink(build_root, recursive = TRUE), add = TRUE)

copied <- file.copy(c(sources, support), build_vignettes)
if (!all(copied)) {
    stop("Could not stage the vignette inputs.", call. = FALSE)
}

highlight_r_fences <- function(lines) {
    output <- character(0)
    blocks <- character(0)
    index <- 1

    while (index <= length(lines)) {
        if (!grepl("^```[[:space:]]*r[[:space:]]*$", lines[index])) {
            output <- c(output, lines[index])
            index <- index + 1
            next
        }

        closing <- which(
            grepl("^```[[:space:]]*$", lines) & seq_along(lines) > index
        )
        if (length(closing) == 0) {
            stop("An R code fence is not closed in the knitted vignette.", call. = FALSE)
        }

        closing <- closing[1]
        code <- lines[seq.int(index + 1, closing - 1)]
        highlighted <- highr::hi_html(code)
        highlighted <- gsub(
            paste0(
                '<span class="hl num">',
                "(TRUE|FALSE|NULL|NA|NA_integer_|NA_real_|NA_complex_|",
                "NA_character_|NaN|Inf)",
                "</span>"
            ),
            '<span class="hl cn">\\1</span>',
            highlighted,
            perl = TRUE
        )
        highlighted <- sub("[\\r\\n]+$", "", highlighted)
        highlighted[!nzchar(highlighted)] <- '<span class="hl blank"></span>'
        blocks <- c(blocks, paste(
            c(
            '<div class="sourceCode"><pre class="sourceCode r"><code class="sourceCode r">',
            highlighted,
            "</code></pre></div>"
            ),
            collapse = "\n"
        ))
        output <- c(output, sprintf("DECLARED-CODE-BLOCK-%04d", length(blocks)))
        index <- closing + 1
    }

    return(list(markdown = output, blocks = blocks))
}

render_static_html <- function(input, output, stylesheet) {
    lines <- readLines(input, warn = FALSE)
    title_line <- grep("^title:[[:space:]]*", lines, value = TRUE)
    if (length(title_line) == 0) {
        stop("No title found in: ", input, call. = FALSE)
    }

    title <- trimws(sub("^title:[[:space:]]*", "", title_line[1]))
    title <- sub("^\"(.*)\"$", "\\1", title)
    lines <- gsub("(`[^`]+`)\\{\\.R\\}", "\\1", lines)
    highlighted <- highlight_r_fences(lines)

    staged <- tempfile(fileext = ".md")
    on.exit(unlink(staged), add = TRUE)
    writeLines(highlighted$markdown, staged)
    markdown::markdownToHTML(
        file = staged,
        output = output,
        title = title,
        stylesheet = stylesheet,
        options = "-js_highlight"
    )

    html <- paste(readLines(output, warn = FALSE), collapse = "\n")
    for (i in seq_along(highlighted$blocks)) {
        token <- sprintf("DECLARED-CODE-BLOCK-%04d", i)
        html <- sub(
            paste0("<p>", token, "</p>"),
            highlighted$blocks[i],
            html,
            fixed = TRUE
        )
    }
    if (grepl("DECLARED-CODE-BLOCK-", html, fixed = TRUE)) {
        stop("Could not insert all highlighted R code blocks into: ", output, call. = FALSE)
    }
    writeLines(html, output)
}

install_output <- system2(
    file.path(R.home("bin"), "R"),
    c(
        "CMD", "INSTALL", "--no-test-load",
        "-l", shQuote(package_library),
        shQuote(package_root)
    ),
    stdout = TRUE,
    stderr = TRUE
)
install_status <- attr(install_output, "status")
if (!is.null(install_status) && install_status != 0) {
    stop(
        "Could not install the current declared source:\n",
        paste(install_output, collapse = "\n"),
        call. = FALSE
    )
}

worker <- file.path(build_root, "render_one.R")
writeLines(
    c(
        "args <- commandArgs(trailingOnly = TRUE)",
        ".libPaths(c(args[4], .libPaths()))",
        "knitr::knit(",
        "    args[1],",
        "    output = args[2],",
        "    envir = new.env(parent = globalenv()),",
        "    quiet = TRUE",
        ")"
    ),
    worker
)

for (name in names) {
    source <- file.path(build_vignettes, paste0(name, ".Rmd.orig"))
    markdown <- file.path(build_vignettes, paste0(name, ".Rmd"))
    html <- file.path(build_vignettes, paste0(name, ".html"))

    render_output <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            "--vanilla",
            shQuote(worker),
            shQuote(source),
            shQuote(markdown),
            shQuote(html),
            shQuote(package_library)
        ),
        stdout = TRUE,
        stderr = TRUE
    )
    render_status <- attr(render_output, "status")
    if (!is.null(render_status) && render_status != 0) {
        stop(
            "Could not execute ", name, ":\n",
            paste(render_output, collapse = "\n"),
            call. = FALSE
        )
    }

    render_static_html(
        markdown,
        html,
        file.path(build_vignettes, "declared.css")
    )

    lines <- readLines(markdown, warn = FALSE)
    lines <- sub(
        "^([[:space:]]+)(highlight|css):",
        "\\1# \\2:",
        lines
    )
    lines <- sub("[[:space:]]+$", "", lines)
    writeLines(lines, markdown)
}

outputs <- unlist(lapply(
    names,
    function (name) file.path(
        build_vignettes,
        paste0(name, c(".Rmd", ".html"))
    )
))

copied <- file.copy(outputs, vignette_dir, overwrite = TRUE)
if (!all(copied)) {
    stop("Could not copy the generated vignettes into place.", call. = FALSE)
}

message("Updated precomputed Rmd files and custom-coloured static HTML vignettes.")
