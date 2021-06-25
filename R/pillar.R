# Dynamically exported, see onLoad.R
# using eval(parse()) to avoid the dependency tree of vctrs, haven, labelled and pillar

`pillar_shaft.declared` <- function(
    x,
    show_labels = getOption("declared.show_pillar_labels", TRUE),
    ...) {

    if (eval(parse(text = "requireNamespace('haven', quietly = TRUE)"))) {
        return(eval(parse(text = "pillar::pillar_shaft(as_haven(x))")))
    }

    if (!isTRUE(show_labels) | !pillar_print_pkgs_available()) {
        return(eval(parse(text = "pillar::pillar_shaft(unclass(x))")))
    }

    if (is.numeric(x)) {
        val <- val_num_pillar_info(x)
        lbl <- lbl_pillar_info(x)

        mw <- max(val$disp_short$lhs_ws + val$disp_short$main_wid + lbl$wid_short)
        w <- max(val$disp_full$lhs_ws + val$disp_full$main_wid + lbl$wid_full)

        eval(parse(text = "pillar::new_pillar_shaft(list(val = val, lbl = lbl), min_width = mw, width = w, class = 'pillar_shaft_declared_num')"))
    }
    else {
        val <- val_chr_pillar_info(x)
        lbl <- lbl_pillar_info(x)

        mw <- max(val$wid_short + lbl$wid_short)
        w <- max(val$wid_full + lbl$wid_full)

        eval(parse(text = "pillar::new_pillar_shaft(list(val = val, lbl = lbl), min_width = mw, width = w, class = 'pillar_shaft_declared_chr')"))
    }
}

`val_num_pillar_info` <- function(x) {
    val_pillar <- eval(parse(text = "pillar::pillar_shaft(haven::zap_labels(undeclare(x)))"))

    disp_short <- num_disp_components(x, val_pillar, attr(val_pillar, "min_width"))
    disp_full <- num_disp_components(x, val_pillar, attr(val_pillar, "width"))

    list(
        disp_short = disp_short,
        disp_full = disp_full
    )
}

`num_disp_components` <- function(x, pillar, width) {
    display <- format(pillar, width)
    # Sometimes there's an extra leading space from pillar
    display <- trim_ws_lhs(display)
    # exponent notation formatting hinders stripping white space in NAs
    display[is.na(unclass(x))] <- eval(parse(text = "crayon::strip_style(display[is.na(unclass(x))])"))

    display_untrimmed_wid <- eval(parse(text = "pillar::get_extent(display)"))
    display_max_wid <- max(display_untrimmed_wid)
    display <- trim_ws_rhs(display)
    main_wid <- eval(parse(text = "pillar::get_extent(display)"))
    display_trimmed_rhs <- display_untrimmed_wid - main_wid

    # display[is.na(unclass(x))] <- eval(parse(text = "pillar::style_na(display[is.na(unclass(x))])"))
    list(
        lhs_ws = max(main_wid + display_trimmed_rhs) - (main_wid + display_trimmed_rhs),
        main_wid = main_wid,
        main_txt = display,
        rhs_ws = display_trimmed_rhs
    )
}

`val_chr_pillar_info` <- function(x) {
    MIN_CHR_DISPLAY <- 4
    val_pillar <- eval(parse(text = "pillar::pillar_shaft(haven::zap_labels(x))"))
    disp_full <- trim_ws_rhs(format(val_pillar, attr(val_pillar, "width")))
    wid_full <- eval(parse(text = "pillar::get_extent(disp_full)"))

    list(
        val_pillar = val_pillar,
        wid_short = pmin(MIN_CHR_DISPLAY, wid_full),
        disp_full = disp_full,
        wid_full = wid_full
    )
}

`lbl_pillar_info` <- function(x) {
    MIN_LBL_DISPLAY <- 6
    labels <- attr(x, "labels")
    if (length(labels) > 0) {
        names(labels) <- eval(parse(text = "pillar::style_subtle(paste0(' [', names(labels), ']'))"))
        attr(x, "labels") <- labels
        label_display <- eval(parse(text = "as.character(haven::as_factor(x, 'labels'))"))
        label_display[is.na(label_display)] <- ""
    } else {
        label_display <- character(length(x))
    }
    label_widths <- eval(parse(text = "pillar::get_extent(label_display)"))
    label_min_widths <- ifelse(label_widths > 0, pmin(MIN_LBL_DISPLAY, label_widths), 0)

    if (inherits(x, "declared")) {
        MIN_NA_DISPLAY <- 4
        na_display <- character(length(x))
        na_index <- attr(x, "na_index")
        eval(parse(text = "na_display[na_index] <- pillar::style_na(' (NA)')"))
        na_widths <- eval(parse(text = "pillar::get_extent(na_display)"))

        label_display <- paste0(na_display, label_display)
        label_widths <- label_widths + na_widths
        label_min_widths <- label_min_widths + ifelse(label_widths > 0, pmin(MIN_NA_DISPLAY, label_widths), 0)
    }

    ret <- list(
        wid_short = label_min_widths,
        disp_full = label_display,
        wid_full = label_widths
    )
    ret
}

# to export
`format.pillar_shaft_declared_num` <- function(x, width, ...) {
    vshort <- x$val$disp_short
    vfull <- x$val$disp_full
    lbl_wid <- pmax(0, x$lbl$wid_short - vfull$rhs_ws)

    if (width >= max(vfull$lhs_ws +vfull$main_wid + lbl_wid)) {
        lbl_width <- width - (vfull$lhs_ws + vfull$main_wid)
        lbl <- str_trunc(x$lbl$disp_full, lbl_width, subtle = TRUE)
        out <- paste_with_align(vfull$main_txt, lbl, vfull$lhs_ws, vfull$rhs_ws)
    } else {
        lbl_width <- width - (vshort$lhs_ws + vshort$main_wid)
        lbl <- str_trunc(x$lbl$disp_full, lbl_width, subtle = TRUE)
        out <- paste_with_align(vshort$main_txt, lbl, vshort$lhs_ws, vshort$rhs_ws)
    }
    eval(parse(text = "pillar::new_ornament(out, width = width, align = 'right')"))
}

# to export
`format.pillar_shaft_declared_chr` <- function(x, width, ...) {
    if (width >= max(x$val$wid_full + x$lbl$wid_short)) {
        lbl_width <- width - x$val$wid_full
        lbl <- str_trunc(x$lbl$disp_full, lbl_width, subtle = TRUE)
        out <- paste0(x$val$disp_full, lbl)
    } else {
        val_widths <- pmin(x$val$wid_full, width - x$lbl$wid_short)
        val_display <- str_trunc(x$val$disp_full, val_widths)
        lbl <- str_trunc(x$lbl$disp_full, width - val_widths, subtle = TRUE)
        out <- paste0(val_display, lbl)
    }
    eval(parse(text = "pillar::new_ornament(out, width = width, align = 'left')"))
}


# Helpers -----------------------------------------------------------------

`str_trunc` <- function(x, widths, subtle = FALSE) {
    str_width <- eval(parse(text = "pillar::get_extent(x)"))
    too_wide <- which(!is.na(x) & str_width > widths)

    continue_symbol <- eval(parse(text = "cli::symbol$continue"))
    if (subtle) continue_symbol <- eval(parse(text = "pillar::style_subtle(continue_symbol)"))

    truncated <- Map(x[too_wide], widths[too_wide], f = function(item, wid) {
        aa <- eval(parse(text = "crayon::col_substr(item, 1, wid - 1)"))
        paste0(aa, continue_symbol)
    })
    truncated <- as.vector(truncated, "character")
    x[too_wide] <- truncated

    x
}

`trim_ws_rhs` <- function(x) {
    sub("[ \t\r\n]+$", "", x)
}

`trim_ws_lhs` <- function(x) {
    sub("^[ \t\r\n]+", "", x)
}

`pad_space` <- function(n) {
    vapply(n, function(x) paste(rep(" ", x), collapse = ""), "")
}

`paste_with_align` <- function(x, y, lhs_ws, rhs_ws) {
    y_wid <- eval(parse(text = "pillar::get_extent(y)"))
    added_chars <- max(y_wid - rhs_ws)
    rhs_ws <- added_chars - (y_wid - rhs_ws)

    paste0(pad_space(lhs_ws), x, y, pad_space(rhs_ws))
}

`pillar_print_pkgs_available` <- function() {
    eval(parse(text = "requireNamespace('crayon', quietly = TRUE)")) &
    eval(parse(text = "requireNamespace('cli', quietly = TRUE)"))
}
