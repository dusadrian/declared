local_edition (3)

test_that ("serial frequency accumulation preserves factor grids", {
    set.seed (942)
    x <- factor (
        sample (c ("a", "b", NA), 300000, TRUE),
        levels = c ("b", "a", "absent")
    )
    y <- factor (sample (c ("x", "y", NA), length (x), TRUE))
    wt <- sample (c (0, 0.25, 1, 2, -1, NA, NaN), length (x), TRUE)

    for (xy in list (list (x = x), list (x = x, y = y))) {
        reference <- tapply (wt, xy, sum, na.rm = TRUE)
        reference[is.na (reference)] <- 0
        if (length (xy) == 2L) {
            result <- wtable (x, y, wt = wt, observed = FALSE)
        }
        else {
            result <- wtable (x, wt = wt, observed = FALSE)
        }
        expect_equal (as.numeric (result), as.numeric (round (reference, 0)))
    }

    expect_length (wtable (factor (character ())), 0L)
})

test_that ("serial frequencies preserve logical weights and complex fallback", {
    x <- factor (c ("a", "a", "b", "c", NA))
    for (wt in list (
        c (1+2i, 2+3i, NA, 4+1i, 9),
        c (TRUE, FALSE, NA, TRUE, TRUE)
    )) {
        reference <- round (tapply (wt, x, sum, na.rm = TRUE), 0)
        result <- wtable (x, wt = wt, observed = FALSE)
        expect_equal (as.vector (unclass (result)), as.vector (reference))
    }
})

test_that ("frequency entry point validates prepared inputs", {
    expect_error (
        .Call ("_wtable", 1:3, NULL, c (1, 2), c (3L, 1L),
            PACKAGE = "declared"), "Invalid frequency"
    )
    expect_error (
        .Call ("_wtable", 1:3, NULL, c (1, 2, 3), c (3L, 2L),
            PACKAGE = "declared"), "Invalid frequency"
    )
})
