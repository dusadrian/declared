test_that ("cross-tab frequencies align by their units digits", {
    counts <- matrix (c (66, 163, 222, 198, 69, 217, 437, 467), ncol = 2)
    cells <- which (counts > 0, arr.ind = TRUE)
    x <- declared (
        rep (cells[, 1], times = counts),
        labels = c (
            "Foarte interesat" = 1, "Destul de interesat" = 2,
            "Putin interesat" = 3, "Deloc interesat" = 4
        )
    )
    y <- declared (
        rep (cells[, 2], times = counts),
        labels = c (Barbat = 1, Femeie = 2)
    )

    for (values in c (TRUE, FALSE)) {
        for (weighted in c (TRUE, FALSE)) {
            tbl <- wtable (
                x, y, values = values,
                wt = if (weighted) rep (2, length (x)) else NULL
            )
            output <- capture.output (print (tbl, startend = FALSE))
            rows <- tail (output, nrow (counts) + 1)
            positions <- lapply (gregexpr ("[0-9]+", rows), function (pos) {
                tail (pos + attr (pos, "match.length") - 1L, 3)
            })

            for (pos in positions[-1]) {
                expect_equal (pos, positions[[1]])
            }
            expected <- rbind (counts, colSums (counts))
            expected <- cbind (expected, rowSums (expected))
            if (weighted) {
                expected <- 2 * expected
            }
            printed <- t (vapply (rows, function (row) {
                numbers <- regmatches (row, gregexpr ("[0-9]+", row))[[1]]
                as.numeric (tail (numbers, 3))
            }, numeric (3)))
            expect_equal (unname (printed), unname (expected))
        }
    }
})
