local_edition (3)

test_that ("all native entry points are registered with dynamic lookup disabled", {
    dll <- getLoadedDLLs ()[["declared"]]
    routines <- getDLLRegisteredRoutines (dll)[[".Call"]]
    expected <- c (
        "_directDeclared" = 10L,
        "_allIndexedNA" = 2L,
        "_tag" = 1L,
        "_hasTag" = 2L,
        "_getTag" = 1L,
        "_anyTagged" = 1L,
        "_wtable" = 4L
    )

    expect_false (dll[["dynamicLookup"]])
    expect_equal (
        vapply (routines, function (routine) routine$numParameters, integer (1)),
        expected
    )
    expect_error (
        .Call ("_getTag", c (1, 2), NULL, PACKAGE = "declared"),
        "Incorrect number of arguments"
    )
})
