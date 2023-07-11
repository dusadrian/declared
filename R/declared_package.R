#' @docType package
#'
#' @name declared_package
#'
#' @title Functions for Declared Missing Values
#'
#' @description A set of functions to declare labels and missing values, coupled
#' with associated functions to create (weighted) tables of frequencies and
#' various other summary measures.
#' Some of the base functions are rewritten to make use of the specific
#' information about the missing values, most importantly to distinguish between
#' empty and declared missing values.
#' Many functions have a similar functionality with the corresponding ones from
#' packages "haven" and "labelled". The aim is to ensure as much compatibility
#' as possible with these packages, while offering an alternative in the objects
#' of class "declared".
#'
#' @author Adrian Dusa
#'
#' Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab declared\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.22\cr
#'   Date: \tab 2023-07-07\cr
#'   License: \tab GPL-v3\cr
#' }
#'
#' @importFrom stats na.omit na.fail na.exclude median
#' @importFrom utils head tail capture.output
#' @useDynLib declared, .registration = TRUE
#'
NULL
