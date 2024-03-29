% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_data_tables.R
\name{index_data_tables}
\alias{index_data_tables}
\title{Create Indices on Common Data Model clinical data tables.}
\usage{
index_data_tables()
}
\description{
This function creates indices for the
clinical data tables in the CDM.
}
\details{
This function requires a configured \code{config.yaml} file created using
\code{amstel::create_config()}. It will use the connection configuration specified
in the \code{cdm} section to create the indices for the tables of the
Common Data Model.

This function assumes \code{amstel::create_cdm_tables()} and
\code{amstel::load_cdm_clinical_data()} have been run successfully.
}
