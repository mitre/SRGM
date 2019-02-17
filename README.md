# SRGM
Software Reliability Growth Model

NOTICE
Copyright 2019 The MITRE Corporation. All Rights Reserved.
This technical data was produced for the U. S. Government under Contract No. FA8702-18-C-0001, 
and is subject to the Rights in Technical Data-Noncommercial Items Clause DFARS 252.227-7013 (JUN 2013)
Approved for Public Release; Distribution Unlimited. Public Release Case Number 19-0034

This project contains content developed by The MITRE Corporation. If this code is used in a deployment or embedded within another project, it is requested that you send an email to opensource@mitre.org in order to let us know where this software is being used.

# Licensing
Apache 2.0

# Description
the purpose of this R Script is two-fold
1) using the JIRA export evaluate a Mean Time between Fault(failure) - MTBF and Mean Time to Fix Code(Repair) - MTTR
   these values are used for determining contractor obligations on delivering code and for calculating A sub Zero (A0)
   further effort is required to tweak these algorithms,but whatever the vaule, this code gives consistency to monthly reports
    * the MTTR is the summary statistics of the data, coerced into a 50 bin histogram of the number of days between 
     created and closed, created using data since inception
    * the MTBF is a probability density function which calculates a cummulative value for the last 52 weeks, the density
      is the (number of defects per work week) / (hours per work week)
2) Manipulate the JIRA export data to a form acceptable by the SFRAT, https://sasdlc.org/lab/projects/srt.html,     https://github.com/LanceFiondella/srt.core
