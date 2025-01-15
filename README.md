# STEM labor market analysis for Agglomerations, January 2025

This analysis uses 2023 1-year American Community Survey microdata from IPUMS to analyze the occupational destinations of college graduates in the United States. Ypu can find the full blog post on [Agglomerations](https://agglomerations.substack.com/).

To replicate the analysis, use the R code and Excel files in the repository. In IPUMS, pull the following variables in the 2023 1-year ACS sample:

YEAR	Census year

SAMPLE	IPUMS sample identifier

SERIAL	Household serial number

CBSERIAL	Original Census Bureau household serial number

HHWT	Household weight

CLUSTER	Household cluster for variance estimation

STRATA	Household strata for variance estimation

GQ	Group quarters status

PERNUM	Person number in sample unit

PERWT	Person weight

AGE	Age

EDUC (general)	Educational attainment [general version]

EDUCD (detailed)	Educational attainment [detailed version]

DEGFIELD (general)	Field of degree [general version]

DEGFIELDD (detailed)	Field of degree [detailed version]

OCC	Occupation

WKSWORK1	Weeks worked last year

UHRSWORK	Usual hours worked per week

INCWAGE	Wage and salary income





