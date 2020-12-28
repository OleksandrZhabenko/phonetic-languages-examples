# Revision history for phonetic-languages-examples

## 0.1.0.0 -- 2020-10-30

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-11-06

* First version revised A. Changed the dependency boundaries so that the package uses the latest versions.

## 0.1.2.0 -- 2020-11-06

* First version revised B. Fixed some issues with the text preprocessing so that avoid unconsistency between different packages. Some simplification of the code for the
propertiesTextG executable.

## 0.2.0.0 -- 2020-11-07

* Second version. Added the possibilities to use new related to generated with r-glpk-phonetic-languages-ukrainian-durations package duration metrices.
Changed the dependencies boundaries for this.

## 0.3.0.0 -- 2020-11-09

* Third version. Added the possibility to use coefficients' version of the rhythmicity-related functions from the phonetic-languages-rhythmicity and
phonetic-languages-properties packages.

## 0.3.1.0 -- 2020-11-09

* Third version revised A. Fixed issue with the wrong output option for one of the columns in the propertiesTextG executable running output.

## 0.3.1.1 -- 2020-11-09

* Third version revised A. Fixed the wrong comment about the first command line argument.

## 0.3.2.0 -- 2020-11-10

* Third version revised B. Fixed issue with missing variant for procBothF function in the working program. Updated the dependency boundaries of
the ukrainian-phonetics-basic.

## 0.3.3.0 -- 2020-11-10

* Third version revised C. Fixed issue with the reverse order of the output for the rewritePoemG executable.

## 0.4.0.0 -- 2020-11-12

* Fourth version. Added the possibility to use constraints from the phonetic-languages-constraints package (it is also a new leightweight dependency)
in the lineVariantsG executable. They allows to reduce number of computations needed and explicitly apply additional needed grammar rules, extend
the extent of understandable text and so on.

## 0.4.1.0 -- 2020-11-12

* Fourth version revised A. Fixed issue with incorrect behaviour because of phonetic-languages-constraints inner issue. Changed the dependency boundaries appropriately. Changed
the behaviour of the lineVariantsG so that you can use no coeffs (specially formatted first command line argument), this then is like for the versions before 0.3.0.0.

## 0.4.2.0 -- 2020-11-12

* Fourth version revised B. Fixed issues with maximumElBy from the phonetic-languages-general package by upgrading to the latest version.

## 0.5.0.0 -- 2020-11-13

* Fifth version. Added file README.md with the persistent link to the instruction (in Ukrainian) how to use the programs of the package.

## 0.6.0.0 -- 2020-11-16

* Sixth version. Added the possibility to select the lines for analysis by propertiesTextG and to print the analyzed text with line numbers. To print it with
line numbers use the command line argument @n. Afterwards, running again, specify the needed line numbers by the first one number and the last one number
separated with colon (':'). You can specify multiple times. The order would have been the same as you have specified.

## 0.6.1.0 -- 2020-11-18

* Sixth version revised A. Changed the usage of the maximumElBy from the Languages.UniquenessPeriods.Vector.General.DebugG module to the respective function from the
Languages.UniquenessPeriods.Vector.General.Simplified module to reduce computations. Changed the boundaries for the dependency of phonetic-languages-general and subG. Please,
update to the version with updating also phonetic-languages-general, phonetic-languages-common and subG.

## 0.6.2.0 -- 2020-11-18

* Sixth version revised B. Fixed issue with the wrong numeration in the propertiesTextG processment so that it does not equals to the needed and
documented one. Now works as expected.

## 0.6.3.0 -- 2020-11-27

* Sixth version revised C. Updated the dependencies boundaries.

