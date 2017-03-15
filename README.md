# GeneSurvey

This is for educational and research purposes only. 

http://bioinformatics.mdanderson.org

The goal of the Gene Survey project is to take TCGA Standardized Data and make gene level cross-disease and sometimes cross platform comparisons and evaluations simpler to perform, as these represent common questions asked of researchers and analysts. That is, we are answering a common question: how does gene XYZ act across disease types?

This is the R package used for this project. A related Java package is available at https://github.com/MD-Anderson-Bioinformatics/GSAccess but the JAR is included with this source.

Data zip files are available at http://bioinformatics.mdanderson.org/TCGA/gsresults where the Download R Package Data Archive link provides the needed zip file.

# General Prerequisites

Before proceeding, please install https://github.com/MD-Anderson-Bioinformatics/ReadMatrixRcpp Please note that all C++ compilations need to be done using the same C++ compiler, otherwise misleading error messages can occur. If you do not wish to use platform level matrices (this is all TCGA data for a particular gene across all platforms supported by GeneSurvey), then the ReadMatrixRcpp R package may be safely omitted and you can ignore the suggested package does not exist warning messages from R.

The GeneSurvey package requires Java 1.8.

These instructions have been tested using R 3.2.3 and R 3.2.2.

# Linux Prerequisites

As the user with sudo access and which will run RStudio, set the JAVA_HOME variable. (This is the directory above the bin, for example, the javac at /usr/local/jdk1.8.0_45/bin/javac should be entered as /usr/local/jdk1.8.0_45.)

Depending on your R installation, you made need to do a javareconf for R using "R CMD javareconf". Make sure Java 1.8 is registered with R.


# Windows and OSX Prerequisites

Windows and OSX installations will also need any extras needed for compiling R packages.
Running under Windows and OSX enviroments is neither tested nor supported, but is theoretically possible.

Do a javareconf for R using "R CMD javareconf". Make sure Java 1.8 is registered with R.

# R Prerequisites

The devtools and rJava package need to be installed.

```r
install.packages("devtools", dependencies=TRUE)
install.packages("rJava", dependencies=TRUE, type="source")
```

# Linux Install

```r
devtools::install_github("MD-Anderson-Bioinformatics/GeneSurvey")
```

# Windows Install

If you are setup for multi-architecture compiles in Windows, you can remove the args parameter.
```r
devtools::install_github("MD-Anderson-Bioinformatics/GeneSurvey", args="--no-multiarch")
```

# Useful Tests

This can be used to test your rJava install and Java version.

```r
library(rJava)
.jinit()
J("java.lang.System")$getProperty("java.version")
```

# Other Projects

The https://github.com/MD-Anderson-Bioinformatics/GSAccess and https://github.com/MD-Anderson-Bioinformatics/ReadMatrixRcpp projects are used by this R package. For installing GeneSurvey, you do not need to do anything with GSAccess as this R package already has copies of those projects' JAR files.

For ReadMatrixRcpp, you can get package install instructions at https://github.com/MD-Anderson-Bioinformatics/ReadMatrixRcpp

# Testing and Default Directories

The default directory and location where GeneSurvey looks for the data file is at /geneSurveyData/GeneSurvey.zip

If you with to implement R code testing as part of build the code in the tests directory looks for /geneSurveyData/TESTING/GeneSurvey.zip and /geneSurveyData/TESTING/test_package. The data zip here should be a copy of the 2016_08_26_0922 data archive. The test_package is a directory writeable for output by the testing user.
