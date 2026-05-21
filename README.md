# **COMIT** <img src="man/figures/logo.png" align="right" height="139" alt="" />

COMIT is an optimisation model of UK industry. It finds the least cost pathway for meeting exogenous industry demand. The cost function which the model minimises includes technology capex, technology opex, carbon cost, and CO2 T&S and hydrogen infrastructure costs.

The `comit` packages includes an Rshiny application that allows users to run models and visualise results.

---

## **🔧 Installation**

### **Install from GitHub**
```r
# install.packages("remotes")
remotes::install_version('highs', '1.10.0-3')
remotes::install_github("Central-Energy-and-Emissions-Modelling/comit")
```
Note that we have included the install code for a specific version of the *highs* package, which is used to solve the optimisation problem. We find this version to solve significantly quicker than the newer version for our problem, though you will still get the same result. This is something we are currently investigating.

### **Load the package**
```r
library(comit)
```

---

## **🚀 Example Usage**
Below shows how to launch the `comit` Rshiny application from R. 

Through the Rshiny app users can provide input assumptions workbooks to run scenarious and produce results. Additionally, users can upload previous solutions to visualise scenario outcomes.

```r
library(comit)

# launch the app
run_app()
```

---

## **🧰 Functions**
Key exported functions from the package:

| Function | Description |
|----------|-------------|
| `run_app()` | Launch the main Rshiny application |
| `run_app_dev()` | Launch the development version of the Rshiny application which contains additional features |
| `read_excel_data_template()` | Reads in the input assumptions spreadsheet |
| `comit_solver()` | Visualise model results for reporting and interpretation |


The functions used to create the `comit` model are available through the package. If users wish to run the code manually, or created an alternative model using the functions, we recommend starting the the "R/fct_comit_solver.R" script which contains the main functionality. From there users can go through the different levels of functions. 

---

## **📊 Dataset Documentation** 

### **data_template_archive/comit_input_1_4_0_public.xlsx**
The input assumptions spreadsheet that is used to run the model. This contains many sheets which correspond to the values used in the objective function and constraints for the linear programming model.

The numbers in the file name are used to denote the package version that the input spreadsheet was developed for. Make sure that your input spreadsheet and the package you have installed are both of the same version.

Please note that this is the public version of the input assumptions, meaning that any sensitive information has been replaced with artificial numbers. Therefore, we recommend that users review all input figures and input their own assumptions. Using the default figures from the public input spreadsheet will produce results that should not be used for any inference - they will be completely inaccurate.

### **assumptions_info/GH_NZIP2.xlsx**
This spreadsheet contains the results of research completed by Guidehouse to form the technology values used in the input assumptions. This has been included for transparency. The data itself is already included in the comit input spreadsheet and this file is not used by the package directly.

---

## **📁 Project Structure**
```
comit/
├─ R/                       # Function scripts
├─ man/                     # Generated documentation
├─ data_template_archive/   # Assumptions data
├─ assumptions_info/        # Details the origin of technology assumptions
├─ dev/                     # Used for development with golem, can be ignored
├─ tests/                   # Unit tests
├─ inst/                    # Files required for package build/usage
├─ DESCRIPTION              # Package metadata
├─ NAMESPACE                # Export/import definitions
└─ README.md                # This file
```

---


