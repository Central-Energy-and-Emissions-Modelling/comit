# **COMIT** <img src="man/figures/logo.png" align="right" height="139" alt="" />

COMIT is an optimisation model of UK industry. It finds the least cost pathway for meeting exogenous industry demand. The cost function which the model minimises includes technology capex, technology opex, carbon cost, and CO2 T&S and hydrogen infrastructure costs.

The `comit` packages includes an Rshiny application that allows users to run models and visualise results.

---
## **Disclaimer**
The COMIT model is an analytical tool designed to explore a range of pathways for industrial decarbonisation.  Outputs are dependent on user-defined assumptions.  As such, they do not represent government policy, forecasts, or preferred outcomes and should be interpreted as indicative rather than predictive.
The Department for Energy Security and Net Zero provides no express or implied warranties concerning the COMIT model and its content and, accordingly, accepts no liability arising from use of the tool or its content.

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

Default inputs are provided to enable model to run and should not be interpreted as recommended by DESNZ. Users are responsible for identifying appropriate model inputs.  Many of the technical assumption values in the input template are, however, taken from research sponsored by DESNZ and published on the COMIT page of gov.uk. 

---

## **📁 Project Structure**
```
comit/
├─ R/                       # Function scripts
├─ man/                     # Generated documentation
├─ data_template_archive/   # Assumptions data
├─ docs/                    # Model documentation and user resources
├─ dev/                     # Used for development with golem, can be ignored
├─ tests/                   # Unit tests
├─ inst/                    # Files required for package build/usage
├─ DESCRIPTION              # Package metadata
├─ NAMESPACE                # Export/import definitions
└─ README.md                # This file
```

---


