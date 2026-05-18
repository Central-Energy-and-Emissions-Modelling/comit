# About COMIT <img src="www/logo.png" align="right" height="139" alt="" />

**COMIT** (Cost Optimisation Model for Industrial Technologies) is a modelling tool developed by the Department for Energy Security and Net Zero.

The model was developed to optimise technology deployment for UK industry, providing insights into the processes and fuels deployed to meet manufacturing demand whilst minimising costs.

Emissions are costed into the model, and their prices can be changed by the user. Emissions can take the form of formal trading scheme costs, estimated social costs or a combination of both. This means that emissions are factored into the optimisation, allowing for cleaner technologies to be favoured when it is cost effective to do so.
<br>
<br>

## Using the app

Each section below outlines how to use each of the app's tabs.
<br>
<br>

### 📉 Model

The COMIT model is written in R and runs through this RShiny application. To use it:

- Upload a COMIT input spreadsheet via the Model tab.

- The model will run automatically.

- Once complete, you can download the output spreadsheet.

- Results will be visualised in the Outputs and Processes tabs.

You’ll be prompted to save the outputs once the model finishes. If you skip this step, you can still download them later using the Download button.

To modify the model, adjust the assumptions in the input spreadsheet before uploading. You can upload multiple templates to run several models in parallel.

**Model runtime varies:** 

- **Yearly timestep:** ~40 minutes (more detailed results)

- **2-year timestep:** ~15 minutes

- **5-year timestep:** ~5 minutes

We recommend testing with larger timesteps first to check feasibility and runtime. Constraint changes can further vary runtimes - heavily constrained models have been known to take upwards of 10 hours to solve. 

Also note that the progress bar displayed for model runs is calibrated to an average run, this means that it only provides a rough guide and won't be accurate for models with different assumptions. It also moves in steps and will remain at the same point for a long period of time whilst the model solves.
<br>
<br>

### ⬆️ Upload 
**Use this tab to:**

- Re-upload outputs from previously solved models for visualisation or comparison.

- Upload up to 10 models (a mix of new runs and saved outputs).

- Optionally upload an input spreadsheet to enable visualisations in the Processes or Attribution tabs (without running the model).
<br>

### 📊 Outputs 

This tab displays summary plots and tables for the selected models. Use the side panel filters to explore results by sector, location, and more.
<br>
<br>

### ⚡ Attribution

This section shows how emissions reductions are attributed to different technologies.

**To use it, you’ll need:**

- A scenario model (run via the Model tab or uploaded).

- A counterfactual model (where technology deployment remains proportionally fixed at start year values).

**To generate a counterfactual:**

- Set *run_counterfactual* to TRUE in the *model_parameters_b* tab of the input spreadsheet.

- Upload this spreadsheet via the Model tab or upload saved counterfactual results (ensure filenames start with “counterfactual”).

You’ll also need to upload the corresponding input assumptions if not running the model directly.
<br>
<br>

### ⚙️ Processes 

This tab features Sankey diagrams showing technology flows and commodity relationships over time.

- Use the year slider to view a specific year or animate across years.

- If the plot appears empty, you may have selected a year not included in the model (e.g., due to multi-year timesteps).

- Input assumptions are required—upload an input spreadsheet via the Upload tab if not already present.
