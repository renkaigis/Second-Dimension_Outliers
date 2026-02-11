## Second-Dimension Outliers (SDO) for Spatial Prediction

### 📜 Publication & Citation

**Title:** Second-dimension outliers for spatial prediction

**Authors:** Kai Ren, Yongze Song*, Qiang Yu* 

**Journal:** *International Journal of Geographical Information Science* (2025)

**Published:** 24 November 2025

**Citation:**

> **Ren, K., Song, Y.<sup>*</sup>, & Yu, Q.<sup>*</sup>** (2025). Second-dimension outliers for spatial prediction. *International Journal of Geographical Information Science*, 1–28. [https://doi.org/10.1080/13658816.2025.2580414](https://doi.org/10.1080/13658816.2025.2580414)

or: 

```bibtex
@article{Ren2025SDO,
  author = {Ren, K. and Song, Y. and Yu, Q.},
  title = {Second-dimension outliers for spatial prediction},
  journal = {International Journal of Geographical Information Science},
  pages = {1--28},
  year = {2025},
  doi = {10.1080/13658816.2025.2580414}
}

```

**Funding:** Supported by the China Scholarship Council (Grant No. 202206300058).

---

### 📖 Overview

This repository provides the core datasets and codes associated with the SDO model. It serves as a comprehensive guide to ensure complete **reproducibility** of the results presented in the manuscript. All codes are fully executable, provided the required R packages are installed.

---

### 📂 Repository Structure

The repository is divided into four main folders:

#### 1. `SDO_simulation_analysis`

*Focus: Evaluation of SDO model performance under controlled conditions.*

* **`dt_sim.rda`**: R dataset for simulation experiments.
* **`generate_sdo_var.R`**: The **core function** for constructing SDO variables. It quantifies spatial outlier characteristics around unsampled locations using parameters:
* `pointlocation`: Sample coordinates.
* `gridlocation`: Prediction grid coordinates.
* `gridvar`: Variables of the grid cells.
* `distuf`: Buffer distance.
* `sd`: Standard deviation threshold for outlier identification.


* **`sdo_simdata.R`**: Main script for simulation workflow, including prediction, validation, and visualization (Ref: Figures 2–4).

#### 2. `SDO_wheat_case`

*Focus: Australian wheat production case study.*

* **`wheat.rda`**: Contains `dt_points` (179 LGAs with variables like AT, ETa, NDVI, etc.) and `dt_grids` (prediction grid).
* **`sdo_case.R`**: Main execution script for:
* Generating distribution maps (Ref: Figure 5).
* Correlation analysis (Ref: Figure 8).
* Visualizing SDO variables for AT, TP, NDVI, and SND (Ref: Figures 9–10).
* Variable importance and performance metrics (R², RMSE, MAE) for 7 ML models (Ref: Table 2).


* **`predvalue_svm.csv`**: Prediction results across the Australian wheat belt.
* **`STE_AUS_shape/`**: State boundaries used as base layers.

#### 3. `SDO_validation`

*Focus: Sensitivity analysis, robustness testing, and specific visualizations.*

* **`sdo_validation.R`**: Generates cross-section yield comparisons (Ref: Figure 13) and density distribution plots (Ref: Figure 14).
* **`sdo_sensitivity_analysis.R`**: Tests variations in buffer distance and outlier thresholds (Ref: Figure 11).
* **`CSV files/`**: State-specific prediction results (NSW, VIC, QLD, SA, WA) and cross-section points (A & B).

#### 4. `shapefiles`

*Focus: Spatial data layers (Coordinate System: GDA_1994_Australia_Albers).*

| File | Description |
| --- | --- |
| `AUS_Simple_boundary.shp` | Simplified national boundary. |
| `AUS_State_boundary.shp` | Administrative boundaries of states/territories. |
| `AUS_Wheatbelt_boundary.shp` | Spatial extent of the major wheat-growing zones. |
| `Australia_wheat_production_LGA_2021.shp` | Primary response dataset with 2021 production data. |
| `SVM_Prediction_results.shp` | Grid-based results for SDO-SVM and Aspatial SVM. |
| `cross_sectionA/B.shp` | Transect lines for spatial comparison (Ref: Figure 13). |


---

### 🛠️ Requirements & Usage

* **Language:** R
* **Key Parameters for `generate_sdo_var` function:**
* `pointlocation`: Sample coordinates.
* `distuf`: Buffer distance for spatial calculation.
* `sd`: Standard deviation for outlier identification.



**Note:** Ensure all spatial datasets remain under the `GDA_1994_Australia_Albers` coordinate system to maintain consistency.

---

