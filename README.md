# Vegetation Survey Analysis & Report Automation App

This Shiny app is developed to support field ecologists and surveyors in processing and analyzing vegetation data. It streamlines the generation of comprehensive summaries and reports from vegetation plots or transect surveys, and ensures consistency, transparency, and data integrity throughout the process.

## Features

- **Data Validation and Quality Control**
- **Automated Analysis of Vegetation Structure and Composition**
- **Customizable Report Generation**

## Screenshots

![Screenshot 1](screenshots/veg_screenshot1.png)  
![Screenshot 2](screenshots/veg_screenshot2.png)  
![Screenshot 3](screenshots/veg_screenshot3.png)

## Usage

### Data Validation

The app ensures high-quality input by validating all uploaded vegetation data against predefined formats and taxonomic references. This helps maintain data consistency and scientific rigor.

Key validation steps include:

1. **Format Verification:**  
   All columns are checked for correct data types (e.g., numeric for DBH, categorical for species names), mandatory fields, and standard structure (e.g., plot ID, species, abundance, DBH, height).

2. **Scientific Name Matching:**  
   The app cross-checks species names with a botanical database (e.g., [The Plant List](http://www.theplantlist.org), [Tropicos](https://www.tropicos.org), or [Plants of the World Online](http://powo.science.kew.org)). This step reduces errors in taxonomy and enhances data interoperability.

### Vegetation Analysis

After validation, users can run rapid analyses of vegetation community metrics such as:

- **Species Richness and Diversity** (e.g., Shannon, Simpson Index)
- **Basal Area and Importance Value Index (IVI)**
- **Structural Profiles** (based on DBH or height class distribution)

Visual outputs include:

- Species composition barplots and pie charts  
- DBH class histograms  
- IVI tables  
- Richness comparisons across plots or transects  

All visualizations and summary tables can be exported for further use or review.

### Report Generation

Following analysis, the app allows users to compile results into a customizable report. The report includes:

- Executive summary  
- Survey method and effort description  
- Community composition tables and graphs  
- Structural analysis outputs  
- Custom narrative sections (user editable)

Reports can be exported in formats such as `.docx` for integration with other documents or for direct use in submissions and publications.

## Contributors

- **Achmad Alifianto**  
- **Erlangga Muhammad**  
- **Farhan Adhyn**
- **Oktavianus Limpa**
- **Rodiyatul Rahmat**  

Special thanks to all contributors and collaborators involved in field data collection, app testing, and continuous improvement of our tools and workflows.
