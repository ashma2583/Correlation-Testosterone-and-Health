# Correlation Between Testosterone and Health Indicators

**DATASCI 306 Final Project | Group 49**

**Authors:** Pengzhe Lin, Mihir Barlinge, Ashton Ma

## 📌 Project Overview
This interactive web application explores the relationship between **testosterone levels** and various cardiovascular, metabolic, and renal health indicators among adult males. Using data from the **National Health and Nutrition Examination Survey (NHANES)**, the project aims to identify how everyday health metrics such as BMI, resting pulse, and urine flow correlate with testosterone.

The application allows users to visualize these relationships dynamically, providing statistical context through regression analysis and correlation metrics.

### Key Findings
* **BMI is the Strongest Predictor:** Body Mass Index (BMI) showed the most significant negative association with testosterone levels ($t = -8.99, p < 2e^{-16}$), suggesting that higher body mass is strongly linked to lower hormonal levels.
* **Cardiovascular Indicators:** * **Resting Pulse:** A higher resting pulse was significantly associated with lower testosterone levels ($p < 0.001$).
    * **Diastolic Blood Pressure:** Interestingly, higher diastolic blood pressure showed a positive correlation with testosterone levels ($p \approx 0.0015$).
* **Renal Function:** Urine flow rate (`UrineFlow1`) remained a significant positive predictor ($p \approx 0.004$), reinforcing the link between renal health and testosterone.
* **Non-Significant Factors:** When controlling for the above health metrics, **Age**, **Systolic Blood Pressure**, and **Diabetes** status were not found to be statistically significant predictors in this specific model.
* **Model Variance:** The combined health indicators explain approximately **11% of the variance** in testosterone levels ($R^2 = 0.1108$), indicating that while these physical health markers are important, testosterone levels are likely influenced by a complex array of other unmeasured factors.

## 🔗 Live Application
**[Click here to view the interactive app]** ( https://ashtonma.shinyapps.io/ds306group49/)

> *Note: If the link is not active, please follow the "How to Run Locally" instructions below.*

## 📂 Project Structure

```text
DS-306-Group-49/
├── shiny.R                   # Main application file (UI & Server)
├── testosterone_data_analysis.Rmd # Focused analysis on testosterone variables
├── DS-306-Group-49.Rproj       # RStudio Project file
└── README.md                   # Project documentation
```

## 💻 How to Run Locally

To run this application on your own machine, follow these steps:

### 1. Clone the Repository
Open your terminal or Git Bash and run:
```bash
git clone [https://github.com/ashma2583/DS-306-Group-49.git](https://github.com/ashma2583/DS-306-Group-49.git)
```
Open RStudio and navigate to and open the project.
In the console, run:
```{r}
install.packages(c("shiny", "tidyverse", "ggplot2", "dplyr", "broom"))
shiny::runApp("shinyapp")
```

