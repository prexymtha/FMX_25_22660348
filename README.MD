Financial Econometrics 871 Practical Examination 2025
SU number: 22660348
Author: Precious Nhamo
Course: Financial Econometrics 871
Dates: 18 to 19 November 2025

---

## Table of contents

> The entries below are collapsible. Click on a heading to see its subtopics.

<details>
<summary>Core document sections</summary>

Project overview
Computational environment and setup
Repository structure
General replication instructions
Question by question reproducibility and thinking process

</details>

<details>
<summary>Question 1</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 2</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 3</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 4</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 5</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 6</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 7</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

---

## 1. Project overview

This repository contains the practical examination submission for the Financial Econometrics 871 course.

The project is organised around seven core questions, Q1 to Q7. Each question requires the application of specific econometric and financial techniques to real data. Across the questions, the objectives are to

* demonstrate a clear and reproducible workflow in R
* show the link between financial or economic intuition and the chosen methods
* document the thinking process step by step, rather than only reporting final answers

For each question this README records

* what the question aims to achieve
* which data and variables are used
* which conceptual and econometric approaches are followed
* the logical sequence of steps taken in the analysis
* how the code is structured and how it can be rerun
* which outputs are produced and how they answer the question
* which theoretical results and references underpin the work

This README at the root of the project is the only README that will be graded. Question folders do not contain their own READMEs.

---

## 2. Computational environment and setup

### 2.1 Software

R version: [insert version used]
R project: initialised in the root folder `22660348`
Operating system: [insert operating system]

Key packages used across the questions include for example

* `tidyverse` for data wrangling and plotting
* `lubridate` for handling dates
* `PerformanceAnalytics` for return and risk measures
* time series and volatility packages where needed, for example `rugarch`, `rmgarch`, `MTS`

Any question specific packages are mentioned in that question’s subsection under “Conceptual and econometric approach” or “Implementation and how to run the code”.

### 2.2 Random seeds and reproducibility

Wherever random elements appear in the workflow, such as simulations, random portfolios or resampling, a fixed seed is set at the start of the main script for that question using `set.seed()`.

For transparency the seed used in each question is stated explicitly under “Implementation and how to run the code” for that question, so that the examiner can reproduce stochastic results exactly.

### 2.3 Lecturer supplied project setup

The initial folder structure and data follow the lecturer’s instructions. In particular

* A folder was created on the desktop.
* Inside an R session, `fmxdat::make_project()` was used to initialise the basic project structure.
* The archive `FMX_data_2021.zip` supplied by the lecturer was downloaded and unzipped into the main `data` folder in the project root.
* The `data` folder and any subfolders that start with `data` are added to the `.gitignore` file, so that raw data are not committed to version control.
* Within the questions area, subfolders for individual questions were created and Texevier templates can be used where relevant, for example

  * `Texevier::create_template(directory = "Questions", template_name = "Q1")` for a pdf style template
  * `Texevier::create_template_html(directory = "Questions", template_name = "Q2")` for an html style template

The Texevier templates are used to present selected results neatly for some questions, but the logic and code behind those results are still described here in the README.

---

## 3. Repository structure

The project is organised by question. Each question Q1 to Q7 has its own folder with a consistent internal layout. Within each question folder there are subfolders for code, data and outputs.

The overall layout is as follows

```text
22660348/
│
├── Q1/
│   ├── code/          R scripts and functions for Question 1
│   ├── data/          Raw and processed data for Question 1
│   └── output/
│       ├── plots/     Figures for Question 1
│       └── results/   Tables, model summaries and numerical output for Question 1
│
├── Q2/
│   ├── code/
│   ├── data/
│   └── output/
│       ├── plots/
│       └── results/
│
├── Q3/
│   ├── code/
│   ├── data/
│   └── output/
│       ├── plots/
│       └── results/
│
├── Q4/
│   ├── code/
│   ├── data/
│   └── output/
│       ├── plots/
│       └── results/
│
├── Q5/
│   ├── code/
│   ├── data/
│   └── output/
│       ├── plots/
│       └── results/
│
├── Q6/
│   ├── code/
│   ├── data/
│   └── output/
│       ├── plots/
│       └── results/
│
├── Q7/
│   ├── code/
│   ├── data/
│   └── output/
│       ├── plots/
│       └── results/
│
└── README.md          This document
```

Additional subfolders can be added inside `code` or `output` if helpful, for example `code/functions` or `output/diagnostics`, but the basic pattern above is preserved so that navigation stays straightforward.

---

## 4. General replication instructions

For any question Qk, with k from 1 to 7, the replication steps are the same.

1. Open the R project associated with the root folder `22660348`.

2. Ensure that the packages needed for question Qk are installed. These are listed at the top of the main script and briefly mentioned in the question’s subsection under “Conceptual and econometric approach”.

3. Check that the folder `Qk/data` contains the required datasets, as described in that question’s “Data and variables” subsection.

4. Run the main script in `Qk/code` from the project root. A typical naming convention is `Qk_main.R`. Each main script is written to

   * load the required packages
   * import and clean the data in `Qk/data`
   * carry out the econometric analysis step by step
   * write figures to `Qk/output/plots` and tables or other results to `Qk/output/results`

5. After the script has run, inspect

   * plots in `Qk/output/plots` for the visual story, and
   * tables or model summaries in `Qk/output/results` for numerical details

The written explanations of the thinking process, model choice and interpretation for each question are in section 5 of this README.

---

## 5. Question by question reproducibility and thinking process

The subsections below give a template for documenting each question in a structured and repeatable way. For Question 1 the full template is written out. For Questions 2 to 7 the same headings are used.

You can replace the example sentences in brackets with the actual details that apply in your exam answers.

---

### Question 1: [Insert short descriptive title]

#### 5.1.1 Aim of the question

Summarise what the question is asking and what the analysis is meant to achieve.

Example

*The aim of this question is to estimate and interpret the volatility of daily equity returns using a GARCH type model and to compute Value at Risk and Expected Shortfall measures for a specified portfolio.*

This is also the place to state any sub aims such as model comparison, diagnostic checking or communication of results to a practitioner audience.

#### 5.1.2 Data and variables

Here you describe precisely what you used.

* Dataset name and location

  * File path, for example `Q1/data/returns_data.csv`
  * Source, for example supplied by the lecturer, generated in the code, or a public data source

* Sample period and frequency

  * For example, daily data from 1 January 2010 to 31 December 2020

* Key variables

  * For example, price index, total return index, risk free rate

* Transformations applied

  * For example, log returns defined as
    *(r_t = \log(P_t) - \log(P_{t-1}))*
  * Any scaling, winsorising or subsetting applied before modelling

#### 5.1.3 Conceptual and econometric approach

Explain the main idea first, then the specific tools.

* Main economic or financial question

  * For example, modelling volatility clustering, testing market efficiency, evaluating downside risk

* Model or technique used

  * For example

    * ARMA model for the conditional mean
    * GARCH model for conditional variance
    * Value at Risk and Expected Shortfall at specified confidence levels

* Justification

  * Short explanation of why this approach is appropriate for the data and the exam question

This subsection should make sense even to a reader who does not know the code yet.

#### 5.1.4 Thinking process and steps followed

Record the logical sequence you followed from raw data to final answer. A typical flow could be

* Exploratory analysis

  * Plot price and return series
  * Calculate summary statistics, skewness, kurtosis
  * Check for volatility clustering and outliers

* Model specification

  * Decide whether a mean equation is needed
  * Select a GARCH specification and order
  * Note any information criteria or diagnostic checks used for this choice

* Estimation

  * Fit the chosen model with the relevant R function
  * Record estimated parameters and their standard errors

* Diagnostics

  * Check residuals for autocorrelation
  * Test for remaining conditional heteroskedasticity
  * Comment briefly on whether assumptions seem reasonable

* Risk measures and interpretation

  * Compute Value at Risk and Expected Shortfall if required
  * Compare model based measures with historical estimates where relevant
  * Summarise the key findings in plain language and link them back to the original question

The emphasis is on clarity of thought, not on exhaustive detail.

#### 5.1.5 Implementation and how to run the code

Describe how the code is structured for this question.

* Main script

  * File name and location, for example `Q1/code/Q1_main.R`
  * Purpose of the script

    * load libraries and data
    * call any helper functions
    * run the full analysis pipeline
    * write plots and results to the output folders

* Helper scripts

  * For example `Q1/code/Q1_functions.R` containing user defined functions for estimation and plotting

* Seed

  * If a seed is used, state it here, for example `set.seed(1234)` at the top of the main script

To reproduce the results for Question 1

* open the R project with working directory set to `22660348`
* from there, run the main script for Question 1

All outputs described in the next subsection should appear in the Question 1 output folders.

#### 5.1.6 Outputs

Briefly list the key outputs and how they connect to the question.

* Plots

  * File paths in `Q1/output/plots`
  * Short description, for example

    * time series of log returns
    * standardised residuals and autocorrelation function

* Tables and result files

  * File paths in `Q1/output/results`
  * Short description, for example

    * table of estimated GARCH parameters
    * summary table of Value at Risk and Expected Shortfall

The idea is that the examiner can read this section, open the relevant files and immediately see how each figure or table contributes to your answer.

#### 5.1.7 Theoretical results and references

List the main mathematical or statistical results you rely on, and where they come from. Full derivations are not needed, but the source should be clear.

Examples

* Definition of log return

  *(r_t = \log(P_t) - \log(P_{t-1}))*

  Source: lecture notes, or a standard textbook

* GARCH variance equation

  *(\sigma_t^2 = \omega + \alpha \varepsilon_{t-1}^2 + \beta \sigma_{t-1}^2)*

  Source: time series textbook or lecture notes

* Definition of Value at Risk and Expected Shortfall at confidence level (\alpha)

  Short statement and reference to the material where these definitions are taken from

If likelihood functions, information criteria or test statistics are used, state which standard formula is followed and give the reference, for example Hamilton’s time series text or other prescribed material.

---

### Question 2: [Insert title]

Use the same structure as for Question 1, with headings

* 5.2.1 Aim of the question
* 5.2.2 Data and variables
* 5.2.3 Conceptual and econometric approach
* 5.2.4 Thinking process and steps followed
* 5.2.5 Implementation and how to run the code
* 5.2.6 Outputs
* 5.2.7 Theoretical results and references

Fill in the details that apply to Question 2.

---

### Question 3: [Insert title]

Repeat the same headings

* 5.3.1 Aim of the question
* 5.3.2 Data and variables
* 5.3.3 Conceptual and econometric approach
* 5.3.4 Thinking process and steps followed
* 5.3.5 Implementation and how to run the code
* 5.3.6 Outputs
* 5.3.7 Theoretical results and references

---

### Question 4: [Insert title]

Headings

* 5.4.1 Aim of the question
* 5.4.2 Data and variables
* 5.4.3 Conceptual and econometric approach
* 5.4.4 Thinking process and steps followed
* 5.4.5 Implementation and how to run the code
* 5.4.6 Outputs
* 5.4.7 Theoretical results and references

---

### Question 5: [Insert title]

Headings

* 5.5.1 Aim of the question
* 5.5.2 Data and variables
* 5.5.3 Conceptual and econometric approach
* 5.5.4 Thinking process and steps followed
* 5.5.5 Implementation and how to run the code
* 5.5.6 Outputs
* 5.5.7 Theoretical results and references

---

### Question 6: [Insert title]

Headings

* 5.6.1 Aim of the question
* 5.6.2 Data and variables
* 5.6.3 Conceptual and econometric approach
* 5.6.4 Thinking process and steps followed
* 5.6.5 Implementation and how to run the code
* 5.6.6 Outputs
* 5.6.7 Theoretical results and references

---

### Question 7: [Insert title]

Headings

* 5.7.1 Aim of the question
* 5.7.2 Data and variables
* 5.7.3 Conceptual and econometric approach
* 5.7.4 Thinking process and steps followed
* 5.7.5 Implementation and how to run the code
* 5.7.6 Outputs
* 5.7.7 Theoretical results and references

---

End of README


# Description

This folder was created by first creating a folder on your desktop -
copying the adress and then simply running in any instance of R:

`{r, eval = F} fmxdat::make_project()`

Next, download <https://www.fmx.nfkatzke.com/FMX_data_2021.zip> and
unzip its contents into the data/ folder in this root.

> NB: add data/ to your gitignore file when creating a repository.
> Ensure you do not commit the data folder please.

Next, create sub-folders for each question by using, e.g.:

\`\`\`{r}

Do this fo all 5 questions inside ecah question folder we have the following subfolders code , data , output ( subfolders inside this are plots and results )

# PDF Texevier:

Texevier::create_template(directory = “Questions”, template_name =
“Q1” )

# HTML Texevier:

Texevier::create_template_html(directory = “Questions”, template_name =
“Q2” )

\`\`\`

Now add the relevant data for each question into the respective data/
folders in the different Question folders.

Check that your .gitignore file ignores all folders starting with data.

> NB - this README (at the root of your folder) - is what I will be
> grading.

> Space out code per question in this README - detailing your steps and
> thinking.

> Folders in Questions/… should not contain READMEs - I will only be
> looking at their output (pdf / html).

> THIS README should contain and describe the code you used in your
> analyses.

Good luck and enjoy the practical!

Nico
