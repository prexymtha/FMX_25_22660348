# Financial Econometrics 871 Practical Examination 2025

SU number: 22660348
Author: Precious Nhamo
Course: Financial Econometrics 871
Lecturer: NF Katzke
Internal moderator: Prof G Liu
Dates: 18–19 November 2025

---

## Table of Contents

<details>
<summary>Core document sections</summary>

Project overview
Computational environment and setup
Repository structure
General replication workflow
Question by question reproducibility and thinking process

</details>

<details>
<summary>Question 1: Portfolio return comparison</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 2: Peaks and troughs</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 3: Momentum</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 4: Volatility and GARCH estimates</summary>

Aim of the question
Data and variables
Conceptual and econometric approach
Thinking process and steps followed
Implementation and how to run the code
Outputs
Theoretical results and references

</details>

<details>
<summary>Question 5: Portfolio construction</summary>

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

This repository contains my practical examination submission for the Financial Econometrics 871 course. The assessment consists of five core questions, each of which requires the application of quantitative methods to real financial data in order to answer a practical, economically meaningful problem:

Question 1 focuses on portfolio returns and peer comparison for an active fund manager.

Question 2 investigates peak to trough behaviour and volatility patterns across firm size groups and sectors.

Question 3 studies the performance of a momentum signal and relates it to an actual local equity fund.

Question 4 analyses currency volatility and the relationship between the rand, carry, valuation and global risk sentiment using GARCH style measures.

Question 5 constructs and evaluates a global balanced index style portfolio subject to realistic constraints.

Across all questions, the aim is to demonstrate a clear, reproducible and well documented workflow in R. For each question I:

1. State the aim and context in plain language.
2. Describe the data used and transformations applied.
3. Explain the conceptual and econometric choices made.
4. Record the step by step thinking process.
5. Point to the scripts that implement the analysis.
6. Summarise the main outputs and how they answer the question.
7. Link back to the core theory and references used.

This README is the only document that is graded for structure and explanation. No additional READMEs are used inside the question folders.

---

## 2. Computational environment and setup

### 2.1 Software and versions

R version: [insert R version used, for example 4.4.1]
Integrated development environment: [for example RStudio [insert version]]
Operating system: [for example Windows 10 / macOS Sonoma / Ubuntu 22.04]

Key R packages used across questions include, among others:

tidyverse for data wrangling and plotting.
PerformanceAnalytics for portfolio and risk measures.
lubridate and related packages for date handling.
rugarch and rmgarch for univariate and multivariate GARCH modelling where relevant.
MTS or similar time series packages where needed.

Any additional packages used are documented in the code comments for each question and briefly mentioned in the relevant question section below.

### 2.2 Randomness and seeds

Wherever simulations, random portfolios or resampling procedures are used, I fix a random seed at the start of the main script for that question using set.seed([insert seed value]).

The seed value used for each question is stated explicitly in that question’s section under “Implementation and how to run the code” so that stochastic results can be reproduced exactly.

### 2.3 Project and data setup (lecturer instructions)

The project folder with my student number as the root (22660348) follows the structure provided by the lecturer. The setup steps were as follows:

1. Create the root project folder (for example on the desktop) and initialise an R project there.
2. Download the official folder structure archive supplied for the 2025 exam and use it as the basis for the subfolders.
3. Download the exam data archive supplied for the 2025 exam and extract all data files into the data folder as specified by the lecturer.
4. Ensure that all data folders are listed in the .gitignore file so that no raw data are committed to version control.
5. For each question, create a separate question folder using the Texevier helper functions, so that code, data and output are neatly organised.
6. Ensure that this root README is kept up to date, and that no READMEs are stored within individual question folders.

---

## 3. Repository structure

The repository is organised by question. Each question (Q1 to Q5) has its own folder with a consistent internal structure. The only README in the project is this file at the root level.

A schematic folder layout is as follows. The exact names of the top level question folders may follow the Texevier template (for example “Q1” or “Question1”), but the internal structure for each is consistent.

22660348/
│
├── Q1/
│   ├── code/          R scripts and functions for Question 1
│   ├── data/          Raw and pre processed data used for Question 1
│   └── output/
│       ├── plots/     Figures for Question 1 (for example time series, scatterplots, boxplots)
│       └── results/   Tables, model summaries and other numerical results for Question 1
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
└── README.md          This document

Where relevant, additional subfolders can be added inside code or output (for example “functions” under code, or “diagnostics” under output), but the basic pattern above is maintained so that the examiner can easily navigate the project.

---

## 4. General replication workflow

To replicate the analysis for any question Qk, with k from 1 to 5, the general steps are:

1. Open the R project associated with the root folder 22660348.

2. Ensure that all required packages for that question are installed. Any non standard packages are mentioned in the relevant question section and in the header of the main script.

3. Confirm that the data folder for that question contains the required data files as described under “Data and variables” for that question.

4. Run the main script for that question from the project root. In this README I refer to these as Q1_main.R, Q2_main.R and so on, located in the appropriate code subfolder. Each main script is designed to:

   a. Load the required libraries.
   b. Import the data from the corresponding data subfolder.
   c. Perform any necessary cleaning and transformation of the data.
   d. Implement the econometric analysis and calculations described in this README.
   e. Save all plots and tables to the relevant output subfolders for that question.

5. After running the main script, inspect the plots in the output/plots subfolder and the tables or text files in the output/results subfolder for that question. These outputs correspond to the results discussed in this README and used to answer the exam questions.

Any additional helper scripts or functions are documented under “Implementation and how to run the code” for the relevant question.

---

## 5. Question by question reproducibility and thinking process

The following subsections provide a template for each exam question. For each question I fill in:

aim,
data and variable definitions,
conceptual and econometric choices,
the stepwise thinking process,
implementation details,
a short description of outputs, and
the main theoretical results and references relied upon.

The structure is intentionally repeated so that the examiner can follow the same logic across all five questions.

---

### Question 1: Portfolio return comparison

**Short descriptive title:** Portfolio return comparison and peer analysis for ABC Fund

#### 5.1.1 Aim of the question

Summarise in words what this question is about. For example, the aim here is to compare the benchmark, a set of active managers and the ABC Fund in terms of return behaviour and risk, and to produce clear visual material that can be used in a conference presentation. This includes showing how ABC Fund differs from the industry in terms of beta, drivers of returns and performance during high dispersion periods.

#### 5.1.2 Data and variables

Data sets used (for example industry benchmark returns, active manager returns and ABC Fund returns) are stored in the data subfolder for Question 1.

For each data set I record:

Name and file location in the project.
Sample period and frequency of the data (for example monthly or daily returns).
Key variables used (for example benchmark index return, peer fund returns, ABC Fund returns).
Any transformations applied (for example conversion to excess returns, rolling windows for betas, construction of dispersion measures).

#### 5.1.3 Conceptual and econometric approach

In this section I explain the main ideas that guide the analysis. For this question, that includes:

How I measure and compare rolling beta across funds.
How I characterise “something other than market beta” for ABC Fund (for example residual return behaviour, factor exposures or tracking error).
How I define and measure market dispersion and how I relate it to ABC Fund’s relative performance.
Which plots and summary statistics I use to communicate the story to a non quantitative audience.

I briefly justify why each method is appropriate for the question and for the data provided.

#### 5.1.4 Thinking process and steps followed

Here I list the logical sequence of steps I followed, from exploratory analysis to final visuals. A typical sequence for this question might include:

Exploratory plots of benchmark, peers and ABC Fund returns.
Computation and plotting of rolling betas relative to the benchmark.
Analysis of cross sectional dispersion across funds and how ABC Fund behaves in high dispersion periods.
Construction of visuals that show relative performance through time in a clear way.

The emphasis is on explaining why I did things in a particular order and how each step feeds into the next.

#### 5.1.5 Implementation and how to run the code

Main script for this question: stored in Q1/code, for example Q1_main.R.

Purpose of the main script:

Load packages and data.
Create derived variables (rolling betas, dispersion measures and relative performance series).
Generate all plots used for the manager’s presentation.
Write any summary tables or statistics to the output/results subfolder.

Any additional helper scripts (for example for plotting or repeated calculations) are also located in Q1/code and are briefly described here.

To reproduce the results, open the project at the root folder, ensure the working directory is set to 22660348, and run the main script for Question 1.

#### 5.1.6 Outputs

Key outputs for this question are stored in Q1/output/plots and Q1/output/results.

Plots include, for example:

Rolling beta comparison across funds.
Relative performance of ABC Fund versus peers over time.
Visualisation of performance in high versus low dispersion environments.

Results include, for example:

Summary tables of betas, returns and dispersion statistics.
Any numerical summaries discussed in the written answer.

For each main output, I briefly state how it answers part of the manager’s request.

#### 5.1.7 Theoretical results and references

In this part I list the key definitions and results used, together with their sources. For example:

Definition of beta and rolling beta estimation.
Measures of cross sectional dispersion.
Any portfolio statistics (for example tracking error, information ratio) used in my interpretation.

I mention whether these come from lecture notes, standard textbooks or published articles, without reproducing full derivations.

---

### Question 2: Peaks and troughs

**Short descriptive title:** Peak to trough ranges and volatility across size and sector groups

#### 5.2.1 Aim of the question

Here I state the purpose in plain language. For example, the aim is to explore whether smaller stocks are more volatile than larger stocks and to quantify typical peak to trough moves across size groups and sectors over recent years, using local daily holdings and returns data.

#### 5.2.2 Data and variables

I describe the holdings and returns data set used for this question, including:

The file location in Q2/data.
The time period and frequency of the data.
How sector and size group labels are defined.
How peak to trough measures are constructed (for example within each year for each stock or group).

#### 5.2.3 Conceptual and econometric approach

This section explains how I measure volatility and peak to trough movements and how I compare them across groups. I also mention any additional exploratory angles taken, such as comparing different subperiods or using alternative volatility measures.

#### 5.2.4 Thinking process and steps followed

I outline the steps taken, such as:

Cleaning and merging holdings, returns, sectors and size groups.
Computing yearly peak to trough ranges and volatility measures.
Aggregating and comparing across groups.
Producing tables and plots that reveal the patterns.

#### 5.2.5 Implementation and how to run the code

Main script located in Q2/code, for example Q2_main.R, which carries out the full analysis from data import to output creation. Any helper functions are stored in the same folder and briefly described here.

#### 5.2.6 Outputs

Plots and tables summarising peak to trough behaviour and volatility by size group and sector are stored in Q2/output/plots and Q2/output/results, with a short description of how each one informs the answer.

#### 5.2.7 Theoretical results and references

Definitions and properties of the volatility and range measures used are noted here, together with references to lecture material or textbooks.

---

### Question 3: Momentum

**Short descriptive title:** Momentum as an investment signal and comparison with a local equity fund

#### 5.3.1 Aim of the question

The aim is to evaluate how well a chosen momentum signal has worked locally over the past decade and to compare the performance of a colleague’s long only fund with that signal. This includes examining the information coefficient, signal decay and quintile portfolio performance.

#### 5.3.2 Data and variables

I state which data sets are used (for example holdings and returns, fund holdings) and where they are stored under Q3/data. I define clearly:

How the momentum signal is constructed.
The horizon over which future returns are measured.
How quintiles or other ranks are formed.

#### 5.3.3 Conceptual and econometric approach

This part explains:

How I define and calculate the information coefficient.
How I measure signal decay across future periods.
How I form long minus short portfolios based on momentum quintiles.
How I compare the colleague’s fund to the benchmark and to the momentum strategy.

I also mention that the main results are reported using a Texevier style document as requested in the question.

#### 5.3.4 Thinking process and steps followed

I record the ordered steps taken, from choosing a momentum definition, through cleaning and aligning data, to computing ICs, decay profiles and portfolio returns.

#### 5.3.5 Implementation and how to run the code

The main script for this question is stored in Q3/code. It prepares the data, computes the signal, constructs the evaluation metrics and writes both numerical and Texevier output files to Q3/output. Any separate script that generates the Texevier report is noted here.

#### 5.3.6 Outputs

Outputs include:

Texevier summary document explaining the momentum results.
Plots of IC over time, signal decay and quintile performance.
Tables of key statistics and fund versus momentum comparisons.

#### 5.3.7 Theoretical results and references

I list definitions of the information coefficient, signal decay and portfolio sorting procedures, with references to standard factor investing literature and the article used as inspiration where relevant.

---

### Question 4: Volatility and GARCH estimates

**Short descriptive title:** Currency volatility, rand behaviour and GARCH based measures

#### 5.4.1 Aim of the question

The aim is to assess claims about the rand’s volatility relative to other currencies and its behaviour in relation to carry trades, valuations and global dollar strength.

#### 5.4.2 Data and variables

I describe the currency data and any additional series on carry, valuation and a dollar index, and explain how I construct the return and volatility series used in the analysis.

#### 5.4.3 Conceptual and econometric approach

This section outlines:

How I measure volatility (for example realised volatility, GARCH conditional volatility or implied volatility).
Which currencies I choose for comparison and why.
How I combine volatility estimates with measures of carry, value and dollar strength to address the statements given in the question.

#### 5.4.4 Thinking process and steps followed

I document the sequence from data preparation, through exploratory analysis and model selection, to interpretation of the results in relation to the economic statements.

#### 5.4.5 Implementation and how to run the code

The main script for Question 4 is stored in Q4/code. It imports the necessary data, fits any GARCH models, computes volatility measures and generates the tables and plots used in the written answer.

#### 5.4.6 Outputs

Outputs in Q4/output/plots and Q4/output/results include:

Comparative volatility plots and tables.
Results relating rand behaviour to carry, value and the dollar index.

Each key output is explained briefly here.

#### 5.4.7 Theoretical results and references

I list the main time series and GARCH results used, together with references to lecture notes and standard time series textbooks, noting which variance equations or test statistics I rely on.

---

### Question 5: Portfolio construction

**Short descriptive title:** Global balanced index style portfolio construction

#### 5.5.1 Aim of the question

The aim is to build and evaluate a rules based global balanced portfolio using a set of equity, bond and commodity index series, subject to practical constraints on asset classes and individual exposures, and using realistic covariance and mean estimates.

#### 5.5.2 Data and variables

I describe the index data used, where it is stored in Q5/data, how I select the eligible assets and how I form the monthly return series used in the optimisation.

#### 5.5.3 Conceptual and econometric approach

This section explains:

How I choose the estimation window and handle assets with shorter history.
How I estimate expected returns and the covariance matrix.
Which optimisation or construction approach I use (for example mean variance optimisation subject to constraints, or a heuristic approach if appropriate).
How the constraints on asset classes and single asset exposures are implemented.
How quarterly rebalancing is handled in the backtest.

#### 5.5.4 Thinking process and steps followed

I present the workflow from data preparation through estimation, optimisation, backtesting and evaluation of the resulting portfolio.

#### 5.5.5 Implementation and how to run the code

The main script for Question 5 in Q5/code runs the complete portfolio construction and backtest, saving performance plots and summary tables to Q5/output. Any additional scripts for helper functions are mentioned here.

#### 5.5.6 Outputs

Outputs include:

Time series plots of portfolio and benchmark returns.
Tables of performance and risk statistics.
Any additional diagnostics that illustrate how the constraints and asset allocations evolved over time.

#### 5.5.7 Theoretical results and references

I list the key portfolio theory results used (for example mean variance concepts, constraint handling, performance measure definitions), together with references to relevant lecture notes and textbooks.

---

**End of README**




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
