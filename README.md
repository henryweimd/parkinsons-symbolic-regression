# AI Face-Off: Linear vs Symbolic Regression (Dual-Dataset Showcase)

🚀 **[Click Here to View the Live Interactive Dashboard (R Shiny)](https://henryweimd-parkinsons-symbolic-regression.share.connect.posit.cloud/)**

## 🐍 Try the Python Version!
A modernized, high-performance version of this dashboard has been built using **Python (Streamlit)** and **PySR**.
- **[Live Python Demo (Streamlit)](https://symbolic-regression-demo.streamlit.app/)**
- **[Python Source Code (GitHub)](https://github.com/henryweimd/symbolic-regression-python-demo)**

![App Screenshot](screenshot.png)

This repository contains an interactive R Shiny application that compares the performance and approach of **Symbolic Regression** (via Grammatical Evolution) against standard **Linear Regression** on a medical dataset.

## The Datasets
This dashboard features two completely different medical datasets to perfectly demonstrate where traditional Linear AI works, and where it spectacularly fails.

1. **Parkinson's Telemonitoring (UCI):** Predicting clinical `motor_UPDRS` severity using non-invasive vocal cord measurements (Jitter, Shimmer, HNR, PPE). This demonstrates how AI handles highly noisy, real-world biological data.
2. **US Medical Insurance Costs:** Predicting yearly medical charges based on Age, BMI, and Smoker status. This is the "Mic Drop" dataset—Linear Regression struggles here because BMI only spikes costs *if* the patient is also a smoker. Symbolic regression naturally discovers this hidden multiplier and vastly outperforms the traditional model.

## What is Symbolic Regression?
Instead of assuming a pre-defined mathematical structure (like $y = mx + b$), Symbolic Regression uses evolutionary algorithms to "invent" mathematical formulas from scratch. The algorithm starts with random math operators (addition, sine, logs) and variables, evaluates how well they fit the data, and breeds the best equations together over multiple generations to find an optimal formula.

## Running the App
This app relies on the `gramEvol` and `shiny` R packages. It has been optimized to be lightweight and fast, meaning you can run it purely in the browser using WebAssembly.

To run it instantly in your browser without installing R:
1. Copy the contents of `app.R`
2. Paste it into the [Shinylive Editor](https://shinylive.io/r/editor/)
3. Click "Run"

Alternatively, you can connect this repository to **Posit Connect Cloud** for automatic Git-backed deployments.
