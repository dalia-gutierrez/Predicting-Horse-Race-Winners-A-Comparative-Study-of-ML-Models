# Predicting-Horse-Race-Winners-A-Comparative-Study-of-ML-Models
Scrapes Palermo race data, engineers features (age, weight, jockey, pedigree), and compares Elastic Net, RF, XGBoost, NN to predict winners.

![R](https://img.shields.io/badge/R-4.3%2B-blue)
![caret](https://img.shields.io/badge/caret-ML-orange)
![F1](https://img.shields.io/badge/F1-0.28-red)
![AUC](https://img.shields.io/badge/AUC-0.67-yellow)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

---

## Project Overview

This project builds a **full end-to-end ML pipeline** to predict horse race winners using public data from the **Hipódromo Argentino de Palermo**. It demonstrates **quant-grade rigor** in data collection, feature engineering, and model evaluation — ideal for high-frequency prediction tasks.

**Key challenge**: Predict the winner **per race** using only pre-race information (no odds, no live data). Class imbalance: ~1 in 10 horses wins.

---

## Key Features

| Feature | Description |
|-------|-----------|
| **Web Scraping** | `data_extract.R` uses `rvest` to scrape race metadata, horse pedigrees, jockeys, weights, and track conditions |
| **Feature Engineering** | Age in months, race time, rare category grouping (`Other` for <100 races), one-hot encoding |
| **Race-Level CV** | Train/test split by `race_id` → **no data leakage** |
| **F1 Optimization** | Custom metric + upsampling via `caret` |
| **Model Comparison** | Logistic with Elastic Net, Random Forest, XGBoost, Neural Net (with PCA) |
| **Parallel Training** | `doParallel` for speed |

---

## Model Results

| Model | F1 | AUC |
|------|----|-----|
| Elastic Net | 0.27 | 0.67 |
| Random Forest | 0.28  & 0.67 |
| **XGBoost** | **0.29** | **0.67** |
| Neural Net (PCA) | 0.24 & 0.62 |

> **Honest, real-world performance** — horse racing is near-random without market data. Focus: **correct methodology**, not overfitting.
