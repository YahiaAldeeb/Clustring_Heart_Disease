# 🧠 Clustering Heart Disease Patients

This project investigates whether patients with similar health profiles can be grouped together to support more targeted treatments, using unsupervised machine learning techniques. The analysis is implemented in **R** and applies a variety of clustering algorithms on real-world heart disease patient data.

---

## 📌 Project Objective

The goal is to cluster heart disease patients using unsupervised learning techniques to:

- Help doctors identify patterns in symptoms and treatments
- Group patients with similar health characteristics
- Explore the effects of feature selection and data scaling
- Compare multiple clustering models and evaluate performance using silhouette scores

---

## 🛠️ Tools & Libraries Used

- **R Programming Language**
- `cluster` – Clustering algorithms like K-Medoids, Hierarchical
- `factoextra` – Visualization of clusters
- `dbscan` – Density-based clustering
- `scales`, `caret` – Data scaling and manipulation
- `ggplot2`, `gridExtra` – Data visualization
- `stats` – PCA and distance matrix calculations

---

## 📊 Dataset Overview

- **File**: `heart_disease.csv`
- Contains health records with attributes like:
  - Age, Sex, Cholesterol (chol), Resting BP (trestbps), Max Heart Rate (thalach)
  - Chest Pain Type (cp), ST depression (oldpeak), Fasting Blood Sugar (fbs), etc.

---

## 🧹 Data Preprocessing

- Removed null values and duplicate rows
- Dropped irrelevant columns like `id`
- Applied outlier treatment using the IQR method
- Scaled numeric columns for better clustering performance
- Applied PCA for dimensionality reduction

---

## 📦 Models Implemented

### ✅ From Lab Content:
- **K-Means Clustering**  
- **Hierarchical Clustering**

### 🔍 Using External AI Tools:
- **K-Medoids Clustering**
- **DBSCAN (Density-Based Spatial Clustering of Applications with Noise)**

Each model was applied both on the **scaled dataset** and **PCA-transformed data**.

---

## 📈 Evaluation

- **Elbow Method** was used to find the optimal number of clusters
- **Silhouette Scores** were calculated for each model and dataset:
  - K-Means (Scaled & PCA)
  - K-Medoids (Scaled & PCA)
  - Hierarchical Clustering (Scaled & PCA)
  - DBSCAN (with noise filtering)

---

## 📉 Visualizations

- Histograms for distribution of features (age, sex, trestbps, etc.)
- Bar charts for categorical variables (fbs, cp, restecg, exang)
- Boxplots to identify and remove outliers
- Cluster visualizations using `fviz_cluster` and `ggplot2`
- PCA plots and Dendrograms for hierarchical models

---

## 🚀 How to Run

1. Make sure R and RStudio are installed.
2. Load the required packages:
   ```R
   install.packages(c("cluster", "factoextra", "dbscan", "scales", "caret", "gridExtra", "ggplot2"))
