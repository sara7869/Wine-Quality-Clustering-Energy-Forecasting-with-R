# Wine Quality Clustering & Energy Forecasting with R

## Overview

This repository contains two main projects that focus on clustering analysis for wine quality assessment and time-series forecasting for energy consumption:

1. **Wine Quality Clustering**: A k-means clustering approach is applied to a dataset of white wines from a Portuguese region, using both physicochemical and sensory properties to classify the wine quality. The analysis involves feature scaling, outlier handling, and dimensionality reduction (PCA). We evaluate the performance of different clustering models and interpret the results with standard accuracy metrics.

2. **Energy Consumption Forecasting**: A multilayer perceptron neural network (MLP-NN) model is built to predict next-day electricity consumption. The model uses time-series data from a real-life building and incorporates autoregressive input vectors, exploring the influence of different lagged time periods. The MLP’s architecture is fine-tuned with various configurations, and the forecasting performance is measured with RMSE, MAE, and MAPE metrics.

## Clustering Analysis of Wine Data

### Dataset:
The dataset contains 4710 white wine samples, each described by 11 continuous variables (chemical properties such as acidity, alcohol content, pH, etc.) and one quality score (ordinal variable). The objective is to cluster the wines based on their chemical attributes and compare the clusters with their respective quality scores.

### Key Tasks:
- Pre-processing: Scaling, outlier detection, and removal.
- Cluster center identification: Using Elbow, Silhouette, and NBClust methods to define optimal k.
- K-Means Clustering: Applied for k = 2, 3, and 4, followed by performance analysis.
- PCA for Dimensionality Reduction: Reducing feature space and re-applying k-means clustering.
- Model Evaluation: Confusion matrix, accuracy, precision, and recall indices.

### Tools:
- R for data analysis and visualization.
- `kmeans` function for clustering.
- `prcomp` for PCA.

## Energy Forecasting with Neural Networks

### Dataset:
This dataset contains hourly electricity consumption data for a building in London, with a focus on predicting the 11:00 a.m. hour’s consumption for the next day. The training data consists of 430 samples, and the testing set includes the remaining samples.

### Key Tasks:
- Feature Engineering: Autoregressive input vectors and normalization for input/output matrices.
- MLP Construction: Various network architectures with different hidden layers, learning rates, and activation functions.
- Model Evaluation: RMSE, MAE, and MAPE used to assess forecasting accuracy.
- Comparison: One-hidden-layer vs. two-hidden-layer networks based on weight parameters and performance.

### Tools:
- R for model development and evaluation.
- `neuralnet` package for MLP model implementation.

## Installation and Usage

### Prerequisites:
- R (version 4.0+)
- R libraries: `kmeans`, `prcomp`, `neuralnet`, `NBclust`

### Running the Project:
1. Clone the repository: 
   ```bash
   git clone https://github.com/sara7869/Wine-Quality-Clustering-Energy-Forecasting-with-R.git
