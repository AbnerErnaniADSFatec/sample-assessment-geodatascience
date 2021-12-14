# Active Learning Applied to Land Use and Land Cover Samples Definition

[![Miniconda](https://img.shields.io/badge/miniconda-3-green)](https://docs.conda.io/en/latest/miniconda.html)
[![Docker SITS](https://img.shields.io/badge/BDC_SITS_RStudio-0.15.0-green)](https://hub.docker.com/r/brazildatacube/sits-rstudio)
[![R SITS](https://img.shields.io/badge/BDC_R_SITS-0.15.0-green)](https://github.com/e-sensing/sits)

This repository contains a test environment for the web interface for exploratory samples analysis [Sample Assessment App](https://github.com/AbnerErnaniADSFatec/sample-assessment).

## Abstract

Land Use and Land Cover (LULC) classification require large training data sets with representative samples per class for accurated results. However the current methods for selecting samples is based on random sampling where the study in confusion of the pixels takes no place. This approach aims to demonstrate an active learning method to define training LULC samples based on probability estimation. We use a Temporal Convolutional Neural Network (TempCNN) to rank the unlabelled samples and automatically chooses those that are considered the most confuses for its improvement. With a small and non-optimal initial data set, the TempCNN model is trained and its results gives the confusion and error required for define new samples where will be added for next iteration of training. The method has been tested with LULC samples in Southwest Amazon in Brazil localized in the State of Rondônia. Experimental results confirm the consistency of the methods. This method could reduce the require of large training samples set in 0% and improve the accuracy results based on old methods to LULC classifition.

**Key words** – TempCNN, Classification, Time Series, Samples Assessment, Spatial Temporal Analysis, Satellite Image Data.

## Resumo

A classificação de Uso e Cobertura da Terra (UCT) requer grandes conjuntos de dados de treinamento com amostras representativas por classe para resultados precisos. No entanto, os métodos atuais de seleção de amostras baseiam-se em amostragem aleatória, onde o estudo de confusão dos pixels não ocorre. Esta abordagem visa demonstrar um método de aprendizado ativo para definir amostras de treinamento LULC com base na estimativa de probabilidade. Usamos uma Rede Neural Convolucional Temporal (TempCNN) para classificar as amostras não marcadas e escolhemos automaticamente aquelas que são consideradas mais confusas para o seu aprimoramento. Com um conjunto de dados inicial pequeno e não ideal, o modelo TempCNN é treinado e seus resultados fornecem a confusão e o erro necessários para definir novas amostras onde serão adicionadas para a próxima iteração de treinamento. O método foi testado com amostras LULC no sudoeste da Amazônia no Brasil localizadas no estado de Rondônia. Os resultados experimentais confirmam a consistência dos métodos. Este método pode reduzir a necessidade de grandes amostras de treinamento definidas em 0% e melhorar os resultados de precisão com base em métodos antigos de classificação UCT.

**Palavras-chave** - TempCNN, Classificação, Séries Temporais, Avaliação de Amostras, Análise Temporal Espacial, Dados de Imagens de Satélite.

## Introduction

Over the last years, remote sensing has developed satellite sensors to collect high spatial and temporal resolution images from Earth's surface generating large data and new opportunities for developing methods to access and analyse this data, as Earth observation data cubes. With data cubes it is possible to analyse the Earth's surface over temporal and spatial attributes and allows the extraction of Land Use and Land Cover (LULC) features and patterns to map land use changes. This also provides an extensive mass of historical data about phenology and years of image time series [[1]](./README.md#References). Soon, this historical data can be useful for training deep learning algorithms based on temporal and spatial dimensions.

The paper [[1]](./README.md#References) demonstrated how this historical data can be used to generate LULC samples using Time-Weighted Dynamic Time Warping (TWDTW). As demonstrated in [[2]](./README.md#References) the current ways to classify land use and land cover maps like Random Forest and Support Vector Machines (SVM) algorithms have been successfully applied to the LULC change classification.

However these algorithms do not use the temporal dimension, an important characteristic in data cubes, in the classification process, resulting in lost data. In other words, the order of the images does not influence the results. The approach presented by the paper [[2]](./README.md#References) proposed a comprehensive study of Temporal Convolutional Neural Networks (TempCNNs), an in-depth approach that applies convolutions in the temporal dimension to automatically learn temporal and spectral features.

The study presented by [[3]](./README.md#References) demonstrates the good quality of TempCNN to accurately map LULC without over-representation of majority classes using TempCNN. This approach aims to present a study about remote sensing time series features extraction using TempCNN to LULC classification to estimate the probability of LULC class based on deep learning algorithms in order to contribute to LULC sample generation over the Brazilian Biomes territory.

This approach will use the Brazil Data Cube infrastructure to collect data and analyse them. To estimate the probability of LULC classes, this study will use an active learning method, a stored model that will predict the class of unlabeled data based on labeled data and stored, as an oracle. This study also seeks to demonstrate a comparison between the temporal based Neural Networks as TempCNN, non-temporal based like CNN and the current methods as the Random Forest and SVM Algorithm.

## Methodology

### Material and Methods

This approach uses a dataset of LULC samples coupled with time series collected with the creation of a local data cube. The Data extraction was performed by three steps: (1) the images download and analysis of quality, (2) the time series data extraction and (3) the samples quality analysis for the Random Forest algorithm input.

For this approach, a dataset of LULC samples identified by specialists based on high-resolution images combined with time series collected with the creation of a Sentinel-2 satellite local data cube, acquired with the STAC Client tool in Python programming language, was used. This images was previously processed and analyzed in the Jupyter Notebook workflow publishing and presentation platform.

With the [SITS - Satellite Image Time Series](https://github.com/e-sensing/sits) package for the R programming language, time series were extracted to perform the exploratory analysis of the samples, evaluating the quality of the samples and forming a new data set, thus calculating the statistical moments for each series associated with a class, where the Random Forest algorithm was used for classification, demonstrated that the accuracy of the classification process was somehow interesting.

This project uses a [Miniconda](https://docs.conda.io/en/latest/miniconda.html) virtual environment with the installation of necessary libraries.

Miniconda is a free minimal installer for conda. It is a small, bootstrap version of Anaconda that includes only conda, Python, the packages they depend on, and a small number of other useful packages, including pip, zlib and a few others. Use the conda install command to install 720+ additional conda packages from the Anaconda repository.

Follow the steps for installation and configuration below:


 - **Step 1**. Update and install the virtual environment management with command line interface for Miniconda and creates the environment for run the Jupyter Notebooks:

~~~dos
conda update -n base -c defaults conda && \
    conda create --name sample-assessment r-base
~~~

 - Starts the last virtual environment created:

~~~dos
conda activate sample-assessment
~~~

 - **Step 2**. Generate the R Kernel for the environment. First install the packages `R-kernel`, `Jupyter`, `SITS` dependencies and the `stars` library for image raster data manipulation:

~~~dos
conda install -c conda-forge --file ./utils/R/requirements.txt
~~~

 - Install the necessary dependencies for data visualization and analysis that are listed in this file [`./R/install-requirements.R`](./R/install-requirements.R) to facilitate the installation for R packages:

~~~dos
R -e "source('./R/install-requirements.R')"
~~~

 - Update the `Python` dependencies installation.

~~~dos
python -m pip install --upgrade pip &&
    python -m pip install ipywidgets seaborn tensorflow
~~~

- Add `R-Kernel` kernel specification to `Jupyter` installation, so long the `Jupyterhub` will recognize the kernel installation to work interactively:

~~~dos
R -e "IRkernel::installspec(name = 'R3', displayname = 'sample-assessment')"
~~~

 - **Step 3**. Run jupyter lab environment server and enjoy:

~~~dos
jupyter lab
~~~

### Download Data

With the dependencies installed it is possible to run the Jupyter notebook environment and also possible to run the _scripts_ present in this repository. This approach is based on two `Jupyter Notebooks` as you see below:

 - The notebook for application and analysis of active learning for land use and land cover using the R programming language: ["Jupyter Notebook to Defining Land Use and Land Cover Samples for classification"](GeoDataScience.ipynb).

The Jupyter Notebook require to download the images for reproduction of the tests is available on this link ["Data Science for LULC Samples"](https://github.com/AbnerErnaniADSFatec/sample-assessment-data-science).

All test data can be acquired from github via the link ["Data Source"](https://github.com/AbnerErnaniADSFatec/computational-statistics-data/tree/main/data-science).

## Conclusion

Under Development...

## References

 - [1] Mariana  Belgiu,  Wietske  Bijker,  Ovidiu  Csillik,  and  AlfredStein. Phenology-based sample generation for supervised croptype  classification.International  Journal  of  Applied  EarthObservation and Geoinformation, 2021 https://doi.org/10.1016/j.jag.2020.102264.
 - [2] Pelletier, C.; Webb, G.I.; Petitjean, F. Temporal Convolutional Neural Network for the Classification of Satellite Image Time Series. Remote Sens. 2019, 11, 523. https://doi.org/10.3390/rs11050523.
 - [3] Simoes R, Camara G, Queiroz G, Souza F, Andrade PR, Santos L, Carvalho A, Ferreira K. Satellite Image Time Series Analysis for Big Earth Observation Data. Remote Sensing. 2021; 13(13):2428. https://doi.org/10.3390/rs13132428.
 - [4] Gilberto Camara and Rolf Simoes. Data sets for the sits package, 2021.
 - [5] Adeline Marinho Maciel and Lúbia Vinhas. Time series classification using features extraction to identification of use land and cover land: A case study in the municipality of itaqui, south region of brazil. Anais do XVIII Simpósio Brasileiro de Sensoriamento Remoto, 2017.
 - [6] Rolf Simoes, Gilberto Camara, Felipe Souza, Pedro Andrade, Lorena Santos, Karine Ferreira, Gilberto Queiroz, Alexandre Ywata de Carvalho, and Victor Maus. sits: Data Analysis and Machine Learning using Satellite Image Time Series. INPE - Brazilian National Institute for Space Research, Sao Jose dos Campos, Brazil, 2021.
 - [7] Adeline Marinho Maciel and Lúbia Vinhas. Time series classification using features extraction to identification of use land and cover land: A case study in the municipality of itaqui, south region of brazil. Anais do XVIII Simpósio Brasileiro de Sensoriamento Remoto, 2017.