# Sample Assessment Geo Data Science

## Abstract

Over the last years, remote sensing has developed satellite sensors to collect high spatial and temporal resolution images from Earth's surface generating large data and new opportunities for developing methods to access and analyse this data. Earth observation data cubes have become the main method for structuring data and providing access to classification methods. With data cubes it is possible to analyse the Earth's surface over temporal and spatial attributes and allows the extraction of Land Use and Land Cover (LULC) features and patterns to map land use changes. This also provides an extensive mass of historical data about phenology and years of image time series [[1]](./README.md#References). Soon, this historical data can be useful for training deep learning algorithms based on temporal and spatial dimensions. The paper [[1]](./README.md#References) demonstrated how this historical data can be used to generate LULC samples using Time-Weighted Dynamic Time Warping (TWDTW). As demonstrated in [[2]](./README.md#References) the current ways to classify land use and land cover maps like Random Forest and Support Vector Machines (SVM) algorithms have been successfully applied to the LULC change classification. However these algorithms do not use the temporal dimension, an important characteristic in data cubes, in the classification process, resulting in lost data. In other words, the order of the images does not influence the results. The approach presented by the paper [[2]](./README.md#References) proposed a comprehensive study of Temporal Convolutional Neural Networks (TempCNNs), an in-depth approach that applies convolutions in the temporal dimension to automatically learn temporal and spectral features. The study presented by [[3]](./README.md#References) demonstrates the good quality of TempCNN to accurately map LULC without over-representation of majority classes using TempCNN. This approach aims to present a study about remote sensing time series features extraction using TempCNN to LULC classification. The purpose is to provide and demonstrate the probability that a LULC class is accurate based on deep learning algorithms in order to contribute to LULC sample generation over the Brazilian Biomes territory. This approach will use the Brazil Data Cube infrastructure to collect data and analyse them. This study also seeks to demonstrate a comparison between the temporal based Neural Networks as TempCNN, non-temporal based like CNN and the current methods as the Random Forest and SVM Algorithm. As an expected result, this study aims to demonstrate the estimation of probabilities for LULC sample generation.

## Material and Methods

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
python -m pip install --upgrade pip
~~~

- Add `R-Kernel` kernel specification to `Jupyter` installation, so long the `Jupyterhub` will recognize the kernel installation to work interactively:

~~~dos
R -e "IRkernel::installspec(name = 'R3', displayname = 'sample-assessment')"
~~~

 - **Step 3**. Run jupyter lab environment server and enjoy:

~~~dos
jupyter lab
~~~

With the dependencies installed it is possible to run the Jupyter notebook environment and also possible to run the _scripts_ present in this repository. This approach is based on two `Jupyter Notebooks` as you see below:

 - The notebook for creation of data cube and analysis and classification land use and land cover using the R programming language: [Jupyter Notebook for a Sample Analysis and Estimating Probabilities](GeoDataScience.ipynb).

All test data can be acquired from github via the link ["Data Source"](https://github.com/AbnerErnaniADSFatec/computational-statistics-data/tree/main/data-science).

## Conclusion

Under Development...

## References

 - [1] Mariana  Belgiu,  Wietske  Bijker,  Ovidiu  Csillik,  and  AlfredStein. Phenology-based sample generation for supervised croptype  classification.International  Journal  of  Applied  EarthObservation and Geoinformation, 2021 https://doi.org/10.1016/j.jag.2020.102264.
 - [2] Pelletier, C.; Webb, G.I.; Petitjean, F. Temporal Convolutional Neural Network for the Classification of Satellite Image Time Series. Remote Sens. 2019, 11, 523. https://doi.org/10.3390/rs11050523.
 - [3] Simoes R, Camara G, Queiroz G, Souza F, Andrade PR, Santos L, Carvalho A, Ferreira K. Satellite Image Time Series Analysis for Big Earth Observation Data. Remote Sensing. 2021; 13(13):2428. https://doi.org/10.3390/rs13132428.