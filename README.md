# Active Learning Applied to Land Use and Land Cover Samples Definition

[![Miniconda](https://img.shields.io/badge/miniconda-3-green)](https://docs.conda.io/en/latest/miniconda.html)
[![Docker SITS](https://img.shields.io/badge/BDC_SITS_RStudio-0.15.0-green)](https://hub.docker.com/r/brazildatacube/sits-rstudio)
[![R SITS](https://img.shields.io/badge/BDC_R_SITS-0.15.0-green)](https://github.com/e-sensing/sits)


> **INPE - Instituto Nacional de Pesquisas Espaciais**</br>
> Master's Program Applied Computing – Geospatial Data Science</br>
> CAP-421-3 Deep Learning</br>
> Master Degree Period 3° / 2021 December 20</br>
> Abner Ernâni dos Anjos</br>
> **Trabalho Final**
>
> Este Jupyter Notebook contém os estudos relacionados ao projeto final da disciplina de aprendizado profundo.
> A tabela apresenta as entregas da disciplina
>
> Entrega                  | Descrição
> -------------------------|-----------
> [Slides da Apresentação](https://drive.google.com/file/d/17S3msSZWz80V-7kC5LOzq29I6qf-kzla/view?usp=sharing) | Link do google drive para o powerpoint da apresentação em formato PDF.
> [Artigo do Trabalho Final](https://drive.google.com/file/d/16Vr_onhYtsL0M0Rw4wi9hUUjoMA0GEij/view?usp=sharing) | Link do google drive para o artigo do trabalho final em formato PDF.
> [Código-fonte](https://github.com/AbnerErnaniADSFatec/sample-assessment-geodatascience/tree/main/GeoDataScience.ipynb) | Link para o Jupyter Notebook no github com o código e a explicação dos dados.
> [Dados Usados](https://github.com/AbnerErnaniADSFatec/sample-assessment-geodatascience/tree/main/data/samples) | Link para os dados usados no repositório do github contendo os dados brutos.
>
> Obs.: Contact abiner.anjos@inpe.br or abiner.anjos@gmail.com.

## Development Environment

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
conda install -c conda-forge --file ./R/requirements.txt
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

### Material and Source Code

With the dependencies installed it is possible to run the Jupyter notebook environment and also possible to run the _scripts_ present in this repository. This approach is based on two `Jupyter Notebooks` as you see below:

- The Jupyter Notebook require to download the images for reproduction of the tests is available on this link ["Data Science for LULC Samples"](https://github.com/AbnerErnaniADSFatec/sample-assessment-data-science).

- All test data can be acquired from github via the link ["Data Source"](https://github.com/AbnerErnaniADSFatec/computational-statistics-data/tree/main/data-science).

 - The notebook for application and analysis of active learning for land use and land cover using the R programming language: ["Jupyter Notebook to Defining Land Use and Land Cover Samples for classification"](GeoDataScience.ipynb).


> Obs.: This repository contains a test environment for the web interface for exploratory samples analysis [Sample Assessment App](https://github.com/AbnerErnaniADSFatec/sample-assessment).