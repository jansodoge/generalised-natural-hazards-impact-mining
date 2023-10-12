# Generalised natural hazards impact text-mining


This repository contains a versatile version of the drought-impact text-mining method introduced by Sodoge et al. in 2023.

## Overview

The code provided here is a generalized and well-documented adaptation of the original code used in Sodoge et al. (2023). Its purpose is to streamline the process of extracting socio-economic drought impacts from newspaper text data and make it easily adaptable for different case studies. This repository eliminates the need to grapple with project-specific code, enabling swift implementation and replication.

## Key Features

- **Simplified Adaptation**: Easily adapt the approach to different datasets and case studies.
- **Robust Documentation**: Comprehensive documentation to assist users in understanding and implementing the code.
- **Replicability**: Designed with replication in mind, facilitating reproducible research.
- **Modular Structure**: The code is structured using the "targets" package in R, providing a clear and organized pipeline with separate functional components for tasks like impact classification and location extraction.

## Getting Started

To use this toolkit, you'll need to replace specific input files such as text data and geographic shape files with your own. Detailed instructions and examples for this process are provided in the documentation.

To structure the code and ensure replicability, we employ the "targets" package in R. You can find information on how to work with the "targets" package in our documentation [here](https://books.ropensci.org/targets/).


The entire code can initially be run using the command 
```
targets::tar_make()
```

An overview of the pipeline can be obtained using
```
targets::tar_glimpse()
```


## Contribution

Contributions and feedback are welcome. Feel free to open issues, submit pull requests, or reach out to the maintainers for any questions or suggestions.


