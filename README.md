# spatialR workshop

This repository is for teaching materials associated with the spatialR workshop

Spatial Analysis in R: An introduction to data manipulation, spatial data analysis and statistical modeling. 

The statistical program R has become the de facto standard in the statistical, spatial and ecological modeling communities. Due to an extensive academic user community, developing R libraries, the software is readily extendable to encompass a very large variety of statistical and analytical approaches including: complex multi-criteria data summaries, linear/nonlinear regression, machine learning, spatial statistics, time series, ordination, imputation, clustering and wildlife modeling all with well-developed spatial object classes. Additionally, customized analysis is easily implemented through scripting, user-written functions and integration of external code support. R is object oriented with a complex scoping language that provides a robust programming environment. The software also has the capacity for online access, database connection and flexible data import functionality. The R programming environment, combined with an extensive suite of package environments, provides robust data manipulation and modeling, with state of the art algorithms, to both novice and advanced users.     

This workshop will focus on spatial analysis and modeling in R using experiential learning where students are expected to formulate and implement analysis. As such, participants will be required to conceptualize, develop procedural steps and then write R code. Instructors will provide brief background followed by “hands on” interactive guidance to assist in this process. The content and structure of the exercises will be high-paced with the goal to build basic knowledge in programming, complex spatial analytical methods and statistics. We will illustrate the flexibility of R for implementing specialized analysis that leverages a variety of data and novel statistical methodologies. Participants can expect to come away with skills to perform exploratory analysis, perform complex data manipulation, summary and query tasks and, implement a variety of spatial modeling approaches.

DAY 1

Section 1 (9:00am – 12:00pm) – Foundations of R for spatial analysis

Introduction to coding structure, object classes, data manipulation, writing functions, for loops and reading/writing spatial data. We will introduce the basic foundations of R logic and coding syntax, object-oriented structure, object classes and looping. Building on these basics, we will then cover data manipulation of the various spatial class objects introducing indexing, query, merging and subsetting data. We will also introduce creating, reading and writing vector spatial classes.  For those wishing to improve their R skills in preparation for the workshop, some suggested foundation tutorial materials will be made available.  

Learning objectives: 
1.	Understanding vector, data frame, matrix, list object types and other object classes
2.	Understanding base functions for manipulating and query of data.
3.	Understanding bracket “indexing” and data manipulation. 
4.	Implementing “for/while” loops for repetitive task and simulation.
5.	Understanding the use of apply and other recursive functions  
6.	How to write functions
7.	Creating, reading and writing spatial classes (sp, sf, raster, terra). 

Section 2 (1:00pm – 5:00pm) – Spatial data query, manipulation, overlay and distance/neighbor analysis 

One limitation to full implementation of spatial analysis in R is ability to implement some basic GIS functions.  In this section, we will cover various vector overlay procedures, distance and proximity analysis. We will focus on spatial data analysis, starting with data manipulation, query and overlay. We will also cover distance and proximity analysis thus, providing a foundation to topics covered later in the workshop (e.g., assessing spatial autocorrelation, network analysis). To provide a context to common spatial analytic tasks, we will replicate a few tasks available in common GIS software and then extend them into specialized analysis. 

Learning objectives
1.	Understanding vector overlay analysis and translate complex analysis to workflows
2.	Build skills for using available libraries/functions to replicate, customize and batch common GIS tasks.
3.	Ability to conduct distance and proximity analysis on vector data.

DAY 2

Section 3 (9:00am – 12:00pm) – Spatial autocorrelation and testing model assumptions.  

Understanding spatial autocorrelation is fundamental for understanding spatial process, testing model assumptions and conducting spatial analysis.  In this section, we will cover what is spatial autocorrelations, importance and implications of spatial autocorrelation in modeling ecological systems, how to assess spatial autocorrelation (global and local), and how to test model assumptions using spatial autocorrelation tests.  

Learning objectives:
1.	Understand how and why to test for spatial autocorrelation.  
2.	Be able to interpret spatial autocorrelation results for both population and sample data.  
3.	Implement spatial autocorrelation analyses to test parametric model assumptions.  

Section 4 (1:00pm – 3:00pm) – Raster data 

Introduction to raster analysis including moving window, overlay and integration with vector data. 

Learning objectives:
1.	Understanding import/export of raster formats and raster data structures 
2.	Understanding univariate, overlay and descriptive raster analysis
3.	Understanding raster moving window analysis
4.	Applying functions to rasters
5.	Implementing raster and vector integration

Section 5 (3:00pm – 5:00pm) – Quantifying landscape structure 

Will provide a general understanding of landscape pattern, fragmentation and gradients including derivation and implementation of landscape metrics.  

Learning objectives:
1.	Implementing “landscape metrics” using raster and vector data
2.	Gradient metrics and surface characterization
3.	Incorporating pattern and gradient metrics into statistical models   

DAY 3

Section 6 (8:30am – 12:30pm) – Graph theoretical and gravity models 

We will provide an overview to graph theoretical approaches with emphasis on gravity models. There are many ways graphs can be implemented to understand population structure and relate that structure to landscape characteristics (see Dyer and Nason 2004).  In this exercise, we will focus on one specialized case.  Gravity models are a type of inferential model that exploit graph characteristics.  Gravity models include both at site (nodes) and between side (edges) landscape data.  In this section, we will use the gravity model framework to build an empirical model of connectivity for a Columbia spotted frog dataset in central Idaho (Murphy et al. 2010).

Learning objectives:
1.	Understanding graph structures 
2.	Implementing the gravity model form
3.	How to structure and specify a gravity model in R, AIC and model selection
4.	Implement concepts from sections 1-5 in a real analytical framework (spatial objects, manipulating spatial data, spatial weight matrices, extract function, etc.).

Section 7 (1:30pm – 4:30pm) – Predictive models with Random Forests

We will introduce the conceptual and mathematical foundations of recursive partitioning, ensemble and Random Forests methods along with implementation of a binomial predictive model in R and methods for model selection, significance testing and validation of fit and performance. Time allowing, we will also cover overfit, the class imbalance problem and multi-model approaches for sample convergence.  

Learning objectives:
1.	Concepts and caveats of recursive partitioning, ensemble methods and Random Forests
2.	Background on “presence-only” verses “pseudo-absence” modeling approaches
3.	How to structure data for a spatial Random Forests model
4.	Implementation of a Random Forest model, including class imbalance, model selection and multi-model approaches for sample convergence.  
5.	Model validation


If you find bugs in our the teaching materials, or would like to suggest improvements,
please [file an issue][issues]
or [mail us][email].

[email]: mailto:jeffrey_evans@tnc.org
[tnc-profile]: https://www.nature.org/en-us/about-us/who-we-are/our-people/jeff-evans/
[design]: https://carpentries.github.io/workshop-template/design/index.html
[issues]: https://github.com/jeffreyevans/spatialR/issues
