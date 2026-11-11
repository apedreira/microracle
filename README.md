
# Microracle <img src="var/img/logo_black.png" align="right" width="200" />

Microracle allows users to employ the bacterial inactivation models developed in our group with their own experimental data. Alternatively, the user can simply load the available experimental data for _Escherichia coli_ and _Bacillus cereus_ and modify the model parameters to test how they affect the model output. 

Currently, the following models are available

<!-- The list below is generated from models.R: run `Rscript update_readme.R` instead of editing it -->
<!-- MODELS:START -->
* **Carvacrol sub-MIC**: Data & Model to predict Carvacrol inactivation of <em>E. coli</em> and <em>B. cereus</em> at sub-MIC levels.
    * Reference: Pedreira, A., Martínez-López, N., Vázquez, J. A., & García, M. R. (2024). Modelling the antimicrobial effect of food preservatives in bacteria: Application to <i>Escherichia coli</i> and <i>Bacillus cereus</i> inhibition with carvacrol. <i>Journal of Food Engineering</i>, 361, 111734. doi: <a href='https://doi.org/10.1016/j.jfoodeng.2023.111734' target='_blank'>doi.org/10.1016/j.jfoodeng.2023.111734</a>
* **DDAC sub-MIC**: Data & Model to predict DDAC inactivation of <em>E. coli</em> and <em>B. cereus</em> at sub-MIC levels.
    * Reference: Pedreira, A., Vázquez, J. A., & García, M. R. (2022). Kinetics of Bacterial Adaptation, Growth, and Death at Didecyldimethylammonium Chloride sub-MIC Concentrations. <i>Frontiers in Microbiology</i>, 13, 758237. doi: <a href='https://doi.org/10.3389/fmicb.2022.758237' target='_blank'>doi.org/10.3389/fmicb.2022.758237</a>
* **QACs–EOCs synergy**: Data & Model to fit growth, lag and inactivation in time-kill curves of <em>E. coli</em> and <em>B. cereus</em> exposed to Quaternary Ammonium Compounds (QACs), Essential Oil Constituents and their combinations.
    * Reference: Pedreira, A., Fernandes, S., Simões, M., García, M. R., & Vázquez, J. A. (2024). Synergistic bactericidal effects of quaternary ammonium compounds with essential oil constituents. <i>Foods</i>, 13(12), 1831. doi: <a href='https://doi.org/10.3390/foods13121831' target='_blank'>doi.org/10.3390/foods13121831</a>
* **Selection & Extinction**: Analysis of competitive strain dynamics under cyclic treatments based on growth-kill trade-offs.
    * Reference: Martínez-López, N., Nordholt, N., Schreiber, F. & García, M. R. (2026). Conditions for Bacterial Selection and Extinction Driven by Growth-Kill Trade-Off in Cyclic Antimicrobial Treatments. <i>arXiv</i>. doi: <a href='https://doi.org/10.48550/arXiv.2602.14645' target='_blank'>https://doi.org/10.48550/arXiv.2602.14645</a>
<!-- MODELS:END -->



## Authoring

Microracle has been developed by researchers of the Biosystems and Bioprocess Engineering Group (Bio2Eng) and
Recycling and Valorization of Waste Materials (REVAL) groups from the IIM-CSIC (Vigo, Spain):

* Adrián Pedreira: Web developer, experimentation and data collection.
* Nerea Martínez: Development of mathematical models. 
* Xosé A. Vázquez: Experimental design
* Míriam R. García: Development of mathematical models.  

For any question related to the mathematical models, please contact with Míriam R. García (miriamr@iim.csic.es)
Microracle is a project in early development, so you may encounter some bugs on the web. To report any bug or a problem (https://github.com/apedreira/microracle), please generate a new issue in the GitHub repository or contact with Adrián Pedreira (apedreira@iim.csic.es).



## Access

Microracle can be freely accessed from [https://microracle.shinyapps.io/Microracle/](https://microracle.shinyapps.io/Microracle/). No requirements other than internet connection from a web browser are necessary.
