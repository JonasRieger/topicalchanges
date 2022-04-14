# Dynamic change detection in topics based on rolling LDAs

This repository provides some results, data and scripts related to the paper:

* Rieger, J., Lange, K.-R., Flossdorf, J. & Jentsch, C. (2022). Dynamic change detection in topics based on rolling LDAs. Proceedings of the [Text2Story'22 Workshop](https://text2story22.inesctec.pt/). CEUR-WS 3117, pp. 5-13. [pdf](http://ceur-ws.org/Vol-3117/paper1.pdf).

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/topicalchanges/issues).

## Usage
The scripts``1_fit_rolling.R``, ``2_compute_sim_quantile.R``, ``3_changes_pdf.R`` und ``4_loo_wordimpact_pdf.R`` represent the workflow of the study design with application to the CNN dataset. The RollingLDA models that form the basis for the analysis are stored in the ``roll`` folder, and the similarities and thresholds depending on the number of topics, the maximum run length, and the mixture parameter are stored in the ``sim`` folder. The final results of the change detection can be found in the ``results`` folder. In addition, ``changes.pdf`` and ``wordimpact.pdf`` provide insight into the results for the parameter combination ``K=12, z_max=4, p=0.85`` and ``changes.csv`` tabulates the corresponding detected changes.

The scripts ``guardian.R``, ``publico.R`` and ``observador.R`` perform the above detailed analysis using a smaller set of parameters on the corresponding other subsets of the [TLS-Covid19](https://doi.org/10.1007/978-3-030-72113-8_33) dataset with the results in the folders named according to the newspaper.
