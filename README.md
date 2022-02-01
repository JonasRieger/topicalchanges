# Dynamic change detection in topics based on rolling LDAs

This repository provides some results, data and scripts related to the paper:

* Rieger, J., Lange, K.-R., Flossdorf, J. & Jentsch, C. (submitted). Dynamic change detection in topics based on rolling LDAs.

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/topicalchanges/issues).

## Usage
Die Skripte ``1_fit_rolling.R``, ``2_compute_sim_quantile.R``, ``3_changes_pdf.R`` und ``4_loo_wordimpact_pdf.R`` bilden den Workflow des Studiendesign mit Anwendung auf dem CNN Datensatz ab. Die RollingLDA Modelle, die die Basis für die Analyse bilden, sind im ordner ``roll`` gespeichert, die Ähnlichkeiten und Thresholds in Abhängigkeit von der Anzahl an topics, der maximalen run length und des Mixture Parameters im Ordner ``sim``. Die schlussendlichen Ergebnisse der Change Detection können dem Ordner ``results`` entnommen werden. Außerdem bieten ``changes.pdf`` und ``wordimpact.pdf`` einen Einblick in die Ergebnisse für die Parameterkombination ``K=12, z_max=4, p=0.85`` und ``changes.csv`` gibt die entsprechend detektierten Changes tabellarisch an.

Die Skripte ``guardian.R``, ``publico.R`` und ``observador.R`` führen die obige ausführliche Analyse anhand eines kleineren Parametersets auf den entsprechenden anderen Teildatensätzen des tls-Covid19 Datensatzes durch mit den Ergbnissen in den entsprechend der Zeitung benannten Ordner.

## References
tbd
