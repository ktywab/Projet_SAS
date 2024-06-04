/*Creation de blibliothèque*/
libname UESAS "/home/u63759198/sasuser.v94";

/*------------------------------------------------------------------------------------------------------------------*/
*Exo1;
*G-test;
*Independance de deux variables qualitatives;

Data UESAS.sexe_couleur;
	input sexe $ cheveux $ effectif;/*$ à la fin des variables qualitatifs*/
	datalines; /*Indique que les lignes qui suivent sont des données*/
	GARCON BLOND 592
	GARCON ROUX 119
	GARCON CHATAIN 849
	GARCON BRUN 504
	GARCON NOIR_DE_JAIS 36
	FILLE BLOND 544
	FILLE ROUX 97
	FILLE CHATAIN 677
	FILLE BRUN 451
	FILLE NOIR_DE_JAIS 14
	;
Run;

/* Création du graphique en barres empilées */
proc sgplot data=UESAS.sexe_couleur;
    vbar cheveux / response=effectif group=sexe groupdisplay=stack stat=sum;
    xaxis label='Couleurs de cheveux';
    yaxis label='Effectif';
    keylegend / title='Sexe';
    title 'Répartition par sexe et couleurs de cheveux';
run;

*Gtest;
Proc freq data=UESAS.sexe_couleur order=data;
	tables sexe*cheveux / chisq expected nocol nocum nofreq nopercent norow;
	weight effectif;
	title1 "Tableau de contingence des effectifs théoriques";
	title2 "-------------------------------------------------------";
Run;

/*------------------------------------------------------------------------------------------------------------------*/
/*Exo2*/
PROC IMPORT OUT = UESAS.molecule dbms=csv datafile = "/home/u63759198/sasuser.v94/UE_SAS/molecule.csv" replace; 
	getnames=yes;
RUN;

*Moyenne et Ecart type dans chacun des groupes;
/* Calculer les moyennes et les écarts-types pour chaque groupe */
PROC MEANS DATA=UESAS.molecule NMISS MEAN STD;
    CLASS traitement;
    VAR mesure;
RUN;
*test de student;
*Test de normalité dans chaque groupe avec les QQplot;
Proc univariate data=UESAS.molecule normal;
	var mesure;
	class traitement;
	QQPLOT mesure / normal (mu=est sigma=est);
Run;
*Test de student et de Fisher-Snedecor;
Proc ttest data=UESAS.molecule;
	var mesure;
	class traitement;
Run;

/*------------------------------------------------------------------------------------------------------------------*/
*Exo3;
*dbms : extension du fichier;
*datafile : chemin sur notre serveur du fichier;
PROC IMPORT OUT = UESAS.ozone dbms=xls datafile = "/home/u63759198/sasuser.v94/UE_SAS/données/ozone.xls"; 
	getnames=yes;
RUN;

PROC CONTENTS data= UESAS.ozone;
RUN;

/* Statistiques descriptives */
PROC MEANS DATA=UESAS.ozone N NMISS MEAN STD MIN Q1 MEDIAN Q3 MAX;
    VAR _NUMERIC_; /* Remplacez par les variables que vous souhaitez analyser */
RUN;



/* Visualisation des données */
PROC SGSCATTER DATA=UESAS.ozone;
    MATRIX _NUMERIC_; /* Remplacez par les variables que vous souhaitez visualiser */
RUN;
/*Comme on peut le voir ci-dessus, les nuages de points pour chaque paire de variables entre la variable expliquée maxO3 et 
les variables explicatives sauf Ne9, Ne12 et Ne15 montre une relation plus ou moins linéaire.*/

*Modèle;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T12 T15 Ne9 Ne12 Ne15 Vx9 Vx12 Vx15 / dist=poisson link=log;
Run;


*Elimination de l'information redondante dans notre modèle;
/*Avant de faire la selection il faut eliminer de l'information redondante dans mon modèle
Il ne faut pas que mes variables explicative soit très correllé dans l'absolue elles doivent être indépendantes 
Pour ce faire on fait appel au VIF avce un seuil de VIF = 10 car je n'aurai pas de probleme de surparamétrage de modèle avec 112 observation pour au plus 10 paramètre.
*/
proc reg data=UESAS.ozone plots = fitplot(nolimits);
 model maxO3= T9 T12 T15 Ne9 Ne12 Ne15 Vx9 Vx12 Vx15 / VIF;
Run;
/*On remarque que le VIF des variables explicatives T15 et T12 sont superieur à 10, on élimine donc celui qui a le plus VIF T12 */
proc reg data=UESAS.ozone plots = fitplot(nolimits);
 model maxO3= T9 T15 Ne9 Ne12 Ne15 Vx9 Vx12 Vx15 / VIF;
Run;
*On a donc notre modèle optimal au sens du VIF c'est à dire que les variables explicatives ne sont pas trop correlé ;


*Maintenant on vas faire la selection de model avec le modèle corrigé de la surdispersion;
*Ne15 éliminé;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T15 Ne9 Ne12 Ne15 Vx9 Vx12 Vx15 / dist=poisson link=log type3 dscale;
Run;
*Vx15 éliminé;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T15 Ne9 Ne12 Vx9 Vx12 Vx15 / dist=poisson link=log type3 dscale;
Run;
*Vx12 éliminé;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T15 Ne9 Ne12 Vx9 Vx12 / dist=poisson link=log type3 dscale;
Run;

*Ne9 éliminé;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T15 Ne9 Ne12 Vx9 / dist=poisson link=log type3 dscale;
Run;

*Plus aucune varaible éliminé;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T15 Ne12 Vx9 / dist=poisson link=log type3 dscale;
Run;



*Notre modèle optimal après la selection est le suivant ;
Proc genmod data=UESAS.ozone;
	model maxO3= T9 T15 Ne12 Vx9 / dist=poisson link=log dscale;
Run;
*En effet notre modèle n'est pas surparamétré car 8 paramètre à estimer pour un effectif de 112 est assez raisonnable;
/*------------------------------------------------------------------------------------------------------------------*/


