Glacier Blanc data
OK - replace "_" per "-" in Plot (eg. B4_10 -> B4-10)- R code added to modify it

Pelerins data
ok - remove "-" in the Plot name (gps_id) -> IN DOC EXPORT
 done for P-23, P-25, P-26, P-30, P-31, P-36, P-44, P-49, P-61, P-62, P-63, P-68, P-72, P-84, P-87, P-98, P-10, P-07, P-04.
-> j'ai corrigé. C'est bizarre, il y a des données de M. Bidat de 2019, alors qu'il a fait des relevés avec toi en 2020 si je ne me trompe pas...
-> ça va rendre mes collègues botanistes dingues tous ces changements, car à chaque changement (nom du relevé ou autre), ils doivent revalider les espèces... je vais voir comment contourner ce problème

- Gps code (Plot) changed -> R code added to modify it
	T13a -> T13A -> OK j'ai fait le changement
	T08a -> T08A -> OK j'ai fait le changement
	S3-5 -> S3-05 -> était déjà écrit comme ça
	SLIA-9 -> SLIA-09 -> OK j'ai fait le changement
	T-65 -> T65 -> OK j'ai fait le changement
	SLIA-099->SLIA-99 -> était déjà écrit comme ça
	
	en revanche il reste un plot S3-07a, or il n'existe pas d'autre S3-07 -> on le change en S3-07 ou en S3-07A?
	Attention, il y a 2 T60!

OK - Glacier de Grébroulaz -> Glacier de Gébroulaz (Apparement ca a ete changé)


QUESTIONS POUR SOPHIE

*- Je crois que P100 correspond a P New 1985-2003, mais les deux sont absent de la base du CBNA, pourquoi? je les ai dans mes donnees de base -  a ajouter??
*- De meme pour T13B. Serait ce parce que nous n'avons pas les points GPS??
-> à clarifier

*- Apparement P62 a ete rentré deux fois dans la BD (Il a une richness de 36 et les memes especes qui se repetent!)
-> Pb réglé, il y avait P62 et P-62, j'ai supprimé P-62

*- error avec S1-12 dans export CBNA: 9 especes mais 0 en cover. Nos donnes brutes floristic valident aussi un relevé vide, 
et j'ai verifié avec ton export precedent du 22 nov 2021 et S1-12 etait aussi vide. Je comprends pas ce qui a pu se passer entre temps
-> c'est corrigé, j'ai supprimé les espèces

* certains plot on un richesse differente (de 1 en general) (je ne pense pas que cela vienne des changements fait par tes collegues)
	- P98: Sur nos fiches d'origine et sur le premier export CBNA il y a Oxyria digyna, Betula pubescens, Saxifraga bryoides Cerastium pedunculatum
	sur cet export l'Oxyria est absente
	-> c'est corrigé
	
	- T56: (pareil que P98) Dans les donnees de base on a aussi Leucanthemopsis alpina, qui ici est absent de l'export
	-> c'est corrigé
	
	- T34: Agrostis rupestris a disparu dans cet export, il etait dans les donnees de base
	-> c'est corrigé
	
	- T60: a 1% de cover et seulement une Poa laxa dans nos donnees de base (et jai verifie sur la photo). Dans l'export il a 7 especes.
	-> c'est corrigé (il y avait 2 T60...)
	
	- Pour Orny, O63 certaines especes ont changé (Minuartia verna) normal? + OLIA-02; OLIA-07

Notes:
Les plots suivant on -1 en richesse comparer aux donnees precedente car les NA sont retirés:
GB-03 ; O47
Les plots suivant on +1 : Aucune idee pourquoi car c'est coherent avec les donnees des fiches
GLIAX-07
PC-05 avait un doublon avant->OK





SLIA-x
S1-x -> 1% de plant cover absent dans export cbna -> verifier sur fiches (doit avoir un autre nom)
