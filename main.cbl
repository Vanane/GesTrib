IDENTIFICATION DIVISION.
PROGRAM-ID. ProjetGesTrib.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

SELECT FJures ASSIGN TO "jures.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS jureCR
    RECORD KEY IS fj_cle
    ALTERNATE RECORD KEY IS fj_departement WITH DUPLICATES.

SELECT FConvocations ASSIGN TO "convocations.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS convoCR
    RECORD KEY IS fc_cle.

SELECT FSeances ASSIGN TO "seances.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS seanceCR
    RECORD KEY IS fse_numSeance
    ALTERNATE RECORD KEY IS fse_refAffaire WITH DUPLICATES
    ALTERNATE RECORD KEY IS fse_numSalle WITH DUPLICATES.

SELECT FAffaires ASSIGN TO "affaires.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS affaireCR.

SELECT FSalles ASSIGN TO "salles.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS salleCR.

DATA DIVISION.
FILE SECTION.
FD FJures.
01 jureTampon.
    02 fj_cle.
       03 fj_nom PIC A(25).
       03 fj_prenom PIC A(25).
    02 fj_departement PIC 9(3).
    02 fj_adresse PIC A(50).
    
FD FConvocations.
01 convoTampon.
    02 fc_cle.
       03 fc_numSeance PIC 9(2).
       03 fc_jure.
          04 fc_nom PIC A(25).
          04 fc_prenom PIC A(25).
    02 fc_valide PIC 9(1).

FD FSeances.
01 seanceTampon.
    02 fse_numSeance PIC 9(2).
    02 fse_typeTribunal PIC A(25).
    02 fse_juge PIC A(25).
    02 fse_date PIC X(10).
    02 fse_refAffaire PIC A(9).
    02 fse_numSalle PIC 9(2).
    
FD FAffaires.
01 affaireTampon.
    02 fa_refAffaire PIC A(9).
    02 fa_classee PIC 9(1).
    02 fa_contexte PIC A(128).

FD FSalles.
01 salleTampon.
    02 fsa_numSalle PIC 9(2).
    02 fsa_numTribunal PIC 9(3).
    02 fse_capacite PIC 9(3).

WORKING-STORAGE SECTION.
77 jureCR PIC 9(2).
77 convoCR PIC 9(2).
77 seanceCR PIC 9(2).
77 affaireCR PIC 9(2).
77 salleCR PIC 9(2).

77 choixMenu PIC 9(2).
77 choixMenuSec PIC 9(2).
77 nomJure PIC A(25).
77 prenomJure PIC A(25).

PROCEDURE DIVISION.
PERFORM MenuPrincipal.
STOP RUN.

MenuPrincipal.
DISPLAY '----------------'
DISPLAY 'Menu Principal:'
DISPLAY '  1 : Login Juré'
DISPLAY '  2 : Login Administrateur'
DISPLAY '----------------'
DISPLAY ' 0 : Quitter'
ACCEPT choixMenu

EVALUATE choixMenu
    WHEN 1 PERFORM MenuJure
    WHEN 2 PERFORM MenuAdmin
END-EVALUATE.

MenuJure.
DISPLAY '----------------'
DISPLAY 'Menu Juré:'
DISPLAY '  1 : Consulter les prochaines séances'
DISPLAY '----------------'
DISPLAY '0 : Quitter'  
ACCEPT choixMenu
EVALUATE choixMenu
    WHEN 1 PERFORM ConsulterSeances
END-EVALUATE.


MenuAdmin.
PERFORM WITH TEST AFTER UNTIL choixMenu = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Admin:'
       DISPLAY '   1 : Gestion Jurés'
       DISPLAY '   2 : Gestion Convocations'
       DISPLAY '   3 : Gestion Séances'
       DISPLAY '   4 : Gestion Affaires'
       DISPLAY '   5 : Gestion Salles'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenu
       EVALUATE choixMenu
           WHEN 1 PERFORM MenuGestionJures
           WHEN 2 PERFORM MenuGestionConvocations
           WHEN 3 PERFORM MenuGestionSeances
           WHEN 4 PERFORM MenuGestionAffaires
           WHEN 5 PERFORM MenuGestionSalles
       END-EVALUATE
END-PERFORM.


MenuGestionJures.
PERFORM WITH TEST AFTER UNTIL choixMenuSec = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Jurés :'
       DISPLAY '   1 : Consulter'
       DISPLAY '   2 : Ajouter'
       DISPLAY '   3 : Modifier'
       DISPLAY '   4 : Supprimer'
       DISPLAY '   5 : Rechercher les jurés non-convoqués'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM ConsulterJures
           WHEN 2 PERFORM AjouterJure
           WHEN 3 PERFORM ModifierJure
           WHEN 4 PERFORM SupprimerJure
       END-EVALUATE
END-PERFORM.


MenuGestionConvocations.
PERFORM WITH TEST AFTER UNTIL choixMenuSec = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Convocations:'
       DISPLAY '   1 : Consulter'
       DISPLAY '   2 : Ajouter'
       DISPLAY '   3 : Modifier'
       DISPLAY '   4 : Supprimer'
       DISPLAY '   5 : Rechercher les convocations non-validées'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM ConsulterConvocations
           WHEN 2 PERFORM AjouterConvocation
           WHEN 3 PERFORM ModifierConvocation
           WHEN 4 PERFORM SupprimerConvocation
           WHEN 5 PERFORM RechercherConvosNonValides
       END-EVALUATE
END-PERFORM.


MenuGestionSeances.
PERFORM WITH TEST AFTER UNTIL choixMenuSec = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Séances :'
       DISPLAY '   1 : Consulter'
       DISPLAY '   2 : Ajouter'
       DISPLAY '   3 : Modifier'
       DISPLAY '   4 : Supprimer'
       DISPLAY '   5 : Rechercher les séances à venir d''juré'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM ConsulterSeances
           WHEN 2 PERFORM AjouterSeance
           WHEN 3 PERFORM ModifierSeance
           WHEN 4 PERFORM SupprimerSeance
           WHEN 5 PERFORM RechercherSeancesJureVenir
       END-EVALUATE
END-PERFORM.


MenuGestionAffaires.
PERFORM WITH TEST AFTER UNTIL choixMenuSec = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Affaires :'
       DISPLAY '   1 : Consulter'
       DISPLAY '   2 : Ajouter'
       DISPLAY '   3 : Modifier'
       DISPLAY '   4 : Supprimer'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM ConsulterAffaires
           WHEN 2 PERFORM AjouterAffaire
           WHEN 3 PERFORM ModifierAffaire
           WHEN 4 PERFORM SupprimerAffaire
       END-EVALUATE
END-PERFORM.


MenuGestionSalles.
PERFORM WITH TEST AFTER UNTIL choixMenuSec = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Salles :'
       DISPLAY '   1 : Consulter'
       DISPLAY '   2 : Ajouter'
       DISPLAY '   3 : Modifier'
       DISPLAY '   4 : Supprimer'
       DISPLAY '   5 : Rechercher les salles libres'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM ConsulterSalles
           WHEN 2 PERFORM AjouterSalle
           WHEN 3 PERFORM ModifierSalle
           WHEN 4 PERFORM SupprimerSalle
           WHEN 5 PERFORM RechercherSallesLibres
       END-EVALUATE
END-PERFORM.


ConsulterJures..


AjouterJure..


ModifierJure..


SupprimerJure..


ConsulterConvocations..


AjouterConvocation..


ModifierConvocation..


SupprimerConvocation..


RechercherConvosNonValides..


ConsulterSeances..


AjouterSeance..


ModifierSeance..


SupprimerSeance..


RechercherSeancesJureVenir..


ConsulterAffaires..


AjouterAffaire..


ModifierAffaire..


SupprimerAffaire..


ConsulterSalles..


AjouterSalle..


ModifierSalle..


SupprimerSalle..


RechercherSallesLibres..

