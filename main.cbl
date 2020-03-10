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
    RECORD KEY IS fc_cle
    ALTERNATE RECORD KEY IS fc_jure WITH DUPLICATES.

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
    02 fse_date PIC 9(8).       
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
    02 fsa_capacite PIC 9(3).

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

77 WFin PIC 9(1).
77 WTrouve PIC 9(1).

01 dateAjd PIC 9(8).

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
           WHEN 5 PERFORM RechercherJuresNonConvoques
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


ConsulterJures.
OPEN INPUT FJures
MOVE 0 TO WFin
IF jureCR <> 0
    DISPLAY 'Fichier vide !'
ELSE
    PERFORM WITH TEST AFTER UNTIL WFin = 1
       READ FJures NEXT
       AT END MOVE 1 TO WFin
       NOT AT END
           DISPLAY fj_nom, ' ', fj_prenom, ' ', fj_adresse' ', fj_departement
       END-READ
    END-PERFORM
END-IF
CLOSE FJures.
*> Alvin

AjouterJure.
*> Alvin
DISPLAY 'Saisir le nom et le prénom du juré :'
ACCEPT fj_nom
ACCEPT fj_prenom

OPEN INPUT FJures
READ FJures
KEY IS fj_cle
END-READ 
IF jureCR = 0
    DISPLAY 'Ce juré existe déjà.'
ELSE
    CLOSE FJures
    OPEN I-O FJures
    DISPLAY 'CR : ', jureCR
    *>Vérification de l'existence du fichier
    IF jureCR <> 0
       CLOSE FJures
       OPEN OUTPUT FJures
    END-IF
    DISPLAY 'CR : ', jureCR
    DISPLAY 'Saisir le numéro de département :'
    ACCEPT fj_departement
    DISPLAY 'Saisir l''adresse :'
    ACCEPT fj_adresse
    WRITE jureTampon
       INVALID KEY
           DISPLAY "zut."
       NOT INVALID KEY
           DISPLAY "Ajouté !"
    END-WRITE
END-IF

CLOSE FJures.

ModifierJure.
OPEN INPUT FJures
DISPLAY 'Saisir le nom et le prénom du juré à modifier :'
DISPLAY '  Nom :'
ACCEPT fj_nom
DISPLAY '  Prénom :'
ACCEPT fj_prenom

READ FJures
KEY IS fj_cle
END-READ
IF jureCR <> 0
       DISPLAY 'Ce juré n''existe pas'
ELSE
    CLOSE FJures
    OPEN I-O FJures
    DISPLAY ' '
    DISPLAY 'Informations actuelles :'
    DISPLAY '  Nom : ', fj_nom
    DISPLAY '  Prénom : ', fj_prenom
    DISPLAY '  Adresse : ', fj_adresse
    DISPLAY '  Département : ', fj_departement

    DISPLAY 'Saisir les informations à modifier :'
    DISPLAY '  Adresse :'
    ACCEPT fj_adresse
    DISPLAY '  Département :'
    ACCEPT fj_departement
    REWRITE jureTampon END-REWRITE
    IF jureCR = 0
       DISPLAY 'Informations enregistrées !'
    ELSE
       DISPLAY 'Erreur d''enregistrement (',jureCR,')'
END-IF
CLOSE FJures.
*> Alvin

SupprimerJure.
MOVE 0 TO WFin
OPEN I-O FJures

DISPLAY 'Saisir le nom et le prénom du juré à modifier :'
DISPLAY '  Nom :'
ACCEPT fj_nom
DISPLAY '  Prénom :'
ACCEPT fj_prenom

READ FJures KEY IS fj_cle END-READ
IF jureCR = 0
    MOVE fj_nom TO fc_nom
    MOVE fj_prenom TO fc_prenom

    OPEN I-O FConvocations
    START FConvocations KEY EQUALS fc_jure
    INVALID KEY 
        DISPLAY 'Pas de convocation pour ce juré'
        DISPLAY 'CR invalid key convo : ', convoCR
    NOT INVALID KEY
       OPEN INPUT FSeances
       DISPLAY 'CR seance :', seanceCR
       IF seanceCR <> 0
           DISPLAY 'Aucune séance n''existe.'
       ELSE
           PERFORM WITH TEST AFTER UNTIL WFin = 1
               READ FConvocations NEXT
               AT END MOVE 1 TO WFin
               NOT AT END
                   DISPLAY 'prout'
                   IF fj_nom <> fc_nom OR fj_prenom <> fc_prenom
                       MOVE 1 TO WFin
                   ELSE
                       MOVE fc_numSeance TO fse_numSeance
                       READ FSeances KEY IS fse_numSeance END-READ
                       IF seanceCR <> 0
                           DISPLAY 'La séance n°', fse_numSeance, ' n''existe pas'
                       ELSE
                           DISPLAY 'La séance n°', fse_numSeance, ' existe !'
                           MOVE FUNCTION CURRENT-DATE TO timestampAjd
                           DISPLAY 'lecture de la date...'
                           IF fse_date <= dateAjd
                               DELETE FConvocations RECORD
                               NOT INVALID KEY
                                      DISPLAY 'convo supprimée !'
                               END-DELETE
                            ELSE
                               DISPLAY 'convo gardée'
                           END-IF
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
       END-IF
    END-START
    CLOSE FConvocations
    CLOSE FSeances
ELSE
    DISPLAY 'Ce juré n''existe pas.'
END-IF

DELETE FJures RECORD
NOT INVALID KEY
       DISPLAY 'Juré supprimé !'
END-DELETE
CLOSE FJures.
*> Alvin

RechercherJuresNonConvoques.
*>Lire Jurés. Pour chaque juré
.
*> Alvin

ConsulterConvocations.
OPEN INPUT FConvocations
IF convoCR <> 0
    DISPLAY 'Fichier vide'
    DISPLAY 'CR : ', convoCR
ELSE
    MOVE 0 TO WFin
    PERFORM WITH TEST AFTER UNTIL WFin = 1
        READ FConvocations NEXT
        AT END MOVE 1 TO WFin
        NOT AT END
            DISPLAY 'CR : ', convoCR
            DISPLAY fc_nom
        END-READ
    END-PERFORM
END-IF
CLOSE FConvocations.
*> Oriane

AjouterConvocation.
DISPLAY 'num seance'
ACCEPT fc_numSeance
DISPLAY 'nom jure'
ACCEPT fc_nom
DISPLAY 'prenom jure'
ACCEPT fc_prenom
DISPLAY 'valide'
ACCEPT fc_valide
OPEN I-O FConvocations
IF convoCR <> 0
CLOSE FConvocations
OPEN OUTPUT FConvocations
END-IF
WRITE convoTampon END-WRITE
CLOSE FConvocations.
*> Oriane

ModifierConvocation..
*> Oriane

SupprimerConvocation..
*> Oriane

RechercherConvosNonValides..
*> Oriane

ConsulterSeances..
*> Mathieu

AjouterSeance.
DISPLAY 'num seance'
ACCEPT fse_numSeance
DISPLAY 'nom juge'
ACCEPT fse_juge
MOVE ZERO TO fse_date
MOVE ZERO TO fse_numSalle
MOVE 'A' TO fse_typeTribunal
OPEN I-O FSeances
IF seanceCR <> 0
CLOSE FSeances
OPEN OUTPUT FSeances
END-IF
WRITE seanceTampon END-WRITE
IF seanceCR = 0
    DISPLAY 'séance ajoutée'
END-IF
CLOSE FSeances.

*> Mathieu

ModifierSeance..
*> Mathieu

SupprimerSeance..
*> Mathieu

RechercherSeancesJureVenir..
*> Mathieu

ConsulterAffaires..
*> Mathieu

AjouterAffaire..
*> Mathieu

ModifierAffaire..
*> Mathieu

SupprimerAffaire..
*> Mathieu

ConsulterSalles..
*> Oriane

AjouterSalle..
*> Oriane

ModifierSalle..
*> Oriane

SupprimerSalle..
*> Oriane

RechercherSallesLibres..
*> Oriane
