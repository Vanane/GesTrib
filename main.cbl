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


SELECT FSallesTemp ASSIGN TO "sallesTemp.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS salleTempCR.


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
    02 fsa_capacite PIC 9(3).

FD FSallesTemp.
01 salleTamponTemp.
    02 fsa_numSalleTemp PIC 9(2).
    02 fsa_numTribunalTemp PIC 9(3).
    02 fsa_capaciteTemp PIC 9(3).
    
WORKING-STORAGE SECTION.
77 jureCR PIC 9(2).
77 convoCR PIC 9(2).
77 seanceCR PIC 9(2).
77 affaireCR PIC 9(2).
77 salleCR PIC 9(2).
77 salleTempCR PIC 9(2).

77 choixMenu PIC 9(2).
77 choixMenuSec PIC 9(2).
77 nomJure PIC A(25).
77 prenomJure PIC A(25).
77 derniereSalle PIC 9(2).
77 WFin PIC 9(1).
77 WTrouve PIC 9(1).

77 numS PIC 9(2).
77 numT PIC 9(3).
77 capa PIC 9(3).
77 rep PIC 9(1).
77 valid PIC 9(1).

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


ConsulterConvocations.

OPEN INPUT FConvocations
IF convoCR <> 0
DISPLAY 'Fichier vide'
ELSE
MOVE 0 TO Wfin
    PERFORM WITH TEST AFTER UNTIL WFin = 1
       READ FConvocations 
       AT END MOVE 1 to WFin
       NOT AT END
           DISPLAY 'Numéro de seance : 'fc_numSeance
           DISPLAY 'Nom du juré : 'fc_nom
           DISPLAY 'Prénom du juré : 'fc_prenom
           DISPLAY 'Validité de la convocation : 'fc_valide
           DISPLAY ''
       END-READ  
    END-PERFORM
END-IF
CLOSE FConvocations.

AjouterConvocation.

OPEN I-O FConvocations
IF convoCR <> 0
    CLOSE FConvocations
    OPEN OUTPUT FConvocations
ELSE

MOVE 0 to Wtrouve
Display 'Numéro de la séance'
Accept numS

Display ' Verification que la séance existe'
OPEN INPUT FSeances
READ FSeances ON numS
IF seanceCR <> 0
    CLOSE FSeances
    DISPLAY 'Séance inexistante'
    CLOSE FConvocations
ELSE

    Display 'Nom du juré'
    accept nomJure
    Display 'Prénom du juré'
    accept prenomJure

    OPEN INPUT FJures
    READ FJures ON nomJure AND prenomJure
    IF jureCR <> 0
        CLOSE FJures
        DISPLAY 'Erreur invalide !Juré non renseigné dans la liste des jurés'
        CLOSE FConvocations
    ELSE

        Display 'Verification de l'existance du juré'

        Display 'Validité initialisé'
        Move 0 to valid

        move numS TO fc_numSeance
        Move nomJure TO fc_nom
        MOvE prenomJure TO fc_prenom
        MOVE valid TO fc_valide

        Write convoTampon END-Write
            DISPLAY 'Convocation créée'
            CLOSE FConvocations
    END-IF
END-IF
.


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


ConsulterSalles.
MOVE 0 TO WFin

OPEN INPUT FSalles
IF salleCR <> 0
       DISPLAY 'Fichier vide'
ELSE 
    PERFORM WITH TEST AFTER UNTIL WFin = 1 OR WTrouve = 1
       READ FSalles 
       AT END MOVE 1 to WFin
       NOT AT END
           DISPLAY 'Numéro de salle : 'fsa_numSalle
           DISPLAY 'Numéro de tribunal : 'fsa_numTribunal
           DISPLAY 'Capacité de la salle : 'fsa_capacite
           DISPLAY ' '
       END-READ  
    END-PERFORM
END-IF
CLOSE FSalles.

AjouterSalle.
MOVE 0 TO WFin
MOVE 0 TO WTrouve
DISPLAY 'Numero salle'
ACCEPT numS
DISPLAY 'Saisir le numéro du tribunal'
ACCEPT numT
DISPLAY 'Saisir la capacité de la nouvelle salle'
ACCEPT capa

OPEN INPUT FSalles
IF salleCR <> 0
       CLOSE FSalles
       OPEN OUTPUT FSalles
ELSE 
    PERFORM WITH TEST AFTER UNTIL WFin = 1 OR WTrouve = 1
       READ FSalles 
       AT END MOVE 1 to WFin
       NOT AT END
           IF fsa_numSalle = numS AND fsa_numTribunal = numT 
               MOVE 1 to WTrouve
           END-IF      
       END-READ  
    END-PERFORM
END-IF
CLOSE FSalles

IF WTrouve <> 1
    OPEN Extend FSalles
    MOVE capa TO fsa_capacite
    MOVE numS TO fsa_numSalle
    MOVE numT TO fsa_numTribunal
    Write salleTampon END-Write
    DISPLAY 'Salle créée'
    CLOSE FSalles
ELSE 
    DISPLAY 'Salle déjà existante'
END-IF.



ModifierSalle.

MOVE 0 TO WFin
MOVE 0 TO WTrouve
DISPLAY ' Saisir le numéro de la salle à modifier'
ACCEPT numS
DISPLAY 'Saisir le numéro du tribunal de la salle correspondante'
ACCEPT numT

OPEN INPUT FSalles
IF salleCR <> 0
       DISPLAY 'Fichier vide'       
ELSE 
    PERFORM WITH TEST AFTER UNTIL WFin = 1 OR WTrouve = 1
       READ FSalles 
       AT END MOVE 1 to WFin
       NOT AT END
           IF fsa_numSalle = numS AND fsa_numTribunal = numT 
               MOVE 1 to WTrouve
           END-IF      
       END-READ  
    END-PERFORM
END-IF
CLOSE FSalles

IF WTrouve = 1
    OPEN Extend FSalles
       DISPLAY 'Informations actuelles de la salle'
       DISPLAY 'capacité : ' fsa_capacite
       DISPLAY '****'

       DISPLAY 'Saisir la capacité de la nouvelle salle'
       ACCEPT fsa_capacite
       

    REWrite salleTampon END-REWrite
    IF salleCR = 0
       DISPLAY 'Salle 'fsa_numSalle' du tribunal 'fsa_numTribunal' modifiée'
    ELSE
       DISPLAY 'Erreur lors de la modification'
    END-IF
    CLOSE FSalles
ELSE 
    DISPLAY 'Salle non trouvée'
END-IF.

SupprimerSalle.

MOVE 0 TO WFin
MOVE 0 TO WTrouve
DISPLAY ' Saisir le numéro de la salle à supprimer'
ACCEPT numS
DISPLAY 'Saisir le numéro du tribunal de la salle correspondante'
ACCEPT numT

OPEN INPUT FSalles
IF salleCR <> 0
       DISPLAY 'Fichier vide'       
ELSE 
    PERFORM WITH TEST AFTER UNTIL WFin = 1 OR WTrouve = 1
       READ FSalles 
       AT END MOVE 1 to WFin
       NOT AT END
           IF fsa_numSalle = numS AND fsa_numTribunal = numT 
               MOVE 1 to WTrouve
           END-IF      
       END-READ  
    END-PERFORM
END-IF
CLOSE FSalles

IF WTrouve = 1
    OPEN Extend FSalles
       DISPLAY ' ** Informations actuelles de la salle **'
       DISPLAY 'NumSalle : 'fsa_numSalle
       DISPLAY 'NumTribunal : 'fsa_numTribunal
       DISPLAY 'capacité : ' fsa_capacite
       DISPLAY '****'

       DISPLAY 'Souhaitez vous vraiment supprimer cette salle ? 1/0'
       ACCEPT rep
       
       IF rep = 1

            DISPLAY '** Debug 01 ** '
            OPEN OUTPUT FSallesTemp
            MOVE 0 to WFin
               PERFORM WITH TEST AFTER UNTIL WFin = 1
                    display "** Debug 02**"
                    READ FSalles
                    AT END MOVE 1 TO WFin
                    NOT AT END
                    If fsa_numSalle <> numS OR fsa_numTribunal <> numT
                        Write salleTampon END-Write
                    END-IF
                    END-READ
               END-PERFORM

               MOVE 0 to WFin 
               PERFORM WITH TEST AFTER UNTIL WFin = 1
                  READ FSallesTemp
                  AT END MOVE 1 TO WFin
                  NOT AT END 
                  Write salleTamponTemp END-Write
                  END-READ
               END-PERFORM


            DISPLAY 'Salle 'fsa_numSalle' du tribunal 'fsa_numTribunal' modifiée'
            CLOSE FSallesTemp

       ELSE           
       DISPLAY 'Suppression annulée'
       END-IF
   
    CLOSE FSalles
ELSE 
    DISPLAY 'Salle non trouvée'
END-IF.



RechercherSallesLibres..

