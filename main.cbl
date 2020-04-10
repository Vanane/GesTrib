IDENTIFICATION DIVISION.
PROGRAM-ID. ProjetGesTrib.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

SELECT FAffaires ASSIGN TO "affaires.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS affaireCR.

SELECT FAffairesTemp ASSIGN TO "affairesTemp.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS affairesTempCR.

SELECT FConvocations ASSIGN TO "convocations.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS convoCR
    RECORD KEY IS fc_cle
    ALTERNATE RECORD KEY IS fc_jure WITH DUPLICATES
    ALTERNATE RECORD KEY IS fc_numSeance WITH DUPLICATES.

SELECT FJures ASSIGN TO "jures.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS jureCR
    RECORD KEY IS fj_cle
    ALTERNATE RECORD KEY IS fj_departement WITH DUPLICATES.

SELECT FSalles ASSIGN TO "salles.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS salleCR.

SELECT FSallesTemp ASSIGN TO "sallesTemp.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS salleTempCR.

SELECT FSeances ASSIGN TO "seances.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS seanceCR
    RECORD KEY IS fse_numSeance
    ALTERNATE RECORD KEY IS fse_refAffaire WITH DUPLICATES
    ALTERNATE RECORD KEY IS fse_salle WITH DUPLICATES.

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
       03 fc_numSeance PIC 9(9).
       03 fc_jure.
          04 fc_nom PIC A(25).
          04 fc_prenom PIC A(25).
    02 fc_valide PIC 9(1).

FD FSeances.
01 seanceTampon.
    02 fse_numSeance PIC 9(9).
    02 fse_typeTribunal PIC A(25).
    02 fse_juge PIC A(25).
    02 fse_date PIC 9(8).
    02 fse_refAffaire PIC A(9).
    02 fse_salle.
        03 fse_numSalle PIC 9(2).
        03 fse_numTribunal PIC 9(3).

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

FD FAffairesTemp.
01 affaireTamponTemp.
    02 fa_refAffaireTemp PIC A(9).
    02 fa_classeeTemp PIC 9(1).
    02 fa_contexteTemp PIC A(128).
    
WORKING-STORAGE SECTION.
77 jureCR PIC 9(2).
77 convoCR PIC 9(2).
77 seanceCR PIC 9(2).
77 affaireCR PIC 9(2).
77 salleCR PIC 9(2).
77 salleTempCR PIC 9(2).
77 affairesTempCR PIC 9(2).

77 choixMenu PIC 9(2).
77 choixMenuSec PIC 9(2).
77 nomJure PIC A(25).
77 prenomJure PIC A(25).
77 derniereSalle PIC 9(2).
77 dateAjd PIC 9(8).

77 numS PIC 9(9).
77 numT PIC 9(3).
77 capa PIC 9(3).
77 valide PIC 9(1).
77 nbCount PIC 9(4).

77 WFin PIC 9(1).
77 WChoix PIC 9(1).
77 WAuto PIC 9(1).
77 WFin2 PIC 9(1).
77 WTtrib PIC A(25).
77 WNJuge PIC A(25).
77 WNSalle PIC 9(2).
77 WNTrib PIC 9(3).
77 WFin1 PIC 9(1).
77 WRep pic 9(1).
77 WTrouve PIC 9(1).
77 Wtrouve1 PIC 9(1).
77 WRef PIC A(9).
77 WOut PIC 9(1).
77 WCr PIC 9(2).
77 WNse PIC 9(9).
77 WNseAncien PIC 9(9).
77 WClasse PIC 9(1).
77 WDate PIC 9(8).
77 WDate2 PIC 9(8).
77 WAge PIC 9(3).

PROCEDURE DIVISION.
    PERFORM MenuPrincipal
STOP RUN.

MenuPrincipal.
    DISPLAY '----------------'
    DISPLAY 'Menu Principal:'
    DISPLAY '  1 : Login Juré'
    DISPLAY '  2 : Login Administrateur'
    DISPLAY '----------------'
    DISPLAY ' 0 : Quitter'
ACCEPT choixMenu.

EVALUATE choixMenu
    WHEN 1 PERFORM MenuJure
    WHEN 2 PERFORM MenuAdmin
END-EVALUATE.

MenuJure.
    DISPLAY '----------------'
    DISPLAY 'Menu Juré:'
    DISPLAY '  1 : Consulter vos prochaines séances'
    DISPLAY '----------------'
    DISPLAY '0 : Quitter'  
    ACCEPT choixMenu
    EVALUATE choixMenu
        WHEN 1 PERFORM ConsulterProchainesSeances
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
       DISPLAY '   6 : Outils'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenu
       EVALUATE choixMenu
           WHEN 1 PERFORM MenuGestionJures
           WHEN 2 PERFORM MenuGestionConvocations
           WHEN 3 PERFORM MenuGestionSeances
           WHEN 4 PERFORM MenuGestionAffaires
           WHEN 5 PERFORM MenuGestionSalles
           WHEN 6 PERFORM MenuUtilitaire
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
       DISPLAY '           * Ajouter une séance avec un préavis de 7 jours minimum.'
       DISPLAY '   3 : Modifier'
       DISPLAY '   4 : Supprimer'
       DISPLAY '   5 : Rechercher les séances à venir d''un juré'
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
       DISPLAY '   5 : Classer une Affaire'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM ConsulterAffaires
           WHEN 2 PERFORM AjouterAffaire
           WHEN 3 PERFORM ModifierAffaire
           WHEN 4 PERFORM SupprimerAffaire
           WHEN 5 PERFORM ClasserAffaire
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

MenuUtilitaire.
PERFORM WITH TEST AFTER UNTIL choixMenuSec = 0
       DISPLAY '----------------'
       DISPLAY 'Menu Utilitaire :'
       DISPLAY '   1 : Afficher les séances non-réglementaires'
       DISPLAY '           * Séances avec un nombre de jurés incorrect'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenuSec
       EVALUATE choixMenuSec
           WHEN 1 PERFORM AfficherSeancesIncorrectes
       END-EVALUATE
END-PERFORM.

ConsulterProchainesSeances.
    OPEN INPUT FSeances
    IF seanceCR = 0
       OPEN INPUT FConvocations
       MOVE 0 TO WFin
       IF convoCR = 0
           DISPLAY 'Saisissez votre nom :'
           ACCEPT nomJure
           DISPLAY 'Saisissez votre prénom :'
           ACCEPT prenomJure
           MOVE 0 TO WDate
           ACCEPT dateAjd FROM DATE YYYYMMDD
           MOVE nomJure TO fc_nom
           MOVE prenomJure TO fc_prenom
           START FConvocations KEY EQUALS fc_jure
           INVALID KEY
              DISPLAY 'Ce juré n''existe pas'
           NOT INVALID KEY
              PERFORM WITH TEST AFTER UNTIL WFin = 1
                  READ FConvocations NEXT
                  NOT AT END
                       IF fc_nom <> nomJure OR fc_prenom <> prenomJure
                           MOVE 1 TO WFin
                       ELSE
                           MOVE fc_numSeance TO fse_numSeance
                           READ FSeances KEY IS fse_numSeance
                           NOT INVALID KEY
                               IF WDate < fse_date
                                   MOVE fse_date TO WDate
                                   MOVE fse_numSeance TO numS
                               END-IF
                           END-READ
                       END-IF 
                  END-READ
              END-PERFORM
           END-START
           IF WDate >= dateAjd
               MOVE numS TO fse_numSeance
               READ FSeances KEY IS fse_numSeance
               NOT INVALID KEY
                   DISPLAY "Votre prochaine séance est la n°", fse_numSeance
                   DISPLAY "Elle aura lieu en salle ", fse_numSalle, " tribunal ", fse_numTribunal
                   DISPLAY "Pour l'affaire n°", fse_refAffaire
                   DISPLAY "Le ", fse_date, "."
               END-READ
           ELSE
               DISPLAY 'Vous n''avez pas de prochaine séance.'
           END-IF
       ELSE
           DISPLAY 'Aucune convocation ''est disponible.'
       END-IF
       CLOSE FConvocations
   ELSE 
       DISPLAY 'Aucune séance n''est programmée.' 
   END-IF
   CLOSE FSeances.

ConsulterSeancesAffaire.
OPEN OUTPUT FSeances
MOVE 0 To Wfin
IF seanceCR <> 0
       DISPLAY 'Fichier vide'
ELSE
Display 'Saisir la référence de l''affaire :'
ACCEPT WRef
Move WRef to fse_refAffaire
START FSeances KEY EQUALS fse_refAffaire
        INVALID KEY DISPLAY 'Cette affaire n''existe pas.'
        NOT INVALID KEY
           PERFORM with test after until Wfin = 1
           READ FSeances NEXT
           NOT AT END 
                IF fse_refAffaire <> wRef
                MOVE 1 to Wfin
                ELSE
                    DISPLAY "N°séance : ", fse_numSeance
                    Display "N°tribunal : ", fse_numTribunal
                    DISPLAY "N°salle : ", fse_numSalle
                    DISPLAY "Juge en charge de la séance : ", fse_juge
                    DISPLAY ' '
                end-if
           END-READ
           END-PERFORM
END-IF
CLOSE FSeances.
       
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
    CLOSE FJures
.

AjouterJure.
    DISPLAY 'Saisir le nom, puis le prénom du juré :'
    ACCEPT fj_nom
    ACCEPT fj_prenom
    DISPLAY 'Saisissez l''âge du juré :'
    ACCEPT WAge
    IF WAge < 21
        DISPLAY 'Cette personne n''a pas l''âge requis pour être juré ! (21 ans minimum)'
    ELSE
       OPEN INPUT FJures
       READ FJures
       KEY IS fj_cle
       END-READ 
       IF jureCR = 0
           DISPLAY 'Ce juré existe déjà.'
       ELSE
           CLOSE FJures
           OPEN I-O FJures
           *>Vérification de l'existence du fichier
           IF jureCR <> 0
           CLOSE FJures
           OPEN OUTPUT FJures
           END-IF
           DISPLAY 'Saisir le numéro de département :'
           ACCEPT fj_departement
           DISPLAY 'Saisir l''adresse :'
           ACCEPT fj_adresse
           WRITE jureTampon END-WRITE
       END-IF
       CLOSE FJures
    END-IF
.

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

    DISPLAY 'Saisir le nom et le prénom du juré à supprimer :'
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
        NOT INVALID KEY
        OPEN INPUT FSeances
        IF seanceCR <> 0
            DISPLAY 'Aucune séance n''existe.'
        ELSE
            PERFORM WITH TEST AFTER UNTIL WFin = 1
                READ FConvocations NEXT
                *>AT END MOVE 1 TO WFin
                NOT AT END
                    IF fj_nom <> fc_nom OR fj_prenom <> fc_prenom
                        MOVE 1 TO WFin
                    ELSE
                        MOVE fc_numSeance TO fse_numSeance
                        READ FSeances KEY IS fse_numSeance END-READ
                        IF seanceCR <> 0
                            DISPLAY 'La séance n°', fse_numSeance, ' n''existe pas'
                        ELSE
                            DISPLAY 'La séance n°', fse_numSeance, ' existe !'
                            ACCEPT dateAjd FROM DATE YYYYMMDD
                            DISPLAY 'lecture de la date...'
                            IF fse_date <= dateAjd
                               
                                   DISPLAY '**Informations juré**'
                                   DISPLAY ' Prénom : 'fj_prenom
                                   DISPLAY ' Nom : 'fj_nom
                                   DISPLAY ' Adresse : 'fj_adresse
                                   DISPLAY ' Departement :'fj_departement
                                   DISPLAY ' '
                                   Display 'Etes vous sûr de vouloir supprimer ce juré ? 1/0'
                                   Accept WRep
                                   IF Wrep = 1
                                    DELETE FConvocations RECORD
                                    NOT INVALID KEY
                                            DISPLAY 'Convocation supprimée !'
                                    END-DELETE
                                   END-IF
                                ELSE
                                DISPLAY 'Suppression annulée.'
                            END-IF
                        END-IF
                    END-IF
                END-READ
            END-PERFORM
        END-IF
        END-START
        CLOSE FConvocations
        CLOSE FSeances

        DELETE FJures RECORD
        NOT INVALID KEY
        DISPLAY 'Juré supprimé !'
        END-DELETE
    ELSE *> Si jureCR n'est pas 0 après lecture sur nom prenom
        DISPLAY 'Ce juré n''existe pas.'
    END-IF
CLOSE FJures.
*> Alvin

RechercherJuresNonConvoques.
    *>Lire Jurés. Pour chaque juré,
        *> LireZone ses convocations.
        *> Si pas de convo, alors afficher et passer au juré suivant.
    OPEN INPUT FJures
    IF jureCR <> 0
        DISPLAY 'Aucun juré enregistré.'
    ELSE
        MOVE 0 TO WFin
        PERFORM WITH TEST AFTER UNTIL WFin = 1
        READ FJures NEXT
        AT END MOVE 1 TO WFin
        NOT AT END
            OPEN INPUT FConvocations
            IF convoCR <> 0
                DISPLAY fj_nom, fj_prenom, ' n''a pas reçu de convocation.'
            ELSE
                MOVE fj_nom TO fc_nom
                MOVE fj_prenom TO fc_prenom
                START FConvocations KEY EQUALS fc_jure
                INVALID KEY
                    DISPLAY fj_nom, fj_prenom, ' n''a pas reçu de convocation.'
                END-START
            CLOSE FConvocations
            END-IF
        END-READ
        END-PERFORM
    END-IF
CLOSE FJures.
*> Alvin
      
ConsulterConvocations.
    OPEN INPUT FConvocations
    IF convoCR <> 00
        DISPLAY 'Fichier vide'
    ELSE
        MOVE 0 TO WFin
        PERFORM WITH TEST AFTER UNTIL WFin = 1
            READ FConvocations NEXT
            AT END 
                MOVE 1 to WFin
            NOT AT END
                DISPLAY 'Numéro de seance : ', fc_numSeance
                DISPLAY 'Nom du juré : ', fc_nom
                DISPLAY 'Prénom du juré : ', fc_prenom
                DISPLAY 'Validité de la convocation : ', fc_valide
                DISPLAY ' '  
        END-PERFORM
    END-IF
    CLOSE FConvocations
.

AjouterConvocation.
    OPEN I-O FConvocations
    IF convoCR <> 0
        OPEN INPUT FConvocations
        CLOSE FConvocations
        OPEN OUTPUT FConvocations
    END-IF
    IF WAuto = 0 THEN
        DISPLAY 'Numéro de la séance'
        ACCEPT fse_numSeance
    ELSE
        MOVE WNse TO fse_numSeance
    END-IF
    OPEN INPUT FSeances      
    READ FSeances KEY fse_numSeance
    INVALID KEY
        DISPLAY 'La séance n''existe pas.'
    NOT INVALID KEY                      
        DISPLAY 'Nom du juré'
        ACCEPT fj_nom
        DISPLAY 'Prénom du juré'
        ACCEPT fj_prenom 
        OPEN INPUT FJures
        DISPLAY fj_nom, ' ', fj_prenom
        READ FJures KEY fj_cle
        INVALID KEY
            DISPLAY 'Ce juré n''existe pas !'
        NOT INVALID KEY       
            MOVE 0 TO Wfin
            MOVE 0 TO Wtrouve
            START FConvocations KEY EQUALS fc_jure
            MOVE fse_numSeance TO fc_numSeance
            MOVE fj_prenom TO fc_prenom
            MOVE fj_nom TO fc_nom
            READ FConvocations KEY fc_cle
            INVALID KEY
                MOVE 0 TO fc_valide
                WRITE convoTampon END-WRITE
                DISPLAY 'Convocation créée !'                   
            NOT INVALID KEY
                DISPLAY "Convocation déjà envoyée pour ce juré !"
            END-READ
            CLOSE FConvocations
        END-READ
        CLOSE FJures
    END-READ
    CLOSE FSeances
.

ModifierConvocation.
OPEN I-O FConvocations

DISPLAY 'Numéro de la séance'
ACCEPT fse_numSeance
OPEN INPUT FSeances
READ FSeances KEY fse_numSeance
IF seanceCR <> 0
    CLOSE FSeances
    DISPLAY 'Séance inexistante'
    CLOSE FConvocations
ELSE

DISPLAY 'Nom juré ?'
ACCEPT fc_nom
DISPLAY 'Prenom Juré ?'
ACCEPT fc_prenom

READ FJures KEY fc_jure
    IF jureCR <> 0
       CLOSE FJures
       DISPLAY 'Erreur invalide ! Juré non renseigné dans la liste des jurés'
       CLOSE FConvocations
    ELSE

    DISPLAY 'Caractère valide actuel : 'fc_valide
    DISPLAY 'Quel est le nouveau caractère valide de cette convocation ?'
    ACCEPT valide

    MOVE valide to fc_valide
    REWRITE convoTampon END-REWRITE
    DISPLAY 'Convocation modifiée'
    CLOSE FConvocations
    CLOSE FSeances
    END-IF
END-IF.


SupprimerConvocation.

OPEN I-O FConvocations

Display 'Numéro de la séance'
Accept fse_numSeance

Display ' Verification que la séance existe et futur'
OPEN INPUT FSeances

READ FSeances KEY fse_numSeance
IF seanceCR <> 0
    CLOSE FSeances
    DISPLAY 'Séance inexistante'
    CLOSE FConvocations
ELSE

IF fse_date <= dateAjd THEN
DISPLAY 'On ne peut supprimer que les séances programmées'
CLOSE FSeances
CLOSE FConvocations

ELSE

Display 'Nom juré ?'
Accept fc_nom
Display 'Prenom Juré ?'
Accept fc_prenom

READ FJures KEY fc_jure
    IF jureCR <> 0
       CLOSE FJures
       DISPLAY 'Ce juré n''existe pas !'
       CLOSE FConvocations
    ELSE

    DISPLAY 'Nom du juré :' fc_nom
    DISPLAY 'Prenom Juré :' fc_prenom
    DISPLAY 'Num séance :' fc_numSeance
    DISPLAY 'Caractère valide : 'fc_valide

       DISPLAY 'Souhaitez vous vraiment supprimer cette convocation ? 1/0'
        ACCEPT WRep
        IF WRep = 1
        THEN 
        DELETE FConvocations END-DELETE
       ELSE
       DISPLAY 'Suppression annulée'
       END-IF

    CLOSE FConvocations
    END-IF
.

RechercherConvosNonValides.

OPEN INPUT FConvocations
DISPLAY "Recherche des convocations non valides..."
IF convoCR = 00 THEN
       MOVE 0 To WFin
       DISPLAY ' '
       PERFORM WITH TEST AFter UNTIL Wfin = 1
       READ FConvocations NEXT
       AT END
           MOVE 1 To WFin
       NOT AT END

        IF fc_valide = 0
           DISPLAY ' Nom du juré : 'fc_nom
           DISPLAY 'Prénom du juré : 'fc_prenom
           DISPLAY ' Numéro de la séance correspondante ' fc_numSeance
           DISPLAY ' '
        END-IF
       END-PERFORM
       CLOSE FConvocations
ELSE
DISPLAY 'Erreur ouverture fichier'
END-IF.


ConsulterSeances.
    OPEN I-O FSeances
    IF seanceCR = 00 THEN
        MOVE 0 TO WFin
        DISPLAY ' '
        PERFORM WITH TEST AFTER UNTIL WFin = 1
            READ FSeances NEXT
            AT END 
                MOVE 1 TO WFin
            NOT AT END
                DISPLAY 'Numéro : ', fse_numSeance
                DISPLAY 'Type de tribunal :', fse_typeTribunal
                DISPLAY 'Nom du Juge : ', fse_juge
                DISPLAY 'Date de la séance:  ', fse_date
                DISPLAY 'Salle n°', fse_numSalle
                DISPLAY 'Tribunal : ', fse_numTribunal
                DISPLAY 'Référence de l''affaire : ', fse_refAffaire
                DISPLAY ' '
        END-PERFORM
        CLOSE FSeances
    ELSE
        DISPLAY 'Le fichier Séances n''existe pas !'
    END-IF
.

AjouterSeance.
    OPEN I-O FSeances
    IF seanceCR <> 0 THEN
        OPEN OUTPUT FSeances
        CLOSE FSeances
        OPEN I-O FSeances
    END-IF
    
    *>Récupération du dernier num seance
    MOVE 0 TO WFin
    MOVE 0 TO WNse
    PERFORM WITH TEST AFTER UNTIL WFin = 1
        READ FSeances NEXT
        AT END
            MOVE 1 TO WFin
        NOT AT END
            IF fse_numSeance > WNse THEN
                MOVE fse_numSeance TO WNse
            END-IF
        END-READ
    END-PERFORM

    *>Récupération de la date actuelle
    ACCEPT WDate FROM DATE YYYYMMDD
    COMPUTE WDate = FUNCTION INTEGER-OF-DATE(WDate) + 7
    ADD 1 TO WNse
    DISPLAY 'Séance n°', WNse
    DISPLAY 'Type de tribunal (Appel, Prudhomme, Commerce...) :'
    ACCEPT WTtrib
    DISPLAY 'Nom du juge :'
    ACCEPT WNJuge

    DISPLAY 'Date de la séance (Préavis de 7 jours minimum. Saisie au format AAAAMMJJ) :'
    ACCEPT fse_date
    COMPUTE fse_date = FUNCTION INTEGER-OF-DATE(fse_date)
    IF fse_date >= WDate THEN
        MOVE fse_date TO WDate
        COMPUTE WDate = FUNCTION DATE-OF-INTEGER(WDate)
        PERFORM sallesDispo
        MOVE 0 TO WTrouve
        MOVE 0 TO WFin
        MOVE 0 TO WRep
        DISPLAY 'Numéro de la salle :'
        ACCEPT WNSalle
        DISPLAY 'Numéro du tribunal :'
        ACCEPT WNTrib
        OPEN INPUT FSalles
        IF salleCR = 0 THEN
            MOVE 0 TO WFin
            MOVE 0 TO WTrouve
            PERFORM WITH TEST AFTER UNTIL WFin = 1 OR Wtrouve = 1
                READ FSalles
                AT END MOVE 1 TO WFin
                NOT AT END
                    IF WNTrib = fsa_numTribunal AND WNSalle = fsa_numSalle THEN
                        MOVE 1 TO WTrouve
                    END-IF
            END-PERFORM
            CLOSE Fsalles
            
            IF WTrouve = 1 THEN
                MOVE WNSalle TO fse_numSalle
                MOVE WNTrib TO fse_numTribunal
                MOVE 0 TO WFin
                MOVE 0 TO WTrouve
                START FSeances KEY EQUALS fse_salle
                NOT INVALID KEY
                    PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
                        READ FSeances NEXT
                        NOT AT END 
                            IF fse_numSalle <> WNSalle OR fse_numTribunal <> WNTrib THEN
                                MOVE 1 TO WFin
                            ELSE
                               IF FUNCTION INTEGER-OF-DATE(fse_date) = FUNCTION INTEGER-OF-DATE(WDate)
                                   MOVE 1 TO WTrouve
                               END-IF
                            END-IF
                    END-PERFORM
                END-START

                IF Wtrouve = 0 THEN
                    IF WAuto = 0 THEN
                        OPEN INPUT FAffaires
                        MOVE 0 TO WTrouve
                        MOVE 0 TO WFin
                        DISPLAY 'Référence de l''affaire : '
                        ACCEPT WRef
                        PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
                            READ FAffaires
                            AT END MOVE 1 TO WFin
                            NOT AT END
                                IF WRef = fa_refAffaire THEN
                                    MOVE 1 TO WTrouve
                                    MOVE fa_classee TO WClasse
                                END-IF
                        END-PERFORM
                        CLOSE FAffaires 
                    END-IF
                    IF (Wtrouve = 1 AND WClasse = 0) OR WAuto = 1 THEN
                        MOVE WRef TO fse_refAffaire
                        MOVE WNse TO fse_numSeance
                        MOVE WNJuge TO fse_juge
                        MOVE WTtrib TO fse_typeTribunal
                        MOVE WNSalle TO fse_numSalle
                        MOVE WNTrib TO fse_numTribunal
                        MOVE WDate TO fse_date
                        WRITE seanceTampon END-WRITE
                        IF seanceCR <> 0 THEN
                            DISPLAY 'Erreur d''écriture'
                        ELSE
                            DISPLAY 'Ajout effectué !'
                        END-IF
                        CLOSE FSeances

                        MOVE 0 TO WFin
                        MOVE 0 TO WTrouve
                        OPEN INPUT FSeances
                        START FSeances KEY EQUALS fse_refAffaire
                        NOT INVALID KEY
                           PERFORM WITH TEST AFTER UNTIL WFin = 1
                               READ FSeances NEXT
                               AT END MOVE 1 TO WFin
                               NOT AT END
                                   IF WRef = fse_refAffaire THEN
                                       IF WDate > fse_date AND fse_date > WDate2 AND WNse <> fse_numSalle THEN
                                       *> On cherche la séance dont la date est inférieure à WDate, mais la plus proche possible de celle-ci.
                                           MOVE 1 TO WTrouve
                                           MOVE fse_numSeance TO WNseAncien
                                           MOVE fse_date TO WDate2
                                       END-IF
                                   ELSE
                                       MOVE 1 TO WFin
                                   END-IF
                               END-READ
                           END-PERFORM
                        END-START
                        CLOSE FSeances
                        IF WTrouve = 1 THEN
                           DISPLAY 'num seance trouvée : ', WNseAncien
                           DISPLAY 'Voulez-vous dupliquer les convocations de la dernière séance de cette affaire, sur cette séance ? 1/0'
                           ACCEPT WChoix
                           IF WChoix = 1 THEN
                               OPEN I-O FConvocations
                               OPEN I-O FSeances
                               MOVE WNseAncien TO fc_numSeance
                               START FConvocations KEY EQUALS fc_numSeance
                               INVALID KEY
                                   DISPLAY 'Aucune convocation n''a été ajoutée pour l''ancienne séance.'
                               NOT INVALID KEY
                                   MOVE 0 TO WFin
                                   PERFORM WITH TEST AFTER UNTIL WFin = 1
                                       READ FConvocations NEXT
                                       AT END MOVE 1 TO WFin
                                       NOT AT END
                                           IF fc_numSeance <> WNseAncien
                                               MOVE 1 TO WFin
                                           ELSE
                                               DISPLAY 'Ajout de la convocation de ', fc_jure, 'à la séance ', WNse
                                               MOVE WNse TO fc_numSeance
                                               WRITE convoTampon END-WRITE
                                               MOVE WNseAncien TO fc_numSeance
                                           END-IF
                                       END-READ
                                   END-PERFORM
                               END-START
                               CLOSE FConvocations
                               CLOSE FSeances
                           END-IF   
                        ELSE
                           DISPLAY 'Voulez-vous créer des convocations pour cette séance ? 1/0'
                           ACCEPT WChoix
                           IF WChoix = 1                                  
                               MOVE 0 TO WRep
                               MOVE 1 TO WAuto
                               PERFORM WITH TEST AFTER UNTIL WRep = 6 OR WChoix = 0
                                   PERFORM AjouterConvocation
                                   COMPUTE WRep = WRep + 1
                                   DISPLAY 'Voulez-vous ajouter une autre convocation ? 1/0'
                                   ACCEPT WChoix
                               END-PERFORM
                               MOVE 0 TO WAuto
                           ELSE
                               DISPLAY 'N''oubliez pas d''ajouter des convocations !'
                           END-IF
                        END-IF                           
                    ELSE 
                        DISPLAY 'Affaire inconnue ou déjà classée !'
                    END-IF
                ELSE
                    DISPLAY 'Salle non disponible !'
                END-IF
            ELSE
                DISPLAY 'Salle inexistante !'
            END-IF
        ELSE
            DISPLAY 'Fichier salle inexistant !'
        END-IF
    ELSE
        DISPLAY 'Date invalide !'
    END-IF
    CLOSE FSeances
.

ModifierSeance.
    OPEN I-O FSeances
    IF seanceCR = 00 THEN
        DISPLAY 'Identifiant de la séance :'
        ACCEPT fse_numSeance
        READ FSeances
        INVALID KEY
            DISPLAY 'Séance inexistante !'
        NOT INVALID KEY
            DISPLAY 'Nom du Juge :'
            ACCEPT WNJuge
            DISPLAY 'Nouvelle Date (Supérieure à ', fse_date, ') :'
            ACCEPT WDate

            IF FUNCTION INTEGER-OF-DATE(WDate) > FUNCTION INTEGER-OF-DATE(fse_date) THEN
                DISPLAY 'Numéro de la salle :'
                ACCEPT WNSalle
                DISPLAY 'Numéro du tribunal :'
                ACCEPT WNTrib

                OPEN INPUT FSalles
                IF salleCR = 00 THEN
                    MOVE 0 TO WFin
                    MOVE 0 TO WTrouve
                    PERFORM WITH TEST AFTER UNTIL WFin = 1 OR Wtrouve = 1
                        READ FSalles
                        AT END MOVE 1 TO WFin
                        NOT AT END
                            IF WNTrib = fsa_numTribunal AND WNSalle = fsa_numSalle THEN
                                MOVE 1 TO WTrouve
                            END-IF
                    END-PERFORM
                    CLOSE Fsalles

                    IF Wtrouve = 1
                        MOVE 0 TO WFin
                        MOVE 0 TO WTrouve

                        MOVE WNSalle TO fse_numSalle
                        MOVE WNTrib TO fse_numTribunal

                        START FSeances, KEY IS = fse_salle
                        NOT INVALID KEY
                            PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
                                READ FSeances NEXT
                                AT END 
                                    MOVE 1 TO WFin
                                NOT AT END 
                                    IF FUNCTION INTEGER-OF-DATE(fse_date) = FUNCTION INTEGER-OF-DATE(WDate)
                                        MOVE 1 TO WTrouve
                                    END-IF
                                    IF fse_numSalle = WNSalle AND fse_numTribunal = WNTrib THEN
                                        MOVE 1 TO WFin
                                    END-IF
                            END-PERFORM
                        END-START

                        IF Wtrouve = 0 THEN
                            MOVE WDate TO fse_date
                            MOVE WNTrib TO fse_numTribunal
                            MOVE WNSalle TO fse_numSalle
                            MOVE WNJuge TO fse_juge
                            REWRITE seanceTampon
                            INVALID KEY 
                                DISPLAY 'Erreur d''écriture'
                            NOT INVALID KEY
                                DISPLAY 'La séance a bien été modifiée !'
                            END-REWRITE
                        ELSE
                            DISPLAY 'Salle non disponible !'
                        END-IF
                    ELSE
                        DISPLAY 'Salle inexistante !'
                    END-IF
                ELSE
                    DISPLAY 'Fichier Salles inexistant !'
                END-IF
            ELSE 
                DISPLAY 'Date invalide !'
            END-IF    
        END-READ
        CLOSE FSeances
    ELSE
        DISPLAY 'Le fichier n''existe pas !'
    END-IF
.

SupprimerSeance.
    OPEN I-O FSeances
    IF seanceCR = 00 THEN
        DISPLAY 'Identifiant de la séance :'
        ACCEPT fse_numSeance
        READ FSeances
        INVALID KEY
            DISPLAY 'Séance iexistante !'
        NOT INVALID KEY
            OPEN I-O FConvocations
            MOVE 0 TO WTrouve 
            MOVE 0 TO WFin
            PERFORM WITH TEST AFTER UNTIL WRep = 1 OR WFin = 1
                READ FConvocations
                AT END MOVE 1 TO WFin
                NOT AT END
                IF fc_numSeance = fse_numSeance THEN    
                    MOVE 1 TO WTrouve
                END-IF
            END-PERFORM
            CLOSE FConvocations
            IF Wtrouve = 0 THEN
                DISPLAY 'Voulez-vous vraiment supprimer cette séance ? 1/0'
                PERFORM WITH TEST AFTER UNTIL WRep = 0 OR WRep = 1
                    ACCEPT WRep
                END-PERFORM
                IF WRep = 1 THEN
                    DELETE FSeances RECORD
                    DISPLAY 'Suppression effectuée'
                ELSE
                    DISPLAY 'Suppression annulée'
                END-IF
            ELSE
                DISPLAY 'La séance possède des convocations !'
    ELSE
        DISPLAY 'Le fichier n''existe pas !'
    END-IF
CLOSE FSeances.

sallesDispo.
    DISPLAY "Salles disponible :"
    OPEN INPUT FSalles
    MOVE 0 TO WFin
    IF salleCR = 00 THEN
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
            READ FSalles
            AT END MOVE 1 TO Wfin
            NOT AT end
                MOVE 0 TO WRep
                MOVE 0 TO WTrouve
                MOVE fsa_numSalle TO fse_numSalle
                MOVE fsa_numTribunal TO fse_numTribunal
                START FSeances, KEY IS = fse_salle
                NOT INVALID KEY
                    PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WRep = 1
                        READ FSeances NEXT
                        AT END MOVE 1 TO WRep
                        NOT AT END 
                            IF FUNCTION INTEGER-OF-DATE(fse_date) = FUNCTION INTEGER-OF-DATE(WDate)
                                MOVE 1 TO WTrouve
                            END-IF
                            IF fse_numSalle = WNSalle AND fse_numTribunal = WNTrib THEN
                                MOVE 1 TO WRep
                            END-IF
                    END-PERFORM
                END-START
                IF Wtrouve <> 1 THEN
                    DISPLAY "Numéro de la salle : ", fsa_numSalle
                    DISPLAY "Numéro du tribunal : ", fsa_numTribunal
                    DISPLAY " "
                END-IF
        END-PERFORM
        CLOSE Fsalles
    ELSE
        DISPLAY 'Le fichier Salles n''existe pas !'
    END-IF
.

RechercherSeancesJureVenir.
    OPEN I-O FConvocations
    IF convoCR = 0 THEN
        ACCEPT WDate FROM DATE YYYYMMDD
        COMPUTE WDate = FUNCTION INTEGER-OF-DATE(WDate)
        DISPLAY 'Entrez le nom du juré :'
        ACCEPT fc_nom
        DISPLAY 'Entrez le prenom du juré :'
        ACCEPT fc_prenom
        START FConvocations, KEY IS = fc_jure
        INVALID KEY
            DISPLAY 'Juré inexistant !'
        NOT INVALID KEY
            PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
                READ FConvocations NEXT
                AT END MOVE 1 TO WFin
                NOT AT END
                    IF fc_valide = 0 THEN
                        OPEN I-O FSeances
                        IF seanceCR = 0 THEN
                            MOVE fc_numSeance TO fse_numSeance
                            READ FSeances
                            INVALID KEY
                                DISPLAY 'Séance inexistante !'
                            NOT INVALID KEY
                                IF FUNCTION INTEGER-OF-DATE(fse_date) > WDate THEN
                                    DISPLAY 'Séance n°', fse_numSeance
                                    DISPLAY 'Date : ', fse_date
                                    DISPLAY 'Référence de l''affaire : ', fse_refAffaire
                                    DISPLAY 'Tribunal : ', fse_numTribunal
                                    DISPLAY 'Salle : ', fse_numSalle
                                    DISPLAY ' '
                                END-IF
                            CLOSE FSeances
                        ELSE
                            DISPLAY 'Le fichier Séances n''existe pas !'
                        END-IF
                    END-IF
            END-PERFORM 
    ELSE
        DISPLAY 'Le fichier Convoctions n''existe pas !'
    END-IF
    CLOSE FConvocations
.
      
ConsulterAffaires.
    OPEN INPUT FAffaires
    MOVE 0 TO WFin
    DISPLAY ' '
    IF affaireCR = 00 THEN
        PERFORM WITH TEST AFTER UNTIL WFin = 1
            READ FAffaires 
            AT END 
                MOVE 1 TO WFin
            NOT AT END
                DISPLAY 'Référence: ', fa_refAffaire
                IF fa_classee = 0 THEN
                    DISPLAY 'Non Classée'
                ELSE
                    DISPLAY 'Classée'
                END-IF
                DISPLAY 'Contexte: ', fa_contexte
                DISPLAY ' '
        END-PERFORM
        CLOSE FAffaires
    ELSE
        DISPLAY 'Il n''existe aucune affaire !'
    END-IF
.
      
AjouterAffaire.
    OPEN INPUT FAffaires
    IF affaireCR <> 0 THEN
        OPEN OUTPUT FAffaires
        CLOSE FAffaires
        OPEN INPUT FAffaires
    END-IF
    MOVE 0 TO WRep
    MOVE 0 TO WFin
    MOVE 0 TO Wtrouve
    MOVE 0 TO WAuto
    DISPLAY "Référence de l affaire"
    ACCEPT WRef
    OPEN INPUT FAffaires
    PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
        READ FAffaires
        AT END MOVE 1 TO WFin
        NOT AT END
            IF WRef = fa_refAffaire THEN
                MOVE 1 TO WTrouve
            END-IF
    END-PERFORM
    CLOSE FAffaires

    OPEN EXTEND FAffaires
    IF WTrouve = 0 THEN
        MOVE WRef TO fa_refAffaire
        MOVE 0 TO fa_classee
        DISPLAY 'Contexte de l Affaire: '
        ACCEPT fa_contexte
        WRITE affaireTampon END-WRITE
    ELSE
        DISPLAY 'Affaire déjà existante'
    END-IF
    CLOSE FAffaires
    PERFORM WITH TEST AFTER UNTIL WRep = 0 OR WRep = 1
        DISPLAY 'Voulez vous ajouter des seances ? 1/0'
        ACCEPT WRep
    END-PERFORM
    IF WRep = 1 THEN
        MOVE 1 TO WAuto
        DISPLAY WRef
        PERFORM WITH TEST AFTER UNTIL WRep = 0
            PERFORM AjouterSeance
            DISPLAY "Voulez vous en ajouter un autre ? 1/0"
            PERFORM WITH TEST AFTER UNTIL WRep = 1 OR WRep = 0
                ACCEPT WRep
            END-PERFORM
        END-PERFORM
        MOVE 0 TO WAuto
    ELSE
        DISPLAY "N''oubliez pas d''ajouter des seances à cette affaire !"
    END-IF
.

SupprimerAffaire.
    PERFORM ConsulterAffaires
    OPEN INPUT FAffaires
    IF affaireCR = 0 THEN
        MOVE 0 TO WTrouve
        MOVE 0 TO WFin
        DISPLAY 'Référence de l''affaire'
        ACCEPT WRef
        PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
            READ FAffaires
            AT END MOVE 1 TO WFin 
            NOT AT END
                IF WRef = fa_refAffaire THEN
                    MOVE 1 TO WTrouve
                    MOVE fa_classee TO wClasse
                END-IF
        END-PERFORM
        CLOSE FAffaires
        IF WTrouve = 1 AND wClasse = 0 THEN
            DISPLAY 'Voulez-vous vraiment supprimer cette séance ? 1/0'
            PERFORM WITH TEST AFTER UNTIL WRep = 0 OR WRep = 1
                ACCEPT WRep
            END-PERFORM
            IF WRep = 1 THEN
                OPEN INPUT FAffaires
                OPEN OUTPUT FAffairesTemp
                PERFORM WITH TEST AFTER UNTIL WFin = 1
                    READ FAffaires
                    AT END MOVE 1 TO WFin
                    NOT AT END
                        IF fa_refAffaire <> WRef THEN
                            MOVE fa_refAffaire TO fa_refAffaireTemp
                            MOVE fa_classee TO fa_classeeTemp
                            MOVE fa_contexte TO fa_contexteTemp
                            WRITE affaireTamponTemp END-WRITE
                        END-IF
                END-PERFORM
                CLOSE FAffaires
                CLOSE FAffairesTemp      
                MOVE 0 TO WFin
                OPEN OUTPUT FAffaires
                OPEN INPUT FAffairesTemp
                PERFORM WITH TEST AFTER UNTIL WFin = 1
                    READ FAffairesTemp
                    AT END MOVE 1 TO WFin
                    NOT AT END
                        MOVE fa_refAffaireTemp TO fa_refAffaire
                        MOVE fa_classeeTemp TO fa_classee
                        MOVE fa_contexteTemp TO fa_contexte
                        WRITE affaireTampon END-WRITE
                END-PERFORM
                CLOSE FAffaires
                CLOSE FAffairesTemp
            ELSE
                DISPLAY "Suppression Annulée"
            END-IF
        ELSE
            IF Wtrouve = 0 THEN
                DISPLAY "Affaire Inexistante"
            ELSE
                IF WClasse = 1 THEN
                    DISPLAY "Affaire déjà Classée"
                END-IF
            END-IF
        END-IF
    ELSE 
        display "Fichier affaires inexistant"
    END-IF
.

ModifierAffaire.
    OPEN INPUT FAffaires
    IF affaireCR = 0 THEN
        MOVE 0 to Wtrouve
        MOVE 0 TO WFin
        DISPLAY 'Référence de l''affaire :'
        ACCEPT WRef
        PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
            READ FAffaires
            AT END MOVE 1 TO WFin
            NOT AT END
                IF WRef = fa_refAffaire THEN
                    MOVE 1 TO WTrouve
                    MOVE fa_classee TO wClasse
                END-IF
        END-PERFORM
        CLOSE FAffaires
        IF WTrouve = 1 AND wClasse = 0 THEN
            OPEN INPUT FAffaires
            OPEN OUTPUT FAffairesTemp
            PERFORM WITH TEST AFTER UNTIL WFin = 1
                READ FAffaires
                AT END MOVE 1 TO WFin
                NOT AT END
                    IF fa_refAffaire <> WRef THEN
                        MOVE fa_refAffaire TO fa_refAffaireTemp
                        MOVE fa_classee TO fa_classeeTemp
                        MOVE fa_contexte TO fa_contexteTemp
                        WRITE affaireTamponTemp END-WRITE
                    ELSE 
                        DISPLAY "Reference: ", fa_refAffaire
                        IF fa_classee = 1 
                            DISPLAY "Classée"
                        ELSE
                            DISPLAY "Non Classée"
                        END-IF
                        DISPLAY "Contexte: ", fa_contexte
                    END-IF
            END-PERFORM
            CLOSE FAffaires
            CLOSE FAffairesTemp
            MOVE 0 TO WFin
            OPEN OUTPUT FAffaires
            OPEN INPUT FAffairesTemp
            PERFORM WITH TEST AFTER UNTIL WFin = 1
                READ FAffairesTemp
                AT END MOVE 1 TO WFin
                NOT AT END
                    MOVE fa_refAffaireTemp TO fa_refAffaire
                    MOVE fa_classeeTemp TO fa_classee
                    MOVE fa_contexteTemp TO fa_contexte
                    WRITE affaireTampon END-WRITE
            END-PERFORM
            MOVE WRef TO fa_refAffaire
            DISPLAY "Classée ? 1/0"
            PERFORM WITH TEST AFTER UNTIL fa_classee = 1 OR fa_classee = 0
                ACCEPT fa_classee
            END-PERFORM
            DISPLAY "Nouveau Contexte: "
            ACCEPT fa_contexte
            WRITE affaireTampon END-WRITE
            CLOSE FAffaires
            CLOSE FAffairesTemp
        ELSE
            IF Wtrouve = 0 THEN
                DISPLAY "Affaire inexistante !"
            ELSE
                IF WClasse = 1 THEN
                    DISPLAY "Affaire déjà classée !"
                END-IF
            END-IF
        END-IF
    ELSE
        DISPLAY "Fichier Affaires Inexistant"
    END-IF.


ClasserAffaire.
    OPEN INPUT FAffaires
    IF affaireCR = 0 THEN
        MOVE 0 to Wtrouve
        MOVE 0 TO WFin
        DISPLAY 'Référence de l''affaire :'
        ACCEPT WRef
        PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
            READ FAffaires
            AT END MOVE 1 TO WFin
            NOT AT END
                IF WRef = fa_refAffaire THEN
                    MOVE 1 TO WTrouve
                    MOVE fa_classee TO wClasse
                END-IF
        END-PERFORM
        CLOSE FAffaires
        IF WTrouve = 1 AND wClasse = 0 THEN
            OPEN INPUT FAffaires
            OPEN OUTPUT FAffairesTemp
            PERFORM WITH TEST AFTER UNTIL WFin = 1
                READ FAffaires
                AT END MOVE 1 TO WFin
                NOT AT END
                   MOVE fa_refAffaire TO fa_refAffaireTemp
                   MOVE fa_classee TO fa_classeeTemp
                   MOVE fa_contexte TO fa_contexteTemp
                   IF fa_refAffaire = WRef THEN
                       MOVE 1 TO fa_classeeTemp
                   END-IF
                WRITE affaireTamponTemp END-WRITE
            END-PERFORM
            CLOSE FAffaires
            CLOSE FAffairesTemp
            MOVE 0 TO WFin
            OPEN OUTPUT FAffaires
            OPEN INPUT FAffairesTemp
            PERFORM WITH TEST AFTER UNTIL WFin = 1
                READ FAffairesTemp
                AT END MOVE 1 TO WFin
                NOT AT END
                    MOVE fa_refAffaireTemp TO fa_refAffaire
                    MOVE fa_classeeTemp TO fa_classee
                    MOVE fa_contexteTemp TO fa_contexte
                    WRITE affaireTampon END-WRITE
            END-PERFORM
            CLOSE FAffaires
            CLOSE FAffairesTemp
        ELSE
            IF Wtrouve = 0 THEN
                DISPLAY "Affaire inexistante !"
            ELSE
                IF WClasse = 1 THEN
                    DISPLAY "Affaire déjà classée !"
                END-IF
            END-IF
        END-IF
    ELSE
        DISPLAY "Fichier Affaires Inexistant"
    END-IF.


ConsulterSalles.
    MOVE 0 TO WFin

    OPEN INPUT FSalles
    IF salleCR <> 0
        DISPLAY 'Fichier vide'
    ELSE 
        PERFORM WITH TEST AFTER UNTIL WFin = 1
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
        CLOSE FSalles
        IF WTrouve = 1
           OPEN INPUT FSalles
           DISPLAY 'Informations actuelles de la salle'
           DISPLAY 'capacité : ', fsa_capacite
           DISPLAY '****'
           DISPLAY 'Saisir la capacité de la nouvelle salle'
           ACCEPT capa
           OPEN OUTPUT FSallesTemp
           MOVE 0 to WFin
           PERFORM WITH TEST AFTER UNTIL WFin = 1
           READ FSalles
           AT END MOVE 1 TO WFin
           NOT AT END        
              MOVE fsa_numSalle TO fsa_numSalleTemp
              MOVE fsa_numTribunal TO fsa_numTribunalTemp
              IF fsa_numSalle = numS AND fsa_numTribunal = numT
                  MOVE capa to fsa_capaciteTemp      
              ELSE
                  MOVE fsa_capacite TO fsa_capaciteTemp
              END-IF
               WRITE salleTamponTemp END-WRITE
                          END-READ
           END-PERFORM
           CLOSE FSallesTemp
           CLOSE FSalles
           OPEN OUTPUT Fsalles
           OPEN INPUT FSallesTemp
           MOVE 0 to WFin
           PERFORM WITH TEST AFTER UNTIL WFin = 1
           READ FSallesTemp
           AT END MOVE 1 TO WFin
           NOT AT END
              MOVE fsa_numSalleTemp TO fsa_numSalle
              MOVE fsa_numTribunalTemp TO fsa_numTribunal
              MOVE fsa_capaciteTemp TO fsa_capacite
              WRITE salleTampon END-WRITE
              END-READ
           END-PERFORM
           DISPLAY 'Modification effectuée'
           CLOSE FSallesTemp
           CLOSE FSalles
        ELSE 
           DISPLAY 'Salle non trouvée'
        END-IF
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
        OPEN INPUT FSalles
        DISPLAY ' ** Informations actuelles de la salle **'
        DISPLAY 'NumSalle : 'fsa_numSalle
        DISPLAY 'NumTribunal : 'fsa_numTribunal
        DISPLAY 'capacité : ' fsa_capacite
        DISPLAY '****'

        DISPLAY 'Souhaitez vous vraiment supprimer cette salle ? 1/0'
        ACCEPT WRep
        
        IF WRep = 1
            MOVE 0 to Wtrouve1
            MOVE 0 to Wfin
            OPEN INPUT FSeances
            MOVE fsa_numSalle TO fse_numSalle
            START FSeances KEY EQUALS fse_numSalle
               INVALID KEY
               DISPLAY "null"
                  NOT INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL WFin = 1 OR Wtrouve1 = 1
                               READ FSeances NEXT
                               AT END MOVE 1 TO WFin
                               NOT AT END
                               IF fse_numSalle <> fsa_numSalle
                               MOVE 1 to Wfin
                               ELSE
                                  If FUNCTION INTEGER-OF-DATE(fse_date) > FUNCTION INTEGER-OF-DATE(dateAjd)
                                  MOVE 1 to Wtrouve1
                                  END-IF
                                END-IF
                               END-READ
                           END-PERFORM
                        END-START
               IF Wtrouve1 = 0
                OPEN OUTPUT FSallesTemp
                MOVE 0 to WFin
                PERFORM WITH TEST AFTER UNTIL WFin = 1
                        READ FSalles
                        AT END MOVE 1 TO WFin
                        NOT AT END
                        If fsa_numSalle <> numS OR fsa_numTribunal <> numT
                        MOVE fsa_numSalle TO fsa_numSalleTemp
                        MOVE fsa_numTribunal TO fsa_numTribunalTemp
                        MOVE fsa_capacite TO fsa_capaciteTemp                      
                        Write salleTamponTemp END-Write
                        END-IF
                            
                END-PERFORM
                    CLOSE FSallesTemp
                    CLOSE FSalles
                    OPEN OUTPUT Fsalles
                    OPEN INPUT FSallesTemp
                        MOVE 0 to WFin 
                        PERFORM WITH TEST AFTER UNTIL WFin = 1
                            READ FSallesTemp
                            AT END MOVE 1 TO WFin
                            NOT AT END 
                            MOVE  fsa_numSalleTemp TO fsa_numSalle
                            MOVE fsa_numTribunalTemp TO fsa_numTribunal
                            MOVE fsa_capaciteTemp TO fsa_capacite
                            Write salleTampon END-Write
                            END-READ
                        END-PERFORM
        
        
                        DISPLAY 'Salle 'fsa_numSalle' du tribunal 'fsa_numTribunal' supprimée'
                        CLOSE FSallesTemp
                   ELSE
                   DISPLAY "Suppression impossible, des séances sont prévues dans cette salle"
                   END-IF
        ELSE           
        DISPLAY 'Suppression annulée'
        END-IF
    
        CLOSE FSalles
    ELSE 
        DISPLAY 'Salle non trouvée'
    END-IF.

RechercherSallesLibres.

*> Pour chaque salle (LireSeq)
*> On instancie un bool a faux
*> Lecture de zone sur fse_numSalle : Si date = wdate on read salle next et bool = vrai
*> Si bool = Faux à la fin de la zone on affiche.


OPEN INPUT FSalles
IF salleCR <> 00
       DISPLAY 'Erreur ouverture fichier'
ELSE
       Display "Date choisie ? AAAAMMJJ"
       Accept wdate
       
       OPEN INPUT FSeances
       PERFORM WITH TEST AFTER UNTIL WFin = 1
       READ FSalles 
       AT END MOVE 1 to WFin
       NOT AT END
       
            MOVE 0 TO Wtrouve
            START FSeances KEY EQUALS fse_numSalle
               INVALID KEY
               DISPLAY "Clé invalide"
                NOT INVALID KEY 
                PERFORM with test after until Wfin1 = 1 OR Wtrouve = 1
                READ FSeances NEXT
                NOT AT END
                   IF fse_numSalle <> fsa_numSalle
                   MOVE 1 to Wfin1
                   ELSE
                       If fse_date = wdate
                           MOVE 1 to Wtrouve
                       END-IF
                    END-IF
                END-READ
                END-PERFORM

                IF Wtrouve = 0
                Display "Numéro de salle : "fsa_numSalle
                DISPLAY "Numero tribunal " fsa_numTribunal
                DISPLAY "Capacité de la salle :" fsa_capacite
                Display " "
                END-IF
                       
       END-READ  
    END-PERFORM
    CLOSE FSeances
END-IF
CLOSE FSalles.


AfficherSeancesIncorrectes.
OPEN INPUT FSeances
OPEN INPUT FConvocations
MOVE 0 TO WFin
PERFORM WITH TEST AFTER UNTIL WFin = 1
    READ FSeances NEXT
    AT END MOVE 1 TO WFin
    NOT AT END
       MOVE 0 TO nbCount
       MOVE fse_numSeance TO fc_numSeance
       START FConvocations KEY EQUALS fc_numSeance
       INVALID KEY
           DISPLAY 'La séance n° ', fse_numSeance, ' est invalide (aucun juré).'
       NOT INVALID KEY
           MOVE 0 TO WFin2
           PERFORM WITH TEST AFTER UNTIL WFin2 = 1
               READ FConvocations NEXT
               NOT AT END
                   IF fc_numSeance <> fse_numSeance
                       MOVE 1 TO WFin2
                   ELSE
                       ADD 1 TO nbCount                   
                   END-IF 
               END-READ 
           END-PERFORM
           IF nbCount < 3
           DISPLAY 'La séance n° ', fse_numSeance, ' est invalide (', nbCount, ' jurés au lieu de 3 minimum).'
           ELSE
               IF nbCount > 6
                   DISPLAY 'La séance n° ', fse_numSeance, ' est invalide (', nbCount, ' jurés au lieu de 6 maximum).'
               END-IF
           END-IF
       END-START
    END-READ
END-PERFORM

CLOSE FConvocations
CLOSE FSeances.

