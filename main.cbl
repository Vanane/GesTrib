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
    ALTERNATE RECORD KEY IS fse_salle WITH DUPLICATES.

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

SELECT FAffairesTemp ASSIGN TO "affairesTemp.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS affairesTempCR.

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

77 numS PIC 9(2).
77 numT PIC 9(3).
77 capa PIC 9(3).
77 rep PIC 9(1).
77 valide PIC 9(1).

77 WFin PIC 9(1).
77 wTtrib PIC A(25).
77 wNJuge PIC A(25).
77 wNsalle PIC 9(2).
77 wNtrib PIC 9(3).
77 WRep pic 9(1).
77 WTrouve PIC 9(1).
77 WRef PIC A(9).
77 WOut PIC 9(1).
77 WCr PIC 9(2).
77 WNse PIC 9(2).
77 WClasse PIC 9(1).
77 WDate PIC 9(8).

01 dateAjd PIC 9(8).

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
        READ FJures
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
    IF convoCR <> 0
    DISPLAY 'Fichier vide'
    ELSE
    MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL WFin = 1
        READ FConvocations 
        AT END MOVE 1 to WFin
        NOT AT END
            DISPLAY 'Numéro de seance : ', fc_numSeance
            DISPLAY 'Nom du juré : ', fc_nom
            DISPLAY 'Prénom du juré : ', fc_prenom
            DISPLAY 'Validité de la convocation : ', fc_valide
            DISPLAY ' '
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

    READ FSeances KEY fse_numSeance
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

        READ FJures KEY fc_jure
        IF jureCR <> 0
        CLOSE FJures
        DISPLAY 'Erreur invalide !Juré non renseigné dans la liste des jurés'
        CLOSE FConvocations
        ELSE

            Display 'Verification de l''existance du juré'

            Display 'Validité initialisé'
            Move 0 to valide

            move numS TO fc_numSeance
            Move nomJure TO fc_nom
            MOvE prenomJure TO fc_prenom
            MOVE valide TO fc_valide

            Write convoTampon END-Write
                DISPLAY 'Convocation créée'
                CLOSE FConvocations
        END-IF
    END-IF
.

ModifierConvocation..
*> Oriane

SupprimerConvocation..
*> Oriane

RechercherConvosNonValides..
*> Oriane

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
                DISPLAY 'Numéro: ', fse_numSeance
                DISPLAY 'Type de tribunal:', fse_typeTribunal
                DISPLAY 'Nom du Juge: ', fse_juge
                DISPLAY 'Date de la séance: ', fse_date
                DISPLAY 'Salle n°', fse_numSalle
                DISPLAY 'Référence de l affaire: ', fse_refAffaire
                DISPLAY ' '
        END-PERFORM
        CLOSE FSeances
    ELSE
        DISPLAY 'Erreur d ouverture de FSeances'
    END-IF
.

AjouterSeance.
    OPEN I-O FSeances
    IF seanceCR = 35 THEN
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

    PERFORM WITH TEST AFTER UNTIL WRep = 0
        *>Récupération de la date actuelles
        ACCEPT WDate FROM DATE YYYYMMDD
        COMPUTE WDate = FUNCTION INTEGER-OF-DATE(WDate) + 7
        ADD 1 TO WNse
        DISPLAY 'Séance n°', WNse
        DISPLAY 'Type de tribunal: '
        ACCEPT wTtrib
        DISPLAY 'Nom du juge: '
        ACCEPT wNJuge

        DISPLAY 'Date de la séance (YYYYMMDD): '
        ACCEPT fse_date
        COMPUTE fse_date = FUNCTION INTEGER-OF-DATE(fse_date)
        IF fse_date >= WDate THEN
            MOVE fse_date TO WDate
            COMPUTE WDate = FUNCTION DATE-OF-INTEGER(WDate)
            DISPLAY 'Numéro de la salle: '
            ACCEPT wNsalle
            DISPLAY 'Numéro du Tribunal: '
            ACCEPT wNtrib

            OPEN INPUT FSalles
            IF salleCR = 00 THEN
                MOVE 0 TO WFin
                MOVE 0 TO WTrouve
                PERFORM WITH TEST AFTER UNTIL WFin = 1 OR Wtrouve = 1
                    READ FSalles
                    AT END MOVE 1 TO WFin
                    NOT AT END
                        IF wNtrib = fsa_numTribunal AND wNsalle = fsa_numSalle THEN
                            MOVE 1 TO WTrouve
                        END-IF
                END-PERFORM
                CLOSE Fsalles
                
                IF WTrouve = 1 THEN
                    MOVE 0 TO WFin
                    MOVE 0 TO WTrouve
                    MOVE wNsalle TO fse_numSalle
                    MOVE wNtrib TO fse_numTribunal
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
                                IF fse_numSalle = wNsalle AND fse_numTribunal = wNtrib THEN
                                    MOVE 1 TO WFin
                                END-IF
                        END-PERFORM
                    END-START

                    IF Wtrouve = 0 THEN
                        PERFORM RechercheAffaire 
                        IF Wtrouve = 1 AND WClasse = 0 THEN
                            MOVE WRef TO fse_refAffaire
                            MOVE WNse TO fse_numSeance
                            MOVE wNJuge TO fse_juge
                            MOVE wTtrib TO fse_typeTribunal
                            MOVE wNsalle TO fse_numSalle
                            MOVE wNtrib TO fse_numTribunal
                            MOVE WDate TO fse_date
                            WRITE seanceTampon END-WRITE
                            IF seanceCR NOT = 00 THEN
                                DISPLAY 'Erreur d ecriture'
                            END-IF
                        ELSE 
                            DISPLAY 'Affaire Inconnue Ou déjà Classée'
                        END-If
                    ELSE
                        DISPLAY 'Salle non disponible'
                    END-IF
                ELSE
                    DISPLAY 'Salle Inexistante'
                END-IF
            ELSE
                DISPLAY 'Fichier salle inexistant'
            END-IF
        ELSE
            DISPLAY 'Date Invalide'
        END-IF
        PERFORM WITH TEST AFTER UNTIL WRep = 0 OR WRep = 1
            DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
            ACCEPT WRep 
        END-PERFORM
    END-PERFORM
    CLOSE FSeances
.

ModifierSeance.
    OPEN I-O FSeances
    IF seanceCR = 00 THEN
        DISPLAY 'Identifiant de la séance: '
        ACCEPT fse_numSeance
        READ FSeances
        INVALID KEY
            DISPLAY 'Séance Inéxistante'
        NOT INVALID KEY
            DISPLAY 'Nom du Juge: '
            ACCEPT wNJuge
            DISPLAY 'Nouvelle Date: (Supérieur à ', fse_date, ')'
            ACCEPT WDate

            IF FUNCTION INTEGER-OF-DATE(WDate) > FUNCTION INTEGER-OF-DATE(fse_date) THEN
                DISPLAY 'Numéro de la salle: '
                ACCEPT wNsalle
                DISPLAY 'Numéro du tribunal: '
                ACCEPT wNtrib

                OPEN INPUT FSalles
                IF salleCR = 00 THEN
                    MOVE 0 TO WFin
                    MOVE 0 TO WTrouve
                    PERFORM WITH TEST AFTER UNTIL WFin = 1 OR Wtrouve = 1
                        READ FSalles
                        AT END MOVE 1 TO WFin
                        NOT AT END
                            IF wNtrib = fsa_numTribunal AND wNsalle = fsa_numSalle THEN
                                MOVE 1 TO WTrouve
                            END-IF
                    END-PERFORM
                    CLOSE Fsalles

                    IF Wtrouve = 1
                        MOVE 0 TO WFin
                        MOVE 0 TO WTrouve

                        MOVE wNsalle TO fse_numSalle
                        MOVE wNtrib TO fse_numTribunal

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
                                    IF fse_numSalle = wNsalle AND fse_numTribunal = wNtrib THEN
                                        MOVE 1 TO WFin
                                    END-IF
                            END-PERFORM
                        END-START



                        IF Wtrouve = 0 THEN
                            *>MOVE WDate TO fse_date
                            *>MOVE wNtrib TO fse_numTribunal
                            *>MOVE wNsalle TO fse_numSalle
                            *>MOVE wNJuge TO fse_juge
                            REWRITE seanceTampon
                            INVALID KEY 
                                DISPLAY 'Erreur d ecriture'
                            NOT INVALID KEY
                                DISPLAY 'La séance a été modifié'
                            END-REWRITE
                        ELSE
                            DISPLAY 'Salle non disponible'
                        END-IF
                    ELSE
                        DISPLAY 'Salle Inexistante'
                    END-IF
                ELSE
                    DISPLAY 'Fichier salle inexistant'
                END-IF
            ELSE 
                DISPLAY 'Date Invalide'
            END-IF    
        END-READ
        CLOSE FSeances
    ELSE
        DISPLAY 'Erreur d ouverture de FSeances'
    END-IF
.

SupprimerSeance.
    OPEN I-O FSeances
    IF seanceCR = 00 THEN
        DISPLAY 'Identifiant de la Séance: '
        ACCEPT fse_numSeance
        READ FSeances
        INVALID KEY
            DISPLAY 'Séance Inéxistante'
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
                DISPLAY 'Voulez-vous vraiment supprimer cette séance ? 1 ou 0'
                PERFORM WITH TEST AFTER UNTIL WRep = 0 OR WRep = 1
                    ACCEPT WRep
                END-PERFORM
                IF WRep = 1 THEN
                    DELETE FSeances RECORD
                    DISPLAY 'Suppression effectuée'
                ELSE
                    DISPLAY 'Suppression Annulée'
                END-IF
            ELSE
                DISPLAY "La séance possède des convocations"
            CLOSE FSeances
    ELSE
        DISPLAY 'Erreur d ouverture de FSeances'
    END-IF
.

RechercherSeancesJureVenir..
      
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
        DISPLAY 'Erreur lors de l ouverture du fichier'
    END-IF
.
      
AjouterAffaire.
    OPEN EXTEND FAffaires
    IF affaireCR = 35 THEN
        DISPLAY 'Création du fichier Affaire'
        OPEN OUTPUT FAffaires
        CLOSE FAffaires
        OPEN EXTEND FAffaires
    END-IF
    MOVE 0 TO WRep
    PERFORM WITH TEST AFTER UNTIL WRep = 0
        PERFORM RechercheAffaire
        DISPLAY 'OUTPUT: ', WOut
        IF WOut = 0 THEN
            MOVE WRef to fa_refAffaire
            MOVE 0 TO fa_classee
            DISPLAY 'Contexte de l Affaire: '
            ACCEPT fa_contexte
            WRITE affaireTampon END-WRITE
        ELSE
            DISPLAY 'Affaire déjà existante'
        END-IF
        PERFORM WITH TEST AFTER UNTIL WRep = 0 OR WRep = 1
            DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
            ACCEPT WRep
        END-PERFORM
    END-PERFORM
    CLOSE FAffaires
.

SupprimerAffaire.
    PERFORM RechercheAffaire
    OPEN I-O FSeances
    IF seanceCR = 00 THEN
        MOVE WRef TO fse_refAffaire
        READ FSeances
        INVALID KEY
            MOVE 0 TO WTrouve
        NOT INVALID KEY 
            MOVE 1 TO WTrouve
        END-READ
        CLOSE FSeances
    END-IF
    IF WOut = 1 AND WClasse = 0 AND WTrouve = 0 THEN
        Display 'Souhaitez vous vraiment supprimer cette affaire ? 1 ou 0'
        accept WRep
        IF WRep = 1 THEN
            OPEN INPUT FAffaires
            IF affaireTampon <> 00 THEN
                CLOSE FAffaires
                OPEN INPUT FAffaires
            END-IF
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
            DISPLAY 'Annulation'
        END-IF
    ELSE
        DISPLAY 'Suppression Impossible'
    END-IF
.

ModifierAffaire.
    PERFORM RechercheAffaire
    IF WOut = 1 AND WClasse = 0 THEN
        OPEN INPUT FAffaires
        IF affaireTampon <> 00 THEN
            CLOSE FAffaires
            OPEN INPUT FAffaires
        END-IF
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
        MOVE WRef TO fa_refAffaire
        DISPLAY "Classée ? 1 ou 0"
        PERFORM WITH TEST AFTER UNTIL fa_classee = 1 OR fa_classee = 0
            ACCEPT fa_classee
        END-PERFORM
        DISPLAY "Nouveau Contexte: "
        ACCEPT fa_contexte
        WRITE affaireTampon END-WRITE
        CLOSE FAffaires
        CLOSE FAffairesTemp
    ELSE
        DISPLAY "Affaire innexistante ou déjà classée"
    END-IF
.

RechercheAffaire.
    MOVE 0 to WOut
    MOVE 0 to WFin
    MOVE 0 to WTrouve
    MOVE 0 TO WCr
    MOVE 0 TO WClasse
    MOVE '00000000' TO WRef
    DISPLAY 'Référence de l Affaire: '
    ACCEPT WRef
    OPEN INPUT FAffaires
    MOVE affaireCR to WCr
    IF affaireCR <> 00 THEN
        CLOSE FAffaires
        OPEN INPUT FAffaires
    END-IF
    PERFORM WITH TEST AFTER UNTIL WTrouve = 1 OR WFin = 1
        READ FAffaires
        AT END MOVE 1 TO WFin
        NOT AT END
            IF WRef = fa_refAffaire THEN
                MOVE 1 TO Wtrouve
                MOVE fa_classee TO WClasse
            END-IF
    END-PERFORM
    CLOSE FAffaires
    IF WCr <> 00 THEN
        OPEN EXTEND FAffaires
    END-IF
.

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
        OPEN INPUT FSalles
        
        
        DISPLAY 'Informations actuelles de la salle'
        DISPLAY 'capacité : ' fsa_capacite
        DISPLAY '****'


        DISPLAY 'Saisir la capacité de la nouvelle salle'
        ACCEPT capa

    OPEN OUTPUT FSallesTemp
    MOVE 0 to Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
            READ FSalles
            AT END MOVE 1 TO Wfin
            NOT AT END 
            
                MOVE fsa_numSalle TO fsa_numSalleTemp
                MOVE fsa_numTribunal TO fsa_numTribunalTemp
                IF fsa_numSalle = numS AND fsa_numTribunal = numT
                MOVE capa to fsa_capaciteTemp      
                ELSE
                MOVE fsa_capacite TO fsa_capaciteTemp
                END-IF

            Write salleTamponTemp END-Write
        END-PERFORM
        CLOSE FSallesTemp
        CLOSE FSalles
        OPEN OUTPUT Fsalles
        OPEN INPUT FSallesTemp
        MOVE 0 to WFin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
        READ FSallesTemp
        AT END MOVE 1 TO Wfin
        NOT AT END
        MOVE  fsa_numSalleTemp TO fsa_numSalle
        MOVE fsa_numTribunalTemp TO fsa_numTribunal
        MOVE fsa_capaciteTemp TO fsa_capacite
        Write salleTampon END-Write
        END-READ
        END-PERFORM
        DISPLAY 'Modification effectuée'
        CLOSE FSallesTemp
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
    OPEN INPUT FSalles
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
                    READ FSalles
                    AT END MOVE 1 TO WFin
                    NOT AT END
                    If fsa_numSalle <> numS OR fsa_numTribunal <> numT
                      MOVE fsa_numSalle TO fsa_numSalleTemp
                      MOVE fsa_numTribunal TO fsa_numTribunalTemp
                      MOVE fsa_capacite TO fsa_capaciteTemp                      
                      Write salleTamponTemp END-Write
                       DISPLAY 'Pas bon'
                    END-IF
                    display '** Debug 02**'     
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






