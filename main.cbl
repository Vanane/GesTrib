IDENTIFICATION DIVISION.
PROGRAM-ID. ProjetGesTrib.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

/ * * * * * * * * * * * * /
/ * DEFINITION FICHIERS * /
/ * * * * * * * * * * * * /


SELECT FElecteurs ASSIGN TO "NOM FICHIER.dat"
    ORGANIZATION SEQUENTIAL
    ACCESS IS SEQUENTIAL
    FILE STATUS IS COMPTERENDU.

SELECT FBureaux ASSIGN TO "NOM FICHIER.dat"
    ORGANIZATION INDEXED
    ACCESS IS DYNAMIC
    FILE STATUS IS COMPTERENDU
    RECORD KEY IS CLEPRIMAIRE
    ALTERNATE RECORD KEY IS CLESECONDAIRE
    ALTERNATE RECORD KEY IS CLESECONDAIREAVECDOUBLONS WITH DUPLICATES.


DATA DIVISION.
FILE SECTION.
FD NOMFICHIER.
01 NOMENREGISTREMENT.
    02 ATTRIBUTS PIC 9(8).
    02 ATTRIBUTS PIC A(16).
    
WORKING-STORAGE SECTION.
77 VARIABLES PIC 9(2).
77 choixMenu PIC 9(2).
77 nomJure PIC A(25).
77 prenomJure PIC A(25).

PROCEDURE DIVISION.
PERFORM MenuPrincipal.
STOP RUN.

MenuPrincipal.
DISPLAY '----------------'
DISPLAY 'Menu :'
DISPLAY '  1 : Login Juré'
DISPLAY '  2 : Login Administrateur'
DISPLAY '----------------'
DISPLAY ' 0 : Quitter'
ACCEPT choixMenu

EVALUATE choixMenu
    WHEN 0 PERFORM Quitter
    WHEN 1 PERFORM MenuJure
    WHEN 2 PERFORM MenuAdmin
END-EVALUATE.

MenuJure.
DISPLAY '----------------'
DISPLAY 'Menu :'
DISPLAY '  1 : Consulter les prochaines séances'
DISPLAY '----------------'
DISPLAY '0 : Quitter'  
ACCEPT choixMenu
EVALUATE choixMenu
    WHEN 0 PERFORM Quitter
    WHEN 1 PERFORM ConsulterSeances
END-EVALUATE.


MenuAdmin.
PERFORM WITH TEST AFTER UNTIL choixMenu = 0
       DISPLAY '----------------'
       DISPLAY 'Menu :'
       DISPLAY '   1 : Gestion Jurés'
       DISPLAY '   2 : Gestion Convocations'
       DISPLAY '   3 : Gestion Séances'
       DISPLAY '   4 : Gestion Affaires'
       DISPLAY '   5 : Gestion Salles'
       DISPLAY '----------------'
       DISPLAY '0 : Quitter'
       
       ACCEPT choixMenu
       EVALUATE choixMenu
           WHEN 0 PERFORM Quitter
           WHEN 1 PERFORM MenuGestionJures
           WHEN 2 PERFORM MenuGestionConvocations
           WHEN 3 PERFORM MenuGestionSeances
           WHEN 4 PERFORM MenuGestionAffairess
           WHEN 5 PERFORM MenuGestionSalles
       END-EVALUATE
END-PERFORM.


MenuGestionJures..


MenuGestionConvocations..


MenuGestionSeances..


MenuGestionAffairess..


MenuGestionSalles..


