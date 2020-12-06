//* -*- Mode: JCL -*-
//*
//* hellocg.jcl --
//*
//* IRON MAIN: Testing COBOL job submission.
//*
//* See file COPYING for licensing information.
//*
//HELLOCIX  JOB (1),'CIAO COBOL',
//            USER=MARCOXA,PASSWORD=NMDCDNV,
//            CLASS=A,
//            MSGCLASS=H,
//            MSGLEVEL=(1,1)
//*
//COBCOM    EXEC COBUCG
//COB.SYSIN DD *
      * Hello World in COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       
       PROCEDURE DIVISION.
       HELLO-MAIN SECTION.
           DISPLAY '++++++++++++++++++'.
           DISPLAY 'HELLO COBOL WORLD!'.
           DISPLAY '++++++++++++++++++'.
           STOP RUN.

      * end of file -- hello.cob
//GO.SYSPRINT DD SYSOUT=*
//GO.SYSOUT   DD SYSOUT=*
//
//* hellocg.jcl ends here.
