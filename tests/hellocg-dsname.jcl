//* -*- Mode: JCL -*-
//*
//* hellocg-dsname.jcl --
//*
//* IRON MAIN: Testing COBOL job submission with existent dataset.
//*
//* See file COPYING for licensing information.
//*
//HELLOCDX  JOB (1),'CIAO COBOL',
//            USER=HLQ,PASSWORD=XXXX, CHANGE WHOAMI AND XXXX
//            CLASS=A,
//            MSGCLASS=H,
//            MSGLEVEL=(1,1)
//*
//* We assume that the datset HLQ.TEST.COBOL(HELLO) exists and contains
//* a very simple "Hello World" COBOL program.
//*
//COBCOM    EXEC COBUCG
//COB.SYSIN DD DSN=HLQ.TEST.COBOL(HELLO),DISP=SHR
//GO.SYSPRINT DD SYSOUT=*
//GO.SYSOUT   DD SYSOUT=*
//
//* hellocg-dsname.jcl ends here.
