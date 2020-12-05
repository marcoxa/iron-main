//* -*- Mode: JCL -*-
//* TESTSUBMIT.JCL --
//* TEST JCL FOR EMACS JCL MODE 'SUBMIT' COMMANDS.
//* SUBMITTING THIS JOB TO A RUNNING MVS 3.8 (E.G., TK4-) OR Z/OS WILL
//* CREATE THE PARTITIONED DATASET 'EMACS.TEST' AND CATALOG IT.
//*
//* YOU CAN SUBMIT THIS JOB USING THE "JCL OS" MENU OR BY INVOKING
//* 'M-X SUBMIT' OR 'M-X JCL-SUBMIT' OR 'M-X JCL-SUBMIT-FILE'
//* (LOWERCASE).
//* BEFORE SUBMITTING CHANGE THE 'USER' AND THE 'PASSWORD' JOB
//* PARAMETERS.
//*
//* SEE FILE COPYING FOR LICENSING INFORMATION.
//*
//EMACSJOB JOB (1),'EMACS DOES JCL',
//           USER=MARCOXA,PASSWORD=NMDCDNV,
//           CLASS=A,
//           MSGCLASS=H,
//           MSGLEVEL=(1,1)
//EMACSCHK EXEC PGM=IEFBR14
//SUBMITDD DD DSN=EMACS.TEST,DISP=(NEW,CATLG,DELETE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8160,DSORG=PO),
//            SPACE=(TRK,(1,1,1),RLSE),
//            UNIT=SYSDA,
//            VOL=SER=PUB001
//
//* END OF FILE -- TESTSUBMIT.JCL
