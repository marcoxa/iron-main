//* -*- Mode: JCL -*-
//*
//* factex.jcl --
//*
//* IRON MAIN: Testing PL/I job submission.
//*
//* See file COPYING for licensing information.
//*
//FACTPL1 JOB (TESTPL1),'TEST_FACT_PL/I',
//          USER=MARCOXA,PASSWORD=XXX,
//          CLASS=A,
//          MSGCLASS=H,
//          MSGLEVEL=(1,1),
//          NOTIFY=MARCOXA
//********************************************************************
//*
//* NAME: MARCOXA.NEWPROJ.PLI(FACTCLG)
//*
//* DESC: TEST PLI FACTORIAL PROGRAMS
//*
//********************************************************************
//FACTEXEC EXEC PL1LFCLG
//PL1L.SYSLIN DD UNIT=SYSDA
//PL1L.SYSIN  DD *
 /* FACT.PL1 --
  * TRADITIONAL FACTORIAL EXAMPLES.
  */
 FACTEX: PROCEDURE OPTIONS(MAIN);
     /*
      * FACT --
      * RECURSIVE VERSION.
      * THE DECLARATION IS PROBABLY NECESSARY ONLY FOR THE ANCIENT
      * PL/I (F) COMPILER.
      */
     DCL FACT ENTRY (FIXED DECIMAL) RETURNS (FIXED DECIMAL);
     FACT: PROCEDURE (N) RETURNS (FIXED DECIMAL) RECURSIVE;
         DCL N FIXED DECIMAL;
	 IF N <= 0 THEN
           RETURN(1);
	 ELSE
	   RETURN(N * FACT(N - 1));
     END FACT;
     /*
      * FACTI --
      * ITERATIVE VERION.
      */
     DCL FACTI ENTRY (FIXED DECIMAL) RETURNS (FIXED DECIMAL);
     FACTI: PROCEDURE (N) RETURNS (FIXED DECIMAL);
         DCL N FIXED DECIMAL;
	 DCL RESULT FIXED DECIMAL INIT(1);
	 DO WHILE (N > 0);
	   RESULT = N * RESULT;
	   N = N - 1;
	 END;
	 RETURN(RESULT);
     END FACTI;
     /*
      * MAIN CODE.
      */
     PUT SKIP LIST ('> TESTING THE FACTORIAL.');
     PUT SKIP LIST ('> REC FACT(6) YIELDS ', FACT(6));
     PUT SKIP LIST ('> ITE FACT(6) YIELDS ', FACTI(6));
 END FACTEX;
 /* END OF FILE -- FACT.PL1 */
/*
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//GO.STEPLIB  DD DSN=SYS1.PL1LIB,DISP=SHR
//* GO.SYSOUT   DD SYSOUT=*
//
//* factex.jcl ends here.
