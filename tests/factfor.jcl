//* -*- Mode: JCL -*-
//*
//* factfor.jcl --
//*
//* IRON MAIN: Testing Fortran job submission.
//*
//* See file COPYING for licensing information.
//*
//FACTFOR JOB (TESTFOR),'TEST FACT FORTRAN',
//        USER=MARCOXA,PASSWORD=XXX,
//        CLASS=A,
//        MSGCLASS=H,
//        MSGLEVEL=(1,1),
//        NOTIFY=MARCOXA
//*
//FFCOMP  EXEC FORTHCLG,REGION.FORT=1024
//* FORT.SYSLIN DD UNIT=SYSDA
//FORT.SYSABEND DD SYSOUT=SYSDA
//FORT.SYSIN    DD *
C      -*- Mode: Fortran -*-
C     Computing the Factorial in Fortran.
C     Single file with main.
C
C     Declare the function FACT.
      INTEGER FACT
C
C     The program.
C
      WRITE (6, 4242)
 4242 FORMAT (18H Factorial of 6 is )
      WRITE (6, 4243) FACT(7)
 4243 FORMAT ( I8)
      STOP
      END
C
C     FACT FUNCTION
C
      INTEGER FUNCTION FACT(X)
      INTEGER X, N, F
      F = 1
C     Stop at 1 in the loop
      DO 21, N = X, 1, -1
         F = N * F
 21   CONTINUE
      FACT = F
      RETURN
      END
/*
//* end of file -- factfor.jcl
//
