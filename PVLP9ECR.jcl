//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010026
//PVLP9ECR JOB (EEVL,1),'VL',                                           00020029
//             CLASS=C,                                                 00030026
//             MSGCLASS=0,                                              00040026
//             MSGLEVEL=(1,1),                                          00050020
//             REGION=0M                                                00060020
//*--------------------------------------------------------------------*00070020
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00080020
//*--------------------------------------------------------------------*00090020
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH.EVENTUAL         00100023
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS.EVENTUAL       00110023
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00120023
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00130023
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00140023
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00141023
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00142023
//*--------------------------------------------------------------------*00150020
//* %%SET %%FECHAS  = %%$ODATE                                          00160020
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00170020
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00180020
//* %%SET %%DIA     = 01                                                00190020
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00200020
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00210020
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00220020
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00230020
//* %%SET %%COMIT   = 000001                                            00240020
//**********************************************************************00240132
//* RECUPERA INFORMACI0N DEL BACKUP MENSUAL DIC-2021   -  VLDTHIS      *00240232
//**********************************************************************00240332
//VLBACHIS EXEC PGM=ADUUMAIN,COND=(0,NE),                               00241032
//             REGION=0M,                                               00242032
//             PARM='DSNP,VLDTHISB%%E,NEW,,MSGLEVEL(1)'                 00243032
//STEPLIB  DD DISP=SHR,DSN='BMC.DB2.LOAD'                               00244032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNEXIT'                          00245032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNLOAD'                          00246032
//SYSPRINT DD SYSOUT=*                                                  00247032
//UTPRINT  DD SYSOUT=*                                                  00248032
//SYSOUT   DD SYSOUT=*                                                  00249032
//CART     DD DSN=PEBP.B2.VA12.BVPVLHIS.P00.M211201.H002828,DISP=SHR    00249132
//DATA     DD DSN=PEBP.VLFD.FIX.UNLOREP.VLDTHIS.D1210112,               00249232
//            DISP=(NEW,CATLG,DELETE),BUFNO=30,                         00249332
//            SPACE=(CYL,(150,100),RLSE),UNIT=3390,                     00249432
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS)                         00249532
//CNTL     DD DUMMY                                                     00249632
//SYSIN    DD  *                                                        00249732
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00249832
  INFILE CART FULL                                                      00249932
  FIXEDVARCHAR YES                                                      00250032
  UNLOADDN DATA                                                         00250132
  CNTLDDN  CNTL                                                         00250232
  FORMAT STANDARD                                                       00250332
  SELECT * FROM                                                         00250432
  MBVP.VLDTHIS                                                          00250532
//**********************************************************************00520030
//* RECUPERA INFORMACI0N DEL BACKUP MENSUAL DIC-2021   -  VLDTXEN      *00530030
//**********************************************************************00540030
//VLBACXEN EXEC PGM=ADUUMAIN,COND=(0,NE),                               00550032
//             REGION=0M,                                               00560032
//             PARM='DSNP,VLDTXENB%%E,NEW,,MSGLEVEL(1)'                 00570032
//STEPLIB  DD DISP=SHR,DSN='BMC.DB2.LOAD'                               00580032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNEXIT'                          00590032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNLOAD'                          00600032
//SYSPRINT DD SYSOUT=*                                                  00610032
//UTPRINT  DD SYSOUT=*                                                  00620032
//SYSOUT   DD SYSOUT=*                                                  00630032
//CART     DD DSN=PEBP.B2.VA12.BVPVLXEN.P00.M211201.H002943,DISP=SHR    00640032
//DATA     DD DSN=PEBP.VLFD.FIX.UNLOREP.VLDTXEN.D1210112,               00650032
//            DISP=(NEW,CATLG,DELETE),BUFNO=30,                         00660032
//            SPACE=(CYL,(150,100),RLSE),UNIT=3390,                     00670032
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS)                         00680032
//CNTL     DD DUMMY                                                     00690032
//SYSIN    DD  *                                                        00700032
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00710032
  INFILE CART FULL                                                      00720032
  FIXEDVARCHAR YES                                                      00730032
  UNLOADDN DATA                                                         00740032
  CNTLDDN  CNTL                                                         00750032
  FORMAT STANDARD                                                       00760032
  SELECT * FROM                                                         00770032
  MBVP.VLDTXEN                                                          00780032
//**********************************************************************00790030
//* RECUPERA INFORMACI0N DEL BACKUP MENSUAL DIC-2021   -  VLDTCAM      *00800030
//**********************************************************************00810030
//VLBACCAM EXEC PGM=ADUUMAIN,COND=(0,NE),                               00820032
//             REGION=0M,                                               00830032
//             PARM='DSNP,VLDTCAMB%%E,NEW,,MSGLEVEL(1)'                 00840032
//STEPLIB  DD DISP=SHR,DSN='BMC.DB2.LOAD'                               00850032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNEXIT'                          00860032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNLOAD'                          00870032
//SYSPRINT DD SYSOUT=*                                                  00880032
//UTPRINT  DD SYSOUT=*                                                  00890032
//SYSOUT   DD SYSOUT=*                                                  00900032
//CART     DD DSN=PEBP.B2.VA12.BVPVLCAM.P00.M211201.H002739,DISP=SHR    00910032
//DATA     DD DSN=PEBP.VLFD.FIX.UNLOREP.VLDTCAM.D1210112,               00920032
//            DISP=(NEW,CATLG,DELETE),BUFNO=30,                         00930032
//            SPACE=(CYL,(150,100),RLSE),UNIT=3390,                     00940032
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS)                         00950032
//CNTL     DD DUMMY                                                     00960032
//SYSIN    DD  *                                                        00970032
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00980032
  INFILE CART FULL                                                      00990032
  FIXEDVARCHAR YES                                                      01000032
  UNLOADDN DATA                                                         01010032
  CNTLDDN  CNTL                                                         01020032
  FORMAT STANDARD                                                       01030032
  SELECT * FROM                                                         01040032
  MBVP.VLDTCAM                                                          01050032
//**********************************************************************01060030
//* RECUPERA INFORMACI0N DEL BACKUP MENSUAL DIC-2021   -  VLDTHAC      *01061030
//**********************************************************************01062030
//VLBACHAC EXEC PGM=ADUUMAIN,COND=(0,NE),                               01063032
//             REGION=0M,                                               01064032
//             PARM='DSNP,VLDTHACB%%E,NEW,,MSGLEVEL(1)'                 01065032
//STEPLIB  DD DISP=SHR,DSN='BMC.DB2.LOAD'                               01066032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNEXIT'                          01067032
//         DD DISP=SHR,DSN='LDB2DPPA.SDSNLOAD'                          01068032
//SYSPRINT DD SYSOUT=*                                                  01069032
//UTPRINT  DD SYSOUT=*                                                  01070032
//SYSOUT   DD SYSOUT=*                                                  01080032
//CART     DD DSN=PEBP.B2.VA12.BVPVLHAC.P00.M211201.H002810,DISP=SHR    01090032
//DATA     DD DSN=PEBP.VLFD.FIX.UNLOREP.VLDTHAC.D1210112,               01100032
//            DISP=(NEW,CATLG,DELETE),BUFNO=30,                         01110032
//            SPACE=(CYL,(150,100),RLSE),UNIT=3390,                     01120032
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS)                         01130032
//CNTL     DD DUMMY                                                     01140032
//SYSIN    DD  *                                                        01150032
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01160033
  INFILE CART FULL                                                      01170033
  FIXEDVARCHAR YES                                                      01180033
  UNLOADDN DATA                                                         01190033
  CNTLDDN  CNTL                                                         01200033
  FORMAT STANDARD                                                       01210033
  SELECT * FROM                                                         01220033
  MBVP.VLDTHAC                                                          01230033
/*                                                                      01470033
//* ============================================================        01471033
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        01472033
//* ============================================================        01473033
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 01474033
//*                                                                     01475033
//*--------------------------------------------------------------------*01490033
//* FIN DE JCL                                                         *01500033
//*--------------------------------------------------------------------*01510033
