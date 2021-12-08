//*  %%INCLIB MBVT.PROD.SYMBOLS %%INCMEM GLOBALES                       00010003
//VLJT9020 JOB (ELBG,1),'VL',                                           00020003
//             CLASS=M,                                                 00030003
//             MSGCLASS=X,                                              00040003
//             MSGLEVEL=(1,1),                                          00050003
//             REGION=0M                                                00060003
//**********************************************************************00070003
//*           PROCESO DE CARGA HISTORICO DE CONTABILIDAD               *00080003
//**********************************************************************00090003
//* DEFINICION DE LIBRERIA DE EJECUTABLES                              *00100003
//**********************************************************************00110003
//JOBLIB   DD DISP=SHR,DSN=LDB2DTDB.SDSNEXIT                            00120003
//         DD DISP=SHR,DSN=LDB2DTDB.SDSNLOAD                            00130003
//         DD DISP=SHR,DSN=PEBT.ALTAMIRA.LOADLIB.BATCH                  00140003
//         DD DISP=SHR,DSN=PEBT.ALTAMIRA.LOADLIB.RUTINAS                00150003
//         DD DISP=SHR,DSN=CEE.SCEERUN                                  00160003
//**********************************************************************00170003
//*  BORRADO DE FICHEROS                                               *00180003
//**********************************************************************00190003
//VLBORR01 EXEC PGM=IDCAMS                                              00200003
//SYSPRINT DD  SYSOUT=*                                                 00210003
//SYSIN    DD  *                                                        00220003
        DELETE PEBT.VLFD.FIX.V1DQOPS1.SORT                   PURGE NVSAM00230003
        DELETE PEBT.VLFD.FIX.V1DQOPS2.SORT                   PURGE NVSAM00240003
        DELETE PEBT.VLFD.FIX.V1DQOPS3.SORT                   PURGE NVSAM00250003
        DELETE PEBT.VLFD.FVM06.V1DQOPS4.D1%%ODATE            PURGE NVSAM00260003
        DELETE PEBT.VLFD.FIX.S1DQTCON                        PURGE NVSAM00270003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.D1%%ODATE              PURGE NVSAM00280003
        DELETE PEBT.HAFD.FIX.INTZ.VALORES.D1%%ODATE          PURGE NVSAM00290003
        DELETE PEBT.VLFD.FIX.E1DQ0030                        PURGE NVSAM00300003
        DELETE PEBT.VLFD.FIX.E1DQ0030.SORT                   PURGE NVSAM00310003
        DELETE PEBT.VLFD.FIX.E1DQ0031                        PURGE NVSAM00320003
        DELETE PEBT.VLFD.FIX.E1DQ0031.SORT                   PURGE NVSAM00330003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.VLSORT08               PURGE NVSAM00340003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.CUENTA                 PURGE NVSAM00350003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.VLSORT09               PURGE NVSAM00360003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.VLSORT10               PURGE NVSAM00370003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.VLSORT11               PURGE NVSAM00380003
        DELETE PEBT.VLFD.FIX.S1DQCTG0.VLSORT12               PURGE NVSAM00390003
        DELETE PEBT.VLFD.FIX.S1DQTCON.VLSORTLD               PURGE NVSAM00400003
        DELETE PEBT.VLFD.FIX.S1DQ9080.D1%%ODATE              PURGE NVSAM00410003
        DELETE PEBT.VLFD.FIX.VL4C9090.D1%%ODATE              PURGE NVSAM00420003
        DELETE PEBT.VLFD.FIX.VL4C9100.D1%%ODATE              PURGE NVSAM00430003
        DELETE PEBT.VLFD.FIX.VL4C9110.D1%%ODATE              PURGE NVSAM00440003
           SET  MAXCC =  0                                              00450003
/*                                                                      00460003
//**********************************************************************00470003
//*  SE REALIZA EL SORT DE LAS OPS'S DE ORIGEN                         *00480003
//**********************************************************************00490003
//VLSORT01 EXEC PGM=SORT,                                               00500003
//             COND=(0,NE)                                              00510003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        00520003
//             DISP=SHR                                                 00530003
//SYSOUT   DD  SYSOUT=*                                                 00540003
//SYSPRINT DD  SYSOUT=*                                                 00550003
//SYSUDUMP DD  SYSOUT=*                                                 00560003
//SORTIN   DD  DSN=PEBT.VLFD.VSB.V1DQOPS1,                              00570003
//             DISP=SHR                                                 00580003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.V1DQOPS1.SORT,                         00590003
//             DISP=(NEW,CATLG,DELETE),                                 00600003
//             SPACE=(CYL,(40,20),RLSE),                                00610003
//             UNIT=3390,                                               00620003
//             DCB=(RECFM=FB,LRECL=2438,BLKSIZE=0,DSORG=PS)             00630003
//SYSIN    DD  *                                                        00640003
  SORT FIELDS=(200,15,BI,A)                                             00650003
  END                                                                   00660003
/*                                                                      00670003
//**********************************************************************00680003
//*  SE REALIZA EL SORT DE LAS OPS'S RECIBIDAS DE IMPUTACION A CUENTAS *00690003
//*  SOLO LAS OP'S QUE NO TIENEN ERROR EN PROCESO                      *00700003
//**********************************************************************00710003
//VLSORT02 EXEC PGM=SORT,                                               00720003
//             COND=(0,NE)                                              00730003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        00740003
//             DISP=SHR                                                 00750003
//SYSOUT   DD  SYSOUT=*                                                 00760003
//SYSPRINT DD  SYSOUT=*                                                 00770003
//SYSUDUMP DD  SYSOUT=*                                                 00780003
//SORTIN   DD  DSN=PEBT.BGFD.TMP.BGARIMPU.BGARIM15,                     00790003
//             DISP=SHR                                                 00800003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.V1DQOPS2.SORT,                         00810003
//             DISP=(NEW,CATLG,DELETE),                                 00820003
//             SPACE=(CYL,(40,20),RLSE),                                00830003
//             UNIT=3390,                                               00840003
//             DCB=(RECFM=FB,LRECL=2390,BLKSIZE=0,DSORG=PS)             00850003
//SYSIN    DD  *                                                        00860003
  SORT FIELDS=(275,3,CH,A)                                              00870003
  OMIT COND=(2306,2,CH,EQ,C'OK')                                        00880003
  END                                                                   00890003
/*                                                                      00900003
//**********************************************************************00910003
//*  SE REALIZA EL SORT DE LAS OPS'S RECIBIDAS DE IMPUTACION A CUENTAS *00920003
//**********************************************************************00930003
//VLSORTT2 EXEC PGM=SORT,                                               00940003
//             COND=(0,NE)                                              00950003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        00960003
//             DISP=SHR                                                 00970003
//SYSOUT   DD  SYSOUT=*                                                 00980003
//SYSPRINT DD  SYSOUT=*                                                 00990003
//SYSUDUMP DD  SYSOUT=*                                                 01000003
//SORTIN   DD  DSN=PEBT.BGFD.TMP.BGARIMPU.BGARIM15,                     01010003
//             DISP=SHR                                                 01020003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.V1DQOPS3.SORT,                         01030003
//             DISP=(NEW,CATLG,DELETE),                                 01040003
//             SPACE=(CYL,(40,20),RLSE),                                01050003
//             UNIT=3390,                                               01060003
//             DCB=(RECFM=FB,LRECL=2390,BLKSIZE=0,DSORG=PS)             01070003
//SYSIN    DD  *                                                        01080003
  SORT FIELDS=(152,15,BI,A)                                             01090003
  END                                                                   01100003
/*                                                                      01110003
//**********************************************************************01120003
//*  EJECUCION PROGRAMA          VL3C6080                              *01130003
//*  GENERACION DE REGISTROS DE OPS QUE NO PASARON BIEN LA IMPUTACION  *01140003
//**********************************************************************01150003
//VLPASO10 EXEC PGM=VL3C6080,                                           01160003
//             DYNAMNBR=20,                                             01170003
//             COND=(0,NE)                                              01180003
//SYS007   DD  SYSOUT=H                                                 01190003
//QRLSDB2  DD  SYSOUT=*                                                 01200003
//SYSOUT   DD  SYSOUT=*                                                 01210003
//SYSPRINT DD  SYSOUT=*                                                 01220003
//SYSABOUT DD  SYSOUT=*                                                 01230003
//SYSUDUMP DD  SYSOUT=*                                                 01240003
//SYSTSPRT DD  SYSOUT=*                                                 01250003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           01260003
//             DISP=SHR                                                 01270003
//VLOPSTEM DD  DSN=PEBT.VLFD.FIX.V1DQOPS2.SORT,                         01280003
//             DISP=SHR                                                 01290003
/*                                                                      01300003
//**********************************************************************01310003
//*  EJECUCION PROGRAMA          VL4C9015                              *01320003
//*  GENERACION DE REGISTROS DE OPS QUE NO PASARON BIEN LA CONTABILIDAD*01330003
//**********************************************************************01340003
//VLPASO20 EXEC PGM=IKJEFT1A,                                           01350003
//             DYNAMNBR=20,                                             01360003
//             COND=(0,NE)                                              01370003
//SYSOUT   DD  SYSOUT=*                                                 01380003
//SYSPRINT DD  SYSOUT=*                                                 01390003
//SYSABOUT DD  SYSOUT=*                                                 01400003
//SYSUDUMP DD  SYSOUT=*                                                 01410003
//SYSTSPRT DD  SYSOUT=*                                                 01420003
//QRLSDB2  DD  SYSOUT=*                                                 01430003
//E1TROPS  DD  DSN=PEBT.VLFD.FIX.V1DQOPS3.SORT,                         01440003
//             DISP=SHR                                                 01450003
//E2TROPS  DD  DSN=PEBT.VLFD.FIX.V1DQOPS1.SORT,                         01460003
//             DISP=SHR                                                 01470003
//V1DQCTG0 DD  DSN=PEBT.VLFD.VSB.V1DQCTG0,                              01480003
//             DISP=SHR,                                                01490003
//             AMP=('BUFND=190,BUFNI=160')                              01500003
//V1DQ0030 DD  DSN=PEBT.VLFD.VSB.V1DQ0030,                              01510003
//             DISP=SHR,                                                01520003
//             AMP=('BUFND=90,BUFNI=20')                                01530003
//V1DQ0031 DD  DSN=PEBT.VLFD.VSB.V1DQ0031,                              01540003
//             DISP=SHR,                                                01550003
//             AMP=('BUFND=90,BUFNI=20')                                01560003
//S1DQTCON DD  DSN=PEBT.VLFD.FIX.S1DQTCON,                              01570003
//             DISP=(NEW,CATLG,DELETE),                                 01580003
//             SPACE=(CYL,(40,20),RLSE),                                01590003
//             UNIT=3390,                                               01600003
//             DCB=(RECFM=FB,LRECL=414,BLKSIZE=0,DSORG=PS)              01610003
//E1DQDAE0 DD  DSN=PEBT.QGFD.FIX.A000DAE0.X0000,                        01620003
//             DISP=SHR                                                 01630003
//*TCDF0000 DD  DSN=PEBT.TCFD.VSL.TCDF0001,DISP=SHR                     01640003
//TCDFBLSR DD  DSN=PEBT.TCFD.VSL.TCDF0001,                              01650003
//             DISP=SHR                                                 01660003
//TCDF0000 DD  SUBSYS=(BLSR,'DDNAME=TCDFBLSR,HBUFND=1200,HBUFNI=2400')  01670003
//SYSTSIN  DD  *                                                        01680003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   01690003
   RUN PROGRAM(VL4C9015) PLAN(PBTVLPB)                                  01700003
   END                                                                  01710003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           01720003
//             DISP=SHR                                                 01730003
/*                                                                      01740003
//**********************************************************************01750003
//* SE REALIZA EL SORT DE LAS OPS'S DE ORIGEN PROCESADAS EN LA VL4C9015*01760003
//**********************************************************************01770003
//VLSORTT3 EXEC PGM=SORT,                                               01780003
//             COND=(0,NE)                                              01790003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        01800003
//             DISP=SHR                                                 01810003
//SYSOUT   DD  SYSOUT=*                                                 01820003
//SYSPRINT DD  SYSOUT=*                                                 01830003
//SYSUDUMP DD  SYSOUT=*                                                 01840003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.V1DQOPS1.SORT,                         01850003
//             DISP=SHR                                                 01860003
//SORTOUT  DD  DSN=PEBT.VLFD.FVM06.V1DQOPS4.D1%%ODATE,                  01870003
//             DISP=(NEW,CATLG,DELETE),                                 01880003
//             SPACE=(CYL,(40,20),RLSE),                                01890003
//             UNIT=3390,                                               01900003
//             DCB=(RECFM=FB,LRECL=2438,BLKSIZE=0,DSORG=PS)             01910003
//SYSIN    DD  *                                                        01920003
  SORT FIELDS=(37,12,BI,A)                                              01930003
  END                                                                   01940003
/*                                                                      01950003
//**********************************************************************01960003
//*  SE REALIZA EL SORT DE ACEPTADOS ELIMINANDO REGISTRO INICIAL       *01970003
//**********************************************************************01980003
//VLSORT03 EXEC PGM=SORT,                                               01990003
//             COND=(0,NE)                                              02000003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        02010003
//             DISP=SHR                                                 02020003
//SYSOUT   DD  SYSOUT=*                                                 02030003
//SYSPRINT DD  SYSOUT=*                                                 02040003
//SYSUDUMP DD  SYSOUT=*                                                 02050003
//SORTIN   DD  DSN=PEBT.VLFD.VSB.V1DQ0030,                              02060003
//             DISP=SHR                                                 02070003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.E1DQ0030,                              02080003
//             DISP=(NEW,CATLG,DELETE),                                 02090003
//             SPACE=(CYL,(40,20),RLSE),                                02100003
//             UNIT=3390,                                               02110003
//             DCB=(RECFM=FB,LRECL=140,BLKSIZE=0,DSORG=PS)              02120003
//SYSIN    DD  *                                                        02130003
  SORT FIELDS=(1,36,BI,A)                                               02140003
  OUTREC FIELDS=(37,140)                                                02150003
  OMIT COND=(1,21,BI,EQ,X'000000000000000000000000000000000000000000')  02160003
  END                                                                   02170003
/*                                                                      02180003
//**********************************************************************02190003
//*  SE REALIZA EL SORT DE ACEPTADOS ELIMINANDO REGISTRO INICIAL       *02200003
//**********************************************************************02210003
//VLSORT04 EXEC PGM=SORT,                                               02220003
//             COND=(0,NE)                                              02230003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        02240003
//             DISP=SHR                                                 02250003
//SYSOUT   DD  SYSOUT=*                                                 02260003
//SYSPRINT DD  SYSOUT=*                                                 02270003
//SYSUDUMP DD  SYSOUT=*                                                 02280003
//SORTIN   DD  DSN=PEBT.VLFD.VSB.V1DQ0031,                              02290003
//             DISP=SHR                                                 02300003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.E1DQ0031,                              02310003
//             DISP=(NEW,CATLG,DELETE),                                 02320003
//             SPACE=(CYL,(40,20),RLSE),                                02330003
//             UNIT=3390,                                               02340003
//             DCB=(RECFM=FB,LRECL=134,BLKSIZE=0,DSORG=PS)              02350003
//SYSIN    DD  *                                                        02360003
  SORT FIELDS=(1,36,BI,A)                                               02370003
  OUTREC FIELDS=(37,134)                                                02380003
  OMIT COND=(1,21,BI,EQ,X'000000000000000000000000000000000000000000')  02390003
  END                                                                   02400003
/*                                                                      02410003
//**********************************************************************02420003
//*  SE REALIZA EL SORT DE CTG'S ELIMINANDO REGISTRO INICIAL           *02430003
//**********************************************************************02440003
//VLSORT05 EXEC PGM=SORT,                                               02450003
//             COND=(0,NE)                                              02460003
//SORTLIB  DD  DSN=SYS1.SORTLIB,                                        02470003
//             DISP=SHR                                                 02480003
//SYSOUT   DD  SYSOUT=*                                                 02490003
//SYSPRINT DD  SYSOUT=*                                                 02500003
//SYSUDUMP DD  SYSOUT=*                                                 02510003
//SORTIN   DD  DSN=PEBT.VLFD.VSB.V1DQCTG0,                              02520003
//             DISP=SHR                                                 02530003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.D1%%ODATE,                    02540003
//             DISP=(NEW,CATLG,DELETE),                                 02550003
//             SPACE=(CYL,(40,20),RLSE),                                02560003
//             UNIT=3390,                                               02570003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              02580003
//SYSIN    DD  *                                                        02590003
  SORT FIELDS=(37,306,BI,A)                                             02600003
  OUTREC FIELDS=(37,306)                                                02610003
  OMIT COND=(1,21,BI,EQ,X'000000000000000000000000000000000000000000')  02620003
  END                                                                   02630003
/*                                                                      02640003
//**********************************************************************02650003
//*  COPIA ARCHIVO AL AMBIENTE DE CONTABILIDAD                         *02660003
//**********************************************************************02670003
//VLCOPIA1 EXEC PGM=ICEGENER,                                           02680003
//             COND=(0,NE)                                              02690003
//SYSOUT   DD  SYSOUT=*                                                 02700003
//SYSPRINT DD  SYSOUT=*                                                 02710003
//SYSUT1   DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.D1%%ODATE,                    02720003
//             DISP=SHR                                                 02730003
//SYSUT2   DD  DSN=PEBT.HAFD.FIX.INTZ.VALORES.D1%%ODATE,                02740003
//             DISP=(NEW,CATLG,DELETE),                                 02750003
//             SPACE=(CYL,(40,20),RLSE),                                02760003
//             UNIT=3390,                                               02770003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              02780003
//SYSIN    DD  DUMMY                                                    02790003
/*                                                                      02800003
//**********************************************************************02810003
//*  SORT      PROGRAMA         VL4C9020                               *02820003
//**********************************************************************02830003
//VLSORT06 EXEC PGM=SORT,                                               02840003
//             COND=(0,NE)                                              02850003
//SYSOUT   DD  SYSOUT=*                                                 02860003
//SYSPRINT DD  SYSOUT=*                                                 02870003
//SYSUDUMP DD  SYSOUT=*                                                 02880003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.E1DQ0030,                              02890003
//             DISP=SHR                                                 02900003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.E1DQ0030.SORT,                         02910003
//             DISP=(NEW,CATLG,DELETE),                                 02920003
//             SPACE=(CYL,(40,20),RLSE),                                02930003
//             UNIT=3390,                                               02940003
//             DCB=(RECFM=FB,LRECL=140,BLKSIZE=0,DSORG=PS)              02950003
//*//SYSIN    DD  *                                                     02960003
//*  SORT FIELDS=(31,1,CH,A,1,7,CH,A,25,3,CH,A,16,9,CH,A),FORMAT=BI     02970003
//SYSIN    DD  *                                                        02980003
  SORT FIELDS=(1,7,CH,A,25,3,CH,A,16,9,CH,A),FORMAT=BI                  02990003
  END                                                                   03000003
//**********************************************************************03010003
//*  SORT      PROGRAMA         VL4C9030                               *03020003
//**********************************************************************03030003
//VLSORT07 EXEC PGM=SORT,                                               03040003
//             COND=(0,NE)                                              03050003
//SYSOUT   DD  SYSOUT=*                                                 03060003
//SYSPRINT DD  SYSOUT=*                                                 03070003
//SYSUDUMP DD  SYSOUT=*                                                 03080003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.E1DQ0031,                              03090003
//             DISP=SHR                                                 03100003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.E1DQ0031.SORT,                         03110003
//             DISP=(NEW,CATLG,DELETE),                                 03120003
//             SPACE=(CYL,(40,20),RLSE),                                03130003
//             UNIT=3390,                                               03140003
//             DCB=(RECFM=FB,LRECL=134,BLKSIZE=0,DSORG=PS)              03150003
//*//SYSIN    DD  *                                                     03160003
//*  SORT FIELDS=(31,1,CH,A,1,7,CH,A,25,3,CH,A,16,9,CH,A),FORMAT=BI     03170003
//SYSIN    DD  *                                                        03180003
  SORT FIELDS=(1,7,CH,A,25,3,CH,A,16,9,CH,A),FORMAT=BI                  03190003
  END                                                                   03200003
//**********************************************************************03210003
//*  EJECUCION PROGRAMA         VL4C9019                               *03220003
//*  GRABA HISTORICO CONTABLE EN TABLE VLDTHAC                         *03230003
//**********************************************************************03240003
//VLPASO25 EXEC PGM=IKJEFT1A,                                           03250003
//             DYNAMNBR=20,                                             03260003
//             COND=(0,NE)                                              03270003
//SYSOUT   DD  SYSOUT=*                                                 03280003
//SYSPRINT DD  SYSOUT=*                                                 03290003
//SYSABOUT DD  SYSOUT=*                                                 03300003
//SYSUDUMP DD  SYSOUT=*                                                 03310003
//SYSTSPRT DD  SYSOUT=*                                                 03320003
//QRLSDB2  DD  SYSOUT=*                                                 03330003
//E1DQ9019 DD  DSN=PEBT.VLFD.FIX.E1DQ0030.SORT,                         03340003
//             DISP=SHR                                                 03350003
//SYSTSIN  DD  *                                                        03360003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   03370003
   RUN PROGRAM(VL4C9019) PLAN(PBTVLPB)                                  03380003
   END                                                                  03390003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           03400003
//             DISP=SHR                                                 03410003
//**********************************************************************03420003
//*  EJECUCION PROGRAMA         VL4C9030                               *03430003
//*  LISTADO DE CARGA DEL REG. DE CONTABILIDAD DE ALTAMIRA -> OPS      *03440003
//**********************************************************************03450003
//VLPASO30 EXEC PGM=IKJEFT1A,                                           03460003
//             DYNAMNBR=20,                                             03470003
//             COND=(0,NE)                                              03480003
//SYSOUT   DD  SYSOUT=*                                                 03490003
//SYSPRINT DD  SYSOUT=*                                                 03500003
//SYSABOUT DD  SYSOUT=*                                                 03510003
//SYSUDUMP DD  SYSOUT=*                                                 03520003
//SYSTSPRT DD  SYSOUT=*                                                 03530003
//QRLSDB2  DD  SYSOUT=*                                                 03540003
//ENTRADA  DD  DSN=PEBT.VLFD.FIX.E1DQ0031.SORT,                         03550003
//             DISP=SHR                                                 03560003
//VLLS9031 DD  SYSOUT=H                                                 03570003
//VLLS9032 DD  SYSOUT=H                                                 03580003
//SYSTSIN  DD  *                                                        03590003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   03600003
   RUN PROGRAM(VL4C9030) PLAN(PBTVLPB)                                  03610003
   END                                                                  03620003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           03630003
//             DISP=SHR                                                 03640003
//**********************************************************************03650003
//*  EJECUCION PROGRAMA         VL4C9020                               *03660003
//*  LISTADO DE CARGA DEL REG. DE CONTABILIDAD GENERAL DE ALTAMIRA     *03670003
//**********************************************************************03680003
//VLPASO40 EXEC PGM=IKJEFT1A,                                           03690003
//             DYNAMNBR=20,                                             03700003
//             COND=(0,NE)                                              03710003
//SYSOUT   DD  SYSOUT=*                                                 03720003
//SYSPRINT DD  SYSOUT=*                                                 03730003
//SYSABOUT DD  SYSOUT=*                                                 03740003
//SYSUDUMP DD  SYSOUT=*                                                 03750003
//SYSTSPRT DD  SYSOUT=*                                                 03760003
//QRLSDB2  DD  SYSOUT=*                                                 03770003
//ENTRADA  DD  DSN=PEBT.VLFD.FIX.E1DQ0030.SORT,                         03780003
//             DISP=SHR                                                 03790003
//VLLS9021 DD  SYSOUT=H                                                 03800003
//VLLS9022 DD  SYSOUT=H                                                 03810003
//SYSTSIN  DD  *                                                        03820003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   03830003
   RUN PROGRAM(VL4C9020) PLAN(PBTVLPB)                                  03840003
   END                                                                  03850003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           03860003
//             DISP=SHR                                                 03870003
//**********************************************************************03880003
//*  SORT      PROGRAMA         VL4C9050                               *03890003
//*  FICHERO DE ENTRADA A CONTABILIDAD POR CLAVE DE CONVERSION         *03900003
//**********************************************************************03910003
//VLSORT08 EXEC PGM=SORT,                                               03920003
//             COND=(0,NE)                                              03930003
//SYSOUT   DD  SYSOUT=*                                                 03940003
//SYSPRINT DD  SYSOUT=*                                                 03950003
//SYSUDUMP DD  SYSOUT=*                                                 03960003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.D1%%ODATE,                    03970003
//             DISP=SHR                                                 03980003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT08,                     03990003
//             DISP=(NEW,CATLG,DELETE),                                 04000003
//             SPACE=(CYL,(40,20),RLSE),                                04010003
//             UNIT=3390,                                               04020003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              04030003
//SYSIN    DD  *                                                        04040003
  SORT FIELDS=(24,69,CH,A),FORMAT=BI                                    04050003
  END                                                                   04060003
//**********************************************************************04070003
//*  EJECUCION PROGRAMA         VL4C9050                               *04080003
//*  PROGRAMA QUE BUSCA LA CUENTA CONTABLE EN LA TABLA HADT014 DE      *04090003
//*  CONTABILIDAD Y LA GRABA EN EL CAMPO VARIOS DEL FICHERO.           *04100003
//**********************************************************************04110003
//VLPASO50 EXEC PGM=IKJEFT1A,                                           04120003
//             DYNAMNBR=20,                                             04130003
//             COND=(0,NE)                                              04140003
//S1DLINC0 DD  SYSOUT=*                                                 04150003
//QRLSDB2  DD  SYSOUT=*                                                 04160003
//SYSOUT   DD  SYSOUT=*                                                 04170003
//SYSPRINT DD  SYSOUT=*                                                 04180003
//SYSABOUT DD  SYSOUT=*                                                 04190003
//SYSUDUMP DD  SYSOUT=*                                                 04200003
//SYSTSPRT DD  SYSOUT=*                                                 04210003
//QRLSDB2  DD  SYSOUT=*                                                 04220003
//VLAR040  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT08,                     04230003
//             DISP=SHR                                                 04240003
//VLAR041  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.CUENTA,                       04250003
//             DISP=(NEW,CATLG,DELETE),                                 04260003
//             SPACE=(CYL,(40,20),RLSE),                                04270003
//             UNIT=3390,                                               04280003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              04290003
//SYSTSIN  DD  *                                                        04300003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   04310003
   RUN PROGRAM(VL4C9050) PLAN(PBTVLPB)                                  04320003
   END                                                                  04330003
/*                                                                      04340003
//**********************************************************************04350003
//*  SORT      PROGRAMA         VL3C9055                               *04360003
//*  POR FECON, CEDESTIN, HACDVISA Y FIJOS                             *04370003
//**********************************************************************04380003
//VLSORT09 EXEC PGM=SORT,                                               04390003
//             COND=(0,NE)                                              04400003
//SYSOUT   DD  SYSOUT=*                                                 04410003
//SYSPRINT DD  SYSOUT=*                                                 04420003
//SYSUDUMP DD  SYSOUT=*                                                 04430003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.CUENTA,                       04440003
//             DISP=SHR                                                 04450003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT09,                     04460003
//             DISP=(NEW,CATLG,DELETE),                                 04470003
//             SPACE=(CYL,(40,20),RLSE),                                04480003
//             UNIT=3390,                                               04490003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              04500003
//SYSIN    DD  *                                                        04510003
  SORT FIELDS=(8,10,CH,A,107,4,CH,A,61,3,CH,A,24,41,CH,A),FORMAT=BI     04520003
  END                                                                   04530003
//**********************************************************************04540003
//*  EJECUCION PROGRAMA         VL3C9055                               *04550003
//*  PROGRAMA QUE LISTA LOS SALDOS CONTABLES DIARIOS                   *04560003
//**********************************************************************04570003
//VLPASO60 EXEC PGM=VL3C9055,                                           04580003
//             DYNAMNBR=20,                                             04590003
//             PARM='0018',                                             04600003
//             COND=(0,NE)                                              04610003
//S1DLINC0 DD  SYSOUT=*                                                 04620003
//QRLSDB2  DD  SYSOUT=*                                                 04630003
//SYSOUT   DD  SYSOUT=*                                                 04640003
//SYSPRINT DD  SYSOUT=*                                                 04650003
//SYSABOUT DD  SYSOUT=*                                                 04660003
//SYSUDUMP DD  SYSOUT=*                                                 04670003
//SYSTSPRT DD  SYSOUT=*                                                 04680003
//*                                                                     04690003
//SYSIN    DD  *                                                        04700003
DIARIO                                                                  04710003
/*                                                                      04720003
//E1DQMOVC DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT09,                     04730003
//             DISP=SHR                                                 04740003
//S1DQLSC0 DD  SYSOUT=H                                                 04750003
/*                                                                      04760003
//**********************************************************************04770003
//*  SORT      PROGRAMA         VL3C9060                               *04780003
//*  POR MONEDA, TRAN, USUARIO, TIMESTAMP                              *04790003
//**********************************************************************04800003
//VLSORT10 EXEC PGM=SORT,                                               04810003
//             COND=(0,NE)                                              04820003
//SYSOUT   DD  SYSOUT=*                                                 04830003
//SYSPRINT DD  SYSOUT=*                                                 04840003
//SYSUDUMP DD  SYSOUT=*                                                 04850003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.CUENTA,                       04860003
//             DISP=SHR                                                 04870003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT10,                     04880003
//             DISP=(NEW,CATLG,DELETE),                                 04890003
//             SPACE=(CYL,(40,20),RLSE),                                04900003
//             UNIT=3390,                                               04910003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              04920003
//SYSIN    DD  *                                                        04930003
  SORT FIELDS=(61,3,CH,A,280,4,CH,A,293,8,CH,A,216,20,CH,A),FORMAT=BI   04940003
  END                                                                   04950003
//**********************************************************************04960003
//*  EJECUCION PROGRAMA         VL3C9060                               *04970003
//*  PROGRAMA QUE LISTA LOS MOVIMIENTOS CONTABLES DEL DIA              *04980003
//**********************************************************************04990003
//VLPASO70 EXEC PGM=VL3C9060,                                           05000003
//             DYNAMNBR=20,                                             05010003
//             PARM='0019',                                             05020003
//             COND=(0,NE)                                              05030003
//S1DLINC0 DD  SYSOUT=*                                                 05040003
//QRLSDB2  DD  SYSOUT=*                                                 05050003
//SYSOUT   DD  SYSOUT=*                                                 05060003
//SYSPRINT DD  SYSOUT=*                                                 05070003
//SYSABOUT DD  SYSOUT=*                                                 05080003
//SYSUDUMP DD  SYSOUT=*                                                 05090003
//SYSTSPRT DD  SYSOUT=*                                                 05100003
//*                                                                     05110003
//E1DQMOVC DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT10,                     05120003
//             DISP=SHR                                                 05130003
//S1DQLMC0 DD  SYSOUT=H                                                 05140003
/*                                                                      05150003
//**********************************************************************05160003
//* LISTADOS DEL MOVIMIENTO CONTABLE DIARIO SUMARIZADO. POR FECHA      *05170003
//* CONTABLE, CENTRO ORIGEN, DIVISA.                                   *05180003
//**********************************************************************05190003
//*  SORT      PROGRAMA         VL3C9065                               *05200003
//**********************************************************************05210003
//VLSORT11 EXEC PGM=SORT,                                               05220003
//             COND=(0,NE)                                              05230003
//SYSOUT   DD  SYSOUT=*                                                 05240003
//SYSPRINT DD  SYSOUT=*                                                 05250003
//SYSUDUMP DD  SYSOUT=*                                                 05260003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.CUENTA,                       05270003
//             DISP=SHR                                                 05280003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT11,                     05290003
//             DISP=(NEW,CATLG,DELETE),                                 05300003
//             SPACE=(CYL,(40,20),RLSE),                                05310003
//             UNIT=3390,                                               05320003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              05330003
//SYSIN    DD  *                                                        05340003
  SORT FIELDS=(8,10,CH,A,103,4,CH,A,61,3,CH,A,24,41,CH,A),FORMAT=BI     05350003
  END                                                                   05360003
//**********************************************************************05370003
//*  EJECUCION PROGRAMA         VL3C9065                               *05380003
//*  PROGRAMA QUE LISTA LOS TOTALES CONTABLES DIARIOS                  *05390003
//**********************************************************************05400003
//VLPASO80 EXEC PGM=VL3C9065,                                           05410003
//             DYNAMNBR=20,                                             05420003
//             PARM='0019',                                             05430003
//             COND=(0,NE)                                              05440003
//S1DLINC0 DD  SYSOUT=*                                                 05450003
//QRLSDB2  DD  SYSOUT=*                                                 05460003
//SYSOUT   DD  SYSOUT=*                                                 05470003
//SYSPRINT DD  SYSOUT=*                                                 05480003
//SYSABOUT DD  SYSOUT=*                                                 05490003
//SYSUDUMP DD  SYSOUT=*                                                 05500003
//SYSTSPRT DD  SYSOUT=*                                                 05510003
//*                                                                     05520003
//E1DQMOVC DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT11,                     05530003
//             DISP=SHR                                                 05540003
//S1DQ4810 DD  SYSOUT=H                                                 05550003
/*                                                                      05560003
//**********************************************************************05570003
//* LISTADOS DE MOVIMIENTOS CONTABLES DEL DIA ORDENADO POR CENTRO      *05580003
//* ORIGEN E IMPORTE. CENTRO ORIGEN, MONEDA, IMPORTE, TIMESTAMP        *05590003
//**********************************************************************05600003
//*  SORT      PROGRAMA         VL3C9070                               *05610003
//**********************************************************************05620003
//VLSORT12 EXEC PGM=SORT,                                               05630003
//             COND=(0,NE)                                              05640003
//SYSOUT   DD  SYSOUT=*                                                 05650003
//SYSPRINT DD  SYSOUT=*                                                 05660003
//SYSUDUMP DD  SYSOUT=*                                                 05670003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.CUENTA,                       05680003
//             DISP=SHR                                                 05690003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT12,                     05700003
//             DISP=(NEW,CATLG,DELETE),                                 05710003
//             SPACE=(CYL,(40,20),RLSE),                                05720003
//             UNIT=3390,                                               05730003
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=0,DSORG=PS)              05740003
//SYSIN    DD  *                                                        05750003
  SORT FIELDS=(99,4,CH,A,61,3,CH,A,125,15,CH,A,216,30,CH,A),FORMAT=BI   05760003
  END                                                                   05770003
//**********************************************************************05780003
//*  EJECUCION PROGRAMA         VL3C9070                               *05790003
//*  PROGRAMA QUE LISTA LOS MOVIMIENTOS CONTABLES DEL DIA              *05800003
//**********************************************************************05810003
//VLPASO90 EXEC PGM=VL3C9070,                                           05820003
//             DYNAMNBR=20,                                             05830003
//             PARM='0019',                                             05840003
//             COND=(0,NE)                                              05850003
//S1DLINC0 DD  SYSOUT=*                                                 05860003
//QRLSDB2  DD  SYSOUT=*                                                 05870003
//SYSOUT   DD  SYSOUT=*                                                 05880003
//SYSPRINT DD  SYSOUT=*                                                 05890003
//SYSABOUT DD  SYSOUT=*                                                 05900003
//SYSUDUMP DD  SYSOUT=*                                                 05910003
//SYSTSPRT DD  SYSOUT=*                                                 05920003
//*                                                                     05930003
//E1DQMOVC DD  DSN=PEBT.VLFD.FIX.S1DQCTG0.VLSORT12,                     05940003
//             DISP=SHR                                                 05950003
//S1DQLMC0 DD  SYSOUT=H                                                 05960003
/*                                                                      05970003
//**********************************************************************05980003
//*  EJECUCION PROGRAMA         VL4C9080                               *05990003
//*  ISREG DE COMISIONES DE OPERACIONES FINANCIERAS Y COMISIONES       *06000003
//**********************************************************************06010003
//VLPAS100 EXEC PGM=IKJEFT1A,                                           06020003
//             DYNAMNBR=20,                                             06030003
//             COND=(0,NE)                                              06040003
//SYSOUT   DD  SYSOUT=*                                                 06050003
//SYSPRINT DD  SYSOUT=*                                                 06060003
//SYSABOUT DD  SYSOUT=*                                                 06070003
//SYSDBOUT DD  SYSOUT=*                                                 06080003
//SYSTSPRT DD  SYSOUT=*                                                 06090003
//QRLSDB2  DD  SYSOUT=*                                                 06100003
//E1DQ0030 DD  DSN=PEBT.VLFD.FIX.E1DQ0030.SORT,                         06110003
//             DISP=SHR                                                 06120003
//S1DQ9080 DD  DSN=PEBT.VLFD.FIX.S1DQ9080.D1%%ODATE,                    06130003
//             DISP=(NEW,CATLG,DELETE),                                 06140003
//             SPACE=(CYL,(40,20),RLSE),                                06150003
//             UNIT=3390,                                               06160003
//             DCB=(RECFM=VB,LRECL=23840,BLKSIZE=0,DSORG=PS)            06170003
//SYSTSIN  DD  *                                                        06180003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   06190003
   RUN PROGRAM(VL4C9080) PLAN(PBTVLPB)                                  06200003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           06210003
//             DISP=SHR                                                 06220003
/*                                                                      06230003
//**********************************************************************06240003
//*  EJECUCION PROGRAMA         VL4C9090                               *06250003
//*  ENTRADA / SALIDA TITULOS FISICOS Y DESMATERIALIZACIONES           *06260003
//**********************************************************************06270003
//VLPAS110 EXEC PGM=IKJEFT1A,                                           06280003
//             DYNAMNBR=20,                                             06290003
//             COND=(0,NE)                                              06300003
//SYSOUT   DD  SYSOUT=*                                                 06310003
//SYSPRINT DD  SYSOUT=*                                                 06320003
//SYSABOUT DD  SYSOUT=*                                                 06330003
//SYSDBOUT DD  SYSOUT=*                                                 06340003
//SYSTSPRT DD  SYSOUT=*                                                 06350003
//QRLSDB2  DD  SYSOUT=*                                                 06360003
//S1DQ2500 DD  DSN=PEBT.VLFD.FIX.VL4C9090.D1%%ODATE,                    06370003
//             DISP=(NEW,CATLG,DELETE),                                 06380003
//             SPACE=(CYL,(40,20),RLSE),                                06390003
//             UNIT=3390,                                               06400003
//             DCB=(RECFM=VB,LRECL=23840,BLKSIZE=0,DSORG=PS)            06410003
//SYSTSIN  DD  *                                                        06420003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   06430003
   RUN PROGRAM(VL4C9090) PLAN(PBTVLPB)                                  06440003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           06450003
//             DISP=SHR                                                 06460003
/*                                                                      06470003
//**********************************************************************06480003
//*  EJECUCION PROGRAMA         VL4C9100        (FISICOS)              *06490003
//*  EMITE ENTRADA / SALIDAD DE VALORES (GAR-JUD-FID-MAT-DES-BLO-DESBL)*06500003
//**********************************************************************06510003
//VLPAS120 EXEC PGM=IKJEFT1A,                                           06520003
//             DYNAMNBR=20,                                             06530003
//             COND=(0,NE)                                              06540003
//SYSOUT   DD  SYSOUT=*                                                 06550003
//SYSPRINT DD  SYSOUT=*                                                 06560003
//SYSABOUT DD  SYSOUT=*                                                 06570003
//SYSDBOUT DD  SYSOUT=*                                                 06580003
//SYSTSPRT DD  SYSOUT=*                                                 06590003
//QRLSDB2  DD  SYSOUT=*                                                 06600003
//S1DQ2600 DD  DSN=PEBT.VLFD.FIX.VL4C9100.D1%%ODATE,                    06610003
//             DISP=(NEW,CATLG,DELETE),                                 06620003
//             SPACE=(CYL,(40,20),RLSE),                                06630003
//             UNIT=3390,                                               06640003
//             DCB=(RECFM=VB,LRECL=23840,BLKSIZE=0,DSORG=PS)            06650003
//SYSTSIN  DD  *                                                        06660003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   06670003
   RUN PROGRAM(VL4C9100) PLAN(PBTVLPB)                                  06680003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           06690003
//             DISP=SHR                                                 06700003
/*                                                                      06710003
//**********************************************************************06720003
//*  EJECUCION PROGRAMA         VL4C9110   (DESMATERIALIZADOS)         *06730003
//*  EMITE ENTRADA / SALIDAD DE VALORES (GAR-JUD-FID-MAT-DES-BLO-DESBL)*06740003
//**********************************************************************06750003
//VLPAS130 EXEC PGM=IKJEFT1A,                                           06760003
//             DYNAMNBR=20,                                             06770003
//             COND=(0,NE)                                              06780003
//SYSOUT   DD  SYSOUT=*                                                 06790003
//SYSPRINT DD  SYSOUT=*                                                 06800003
//SYSABOUT DD  SYSOUT=*                                                 06810003
//SYSDBOUT DD  SYSOUT=*                                                 06820003
//SYSTSPRT DD  SYSOUT=*                                                 06830003
//QRLSDB2  DD  SYSOUT=*                                                 06840003
//S1DQ9100 DD  DSN=PEBT.VLFD.FIX.VL4C9110.D1%%ODATE,                    06850003
//             DISP=(NEW,CATLG,DELETE),                                 06860003
//             SPACE=(CYL,(40,20),RLSE),                                06870003
//             UNIT=3390,                                               06880003
//             DCB=(RECFM=VB,LRECL=23840,BLKSIZE=0,DSORG=PS)            06890003
//SYSTSIN  DD  *                                                        06900003
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   06910003
   RUN PROGRAM(VL4C9110) PLAN(PBTVLPB)                                  06920003
//SYSIN    DD  DSN=PEBT.VLFD.FIX.STDS.FECHAS,                           06930003
//             DISP=SHR                                                 06940003
/*                                                                      06950003
//**********************************************************************06960003
//*  SORT UNIFICA DATOS PARA CARGA TABLA VLDTCON                       *06970003
//**********************************************************************06980003
//VLSORTLD EXEC PGM=SORT,                                               06990003
//             COND=(0,NE)                                              07000003
//SYSOUT   DD  SYSOUT=*                                                 07010003
//SYSPRINT DD  SYSOUT=*                                                 07020003
//SYSUDUMP DD  SYSOUT=*                                                 07030003
//SORTIN   DD  DSN=PEBT.VLFD.FIX.S2DQ3500.CONTA,                        07040003
//             DISP=SHR                                                 07050003
//         DD  DSN=PEBT.VLFD.FIX.S1DQTCON,                              07060003
//             DISP=SHR                                                 07070003
//SORTOUT  DD  DSN=PEBT.VLFD.FIX.S1DQTCON.VLSORTLD,                     07080003
//             DISP=(NEW,CATLG,DELETE),                                 07090003
//             SPACE=(CYL,(600,300),RLSE),                              07100003
//             UNIT=3390,                                               07110003
//             DCB=(RECFM=FB,LRECL=414,BLKSIZE=0,DSORG=PS)              07120003
//SYSIN    DD  *                                                        07130003
  SORT FIELDS=(1,5,CH,A,6,5,CH,A,11,2,CH,A),FORMAT=BI                   07140003
  END                                                                   07150003
/*                                                                      07160003
//**********************************************************************07170003
//* LOAD DE TABLA DE CONTABILIDAD VLDTCON CONTABLES DIAS POSTERIORES   *07180003
//**********************************************************************07190003
//SETVAR   SET AMB=T                                                    07200003
//LOADCO4  EXEC LOADBMC,                                                07210003
//             COND=(0,NE),                                             07220003
//             UID=VLCONLOA&AMB.,                                       07230003
//             ENTORNO=GPBT,                                            07240003
//             NOMTAB=VLDTCON,                                          07250003
//             INPREC='DSN=PEBT.VLFD.FIX.S1DQTCON.VLSORTLD'             07260003
//*                                                                     07270003
//**********************************************************************07280003
//*  FIN DE LA CADENA                                                  *07290003
//**********************************************************************07300003
