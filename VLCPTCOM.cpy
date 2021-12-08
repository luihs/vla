      ******************************************************************00010001
      *                        SMWCAVC0                                *00020001
      *                                                                *00030001
      * COPY RUTINA SM9CAVC0 CALCULO INDICAD. FINANCIEROS              *00040001
      ******************************************************************00060001
       01  DCLVLDTCOM.                                                  00070008
           10 VCOM-SITUAC          PIC X(1).                            00080008
           10 VCOM-CUENTA          PIC S9(7)V USAGE COMP-3.             00090008
           10 VCOM-CLACONT         PIC S9(2)V USAGE COMP-3.             00100008
           10 VCOM-TIPTAR          PIC X(1).                            00110008
           10 VCOM-DINIVAL         PIC X(10).                           00120008
           10 VCOM-DFINVAL         PIC X(10).                           00130008
           10 VCOM-CORRE-FIJO      PIC S9(12)V9(2) USAGE COMP-3.        00140008
           10 VCOM-CORRE-PORCEN    PIC S9(2)V9(6) USAGE COMP-3.         00150008
           10 VCOM-CORRE-MINIMO    PIC S9(12)V9(2) USAGE COMP-3.        00160008
           10 VCOM-CORRE-MAXIMO    PIC S9(12)V9(2) USAGE COMP-3.        00170008
           10 VCOM-CTERMIN         PIC X(4).                            00180008
           10 VCOM-DCRIACAO        PIC X(26).                           00190008
           10 VCOM-CUSRCRI         PIC X(7).                            00200008
           10 VCOM-DMODIF          PIC X(26).                           00210008
           10 VCOM-CUSRMOD         PIC X(7).                            00220008
