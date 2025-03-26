*&---------------------------------------------------------------------*
*& Include          ZMM034_I_AF_GONDERME_SS
*&---------------------------------------------------------------------*

TABLES: ekko, ekpo, eket, t001w.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS: s_submi FOR  ekko-submi OBLIGATORY,
                  s_bukrs FOR ekko-bukrs,
                  s_werks FOR t001w-werks,
                  s_ebeln FOR ekko-ebeln,
                  s_lifnr FOR ekko-lifnr,
                  s_banfn FOR eket-banfn,
                  s_ekgrp FOR ekko-ekgrp,
                  s_bednr FOR ekpo-bednr.
SELECTION-SCREEN END OF BLOCK b1.
