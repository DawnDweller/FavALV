*&---------------------------------------------------------------------*
*& Include          ZMM034_I_AF_GONDERME_TOP
*&---------------------------------------------------------------------*

CONSTANTS:
           gt_structure1 type string value 'zmm034_s_af_gond_alv',
           gt_structure2 type string value 'ZMM034_S_POPUP_ALV',
           BEGIN OF ms_gui,
              status TYPE char7 VALUE 'STATUS_',
              title  TYPE char6 VALUE 'TITLE_',
           END OF ms_gui,
           BEGIN OF ms_scr,
              s0100 TYPE sy-dynnr VALUE '0100',
              s0101 TYPE sy-dynnr VALUE '0101',
              s0200 type sy-dynnr value '0200',
              s0201 type sy-dynnr value '0201',
              s0300 type sy-dynnr value '0300',
              s0301 type sy-dynnr value '0301',
              s0400 type sy-dynnr value '0400',
              s0401 type sy-dynnr value '0401',
           END OF ms_scr,
           BEGIN OF ms_ucomm,
              back   TYPE sy-ucomm VALUE 'EX001',
              leave  TYPE sy-ucomm VALUE 'EX002',
              exit   TYPE sy-ucomm VALUE 'EX003',
              cancel200 type sy-ucomm value 'CANCEL200',
              OK200 type sy-ucomm value 'OK200',
              cancel300 type sy-ucomm value 'CANCEL300',
              OK300 type sy-ucomm value 'OK300',
              cancel400 type sy-ucomm value 'CANCEL400',
              OK400 type sy-ucomm value 'OK400',
           END OF ms_ucomm,

           BEGIN OF ms_alv_components,
              fcat TYPE char10 VALUE 'FCAT',
              grid TYPE char10 VALUE 'GRID',
              cont TYPE char10 VALUE 'CONT',
              layo TYPE char10 VALUE 'LAYO',
              vari TYPE char10 VALUE 'VARI',
              sort TYPE char10 VALUE 'SORT',
              itab TYPE char10 VALUE 'ITAB',
           END OF ms_alv_components.

types: begin of ty_row,
       row_id type int4,
       sub_row_id type int4,
  end of ty_row.

    TYPES: BEGIN OF mty_ebeln,
             ebeln      TYPE ebeln,
             lifnr      TYPE lifnr,
             adrnr      TYPE adrnr,
             lfa1_name1 TYPE name1_gp,
             ekgrp      TYPE bkgrp,
           END OF mty_ebeln,
           mtt_ebeln TYPE STANDARD TABLE OF mty_ebeln WITH EMPTY KEY.
    TYPES: BEGIN OF mty_instid_b,
             instid_b TYPE srgbtbrel-instid_b,
           END OF  mty_instid_b,
           mtt_instid_b TYPE STANDARD TABLE OF mty_instid_b WITH EMPTY KEY.

    TYPES: mtt_content     TYPE STANDARD TABLE OF solisti1 WITH NON-UNIQUE DEFAULT KEY,
           mtt_content_hex TYPE STANDARD TABLE OF solix WITH NON-UNIQUE DEFAULT KEY.
    TYPES: BEGIN OF mty_all_row,
             button_type TYPE zmm034_e_bt_typ,
             row_no      TYPE int4,
             doc_id      TYPE so_entryid,
             obj_desc    TYPE so_obj_des,
             obj_type    TYPE so_obj_tp,
             doc_size    TYPE so_doc_siz,
             content     TYPE mtt_content,
             content_hex TYPE mtt_content_hex,
           END OF mty_all_row,
           mtt_all_row TYPE STANDARD TABLE OF mty_all_row WITH EMPTY KEY.
    TYPES: BEGIN OF mty_select_data,
             submi       TYPE submi,
             aedat       TYPE aedat,
             banfn       TYPE banfn,
             bnfpo       TYPE bnfpo,
             ebeln       TYPE ebeln,
             ebelp       TYPE ebelp,
             angnr       TYPE angnr,
             lifnr       TYPE elifn,
             lifnr_name1 TYPE name1_gp,
             adrnr       TYPE adrnr,
             telf1       TYPE telf1,
             fiskn       TYPE fiskn_k,
             fisku       TYPE fisku,
             stcd1       TYPE stcd1,
             stcd2       TYPE stcd2,
             matnr       TYPE matnr,
             txz01       TYPE txz01,
             ktmng       TYPE ktmng,
             menge       TYPE bstmg,
             meins       TYPE bstme,
             angdt       TYPE angab,
             eindt       TYPE eindt,
             bukrs       TYPE bukrs,
             butxt       TYPE butxt,
             werks       TYPE werks_d,
             werks_name1 TYPE name1,
             ekgrp       TYPE bkgrp,
             bednr       TYPE bednr,
             zterm       TYPE dzterm,
             eknam       TYPE eknam,
             tel_number  TYPE ad_tlnmbr,
           END OF mty_select_data,
           mtt_select_data TYPE STANDARD TABLE OF mty_select_data.

            DATA: mo_log      TYPE REF TO cl_ptu_message.
