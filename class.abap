*&---------------------------------------------------------------------*
*& Include          ZMM034_I_AF_GONDERME_CLS
*&---------------------------------------------------------------------*
CLASS lcl_main_controller DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
*      mt_main2           TYPE TABLE OF zco016_s_sabit_degisken2,
*      mt_main            TYPE TABLE OF zco016_s_sabit_degisken2,
*      mv_selected_row    TYPE sy-tabix,
      ms_formoutput      TYPE fpformoutput,
      mt_ebeln           TYPE mtt_ebeln,
      mt_werks           TYPE rsdsselopt_t,
      mt_ekgrp           TYPE rsdsselopt_t,
      mt_teklf_list      TYPE STANDARD TABLE OF zmm034_s_af_gond_alv,
*      mt_row             TYPE salv_t_row,
      mt_instid_b        TYPE mtt_instid_b,
      mt_select_data     TYPE mtt_select_data,
      mt_popup_alv_malz  TYPE STANDARD TABLE OF zmm034_s_popup_alv,
      mt_popup_alv_banfn TYPE STANDARD TABLE OF zmm034_s_popup_alv,
      mt_popup_alv_ebeln TYPE STANDARD TABLE OF zmm034_s_popup_alv,
      mt_all_rows        TYPE mtt_all_row,
*      mo_alv             TYPE REF TO cl_salv_table, ""
      mt_rows            TYPE TABLE OF ty_row,
      mv_repid TYPE sy-repid,
      gv_marker type char1,

      BEGIN OF ms_alv,
        BEGIN OF s0100,
          cont TYPE REF TO cl_gui_custom_container,
        END OF s0100,
        BEGIN OF s0200,
          cont TYPE REF TO cl_gui_custom_container,
        END OF s0200,

        BEGIN OF s0101,
          itab TYPE STANDARD TABLE OF zpp012_s_001,
          grid TYPE REF TO cl_gui_alv_grid,
          fcat TYPE lvc_t_fcat,
          layo TYPE lvc_s_layo,
          vari TYPE disvariant,
          sort TYPE lvc_t_sort,
        END OF s0101,
        BEGIN OF s0201,
          itab TYPE STANDARD TABLE OF zpp012_s_001,
          grid TYPE REF TO cl_gui_alv_grid,
          fcat TYPE lvc_t_fcat,
          layo TYPE lvc_s_layo,
          vari TYPE disvariant,
          sort TYPE lvc_t_sort,
        END OF s0201,
      END OF ms_alv.

    CLASS-METHODS:

      end_of_selection,
      get_data RAISING lcx_exceptions,
      maok,
      taok,
      teok,
      on_hotspot          FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING sender e_row_id e_column_id es_row_no,
      pbo IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,
      pai IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,
      ext IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,
      validate_ss RAISING lcx_exceptions,
      display_mat_attch_list RAISING lcx_exceptions,
      display_purch_req_list RAISING lcx_exceptions,
      display_purch_doc_list RAISING lcx_exceptions,
      display_attachment IMPORTING is_data TYPE zmm034_s_popup_alv,
      read_document_data IMPORTING iv_typeida   TYPE sibftypeid
                                   it_doc       TYPE rsdsselopt_t
                         EXPORTING et_popup_alv TYPE zmm034_tt_popup_alv
                         RAISING   lcx_exceptions,
      display_log,

      open_form RAISING lcx_exceptions,
      send_mail RAISING lcx_exceptions,
      main_alv  IMPORTING VALUE(iv_scrn_alv)  TYPE sy-dynnr
                          VALUE(iv_scrn_cont) TYPE sy-dynnr,
      build_cont IMPORTING VALUE(iv_alv_name) TYPE scrfname
                 RETURNING VALUE(ro_cont)     TYPE REF TO cl_gui_custom_container,
      fill_main_fieldcat IMPORTING VALUE(iv_scrn) TYPE char4
                         RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat,
      build_grid IMPORTING VALUE(io_cont) TYPE REF TO cl_gui_custom_container
                 RETURNING VALUE(ro_grid) TYPE REF TO cl_gui_alv_grid,
      build_layo IMPORTING VALUE(iv_scrn) TYPE char4
                 RETURNING VALUE(rs_layo) TYPE lvc_s_layo,
      build_vari IMPORTING VALUE(iv_scrn) TYPE char4
                 RETURNING VALUE(rs_vari) TYPE disvariant,
      fill_main_sort IMPORTING VALUE(iv_scrn) TYPE char4
                     RETURNING VALUE(rt_sort) TYPE lvc_t_sort.

  PRIVATE SECTION.

METHODS:
      constructor,
      set_repid IMPORTING iv_repid TYPE sy-repid.

ENDCLASS.


CLASS lcl_main_controller IMPLEMENTATION.
 METHOD constructor.
    mo_log = NEW #( ).
*   CREATE OBJECT mo_log.
    set_repid( iv_repid = sy-repid ).
  ENDMETHOD.
METHOD set_repid.
    mv_repid = sy-repid.
  ENDMETHOD.
METHOD validate_ss. "method for authorization check...
    DATA(lr_werks) = VALUE /accgo/cas_tt_plant( ).
    LOOP AT s_werks REFERENCE INTO DATA(lr_wr1).
      APPEND INITIAL LINE TO lr_werks REFERENCE INTO DATA(lr_wr2).
      lr_wr2->* = CORRESPONDING #( lr_wr1->* ).
    ENDLOOP.
    DATA(lo_auth) = NEW znt_016_cl_authority_check( ).
    DATA(lt_return) = VALUE bapirettab( ).
    lo_auth->check_zmm001(
      EXPORTING
        it_werks    = lr_werks
        iv_activity = '01'
      IMPORTING
*        et_werks    = lt_werks
        et_return   = lt_return
    ).

    IF lt_return IS NOT INITIAL.
      mt_werks = VALUE #( FOR ls_return_ IN lt_return ( sign = 'I' option = 'EQ' low = ls_return_-message_v1 ) ).
      MESSAGE i006 INTO DATA(lv_msg).
      RAISE EXCEPTION NEW lcx_exceptions( iv_message = | { lv_msg } |
                                          iv_type    = lcx_exceptions=>mc_msgty_i
                                          io_log     = mo_log ).
    ENDIF.

" Authorization Check for EKGRP Field

validate_ss( ).

    SELECT
      FROM
      t024
      FIELDS
        ekgrp
      WHERE ekgrp IN @s_ekgrp
      INTO TABLE @DATA(lt_ekgrp).

    IF lt_ekgrp IS NOT INITIAL.

      LOOP AT lt_ekgrp INTO DATA(ls_ekgrp).
        AUTHORITY-CHECK OBJECT 'ZMM002'
          ID 'EKGRP' FIELD ls_ekgrp-ekgrp
          ID 'ACTVT' FIELD '01'.
        IF sy-subrc <> 0.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_ekgrp-ekgrp ) TO mt_ekgrp .
          MESSAGE i007 WITH ls_ekgrp-ekgrp INTO DATA(lv_msg1).
          RAISE EXCEPTION NEW lcx_exceptions( iv_message = | { lv_msg1 } |
                                              iv_type    = lcx_exceptions=>mc_msgty_i
                                              io_log     = mo_log ).

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD display_attachment.
    DATA: lv_attachment TYPE borident-objkey.
    IF is_data IS NOT INITIAL.
      lv_attachment = CONV #( is_data-doc_id ).
      DATA(lo_gos_service) = NEW cl_gos_document_service( ).
      CALL METHOD lo_gos_service->display_attachment
        EXPORTING
          ip_attachment = lv_attachment.
    ENDIF.
  ENDMETHOD.
  METHOD get_data.

    SELECT
      FROM ekko AS ek
      INNER JOIN ekpo AS ep ON ek~ebeln = ep~ebeln
                           AND ep~loekz = ' '
      INNER JOIN eket AS et ON ep~ebeln = et~ebeln
                           AND ep~ebelp = et~ebelp
      LEFT OUTER JOIN t001 AS t01 ON ek~bukrs = t01~bukrs
      LEFT OUTER JOIN t001w AS tw ON ep~werks = tw~werks
      LEFT OUTER JOIN lfa1 AS lf ON ek~lifnr = lf~lifnr
      LEFT OUTER JOIN t024 AS t4 ON ek~ekgrp = t4~ekgrp
      FIELDS
        ek~submi,
        ek~aedat,
        et~banfn,
        et~bnfpo,
        ep~ebeln,
        ep~ebelp,
        ep~angnr,
        ek~lifnr,
        lf~name1 AS lifnr_name1,
        lf~adrnr,
        lf~telf1,
        lf~fiskn,
        lf~fisku,
        lf~stcd1,
        lf~stcd2,
        ep~matnr,
        ep~txz01,
        ep~ktmng,
        ep~menge,
        ep~meins,
        ek~angdt,
        et~eindt,
        ek~bukrs,
        t01~butxt,
        ep~werks,
        tw~name1 AS werks_name1,
        ek~ekgrp,
         ep~bednr,
        ek~zterm,
        t4~eknam,
        t4~tel_number
      WHERE ek~submi IN @s_submi
        AND ek~memory = ' '
        AND ek~loekz  = ' '
        AND ek~bstyp = 'A'
        AND ek~bukrs IN @s_bukrs
        AND et~banfn IN @s_banfn
        AND ep~bednr IN @s_bednr
        AND ep~ebeln IN @s_ebeln
        AND ek~ekgrp IN @s_ekgrp
        AND ek~lifnr IN @s_lifnr
        AND ep~werks IN @s_werks
      INTO TABLE @mt_select_data.

    IF sy-subrc <> 0.
      MESSAGE 'Veri bulunamadı.' TYPE 'E'.
        MESSAGE i001 INTO DATA(lv_msg).
      RAISE EXCEPTION NEW lcx_exceptions( iv_message = | { lv_msg } |
                                          iv_type    = lcx_exceptions=>mc_msgty_e
                                          io_log     = mo_log ).

    ELSE.
      MESSAGE i002 INTO DATA(lv_msg1).
      mo_log->add_text( iv_text = | { lv_msg1 } |
                            iv_type = lcx_exceptions=>mc_msgty_i ).
    ENDIF.

    SORT mt_select_data BY matnr.
    IF mt_werks IS NOT INITIAL.
      DELETE mt_select_data WHERE werks IN mt_werks.
    ENDIF.
    IF mt_ekgrp IS NOT INITIAL.
      DELETE mt_select_data WHERE ekgrp IN mt_ekgrp.
    ENDIF.

    LOOP AT mt_select_data ASSIGNING FIELD-SYMBOL(<fs_list>).
      APPEND INITIAL LINE TO mt_teklf_list REFERENCE INTO DATA(lr_list).
      lr_list->* = VALUE #( tklf_talep_grno  = <fs_list>-submi
                            stnalma_tlpno    = <fs_list>-banfn
                            stalm_tlp_klmno  = <fs_list>-bnfpo
                            stnalm_blg_no    = <fs_list>-ebeln
                            satalm_blgklm_no = <fs_list>-ebelp
                            aedat            = <fs_list>-aedat
                            satici_hsp_no    = <fs_list>-lifnr
                            satici_ad        = <fs_list>-lifnr_name1
                            stnalm_malz      = <fs_list>-matnr
                            stnalm_txt       = <fs_list>-txz01
                            stnalm_miktar    = <fs_list>-ktmng
                            stnalma_olcum    = <fs_list>-meins
                            teklf_vrme_sr    = <fs_list>-angdt
                            klm_teslm_tarih  = <fs_list>-eindt
                            stnalm_comp      = <fs_list>-bukrs
                            stnalm_comp_ad   = <fs_list>-butxt
                            stnalm_urtm      = <fs_list>-werks
                            stnalm_urtm_ad   = <fs_list>-werks_name1
                            stnalm_gr        = <fs_list>-ekgrp
                            ihtyc_no         = <fs_list>-bednr
                            odeme_kosul      = <fs_list>-zterm  ).
    ENDLOOP.

    SORT mt_teklf_list BY stnalm_blg_no satalm_blgklm_no.
    IF mt_select_data[] IS NOT INITIAL.
      LOOP AT mt_select_data ASSIGNING FIELD-SYMBOL(<fs_ebeln_lst>).
        APPEND VALUE #( ebeln = <fs_ebeln_lst>-ebeln
                        ekgrp = <fs_ebeln_lst>-ekgrp
                        lifnr = <fs_ebeln_lst>-lifnr
                        adrnr = <fs_ebeln_lst>-adrnr
                        lfa1_name1 = <fs_ebeln_lst>-lifnr_name1 ) TO mt_ebeln.
      ENDLOOP.
      SORT mt_ebeln BY ebeln.
      DELETE ADJACENT DUPLICATES FROM mt_ebeln COMPARING ebeln.
    ENDIF.


  ENDMETHOD.



  METHOD end_of_selection.
    CALL SCREEN 0100.
  ENDMETHOD.



  METHOD main_alv.


    DATA lt_exclude TYPE ui_functions.
    FIELD-SYMBOLS : <lo_grid> TYPE REF TO cl_gui_alv_grid,
                    <lo_prnt> TYPE REF TO cl_gui_container,
                    <lo_cont> TYPE REF TO cl_gui_custom_container.

    DATA(lv_str_alv) = 'S' && iv_scrn_alv.
    ASSIGN COMPONENT lv_str_alv OF STRUCTURE ms_alv TO FIELD-SYMBOL(<ls_alv>).
    IF <ls_alv> IS ASSIGNED.
      ASSIGN COMPONENT ms_alv_components-grid OF STRUCTURE <ls_alv> TO <lo_grid>.
      IF <lo_grid> IS ASSIGNED.
        CLEAR : <lo_grid>.

        "GRID INITIAL CONTROL if it is bound then flush( ).
*        IF <lo_grid> IS NOT BOUND.
        "GRID
        DATA(lv_str_gui) = 'S' && iv_scrn_cont.
        ASSIGN COMPONENT lv_str_gui OF STRUCTURE ms_alv TO FIELD-SYMBOL(<ls_gui>).
        IF <ls_gui> IS ASSIGNED.
          ASSIGN COMPONENT ms_alv_components-cont OF STRUCTURE <ls_gui> TO <lo_cont>.
          IF <lo_cont> IS ASSIGNED.
            <lo_grid> = build_grid( io_cont = <lo_cont> ).
          ENDIF.
        ENDIF.


        "FCAT
        ASSIGN COMPONENT ms_alv_components-fcat OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lt_fcat>).
        IF <lt_fcat> IS ASSIGNED.
          CLEAR : <lt_fcat>.
          <lt_fcat> = fill_main_fieldcat( iv_scrn = iv_scrn_alv ).
        ENDIF.

        "LAYOUT
        ASSIGN COMPONENT ms_alv_components-layo OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<ls_layo>).
        IF <ls_layo> IS ASSIGNED.
          CLEAR : <ls_layo>.
          <ls_layo> = build_layo( iv_scrn = iv_scrn_alv ).
        ENDIF.

        "VARIANT
        ASSIGN COMPONENT ms_alv_components-vari OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<ls_vari>).
        IF <ls_vari> IS ASSIGNED.
          CLEAR : <ls_vari>.
          <ls_vari> = build_vari( iv_scrn = iv_scrn_alv ).
        ENDIF.

        "SORT
        ASSIGN COMPONENT ms_alv_components-sort OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lt_sort>).
        IF <lt_sort> IS ASSIGNED.
          CLEAR : <lt_sort>.
          <lt_sort> = fill_main_sort( iv_scrn = iv_scrn_alv ).
        ENDIF.

        CHECK sy-subrc IS INITIAL.

        "ALV
        ASSIGN COMPONENT ms_alv_components-itab OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lt_itab>).
        IF <lt_itab> IS ASSIGNED.

          CASE iv_scrn_alv.
            WHEN ms_scr-s0101.
              ASSIGN mt_teklf_list TO <lt_itab>.
            WHEN ms_scr-s0201.
                case sy-ucomm.
                  when '&MLZM'.
                     ASSIGN mt_popup_alv_malz TO <lt_itab>.
                  when '&TLP'.
                     ASSIGN mt_popup_alv_banfn to <lt_itab>.
                  when '&TKLF'.
                     ASSIGN mt_popup_alv_ebeln to <lt_itab>.
                endcase.
              SET HANDLER on_hotspot FOR ms_alv-s0201-grid.
          ENDCASE.

          CALL METHOD <lo_grid>->set_table_for_first_display
            EXPORTING
              i_buffer_active               = abap_true
              is_layout                     = <ls_layo>
              i_save                        = 'A'
              it_toolbar_excluding          = lt_exclude
            CHANGING
              it_outtab                     = <lt_itab>
              it_fieldcatalog               = <lt_fcat>
              it_sort                       = <lt_sort>
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          <lo_grid>->set_ready_for_input( i_ready_for_input = 1 ).
          <lo_grid>->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
          <lo_grid>->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter  ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD build_grid.
    CREATE OBJECT ro_grid
      EXPORTING
        i_parent = io_cont.
  ENDMETHOD.
  METHOD build_layo.

    rs_layo-zebra      = abap_true.
    rs_layo-sel_mode   = 'A'.
    rs_layo-col_opt    = abap_true.
    rs_layo-CWIDTH_OPT = abap_true.

    CASE iv_scrn.
      WHEN ms_scr-s0201.
        rs_layo-no_rowmark = abap_true.
    ENDCASE.

  ENDMETHOD.
  METHOD build_vari.
    rs_vari = VALUE #( report = sy-repid username = sy-uname handle = iv_scrn ).
  ENDMETHOD.
  METHOD fill_main_fieldcat.
    DATA: lv_fname     TYPE lvc_fname,
          lv_offset    TYPE i,
          lv_structure TYPE dd02l-tabname.

    CASE iv_scrn.
      WHEN ms_scr-s0101.
        lv_structure = gt_structure1.
      WHEN ms_scr-s0201.
        lv_structure = gt_structure2.
    ENDCASE.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = lv_structure
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CASE iv_Scrn.
      WHEN ms_scr-s0101.
        LOOP AT rt_fcat REFERENCE INTO DATA(lr_fcat).
          CASE lr_fcat->fieldname.
             WHEN 'TKLF_TALEP_GRNO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALMA_TLPNO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STALM_TLP_KLMNO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_BLG_NO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'SATALM_BLGKLM_NO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'AEDAT'.
               lr_fcat->col_opt = abap_true.
             WHEN 'SATICI_HSP_NO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'SATICI_AD'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_MALZ'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_TXT'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_MIKTAR'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALMA_OLCUM'.
               lr_fcat->col_opt = abap_true.
             WHEN 'TEKLF_VRME_SR'.
               lr_fcat->col_opt = abap_true.
             WHEN 'KLM_TESLM_TARIH'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_COMP'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_COMP_AD'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_URTM'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_URTM_AD'.
               lr_fcat->col_opt = abap_true.
             WHEN 'STNALM_GR'.
               lr_fcat->col_opt = abap_true.
             WHEN 'IHTYC_NO'.
               lr_fcat->col_opt = abap_true.
             WHEN 'ODEME_KOSUL'.
               lr_fcat->col_opt = abap_true.
             ENDCASE.
          ENDLOOP.
      WHEN ms_scr-s0201.
        LOOP AT rt_fcat REFERENCE INTO lr_fcat.
          CASE lr_fcat->fieldname.
            WHEN 'ICON'.
              lr_fcat->hotspot = abap_true.
            WHEN 'CHECKBOX'.
              lr_fcat->checkbox = abap_true.
              lr_fcat->edit = abap_true.
            WHEN 'DOC_ID'.
              lr_fcat->no_out = abap_true.
            WHEN 'OBJ_TYPE'.
              lr_fcat->no_out = abap_true.
            WHEN 'DOC_SIZE'.
              lr_fcat->no_out = abap_true.
            WHEN 'CONTENT'.
              lr_fcat->no_out = abap_true.
            WHEN 'CONTENT_HEX'.
              lr_fcat->no_out = abap_true.
            WHEN 'ROW_ID'.
              lr_fcat->no_out = abap_true.
          ENDCASE.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.
  METHOD fill_main_sort.
  ENDMETHOD.
  METHOD build_cont.
    CREATE OBJECT ro_cont
      EXPORTING
        container_name = iv_alv_name.
  ENDMETHOD.
  METHOD pai.

    CLEAR mt_rows.

    CASE iv_scrn.
      WHEN ms_scr-s0200.
        CASE sy-ucomm.
          WHEN ms_ucomm-ok200.
            case gv_marker.
              when 'M'.
                maok( ).
              when 'T'.
                taok( ).
              when 'E'.
                teok( ).
            endcase.
            ext( iv_scrn = sy-dynnr ).
          WHEN OTHERS.
            ext( iv_scrn = sy-dynnr ).
        ENDCASE.
    ENDCASE.

    ms_alv-s0101-grid->get_selected_rows(
   IMPORTING
     et_row_no = mt_rows
 ).
    DATA(lv_selected_lines) = lines( mt_rows ).

    TRY.
        CASE sy-ucomm.
          WHEN '&MLZM'.
            display_mat_attch_list( ).

          WHEN '&TLP'.
            display_purch_req_list( ).
          WHEN '&TKLF'.
            display_purch_doc_list( ).
          WHEN '&FORM'.
            IF mt_rows IS INITIAL OR lv_selected_lines GT 1 .
              MESSAGE ID 'ZMM034' TYPE 'I' NUMBER '004' INTO DATA(lv_msg).
              RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lv_msg }|
                                                  iv_type    = lcx_exceptions=>mc_msgty_i ).
            ELSE.
              open_form( ).
            ENDIF.
          WHEN '&MAIL'.
            send_mail( ).
          WHEN '&LOG'.
            display_log( ).
        ENDCASE.

      CATCH lcx_exceptions INTO DATA(lo_exceptions).
        MESSAGE lo_exceptions->mv_message TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD pbo.

    DATA(lv_status) = |{ ms_gui-status }{ iv_scrn }|.
    SET PF-STATUS lv_status.

    DATA(lv_title) = |{ ms_gui-title }{ iv_scrn }|.
    SET TITLEBAR lv_title.

    CASE iv_scrn.
      WHEN ms_scr-s0100.
        IF ms_alv-s0100-cont IS NOT BOUND.

          " Build Container
          ms_alv-s0100-cont = build_cont( iv_alv_name = |{ 'F_' }{ iv_scrn }| ).
          main_alv( iv_scrn_cont = iv_scrn  iv_scrn_alv = ms_scr-s0101 ).
        ENDIF.
      WHEN ms_scr-s0200.

        IF ms_alv-s0200-cont IS NOT BOUND.
          " Build Container
          ms_alv-s0200-cont = build_cont( iv_alv_name = |{ 'F_' }{ iv_scrn }| ). "Taking container into a bound countrol is extremely crutual in terms of avoiding blank screens.
          ENDIF.
          main_alv( iv_scrn_cont = iv_scrn  iv_scrn_alv = ms_scr-s0201 ).
    ENDCASE.
  ENDMETHOD.
  METHOD ext.
    CASE sy-ucomm.
      WHEN ms_ucomm-cancel200.
        ms_alv-s0201-grid->free( ).
        cl_gui_cfw=>flush( ).
        FREE: ms_alv-s0201-grid.
        LEAVE TO SCREEN 0.
      WHEN ms_ucomm-back.
        LEAVE TO SCREEN 0.
      WHEN ms_ucomm-leave.
        LEAVE TO SCREEN 0.
      WHEN ms_ucomm-exit.
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
        ms_alv-s0201-grid->free( ).
        cl_gui_cfw=>flush( ).
        FREE: ms_alv-s0201-grid.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
  METHOD read_document_data.

    DATA:lv_docid       TYPE sofolenti1-doc_id,
         ls_doc_read    TYPE sofolenti1,
         lt_content     TYPE TABLE OF solisti1,
         lt_content_hex TYPE TABLE OF solix,
         lv_counter     TYPE int4.

    CLEAR: et_popup_alv[], mt_instid_b[].
" fetching the attachment id
    SELECT
      FROM
      srgbtbrel
      FIELDS
       instid_b
      WHERE instid_a IN @it_doc
        AND typeid_a = @iv_typeida
        AND catid_a  = 'BO'
      INTO TABLE @mt_instid_b.

    IF mt_instid_b[] IS NOT INITIAL.
      LOOP AT mt_instid_b ASSIGNING FIELD-SYMBOL(<fs_docid>).

        lv_docid = CONV #( <fs_docid>-instid_b ).
"       get all the details for an attachment
        CALL FUNCTION 'SO_DOCUMENT_READ_API1'
          EXPORTING
            document_id                = lv_docid
          IMPORTING
            document_data              = ls_doc_read
          TABLES
            object_content             = lt_content
            contents_hex               = lt_content_hex
          EXCEPTIONS
            document_id_not_exist      = 1
            operation_no_authorization = 2
            x_error                    = 3
            OTHERS                     = 4.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msgf).
          RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lv_msgf }|
                                              iv_type    = lcx_exceptions=>mc_msgty_e
                                              io_log     = mo_log ).
        ELSE.
          lv_counter += 1.
          APPEND VALUE #( icon       = icon_xls
                          baslik     = ls_doc_read-obj_descr
                          yaratan_ad = ls_doc_read-creat_name
                          yaratma_tarih = ls_doc_read-creat_date
                          yaratma_saat  = ls_doc_read-creat_time
                          doc_id      = ls_doc_read-doc_id
                          obj_type    = ls_doc_read-obj_type
                          doc_size    = ls_doc_read-doc_size
                          content     = lt_content
                          content_hex = lt_content_hex
                          checkbox = abap_true
                          row_id = lv_counter ) TO et_popup_alv.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD display_mat_attch_list.

    DATA: lt_rows   TYPE salv_t_row,
          lo_cols   TYPE REF TO cl_salv_columns,
          lo_column TYPE REF TO cl_salv_column_list.

gv_marker = 'M'.

    IF mt_teklf_list[] IS NOT INITIAL.
      DATA(lt_matnr) = VALUE rsdsselopt_t( FOR ls_list_ IN mt_teklf_list ( sign = 'I'
                                                                           option = 'EQ'
                                                                           low  = CONV sibfboriid( ls_list_-stnalm_malz ) ) ).
      SORT lt_matnr.
      DELETE ADJACENT DUPLICATES FROM lt_matnr.
    ENDIF.
    DATA(lv_typeid) = CONV sibftypeid( 'BUS1001006' ).
" read attached documents and get the details
    TRY.
    read_document_data(
      EXPORTING
        iv_typeida   =  lv_typeid
        it_doc       =  lt_matnr
       IMPORTING
         et_popup_alv = mt_popup_alv_malz
           ).
"  ALV creation for Malzeme Ekleme Listesi

    CALL SCREEN 0200 STARTING AT 10 10.

       SORT mt_all_rows BY button_type row_no.
        DELETE ADJACENT DUPLICATES FROM mt_all_rows COMPARING button_type row_no.
        LOOP AT mt_all_rows ASSIGNING FIELD-SYMBOL(<ls_all_rows>) WHERE button_type = 'MA'.
          APPEND INITIAL LINE TO lt_rows REFERENCE INTO DATA(lo_rows).
          lo_rows->* = <ls_all_rows>-row_no.
        ENDLOOP.
"        mo_alv_malz->get_selections( )->set_selected_rows( lt_rows ).

      CATCH cx_salv_msg INTO DATA(lo_salv_msg).
        RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lo_salv_msg->get_text( ) }|
                                            iv_type    = lcx_exceptions=>mc_msgty_e
                                            io_log     = mo_log ).
      CATCH cx_salv_not_found INTO DATA(lo_salv_not_found).
        RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lo_salv_not_found->get_text( ) }|
                                            iv_type    = lcx_exceptions=>mc_msgty_e
                                            io_log     = mo_log ).
    ENDTRY.

  ENDMETHOD.
  METHOD maok."on_data_changed.
    DELETE mt_all_rows WHERE button_type = 'MA'.
     LOOP AT mt_popup_alv_malz REFERENCE INTO DATA(lr_popup_alv_malz) WHERE checkbox = 'X'.
      DATA(ls_row_data) = mt_popup_alv_malz[ lr_popup_alv_malz->row_id ].
      APPEND VALUE #( button_type = 'MA'
                      row_no = lr_popup_alv_malz->row_id
                      doc_id = ls_row_data-doc_id
                      obj_desc = ls_row_data-baslik
                      obj_type = ls_row_data-obj_type
                      doc_size = ls_row_data-doc_size
                      content  = ls_row_data-content
                      content_hex = ls_row_data-content_hex ) TO mt_all_rows.
ENDLOOP.
  ENDMETHOD.
    METHOD taok.
    DELETE mt_all_rows WHERE button_type = 'TP'.
     LOOP AT mt_popup_alv_banfn REFERENCE INTO DATA(lr_popup_alv_banfn) WHERE checkbox = 'X'.
      DATA(ls_row_data) = mt_popup_alv_banfn[ lr_popup_alv_banfn->row_id ].
      APPEND VALUE #( button_type = 'TP'
                      row_no = lr_popup_alv_banfn->row_id
                      doc_id = ls_row_data-doc_id
                      obj_desc = ls_row_data-baslik
                      obj_type = ls_row_data-obj_type
                      doc_size = ls_row_data-doc_size
                      content  = ls_row_data-content
                      content_hex = ls_row_data-content_hex ) TO mt_all_rows.
ENDLOOP.
  ENDMETHOD.
   METHOD teok.
    DELETE mt_all_rows WHERE button_type = 'TF'.
     LOOP AT mt_popup_alv_ebeln REFERENCE INTO DATA(lr_popup_alv_ebeln) WHERE checkbox = 'X'.
      DATA(ls_row_data) = mt_popup_alv_ebeln[ lr_popup_alv_ebeln->row_id ].
      APPEND VALUE #( button_type = 'TF'
                      row_no = lr_popup_alv_ebeln->row_id
                      doc_id = ls_row_data-doc_id
                      obj_desc = ls_row_data-baslik
                      obj_type = ls_row_data-obj_type
                      doc_size = ls_row_data-doc_size
                      content  = ls_row_data-content
                      content_hex = ls_row_data-content_hex ) TO mt_all_rows.
ENDLOOP.
  ENDMETHOD.
  METHOD display_log.
    IF sy-batch = abap_false.
      mo_log->display_log( EXPORTING  iv_title       = TEXT-015
                                          iv_as_popup    = abap_true
                                          iv_use_grid    = abap_true
                               EXCEPTIONS internal_error = 1
                                          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID 'ZMM034' TYPE 'I' NUMBER '005' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD send_mail.

    DATA: lo_recipient TYPE REF TO if_recipient_bcs,
          lo_sender    TYPE REF TO if_sender_bcs,
          lo_bcs       TYPE REF TO cl_bcs.
    DATA:
      lv_mail_subject TYPE so_obj_des,
      lv_answer       TYPE c LENGTH 1,
      lt_pdf_hex      TYPE solix_tab,
      lt_receiver     TYPE znt_002_tt_001.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-008
        text_question         = TEXT-009
        text_button_1         = TEXT-010
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = TEXT-011
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4  INTO DATA(lv_msg).
      RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lv_msg }|
                                          iv_type    = lcx_exceptions=>mc_msgty_i
                                          io_log     = mo_log ).
    ENDIF.
    CASE lv_answer.
      WHEN '2'.
        RETURN.
    ENDCASE.

    IF mt_ebeln IS NOT INITIAL.
      SELECT
        FROM
          t024 AS t4
        INNER JOIN @mt_ebeln AS en ON t4~ekgrp = en~ekgrp
        FIELDS
          smtp_addr,
          en~ebeln
        WHERE smtp_addr <> ' '
        INTO TABLE @DATA(lt_sender_email).
      SELECT
        FROM adr6
        INNER JOIN @mt_ebeln AS en ON adr6~addrnumber = en~adrnr
        FIELDS
          adr6~smtp_addr,
          en~ebeln
        WHERE smtp_addr <> ' '
        INTO TABLE @DATA(lt_receiver_mail).
      LOOP AT mt_ebeln ASSIGNING FIELD-SYMBOL(<fs_ebeln>).
        TRY .
            " Set the subject of email
            lv_mail_subject = | { TEXT-013 } |.
            "Create mail body
            DATA(lt_body) = VALUE bcsy_text(
                              ( line =  | { TEXT-014 } { <fs_ebeln>-lfa1_name1 } | ) ( )
                              ( line = | { TEXT-005 } | ) ( )
                              ( line = | { TEXT-006 } | )
                              ( )
                              ( line = | { TEXT-007 } | )
                            ).
            DATA(lo_document) = NEW cl_document_bcs( ).
            lo_document = cl_document_bcs=>create_document(
                i_type       = 'RAW'
                i_subject    = lv_mail_subject
                i_language   = sy-langu
                i_importance = '5'
                i_text       = lt_body ).
            lo_bcs = cl_bcs=>create_persistent( ).

" Add receivers
            READ TABLE lt_receiver_mail ASSIGNING FIELD-SYMBOL(<fs_receiver>) WITH KEY ebeln = <fs_ebeln>-ebeln.
            IF sy-subrc EQ 0.
              APPEND INITIAL LINE TO lt_receiver REFERENCE INTO DATA(lr_rec).
              lr_rec->* = VALUE #( smtp_addr = <fs_receiver>-smtp_addr ).
            ENDIF.

            LOOP AT lt_receiver INTO DATA(ls_receivers) WHERE smtp_addr NE space.
              lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_receivers-smtp_addr ).
              CALL METHOD lo_bcs->add_recipient
                EXPORTING
                  i_recipient  = lo_recipient
                  i_express    = ls_receivers-express
                  i_copy       = ls_receivers-copy
                  i_blind_copy = ls_receivers-blind_copy.
            ENDLOOP.
"   Add sender email
            READ TABLE lt_sender_email ASSIGNING FIELD-SYMBOL(<fs_sender>) WITH KEY ebeln = <fs_ebeln>-ebeln.
            IF sy-subrc EQ 0.
              DATA(lv_sender) = <fs_sender>-smtp_addr.
            ENDIF.
            lo_sender = cl_cam_address_bcs=>create_internet_address( lv_sender ).
            CALL METHOD lo_bcs->set_sender
              EXPORTING
                i_sender = lo_sender.
" Add file attachments
            IF mt_all_rows[] IS NOT INITIAL.
              LOOP AT mt_all_rows ASSIGNING FIELD-SYMBOL(<fs_attachment>).
                lo_document->add_attachment(
                                i_attachment_type    = CONV soodk-objtp( <fs_attachment>-obj_type )
                                i_attachment_size    = CONV sood-objlen( <fs_attachment>-doc_size )
                                i_attachment_subject = CONV sood-objdes( <fs_attachment>-obj_desc )
                                i_att_content_hex    = <fs_attachment>-content_hex     ).
              ENDLOOP.
            ENDIF.

" Add smartform attachments
            CALL FUNCTION 'ZMM033_FM_TEKLIF_TALEP_PDF'
              EXPORTING
                iv_ebeln      = CONV ebeln( <fs_ebeln>-ebeln )
              IMPORTING
                es_formoutput = ms_formoutput.
            IF ms_formoutput IS NOT INITIAL.
              lo_document->xstring_to_solix(
                 EXPORTING
                   ip_xstring = ms_formoutput-pdf
                 RECEIVING
                   rt_solix   = lt_pdf_hex       ) .
              IF lt_pdf_hex IS NOT INITIAL.
                lo_document->add_attachment(
                                 i_attachment_type    = 'PDF'
                                 i_attachment_subject = | { TEXT-012 } |
                                 i_att_content_hex    = lt_pdf_hex ).
              ENDIF.
              MESSAGE i009 WITH <fs_ebeln>-ebeln INTO DATA(lv_msg_form).
              mo_log->add_text( iv_text = | { lv_msg_form } |
                                    iv_type = lcx_exceptions=>mc_msgty_i ).
            ELSE.
              MESSAGE i010 WITH <fs_ebeln>-ebeln INTO DATA(lv_msg_form_e).
              mo_log->add_text( iv_text = | { lv_msg_form_e } |
                                    iv_type = lcx_exceptions=>mc_msgty_i ).

            ENDIF.

            "Add document to send request
            lo_bcs->set_document( lo_document ).
            "Send Email
            lo_bcs->set_send_immediately( i_send_immediately = abap_true ).
            DATA(lv_sent_success) = lo_bcs->send( ).
            IF lv_sent_success EQ abap_true.
              MESSAGE i008 WITH <fs_ebeln>-ebeln INTO DATA(lv_msg_mail).
              mo_log->add_text( iv_text = | { lv_msg_mail } |
                                    iv_type = lcx_exceptions=>mc_msgty_i ).
              COMMIT WORK.
            ENDIF.
          CATCH cx_send_req_bcs INTO DATA(lo_req_bsc).
            mo_log->add_text( iv_text = | { lo_req_bsc->get_text( ) } |
                                  iv_type = lcx_exceptions=>mc_msgty_i ).
          CATCH cx_document_bcs INTO DATA(lo_doc_bcs).
            mo_log->add_text( iv_text = | { lo_doc_bcs->get_text( ) } |
                                  iv_type = lcx_exceptions=>mc_msgty_i ).
          CATCH cx_address_bcs  INTO DATA(lo_add_bcs).
            mo_log->add_text( iv_text = | { lo_add_bcs->get_text( ) } |
                                  iv_type = lcx_exceptions=>mc_msgty_i ).
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD open_form.
    DATA(ls_alv_data) = VALUE #( mt_teklf_list[ mt_rows[ 1 ]-row_id ] OPTIONAL ).
    CALL FUNCTION 'ZMM033_FM_TEKLIF_TALEP'
      EXPORTING
        iv_ebeln = ls_alv_data-stnalm_blg_no.
  ENDMETHOD.
 METHOD display_purch_req_list.
    DATA: lt_rows TYPE salv_t_row.

gv_marker = 'T'.

    IF mt_teklf_list[] IS NOT INITIAL.
      DATA(lt_banfn) = VALUE rsdsselopt_t( FOR ls_list_ IN mt_teklf_list ( sign = 'I'
                                                                           option = 'EQ'
                                                                           low  = CONV sibfboriid( ls_list_-stnalma_tlpno ) ) ).
      SORT lt_banfn.
      DELETE ADJACENT DUPLICATES FROM lt_banfn.
    ENDIF.
    DATA(lv_typeid_ban) = CONV sibftypeid( 'BUS2105' ).
" read attached documents and get the details
    TRY.
        read_document_data(
          EXPORTING
            iv_typeida   =  lv_typeid_ban
            it_doc       =  lt_banfn
           IMPORTING
             et_popup_alv = mt_popup_alv_banfn
               ).
"  ALV creation for Talep Ekleme Listesi

call screen 0200 STARTING AT 10 10.
"     Set header

        SORT mt_all_rows BY button_type row_no.
        DELETE ADJACENT DUPLICATES FROM mt_all_rows COMPARING button_type row_no.
        LOOP AT mt_all_rows ASSIGNING FIELD-SYMBOL(<ls_all_rows>) WHERE button_type = 'TP'.
          APPEND INITIAL LINE TO lt_rows REFERENCE INTO DATA(lo_rows).
          lo_rows->* = <ls_all_rows>-row_no.
        ENDLOOP.

      CATCH cx_salv_msg INTO DATA(lo_salv_msg).
        RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lo_salv_msg->get_text( ) }|
                                            iv_type    = lcx_exceptions=>mc_msgty_e
                                            io_log     = mo_log ).
      CATCH cx_salv_not_found INTO DATA(lo_salv_not_found).
        RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lo_salv_not_found->get_text( ) }|
                                            iv_type    = lcx_exceptions=>mc_msgty_e
                                            io_log     = mo_log ).
    ENDTRY.
  ENDMETHOD.
  METHOD display_purch_doc_list.
   DATA: lt_rows  TYPE salv_t_row.

gv_marker = 'E'.

    IF mt_ebeln[] IS NOT INITIAL.
      DATA(lt_ebeln_popup) = VALUE rsdsselopt_t( FOR ls_list_ IN mt_ebeln ( sign = 'I'
                                                                            option = 'EQ'
                                                                            low    = CONV sibfboriid( ls_list_-ebeln )  ) ).
    ENDIF.

    DATA(lv_typeid_eb) = CONV sibftypeid( 'BUS2010' ).
" read attached documents and get the details
    TRY.
        read_document_data(
          EXPORTING
            iv_typeida   =  lv_typeid_eb
            it_doc       =  lt_ebeln_popup
           IMPORTING
             et_popup_alv = mt_popup_alv_ebeln
               ).
"  ALV creation for Teklif Ekleme Listesi
call screen 0200 STARTING AT 10 10.
"     Set header

        SORT mt_all_rows BY button_type row_no.
        DELETE ADJACENT DUPLICATES FROM mt_all_rows COMPARING button_type row_no.
        LOOP AT mt_all_rows ASSIGNING FIELD-SYMBOL(<ls_all_rows>) WHERE button_type = 'TF'.
          APPEND INITIAL LINE TO lt_rows REFERENCE INTO DATA(lo_rows).
          lo_rows->* = <ls_all_rows>-row_no.
        ENDLOOP.

      CATCH cx_salv_msg INTO DATA(lo_salv_msg).
        RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lo_salv_msg->get_text( ) }|
                                            iv_type    = lcx_exceptions=>mc_msgty_i
                                            io_log     = mo_log ).
      CATCH cx_salv_not_found INTO DATA(lo_salv_not_found).
        RAISE EXCEPTION NEW lcx_exceptions( iv_message = |{ lo_salv_not_found->get_text( ) }|
                                            iv_type    = lcx_exceptions=>mc_msgty_i
                                            io_log     = mo_log ).
    ENDTRY.
  ENDMETHOD.
  METHOD on_hotspot.
     e_column_id = 'ICON'.
case gv_marker.
  when 'M'.
    DATA(ls_malz) = mt_popup_alv_malz[ es_row_no-row_id ].
    display_attachment( is_data = ls_malz ).
  when 'T'.
    ls_malz = mt_popup_alv_banfn[ es_row_no-row_id ].
    display_attachment( is_data = ls_malz ).
  when 'E'.
    ls_malz = mt_popup_alv_ebeln[ es_row_no-row_id ].
    display_attachment( is_data = ls_malz ).
endcase.
  ENDMETHOD.
ENDCLASS.
