*&---------------------------------------------------------------------*
*& Include          ZMM034_I_AF_GONDERME_EXC
*&---------------------------------------------------------------------*

CLASS lcx_exceptions DEFINITION INHERITING FROM cx_static_check CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      mc_msgty_e TYPE sy-msgty VALUE 'E',
      mc_msgty_s TYPE sy-msgty VALUE 'S',
      mc_msgty_w TYPE sy-msgty VALUE 'W',
      mc_msgty_i TYPE sy-msgty VALUE 'I'.
    DATA:
      mv_message TYPE string,
      mv_type    TYPE c LENGTH 1,
      mv_like    TYPE c LENGTH 1.
    METHODS:
      constructor IMPORTING iv_message TYPE string OPTIONAL
                            iv_type    TYPE char1  OPTIONAL
                            iv_like    TYPE char1  OPTIONAL
                            io_log     TYPE REF TO cl_ptu_message OPTIONAL,
      get_text REDEFINITION ##CALLED,
      get_type RETURNING VALUE(rv_type) TYPE sy-msgty ##CALLED,
      get_like RETURNING VALUE(rv_like) TYPE sy-msgty ##CALLED.

    CLASS-METHODS:
      exception_as_popup IMPORTING io_exception TYPE REF TO cx_root ##CALLED.
ENDCLASS.

CLASS lcx_exceptions IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->mv_message = iv_message.
    me->mv_type    = iv_type.
    me->mv_like    = iv_like.
    IF io_log IS SUPPLIED.
      io_log->add_text( iv_text = CONV #( iv_message )
                        iv_type = iv_type ).
    ENDIF.
  ENDMETHOD.

  METHOD exception_as_popup.
    DATA(lo_message_list) = cf_reca_message_list=>create( ).
    lo_message_list->add_from_exception( io_exception ).

    DATA: lo_current_exception TYPE REF TO cx_root.
    lo_current_exception = io_exception.
    WHILE lo_current_exception->previous IS BOUND.
      lo_message_list->add_from_exception( lo_current_exception->previous ).
      lo_current_exception = lo_current_exception->previous.
    ENDWHILE.

    CALL FUNCTION 'RECA_GUI_MSGLIST_POPUP'
      EXPORTING
        io_msglist = lo_message_list.
  ENDMETHOD.

  METHOD get_text.
    result = me->mv_message.
  ENDMETHOD.

  METHOD get_type.
    rv_type = me->mv_type.
  ENDMETHOD.

  METHOD get_like.
    rv_like = me->mv_like.
  ENDMETHOD.
ENDCLASS.
