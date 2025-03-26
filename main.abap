*&---------------------------------------------------------------------*
*& Report ZMM034_P_AF_GONDERME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm034_p_af_gonderme  MESSAGE-ID zmm034.

INCLUDE zmm034_i_af_gonderme_exc.
INCLUDE zmm034_i_af_gonderme_top.
INCLUDE zmm034_i_af_gonderme_ss.
INCLUDE zmm034_i_af_gonderme_cls.
INCLUDE zmm034_i_af_gonderme_mdl.

INITIALIZATION.
 mo_log = NEW #( ). "to initialize mo_log.
*lcl_main_controller=>set_repid( ).
*lcl_main_controller=>constructor( ).
* lcl_main_controller=>validate_ss( ).

START-OF-SELECTION.
lcl_main_controller=>get_data( ).

END-OF-SELECTION.
lcl_main_controller=>end_of_selection( ).
