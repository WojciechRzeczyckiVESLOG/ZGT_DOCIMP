*----------------------------------------------------------------------*
***INCLUDE ZGT_DOCIMP_01_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       PBO routines for Screen 1100 - Voorafsetje maintenance dialog
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       Set the form Status and Title
*----------------------------------------------------------------------*
MODULE status_1100 OUTPUT.

  SET PF-STATUS '1100'.
  SET TITLEBAR '1100'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  DYNPRO_1100_FILL  OUTPUT
*&---------------------------------------------------------------------*
*       Populate the maintenance dialog
*----------------------------------------------------------------------*
MODULE dynpro_1100_fill OUTPUT.

  DATA: lv_subrc LIKE sy-subrc.

  PERFORM screen_validate
    CHANGING lv_subrc.

  PERFORM entry_status_update
    USING lv_subrc.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  FIELD_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       Set field attributes in the maintenance dialog
*----------------------------------------------------------------------*
MODULE field_control_1100 OUTPUT.

  PERFORM field_control_1100.

ENDMODULE.
