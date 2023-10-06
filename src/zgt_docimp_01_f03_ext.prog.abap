*&---------------------------------------------------------------------*
*&  Include           ZGT_DOCIMP_01_F03
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*      FORM ALV_EVENT_PF_STATUS_SET
*---------------------------------------------------------------------*
*      Pass parameters into the ALV grid form
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set
  USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS '1000'.

ENDFORM.                    "alv_event_pf_status_set

*&---------------------------------------------------------------------*
*&      Form  ALV_EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*       Handle the user commands from the Results List
*----------------------------------------------------------------------*
*      -->IV_UCOMM    PAI Function Code
*      -->CS_SELFIELD Field information
*----------------------------------------------------------------------*
FORM alv_event_user_command
  USING iv_ucomm    LIKE sy-ucomm
        cs_selfield TYPE slis_selfield.

  DATA: lo_grid  TYPE REF TO cl_gui_alv_grid,
        ls_alv   TYPE zgt_s_docimp_01_alv,
        lv_count TYPE i.

* Allow data-set changes to be reflected in the grid
  cs_selfield-refresh = gc_true.
  cs_selfield-col_stable = gc_true.
  cs_selfield-row_stable = gc_true.

* Get the grid object, and hence the grid data
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  lo_grid->check_changed_data( ).

* Handle the user commands
  CASE iv_ucomm.
    WHEN '&MNT'.  " Maintain entry
* Check that an entry is selected
      LOOP AT gt_docimp_01_ui INTO ls_alv
        WHERE mksel = gc_true.
        ls_alv-tabix = sy-tabix.
        lv_count = lv_count + 1.
      ENDLOOP.
      IF sy-subrc EQ 0.
        IF lv_count GT 1.
          MESSAGE i699.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE i063.
        EXIT.
      ENDIF.
      PERFORM maintain_entry
      USING ls_alv.

    WHEN '&IC1'.  " double-click an entry
      READ TABLE gt_docimp_01_ui INTO ls_alv
        INDEX cs_selfield-tabindex.
      IF sy-subrc EQ 0.
        ls_alv-tabix = cs_selfield-tabindex.
        PERFORM maintain_entry
          USING ls_alv.
      ENDIF.

    WHEN OTHERS.
*      CONCATENATE 'Button: ' iv_ucomm INTO lv_text.
*      MESSAGE i000(/sapsll/core_basis) WITH lv_text.
  ENDCASE.

ENDFORM.                    "alv_event_user_command

*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_ENTRY
*&---------------------------------------------------------------------*
*       Maintain the selected entry
*----------------------------------------------------------------------*
*      -->IS_ALV  Selected entry from the Results List
*----------------------------------------------------------------------*
FORM maintain_entry
  USING ls_alv TYPE zgt_s_docimp_01_alv.

  DATA: lv_exit TYPE xfeld.
**********************************************************************
  CLEAR lv_exit.
* No maintenance allowed for error reports
  IF ls_alv-errsta = gc_msg_status-undefined.
    MESSAGE i036.
    EXIT.
  ENDIF.

* Move entry data to global update structure
  MOVE-CORRESPONDING gs_cuscs TO zgt_s_docimp_01_upd.
  MOVE-CORRESPONDING ls_alv TO zgt_s_docimp_01_upd.

* Re-create icons without text, to avoid display issues on screen
  CASE ls_alv-errsta.
    WHEN gc_msg_status-error.
      macro_create_icon 'ICON_LED_RED' '' zgt_s_docimp_01_upd-status.
    WHEN gc_msg_status-success.
      macro_create_icon 'ICON_LED_GREEN' '' zgt_s_docimp_01_upd-status.
    WHEN gc_msg_status-warning.
      macro_create_icon 'ICON_LED_YELLOW' '' zgt_s_docimp_01_upd-status.
*--> E227 Set Status inactive
    WHEN gc_inactive.
      macro_create_icon 'ICON_NO_STATUS' '' zgt_s_docimp_01_upd-status.
*<-- E227 Set Status inactive
    WHEN OTHERS.
      macro_create_icon 'ICON_LED_RED' '' zgt_s_docimp_01_upd-status.
  ENDCASE.

  IF zgt_s_docimp_01_upd-delnum IS NOT INITIAL.
    CALL FUNCTION 'ZGT_FM_VOORAFSETJE_ENQ_CHK'
      DESTINATION gv_rfcdest
      EXPORTING
        iv_vbeln     = zgt_s_docimp_01_upd-delnum
      EXCEPTIONS
        foreign_lock = 1.

    IF sy-subrc NE 0.
      MESSAGE i000(zgt_msg) WITH zgt_s_docimp_01_upd-delnum.
      CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
        DESTINATION gv_rfcdest
        EXPORTING
          iv_vbeln = zgt_s_docimp_01_upd-delnum
          iv_ebeln = ls_alv-ponum.
      EXIT.
    ENDIF.
  ENDIF.

  IF ls_alv-ponum IS NOT INITIAL.
    CALL FUNCTION 'ZGT_FM_VOORAFSETJE_ENQ_CHK_PO'
      DESTINATION gv_rfcdest
      EXPORTING
        iv_ebeln     = ls_alv-ponum
      EXCEPTIONS
        foreign_lock = 1.

    IF sy-subrc NE 0.
      MESSAGE i022(zgt_msg) WITH ls_alv-ponum.
      CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
        DESTINATION gv_rfcdest
        EXPORTING
          iv_vbeln = zgt_s_docimp_01_upd-delnum
          iv_ebeln = ls_alv-ponum.
      EXIT.
    ENDIF.
  ELSE.
    LOOP AT gt_repdata ASSIGNING FIELD-SYMBOL(<ls_rep>)
      WHERE delnum EQ zgt_s_docimp_01_upd-delnum.
      CALL FUNCTION 'ZGT_FM_VOORAFSETJE_ENQ_CHK_PO'
        DESTINATION gv_rfcdest
        EXPORTING
          iv_ebeln     = <ls_rep>-ponum
        EXCEPTIONS
          foreign_lock = 1.

      IF sy-subrc NE 0.
        MESSAGE i022(zgt_msg) WITH <ls_rep>-ponum.
        LOOP AT gt_repdata ASSIGNING FIELD-SYMBOL(<ls_rep_de>)
          WHERE delnum EQ zgt_s_docimp_01_upd-delnum..
          CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
            DESTINATION gv_rfcdest
            EXPORTING
              iv_vbeln = zgt_s_docimp_01_upd-delnum
              iv_ebeln = <ls_rep_de>-ponum.
        ENDLOOP.
        lv_exit = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lv_exit EQ abap_true.
    EXIT.
  ENDIF.

* Show the maintenance dialog
  CALL SCREEN 1100 STARTING AT 45 1.

  IF ls_alv-ponum IS NOT INITIAL.
    CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
      DESTINATION gv_rfcdest
      EXPORTING
        iv_vbeln = zgt_s_docimp_01_upd-delnum
        iv_ebeln = ls_alv-ponum.
  ELSE.
    LOOP AT gt_repdata ASSIGNING <ls_rep_de>
      WHERE delnum EQ zgt_s_docimp_01_upd-delnum.
      CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
        DESTINATION gv_rfcdest
        EXPORTING
          iv_vbeln = zgt_s_docimp_01_upd-delnum
          iv_ebeln = <ls_rep_de>-ponum.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " maintain_entry
