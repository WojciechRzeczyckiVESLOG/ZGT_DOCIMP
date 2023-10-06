*&---------------------------------------------------------------------*
*&  Include           ZGT_DOCIMP_01_D01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FIELD_CONTROL_1100
*&---------------------------------------------------------------------*
*       Set field attributes in the maintenance dialog
*----------------------------------------------------------------------*
FORM field_control_1100 .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN '-'.
        IF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'
        OR zgt_s_docimp_01_upd-qaldoc EQ 'SUMA'.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-DELITM'.
        IF zgt_s_docimp_01_upd-delitm IS INITIAL.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-LICCU'.
        IF zgt_s_docimp_01_upd-qaldoc NE 'CONT'
        AND zgt_s_docimp_01_upd-qaldoc NE 'CUII'.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-LICUM'.
        IF zgt_s_docimp_01_upd-qaldoc NE 'CONT'
        AND zgt_s_docimp_01_upd-qaldoc NE 'CUII'.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPAD'.
        IF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPD2'.
        IF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'
        OR zgt_s_docimp_01_upd-qaldoc EQ 'SUMA'.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPDE'.
        IF zgt_s_docimp_01_upd-cuscs_papof IS INITIAL.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPDT'.
        IF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'
        OR zgt_s_docimp_01_upd-qaldoc EQ 'SUMA'.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPIT'.
        IF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPOF'.
        IF zgt_s_docimp_01_upd-cuscs_papof IS INITIAL.
          screen-active = 0.
          screen-output = 0.
        ELSEIF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'.
          screen-active = 0.
          screen-output = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPPC'.
        IF zgt_s_docimp_01_upd-qaldoc NE 'CONT'
        AND zgt_s_docimp_01_upd-qaldoc NE 'CUII'.
          screen-active = 0.
          screen-output = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPQU'.
        IF zgt_s_docimp_01_upd-qaldoc NE 'CONT'
        AND zgt_s_docimp_01_upd-qaldoc NE 'CUII'.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPST'.
        IF zgt_s_docimp_01_upd-cuscs_papst IS INITIAL.
          screen-active = 0.
          screen-output = 0.
        ELSEIF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'.
          screen-active = 0.
          screen-output = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPTY'.
        IF zgt_s_docimp_01_upd-qaldoc NE 'CUIP'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPVL'.
        IF zgt_s_docimp_01_upd-qaldoc NE 'CONT'
        AND zgt_s_docimp_01_upd-qaldoc NE 'CUII'.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PRTXT'.
        IF zgt_s_docimp_01_upd-delitm IS INITIAL.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PRVSY'.
        IF zgt_s_docimp_01_upd-delitm IS INITIAL.
          screen-active = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'ZGT_S_DOCIMP_01_UPD-QUALD'.
        IF zgt_s_docimp_01_upd-cuscs_quald IS INITIAL.
          screen-active = 0.
          screen-output = 0.
        ELSEIF zgt_s_docimp_01_upd-qaldoc EQ 'CUAI'.
          screen-active = 0.
          screen-output = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'ZGT_S_DOCIMP_01_UPD-QALDOC'.
        IF zgt_s_docimp_01_upd-qaldoc IS INITIAL.
          screen-active = 1.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
********************************************************************** E227
      WHEN 'ZGT_S_DOCIMP_01_UPD-DEACT'.
        IF zgt_s_docimp_01_upd-qaldoc EQ gc_cat_cuip.
          screen-active = 1.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'ZGT_S_DOCIMP_01_UPD-PAPNO'.
        IF zgt_s_docimp_01_upd-deact EQ abap_true.
          screen-required = 0.
        ENDIF.
        MODIFY SCREEN.
**********************************************************************
    ENDCASE.
  ENDLOOP.

ENDFORM.                               " field_control_1100

*&---------------------------------------------------------------------*
*&      Form  SCREEN_VALIDATE
*&---------------------------------------------------------------------*
*       Validate the screen contents (maintenance dialog)
*----------------------------------------------------------------------*
*      <--CV_RCODE  Return status code
*----------------------------------------------------------------------*
FORM screen_validate
  CHANGING cv_rcode LIKE sy-subrc.

  cv_rcode = 0.

* Document category always needed
  IF zgt_s_docimp_01_upd-qaldoc IS INITIAL.
    cv_rcode = 4.
    MESSAGE i153.
    RETURN.
  ENDIF.

* Document number always needed
  IF zgt_s_docimp_01_upd-papno IS INITIAL
    AND zgt_s_docimp_01_upd-deact NE abap_true. " E227 line added
    cv_rcode = 4.
    MESSAGE i062.
    RETURN.
  ENDIF.

* Currency needed, if value entered
  IF zgt_s_docimp_01_upd-papvl IS NOT INITIAL
    AND zgt_s_docimp_01_upd-liccu IS INITIAL.
    cv_rcode = 4.
    MESSAGE i046.
    RETURN.
  ENDIF.

* UoM needed, if quantity entered
  IF zgt_s_docimp_01_upd-papqu IS NOT INITIAL
    AND zgt_s_docimp_01_upd-licum IS INITIAL.
    cv_rcode = 4.
    MESSAGE i047.
    RETURN.
  ENDIF.

* "From" data needed, if "To" date entered
  IF zgt_s_docimp_01_upd-papd2 IS NOT INITIAL
    AND zgt_s_docimp_01_upd-papdt IS INITIAL.
    cv_rcode = 4.
    MESSAGE i884.
    RETURN.
  ENDIF.

* "From" data must not be earlier than "To" date
  IF zgt_s_docimp_01_upd-papdt IS NOT INITIAL
    AND zgt_s_docimp_01_upd-papd2 IS NOT INITIAL.
    IF zgt_s_docimp_01_upd-papdt GT zgt_s_docimp_01_upd-papd2.
      cv_rcode = 4.
      MESSAGE i521.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                               " screen_validate

*&---------------------------------------------------------------------*
*&      Form  ENTRY_STATUS_UPDATE
*&---------------------------------------------------------------------*
*       Update the maintenance screen Status icon
*----------------------------------------------------------------------*
*      -->IV_RCODE  Return code, from entry validation
*----------------------------------------------------------------------*
FORM entry_status_update
  USING iv_rcode LIKE sy-subrc.

* Show error, if validation failed
  IF iv_rcode IS NOT INITIAL.
    zgt_s_docimp_01_upd-errsta = gc_msg_status-error.
    macro_create_icon 'ICON_LED_RED' '' zgt_s_docimp_01_upd-status.
*------------------------------------------------------------------E227 Set status inactive
  ELSEIF iv_rcode IS INITIAL AND zgt_s_docimp_01_upd-deact EQ abap_true.
    zgt_s_docimp_01_upd-errsta = gc_inactive.
    macro_create_icon 'ICON_NO_STATUS' '' zgt_s_docimp_01_upd-status.
*------------------------------------------------------------------E227 Set status inactive
  ELSEIF zgt_s_docimp_01_upd-flvdo IS INITIAL.
    zgt_s_docimp_01_upd-errsta = gc_msg_status-warning.
    macro_create_icon 'ICON_LED_YELLOW' '' zgt_s_docimp_01_upd-status.
  ELSE.
    zgt_s_docimp_01_upd-errsta = gc_msg_status-success.
    macro_create_icon 'ICON_LED_GREEN' '' zgt_s_docimp_01_upd-status.
  ENDIF.

ENDFORM.                               " entry_status_update

*&---------------------------------------------------------------------*
*&      Form  ENTRY_SAVE
*&---------------------------------------------------------------------*
*       Save the maintained entry, and update the ALV grid
*----------------------------------------------------------------------*
*      <--CV_RCODE  Return status code
*----------------------------------------------------------------------*
FORM entry_save
  CHANGING cv_rcode LIKE sy-subrc.

  DATA: lt_docimp_st TYPE zgt_tt_docimp_st,
        ls_docimp_pk TYPE zgt_s_docimp_pk,
        ls_docimp    TYPE zgt_docimp,
        lv_otheruser TYPE xubname.

  DATA: lt_return TYPE STANDARD TABLE OF bapiret2. " E227

  FIELD-SYMBOLS:
       <ls_alv> TYPE zgt_s_docimp_01_alv.

* Save the entry to the database table
  IF zgt_s_docimp_01_upd-guid_docimp IS INITIAL.

* - New database entry
    CLEAR ls_docimp.
    SELECT SINGLE * FROM zgt_docimp                        ##WARN_OK
      INTO ls_docimp
      WHERE delnum = zgt_s_docimp_01_upd-delnum
        AND delitm = zgt_s_docimp_01_upd-delitm
        AND qaldoc = zgt_s_docimp_01_upd-qaldoc
        AND papty = zgt_s_docimp_01_upd-papty.
    IF sy-subrc EQ 0.

* --- Update record recently added by another user
* --- Lock the entry
      CALL FUNCTION 'ZGT_FM_DOCIMP_DB_SNG_ENQUEUE' ##FM_SUBRC_OK
        EXPORTING
          iv_guid_docimp  = ls_docimp-guid_docimp
        IMPORTING
          ev_rcode        = cv_rcode
          ev_locking_user = lv_otheruser
        EXCEPTIONS
          invalid_entry   = 1
          foreign_lock    = 2.

* --- Update the entry
      CASE cv_rcode.
        WHEN 0.
          MOVE-CORRESPONDING zgt_s_docimp_01_upd TO ls_docimp.
          INSERT ls_docimp INTO lt_docimp_st INDEX 1.
          CALL FUNCTION 'ZGT_FM_DOCIMP_DB_UPD'
            EXPORTING
              it_upd   = lt_docimp_st
              is_debug = gs_debug.

* --- Report if no records, or records locked
        WHEN 1.
          MESSAGE i007(/sapsll/leg_wda_pre).
          EXIT.
        WHEN 2.
          MESSAGE i547 WITH lv_otheruser.
          EXIT.
      ENDCASE.
    ELSE.

* --- New record - add GUID and LAUNR
      CLEAR ls_docimp.
      MOVE-CORRESPONDING zgt_s_docimp_01_upd TO ls_docimp.
      TRY.
          ls_docimp-guid_docimp = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.                               ##NO_HANDLER
      ENDTRY.
      ls_docimp-launr = 1.
      INSERT ls_docimp INTO lt_docimp_st INDEX 1.
      CALL FUNCTION 'ZGT_FM_DOCIMP_DB_INS'
        EXPORTING
          it_ins   = lt_docimp_st
          is_debug = gs_debug.
      IF sy-subrc EQ 0.
      ENDIF.
    ENDIF.

  ELSE.

* - Get the existing database entry
    CLEAR ls_docimp.
    ls_docimp_pk-mandt = sy-mandt.
    ls_docimp_pk-guid_docimp = zgt_s_docimp_01_upd-guid_docimp.
    CALL FUNCTION 'ZGT_FM_DOCIMP_DB_SGL_READ'
      EXPORTING
        is_pk        = ls_docimp_pk
        is_debug     = gs_debug
      IMPORTING
        es_result    = ls_docimp
      EXCEPTIONS
        no_data      = 1
        invalid_call = 4.
    IF sy-subrc EQ 0.

* --- Lock the entry
      CALL FUNCTION 'ZGT_FM_DOCIMP_DB_SNG_ENQUEUE' ##FM_SUBRC_OK
        EXPORTING
          iv_guid_docimp  = ls_docimp-guid_docimp
        IMPORTING
          ev_rcode        = cv_rcode
          ev_locking_user = lv_otheruser
        EXCEPTIONS
          invalid_entry   = 1
          foreign_lock    = 2.

* --- Update the entry
      CASE cv_rcode.
        WHEN 0.
          MOVE-CORRESPONDING zgt_s_docimp_01_upd TO ls_docimp.
          INSERT ls_docimp INTO lt_docimp_st INDEX 1.
          CALL FUNCTION 'ZGT_FM_DOCIMP_DB_UPD'
            EXPORTING
              it_upd   = lt_docimp_st
              is_debug = gs_debug.

* --- Report if no records, or records locked
        WHEN 1.
          MESSAGE i007(/sapsll/leg_wda_pre).
          EXIT.
        WHEN 2.
          MESSAGE i547 WITH lv_otheruser.
          EXIT.
      ENDCASE.
    ENDIF.
  ENDIF.

* Now update the Results List
  READ TABLE gt_docimp_01_ui ASSIGNING <ls_alv>
    INDEX zgt_s_docimp_01_upd-tabix.

* - Copy the updated entry to the Results List entry
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING zgt_s_docimp_01_upd TO <ls_alv>.

* - Update the Status icon
    CASE zgt_s_docimp_01_upd-errsta.
      WHEN gc_msg_status-error.
        macro_create_icon 'ICON_LED_RED' TEXT-i01 <ls_alv>-status.
      WHEN gc_msg_status-success.
        macro_create_icon 'ICON_LED_GREEN' TEXT-i03 <ls_alv>-status.
      WHEN gc_msg_status-warning.
        macro_create_icon 'ICON_LED_YELLOW' TEXT-i02 <ls_alv>-status.
*--> E227 - Set Status Inactive
      WHEN gc_inactive.
        macro_create_icon 'ICON_NO_STATUS' TEXT-i07 <ls_alv>-status.
*<-- E227 - Set Status Inactive
      WHEN OTHERS.
        macro_create_icon 'ICON_LED_RED' TEXT-i01 <ls_alv>-status.
    ENDCASE.

* - Add the GUID (key), in case new entry in database
    IF <ls_alv>-guid_docimp IS INITIAL.
      <ls_alv>-guid_docimp = ls_docimp-guid_docimp.
    ENDIF.
  ENDIF.

* ---> E227 Retail Update
  " Update Delivery
  LOOP AT lt_docimp_st INTO DATA(ls_docimp_st).
    DATA(lv_voora_compl) = abap_true.
    LOOP AT gt_docimp_01_ui INTO DATA(ls_docimp_01_ui) WHERE delnum EQ ls_docimp_st-delnum AND
                                                             delitm IS NOT INITIAL AND
                                                             papno EQ space AND
                                                             errsta NE gc_inactive.
      lv_voora_compl = abap_false.
      EXIT.
    ENDLOOP.

    IF lv_voora_compl EQ abap_true.
      READ TABLE gt_docimp_01_ui WITH KEY guid_docimp = ls_docimp_st-guid_docimp INTO DATA(ls_temp).
      IF sy-subrc NE 0.
        CLEAR ls_temp.
      ENDIF.

      IF ls_temp-ponum IS NOT INITIAL.
        CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
          DESTINATION gv_rfcdest
          EXPORTING
            iv_vbeln = ls_docimp_st-delnum
            iv_ebeln = ls_temp-ponum.
      ELSE.
        LOOP AT gt_repdata ASSIGNING FIELD-SYMBOL(<ls_rep>)
          WHERE delnum EQ ls_docimp_st-delnum.
          CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
            DESTINATION gv_rfcdest
            EXPORTING
              iv_vbeln = ls_docimp_st-delnum
              iv_ebeln = <ls_rep>-ponum.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEL_UPD' DESTINATION p_logsys
        EXPORTING
          iv_vbeln              = ls_docimp_st-delnum
          iv_vor_compl          = abap_true
        TABLES
          rt_return             = lt_return
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.

      APPEND LINES OF lt_return TO gt_return.
      CLEAR: lt_return.
    ENDIF.
  ENDLOOP.

  " Update PO
  IF zgt_s_docimp_01_upd-qaldoc EQ gc_cat_cuip AND zgt_s_docimp_01_upd-deact EQ abap_true.
    READ TABLE gt_docimp_01_ui WITH KEY delnum = ls_docimp_st-delnum INTO ls_temp.
    IF sy-subrc NE 0.
      CLEAR ls_temp.
    ENDIF.

    IF ls_temp-ponum IS NOT INITIAL.
      CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
        DESTINATION gv_rfcdest
        EXPORTING
          iv_vbeln = ls_docimp-delnum
          iv_ebeln = ls_temp-ponum.
    ELSE.
      LOOP AT gt_repdata ASSIGNING <ls_rep>
          WHERE delnum EQ ls_docimp_st-delnum.
        CALL FUNCTION 'ZGT_FM_VOORAFSETJE_DEQ_CHK'
          DESTINATION gv_rfcdest
          EXPORTING
            iv_vbeln = ls_docimp-delnum
            iv_ebeln = <ls_rep>-ponum.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'ZGT_FM_VOORAFSETJE_PO_UPD' DESTINATION p_logsys
      EXPORTING
        iv_vbeln              = ls_docimp-delnum
        iv_posnr              = ls_docimp-delitm
      TABLES
        et_return             = lt_return
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
  ENDIF.

  APPEND LINES OF lt_return TO gt_return.
  CLEAR: lt_return.

  IF gt_return IS NOT INITIAL.
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES
        it_return = gt_return.
  ENDIF.
  CLEAR: gt_return.
* <--- E227 Change PO Retail

ENDFORM.                               " entry_save
