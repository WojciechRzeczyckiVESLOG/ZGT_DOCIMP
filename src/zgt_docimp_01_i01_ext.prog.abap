*----------------------------------------------------------------------*
***INCLUDE ZGT_DOCIMP_01_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZGT_DOCIMP_01_I01  INPUT
*&---------------------------------------------------------------------*
*       PAI module for Screen 1100 - Voorafsetje maintenance dialog
*----------------------------------------------------------------------*
MODULE user_command_1100 INPUT.

  DATA: lv_rcode TYPE sysubrc.

* Handle the user commands
  CASE sy-ucomm.

* Check the entry
    WHEN '&CHK'.
      PERFORM screen_validate
        CHANGING lv_rcode.
      PERFORM entry_status_update
        USING lv_rcode.
      LEAVE SCREEN.

* Cancel the processing
    WHEN '&ESC'.
      SET SCREEN 0.
      MESSAGE s075(/sapsll/cus_cuwl).

* Save the entry in the database
    WHEN '&SAV'.
      PERFORM screen_validate
        CHANGING lv_rcode.
      PERFORM entry_status_update
        USING lv_rcode.
      IF lv_rcode EQ 0.
        PERFORM entry_save
          CHANGING lv_rcode.
        IF lv_rcode EQ 0.
          SET SCREEN 0.
          MESSAGE s355.
        ELSE.
          LEAVE SCREEN.
        ENDIF.
      ELSE.
        PERFORM entry_status_update
          USING lv_rcode.
        LEAVE SCREEN.
      ENDIF.
* Set Screen fields
    WHEN 'ACTCHG'.
      LOOP AT SCREEN.
        CASE screen-name.
         WHEN 'ZGT_S_DOCIMP_01_UPD-PAPNO'.
            IF zgt_s_docimp_01_upd-deact EQ abap_true.
              screen-required = 0.
              MODIFY SCREEN.
            ELSE.
              screen-required = 2.
              MODIFY SCREEN.
            ENDIF.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.

  ENDCASE.


ENDMODULE.
