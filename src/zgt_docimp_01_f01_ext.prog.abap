*&---------------------------------------------------------------------*
*&  Include           ZGT_DOCIMP_01_F01
*&---------------------------------------------------------------------*
*&  Selection Screen for Report ZGT_DOCIMP_01
*&---------------------------------------------------------------------*

* Organisation data
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_lgreg LIKE zgt_s_sel-lgreg NO-DISPLAY,
                p_cuscs LIKE zgt_s_sel-cuscs_motra NO-DISPLAY,
                p_ftorg LIKE zgt_s_sel-ftorg OBLIGATORY
                    MEMORY ID /sapsll/ftorg.
SELECT-OPTIONS: s_ftvbs  FOR zgt_s_sel-ftvbs
                    MEMORY ID /sapsll/ftvbs.
SELECTION-SCREEN END OF BLOCK b1.

* FS Logical System
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
PARAMETERS:     p_logsys LIKE zgt_s_sel-logsys OBLIGATORY
                    MEMORY ID /sapsll/orglogsys.
SELECTION-SCREEN END OF BLOCK b2.

* FS Purchase Order
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-t03.
SELECT-OPTIONS: s_ponum  FOR zgt_s_sel-ponum.
SELECTION-SCREEN END OF BLOCK b3.

* FS Inbound Delivery
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-t04.
SELECT-OPTIONS: s_delnum FOR zgt_s_sel-delnum,
                s_erdat  FOR zgt_s_sel-erdat.
SELECTION-SCREEN END OF BLOCK b4.

* Transport data
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-t05.
SELECT-OPTIONS: s_motra  FOR zgt_s_sel-motra NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b5.

* Measures
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-t06.
SELECT-OPTIONS: s_mscid  FOR zgt_s_sel-mscid.
SELECTION-SCREEN END OF BLOCK b6.

* Scope
SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE text-t07.
PARAMETERS:     p_fldec  TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b7.

* Display variant
SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME TITLE text-t08.
PARAMETERS:     p_dvari  TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b8.

INITIALIZATION.

* Get the Default Variant
  PERFORM alv_variant_default_get
    USING    sy-repid
             gc_variant_handle-vas
    CHANGING p_dvari
             gs_variant.

AT SELECTION-SCREEN ON p_dvari.
  IF NOT p_dvari IS INITIAL.
    PERFORM alv_variant_existence_check
      USING sy-repid
            gc_variant_handle-vas
            p_dvari.
  ENDIF.

* Show available variants
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dvari.
  PERFORM alv_variant_f4
    CHANGING p_dvari
             gs_variant.

* Determine the Legal Regulation
AT SELECTION-SCREEN ON p_ftorg.
  PERFORM lgreg_from_fto_get
    USING    p_ftorg
    CHANGING gv_lgreg.

* Determine the Code List for MoT
  PERFORM cuscs_from_lgreg_get
    USING    gv_lgreg
             gc_cuscs_object-transp_mode_border
    CHANGING p_cuscs.
  zgt_s_sel-cuscs_motra = p_cuscs.
