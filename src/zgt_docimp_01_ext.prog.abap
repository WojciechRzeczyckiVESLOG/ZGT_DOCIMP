*&---------------------------------------------------------------------*
*& Report  ZGT_DOCIMP_01
*&
*&---------------------------------------------------------------------*
*& Vooraftsetje worklist
*& Maintenance and monitoring of pre-determined documents for Import
*&---------------------------------------------------------------------*

INCLUDE ZGT_DOCIMP_01_TOP_EXT.
*INCLUDE zgt_docimp_01_top. " Global data
INCLUDE ZGT_DOCIMP_01_F01_EXT.
*INCLUDE zgt_docimp_01_f01. " Selection screen
INCLUDE ZGT_DOCIMP_01_D01_EXT.
*INCLUDE zgt_docimp_01_d01. " Dynpro routines
INCLUDE ZGT_DOCIMP_01_F02_EXT.
*INCLUDE zgt_docimp_01_f02. " Form routines
INCLUDE ZGT_DOCIMP_01_F03_EXT.
*INCLUDE zgt_docimp_01_f03. " Entry processing
INCLUDE ZGT_DOCIMP_01_O01_EXT.
*INCLUDE zgt_docimp_01_o01. " PBO routines
INCLUDE ZGT_DOCIMP_01_I01_EXT.
*INCLUDE zgt_docimp_01_i01. " PAI routines

DATA: lt_delitm  TYPE zgt_tt_delitm,
      lr_bukrs   TYPE fkkcorr_rt_bukrs,
      lr_werks   TYPE /sapsll/werks_d_r_t,
      lr_mtrbc   TYPE /sapsll/mtrbc_r_t,
      lv_rfcdest TYPE rfcdest.

START-OF-SELECTION.

* Optional break-point

* Prepare for RFC
  PERFORM rfc_dest_prepare
    USING    p_logsys
             gc_funcname
    CHANGING lv_rfcdest.
  CHECK lv_rfcdest IS NOT INITIAL.
  gv_rfcdest = lv_rfcdest.

* Map GTS to Feeder System objects
  PERFORM gts_to_fs_map
    USING    p_logsys
    CHANGING lr_bukrs
             lr_werks
             lr_mtrbc.

* Get IBD items from feeder system
  CALL FUNCTION gc_funcname
    DESTINATION lv_rfcdest
    TABLES
      t_crit_bukrs          = lr_bukrs
      t_crit_werks          = lr_werks
      t_crit_ponum          = s_ponum
      t_crit_delnum         = s_delnum
      t_crit_erdat          = s_erdat[]
      t_crit_mtrbc          = lr_mtrbc[]
      t_delitm              = lt_delitm
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 4.
  IF sy-subrc NE 0.
    MESSAGE i223(/sapsll/core_product) WITH p_logsys.
  ELSEIF lt_delitm IS INITIAL.
    MESSAGE i000. "No entries found
  ELSE.

* Get the related configuration data
    PERFORM tariff_config_data_get.

* Enhance the Delivery items
    PERFORM items_enhance1
      USING    lt_delitm
      CHANGING gt_repdata.
    FREE lt_delitm.

* Determine the required documents
    PERFORM docs_determine
      USING    gt_repdata
      CHANGING gt_docimp_01_ui.

* Discard the global table data
*    FREE gt_repdata.
    FREE gt_cucpat_tran.
    FREE gt_cucpat_pref.
    FREE gt_lc_cusb2.
    FREE gt_pedigree.
    FREE gt_ctsmxx.

    IF lines( gt_docimp_01_ui ) = 0.
      MESSAGE i000. "No entries found
    ELSE.
* Complete the ALV structure fields
      PERFORM items_enhance2
        CHANGING gt_docimp_01_ui.

* Display the results
      PERFORM data_display
        USING gs_variant
        CHANGING gt_docimp_01_ui.

    ENDIF.
  ENDIF.
