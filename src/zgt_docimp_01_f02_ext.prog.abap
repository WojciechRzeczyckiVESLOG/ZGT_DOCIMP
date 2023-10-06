*&---------------------------------------------------------------------*
*&  Include           ZGT_DOCIMP_01_F02
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_DEFAULT_GET
*&---------------------------------------------------------------------*
*       Get default variant
*----------------------------------------------------------------------*
*      -->IV_REPID   Report name
*      -->IV_HANDLE  Unique identifier
*      <--CV_DVARI   Variant name
*      <--CS_VARIANT Variant structure
*----------------------------------------------------------------------*
FORM alv_variant_default_get
  USING    iv_repid   TYPE sy-repid
           iv_handle  TYPE slis_handl
  CHANGING cv_dvari   TYPE slis_vari
           cs_variant TYPE disvariant.

  CLEAR cs_variant.
  cs_variant-report = iv_repid.
  cs_variant-handle = iv_handle.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = cs_variant
    EXCEPTIONS
      not_found  = 4.
  IF sy-subrc = 0.
    cv_dvari = cs_variant-variant.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_DEFAULT_GET

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_EXISTENCE_CHECK
*&---------------------------------------------------------------------*
*       Checks that the entered variant exists
*----------------------------------------------------------------------*
*      -->IV_REPID  Report name
*      -->IV_HANDLE Unique identifier
*      -->IV_DVARI  Variant name
*----------------------------------------------------------------------*
FORM alv_variant_existence_check
  USING  iv_repid  TYPE sy-repid
         iv_handle TYPE slis_handl
         iv_dvari  TYPE slis_vari.

  DATA: ls_variant TYPE disvariant.

  ls_variant-report  = iv_repid.
  ls_variant-handle  = iv_handle.
  ls_variant-variant = iv_dvari.

  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = ls_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc = 2.
    MESSAGE e022(/sapsll/core) WITH iv_dvari.
  ENDIF.

ENDFORM.                    " alv_variant_existence_check

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*       Show available variants
*----------------------------------------------------------------------*
*      <->CV_DVARI   Variant name
*      <->CS_VARIANT Variant structure
*----------------------------------------------------------------------*
FORM alv_variant_f4
  CHANGING cv_dvari   TYPE slis_vari
           cs_variant TYPE disvariant.

  DATA lv_exit TYPE c.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = cs_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = lv_exit
      es_variant = cs_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.
    IF lv_exit IS INITIAL.
      cv_dvari = cs_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " alv_variant_f4

*&---------------------------------------------------------------------*
*&      Form  RFC_DEST_PREPARE
*&---------------------------------------------------------------------*
*       Determine & check the RFC destination
*----------------------------------------------------------------------*
*      -->IV_LOGSYS    Logical System name
*      -->IV_FUNCNAME  Function Module to be called
*      <--CV_RFCDEST   RFC destination
*----------------------------------------------------------------------*
FORM rfc_dest_prepare
  USING    iv_logsys   TYPE logsys
           iv_funcname TYPE rs38l_fnam
  CHANGING cv_rfcdest  TYPE rfcdest.

  CALL FUNCTION 'LOGSYS_GET_BOR_DESTINATIONS'
    EXPORTING
      logical_system                = iv_logsys
    IMPORTING
      rfc_destination               = cv_rfcdest
    EXCEPTIONS
      no_rfc_destination_maintained = 2
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE i466(/sapsll/core_leg) WITH iv_logsys.
  ENDIF.

  CALL FUNCTION 'FUNCTION_EXISTS'
    DESTINATION cv_rfcdest
    EXPORTING
      funcname              = iv_funcname
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      function_not_exist    = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    MESSAGE i223(/sapsll/core_product) WITH iv_logsys.
  ENDIF.

ENDFORM.                    " rfc_dest_prepare

*&---------------------------------------------------------------------*
*&      Form  LGREG_FROM_FTO_GET
*&---------------------------------------------------------------------*
*       Determine the associated Legal Regulation
*----------------------------------------------------------------------*
*      -->IV_FTORG  Foreign Trade Organization
*      <--CV_LGREG  Legal Regulation
*----------------------------------------------------------------------*
FORM lgreg_from_fto_get
  USING    iv_ftorg TYPE /sapsll/ftorg
  CHANGING cv_lgreg TYPE /sapsll/lgreg.

  DATA: lt_t606g TYPE /sapsll/t606g_t,
        ls_crit  TYPE /sapsll/t606g_crit_s,
        ls_addr  TYPE addr1_val,
        ls_t606g TYPE /sapsll/t606g.

* Get the FTO country
  CALL FUNCTION '/SAPSLL/PARTNER_ADDRESS_GET'
    EXPORTING
      iv_parno     = iv_ftorg
    IMPORTING
      es_addr1_val = ls_addr.

* Find the Legal Regulation for Customs Management
  macro_append_range sy-mandt ls_crit-mandt.
  macro_append_range gc_regty-customs ls_crit-regty.
  macro_append_range ls_addr-country ls_crit-gglnd.
  CALL FUNCTION '/SAPSLL/T606G_DB_RNG_READ'
    EXPORTING
      is_crit        = ls_crit
      is_debug       = gs_debug
    IMPORTING
      et_result      = lt_t606g
    EXCEPTIONS
      no_data        = 1
      open_sql_error = 4.
  IF sy-subrc EQ 0.
    READ TABLE lt_t606g INTO ls_t606g INDEX 1.
    cv_lgreg = ls_t606g-lgreg.
  ENDIF.

ENDFORM.                    " lgreg_from_fto_get

*&---------------------------------------------------------------------*
*&      Form  CUSCS_FROM_LGREG_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LGREG  text
*      <--CV_CUSCS  text
*----------------------------------------------------------------------*
FORM cuscs_from_lgreg_get
  USING    iv_lgreg TYPE /sapsll/lgreg
           iv_cuobj TYPE /sapsll/cuobj
  CHANGING cv_cuscs TYPE /sapsll/cuscs.

  DATA: ls_tculro_pk TYPE /sapsll/tculro_pk_s,
        ls_tculro    TYPE /sapsll/tculro.

  ls_tculro_pk-mandt = sy-mandt.
  ls_tculro_pk-lgreg = iv_lgreg.
  ls_tculro_pk-cuobj = iv_cuobj.
  CALL FUNCTION '/SAPSLL/TCULRO_DB_SGL_READ'
    EXPORTING
      is_pk        = ls_tculro_pk
      is_debug     = gs_debug
    IMPORTING
      es_result    = ls_tculro
    EXCEPTIONS
      no_data      = 1
      invalid_call = 4.
  IF sy-subrc EQ 0.
    cv_cuscs = ls_tculro-cuscs.
  ENDIF.

ENDFORM.                    " cuscs_from_lgreg_get

*&---------------------------------------------------------------------*
*&      Form  GTS_TO_FS_MAP
*&---------------------------------------------------------------------*
*       Determines Feeder System object values
*----------------------------------------------------------------------*
*      -->IV_LOGSYS Logical System name
*      <--CR_BUKRS  Company Code
*      <--CR_WERKS  Plants
*      <--CR_MTRBC  Modes of Transport
*----------------------------------------------------------------------*
FORM gts_to_fs_map
  USING    iv_logsys TYPE logsys
  CHANGING cr_bukrs  TYPE fkkcorr_rt_bukrs
           cr_werks  TYPE /sapsll/werks_d_r_t
           cr_mtrbc  TYPE /sapsll/mtrbc_r_t.

  DATA: lt_tcoovs    TYPE /sapsll/tcoovs_t,
        lt_tcoogs    TYPE /sapsll/tcoogs_t,
        lt_tlemvs    TYPE /sapsll/tlemvs_t,
        lt_tlemgs    TYPE /sapsll/tlemgs_t,
        lr_bukrs     TYPE fkkcorrr_bukrs,
        lr_werks     TYPE /sapsll/werks_d_r_s,
        lr_mtrbc     TYPE /sapsll/mtrbc_r_s,
        ls_tcogva_pk TYPE /sapsll/tcogva_pk_s,
        ls_crit_orgv TYPE /sapsll/tcoovs_crit_s,
        ls_crit_orgg TYPE /sapsll/tcoogs_crit_s,
        ls_crit_motv TYPE /sapsll/tlemvs_crit_s,
        ls_crit_motg TYPE /sapsll/tlemgs_crit_s,
        ls_tcogva    TYPE /sapsll/tcogva,
        ls_tcoovs    TYPE /sapsll/tcoovs,
        ls_tcoogs    TYPE /sapsll/tcoogs,
        ls_tlemvs    TYPE /sapsll/tlemvs,
        ls_tlemgs    TYPE /sapsll/tlemgs.

* Determine the Logical System Group
  ls_tcogva_pk-logsys = iv_logsys.
  CALL FUNCTION '/SAPSLL/TCOGVA_DB_SGL_READ'
    EXPORTING
      is_pk        = ls_tcogva_pk
      is_debug     = gs_debug
    IMPORTING
      es_result    = ls_tcogva
    EXCEPTIONS
      no_data      = 1
      invalid_call = 4.
  IF sy-subrc EQ 0.
    gv_grvsy = ls_tcogva-grvsy.
  ENDIF.

* Mapping of Foreign Trade Organisation
  IF p_ftorg IS NOT INITIAL.

* Mapping from the Logical System
    CLEAR ls_crit_orgv.
    macro_append_range sy-mandt ls_crit_orgv-mandt.
    macro_append_range iv_logsys ls_crit_orgv-logsys.
    macro_append_range gc_orgty_vs-company_code ls_crit_orgv-orgty.
    macro_append_range p_ftorg ls_crit_orgv-parno.
    CALL FUNCTION '/SAPSLL/TCOOVS_DB_RNG_READ'
      EXPORTING
        is_crit        = ls_crit_orgv
        is_debug       = gs_debug
      IMPORTING
        et_result      = lt_tcoovs
      EXCEPTIONS
        no_data        = 1
        open_sql_error = 4.

    IF sy-subrc EQ 0.
* Fill the range table
      READ TABLE lt_tcoovs INTO ls_tcoovs INDEX 1.
      lr_bukrs-sign   = 'I'.
      lr_bukrs-option = 'EQ'.
      lr_bukrs-low    = ls_tcoovs-orgvs.
      APPEND lr_bukrs TO cr_bukrs.

    ELSEIF NOT gv_grvsy IS INITIAL.
* Mapping from the Logical System Group
      CLEAR ls_crit_orgg.
      macro_append_range sy-mandt ls_crit_orgg-mandt.
      macro_append_range gv_grvsy ls_crit_orgg-grvsy.
      macro_append_range gc_orgty_vs-company_code ls_crit_orgg-orgty.
      macro_append_range p_ftorg ls_crit_orgg-parno.
      CALL FUNCTION '/SAPSLL/TCOOGS_DB_RNG_READ'
        EXPORTING
          is_crit        = ls_crit_orgg
          is_debug       = gs_debug
        IMPORTING
          et_result      = lt_tcoogs
        EXCEPTIONS
          no_data        = 1
          open_sql_error = 4.

      IF sy-subrc EQ 0.
* ----- Fill the range table
        READ TABLE lt_tcoogs INTO ls_tcoogs INDEX 1.
        lr_bukrs-sign   = 'I'.
        lr_bukrs-option = 'EQ'.
        lr_bukrs-low    = ls_tcoogs-orgvs.
        APPEND lr_bukrs TO cr_bukrs.
      ENDIF.
    ENDIF.
  ENDIF.

* Mapping of Legal Unit
  IF s_ftvbs IS NOT INITIAL.

* - Mapping from the Logical System
    CLEAR ls_crit_orgv.
    macro_append_range sy-mandt ls_crit_orgv-mandt.
    macro_append_range iv_logsys ls_crit_orgv-logsys.
    macro_append_range gc_orgty_vs-plant ls_crit_orgv-orgty.
    ls_crit_motv-motra = s_ftvbs[].
    CALL FUNCTION '/SAPSLL/TCOOVS_DB_RNG_READ'
      EXPORTING
        is_crit        = ls_crit_orgv
        is_debug       = gs_debug
      IMPORTING
        et_result      = lt_tcoovs
      EXCEPTIONS
        no_data        = 1
        open_sql_error = 4.

    IF sy-subrc EQ 0.
* --- Fill the range table
      LOOP AT lt_tcoovs INTO ls_tcoovs.
        lr_werks-sign   = 'I'.
        lr_werks-option = 'EQ'.
        lr_werks-low    = ls_tcoovs-orgvs.
        APPEND lr_werks TO cr_werks.
      ENDLOOP.

    ELSEIF NOT gv_grvsy IS INITIAL.
* - Mapping from the Logical System Group
      CLEAR ls_crit_orgg.
      macro_append_range sy-mandt ls_crit_orgg-mandt.
      macro_append_range gv_grvsy ls_crit_orgg-grvsy.
      macro_append_range gc_orgty_vs-plant ls_crit_orgg-orgty.
      ls_crit_motv-motra = s_ftvbs[].
      CALL FUNCTION '/SAPSLL/TCOOGS_DB_RNG_READ'
        EXPORTING
          is_crit        = ls_crit_orgg
          is_debug       = gs_debug
        IMPORTING
          et_result      = lt_tcoogs
        EXCEPTIONS
          no_data        = 1
          open_sql_error = 4.

      IF sy-subrc EQ 0.
* ----- Fill the range table
        LOOP AT lt_tcoogs INTO ls_tcoogs.
          lr_werks-sign   = 'I'.
          lr_werks-option = 'EQ'.
          lr_werks-low    = ls_tcoogs-orgvs.
          APPEND lr_werks TO cr_werks.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

* Mapping of Mode of Tranpsort
  IF s_motra IS NOT INITIAL.

* - Determine Legal Regulation & Code List for MoT
    IF gv_lgreg IS INITIAL.
      PERFORM lgreg_from_fto_get
        USING    p_ftorg
        CHANGING gv_lgreg.
    ENDIF.

    PERFORM cuscs_from_lgreg_get
      USING    gv_lgreg
               gc_cuscs_object-transp_mode_border
      CHANGING p_cuscs.

* - Mapping from the Logical System
    CLEAR ls_crit_motv.
    macro_append_range sy-mandt ls_crit_motv-mandt.
    macro_append_range p_logsys ls_crit_motv-logsys.
    macro_append_range p_cuscs ls_crit_motv-cuscs.
    ls_crit_motv-motra = s_motra[].
    CALL FUNCTION '/SAPSLL/TLEMVS_DB_RNG_READ'
      EXPORTING
        is_crit        = ls_crit_motv
        is_debug       = gs_debug
      IMPORTING
        et_result      = lt_tlemvs
      EXCEPTIONS
        no_data        = 1
        open_sql_error = 4.

    IF sy-subrc EQ 0.
* --- Fill the range table
      LOOP AT lt_tlemvs INTO ls_tlemvs.
        lr_mtrbc-sign   = 'I'.
        lr_mtrbc-option = 'EQ'.
        lr_mtrbc-low    = ls_tlemvs-tmcvs.
        APPEND lr_mtrbc TO cr_mtrbc.
      ENDLOOP.

    ELSEIF NOT gv_grvsy IS INITIAL.
* - Mapping from the Logical System Group
      CLEAR ls_crit_motg.
      macro_append_range sy-mandt ls_crit_motg-mandt.
      macro_append_range gv_grvsy ls_crit_motg-grvsy.
      macro_append_range p_cuscs ls_crit_motg-cuscs.
      ls_crit_motg-motra = s_motra[].
      CALL FUNCTION '/SAPSLL/TLEMGS_DB_RNG_READ'
        EXPORTING
          is_crit        = ls_crit_motg
          is_debug       = gs_debug
        IMPORTING
          et_result      = lt_tlemgs
        EXCEPTIONS
          no_data        = 1
          open_sql_error = 4.

      IF sy-subrc EQ 0.
* ----- Fill the range table
        LOOP AT lt_tlemgs INTO ls_tlemgs.
          lr_mtrbc-sign   = 'I'.
          lr_mtrbc-option = 'EQ'.
          lr_mtrbc-low    = ls_tlemgs-tmcvs.
          APPEND lr_mtrbc TO cr_mtrbc.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " gts_to_fs_map

*&---------------------------------------------------------------------*
*&      Form  ICON_CREATE
*&---------------------------------------------------------------------*
*       Create an icon
*----------------------------------------------------------------------*
*      -->IV_NAME  Name of the Icon
*      -->IV_NAME  Text for the Icon
*      <--CV_ICON  The Icon object
*----------------------------------------------------------------------*
FORM icon_create  USING    iv_name TYPE iconname
                           iv_info
                  CHANGING cv_icon.

  CALL FUNCTION 'ICON_CREATE' ##FM_SUBRC_OK
    EXPORTING
      name                  = iv_name
      text                  = ' '
      info                  = iv_info
      add_stdinf            = ' '
    IMPORTING
      result                = cv_icon
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

ENDFORM.                    " icon_create

*&---------------------------------------------------------------------*
*&      Form  TARIFF_CONFIG_DATA_GET
*&---------------------------------------------------------------------*
*       Get Document and Tariff configuration data
*----------------------------------------------------------------------*
FORM tariff_config_data_get .

  DATA: lt_tlegsu      TYPE /sapsll/tlegsu_t,
        ls_tcots_pk    TYPE /sapsll/tcots_pk_s,
        ls_tcots       TYPE /sapsll/tcots,
        ls_crit_tlegsu TYPE /sapsll/tlegsu_crit_s,
        ls_crit_cucpat TYPE /sapsll/cucpat_crit_s.

  FIELD-SYMBOLS:
    <ls_tlegsu> TYPE /sapsll/tlegsu.

* Get the configured Code List keys
  CLEAR gs_cuscs.
  PERFORM cuscs_from_lgreg_get
  USING    gv_lgreg
           gc_cuscs_object-paper_type
  CHANGING gs_cuscs-cuscs_papty.
  gv_cuscs_papty = gs_cuscs-cuscs_papty.

  PERFORM cuscs_from_lgreg_get
  USING    gv_lgreg
           gc_cuscs_object-paper_status
  CHANGING gs_cuscs-cuscs_papst.

  PERFORM cuscs_from_lgreg_get
  USING    gv_lgreg
           gc_cuscs_object-paper_authority
  CHANGING gs_cuscs-cuscs_papof.

  PERFORM cuscs_from_lgreg_get
  USING    gv_lgreg
           gc_cuscs_object-paper_qualificator
  CHANGING gs_cuscs-cuscs_quald.

* Get the current Tariff Numbering Scheme
  macro_append_range sy-mandt ls_crit_tlegsu-mandt.
  macro_append_range gc_services-customs ls_crit_tlegsu-srvll.
  macro_append_range gv_lgreg ls_crit_tlegsu-lgreg.
  macro_append_range gc_tariff_system-tarif_codes ls_crit_tlegsu-ctsty.
  CALL FUNCTION '/SAPSLL/TLEGSU_DB_RNG_READ'
    EXPORTING
      is_crit        = ls_crit_tlegsu
      is_debug       = gs_debug
    IMPORTING
      et_result      = lt_tlegsu
    EXCEPTIONS
      no_data        = 1
      open_sql_error = 4.
  IF sy-subrc EQ 0.
    SORT lt_tlegsu BY datab DESCENDING.
    LOOP AT lt_tlegsu ASSIGNING <ls_tlegsu>.
      gv_stcts = <ls_tlegsu>-ctsim.
      gv_stctm_tlegsu = <ls_tlegsu>-stctm.
      EXIT.
    ENDLOOP.
  ENDIF.

* Get the Measure Scheme from the Numbering Scheme
  ls_tcots_pk-mandt = sy-mandt.
  ls_tcots_pk-stcts = gv_stcts.
  CALL FUNCTION '/SAPSLL/TCOTS_DB_SGL_READ'
    EXPORTING
      is_pk        = ls_tcots_pk
      is_debug     = gs_debug
    IMPORTING
      es_result    = ls_tcots
    EXCEPTIONS
      no_data      = 1
      invalid_call = 4.
  IF sy-subrc EQ 0 AND ls_tcots-stctm IS NOT INITIAL.
    gv_stctm_tcots = ls_tcots-stctm.
  ENDIF.

* Get the configured doc's for Transport.
  CLEAR ls_crit_cucpat.
  macro_append_range sy-mandt ls_crit_cucpat-mandt.
  macro_append_range gv_cuscs_papty ls_crit_cucpat-cuscs.
  macro_append_range gc_doc_category-awb ls_crit_cucpat-docat.
  macro_append_range gc_doc_category-trar ls_crit_cucpat-docat.
  macro_append_range gc_doc_category-trck ls_crit_cucpat-docat.
  CALL FUNCTION '/SAPSLL/CUCPAT_DB_RNG_READ'
    EXPORTING
      is_crit        = ls_crit_cucpat
      is_debug       = gs_debug
    IMPORTING
      et_result      = gt_cucpat_tran
    EXCEPTIONS
      no_data        = 1
      open_sql_error = 4.
  IF sy-subrc EQ 0.
    DELETE gt_cucpat_tran WHERE docat IS INITIAL.
    SORT gt_cucpat_tran BY docat.
  ENDIF.

* Get the configured doc's for Preference.
  CLEAR ls_crit_cucpat.
  macro_append_range sy-mandt ls_crit_cucpat-mandt.
  macro_append_range gv_cuscs_papty ls_crit_cucpat-cuscs.
  macro_append_range gc_doc_type-cuip ls_crit_cucpat-qaldoc.
  CALL FUNCTION '/SAPSLL/CUCPAT_DB_RNG_READ'
    EXPORTING
      is_crit        = ls_crit_cucpat
      is_debug       = gs_debug
    IMPORTING
      et_result      = gt_cucpat_pref
    EXCEPTIONS
      no_data        = 1
      open_sql_error = 4.
  IF sy-subrc EQ 0.
    DELETE gt_cucpat_pref WHERE prfnw IS INITIAL.
    SORT gt_cucpat_pref BY prfnw.
  ENDIF.

* Get the configured doc's for Measure-related determination
  SELECT qaldoc papty docus
    INTO CORRESPONDING FIELDS OF TABLE gt_papty_docus
    FROM /sapsll/cucpat
    WHERE cuscs = gv_cuscs_papty
      AND qaldoc IN (gc_doc_type-cuid, gc_doc_type-cuie, gc_doc_type-cuii, gc_doc_type-cuis, gc_doc_type-cuip)
      AND docus IN (gc_docus-document, gc_docus-license).
  IF sy-subrc EQ 0.
  ENDIF.

ENDFORM.                    " tariff_config_data_get

*&---------------------------------------------------------------------*
*&      Form  ITEMS_ENHANCE1
*&---------------------------------------------------------------------*
*       Enhance the Delivery Items by adding Product GUIDs
*       and Classification
*----------------------------------------------------------------------*
*      -->IT_DELITM   Table of Delivery items from Feeder System
*      <--CT_REPDATA  Enhanced data table
*----------------------------------------------------------------------*
FORM items_enhance1
  USING    it_delitm  TYPE zgt_tt_delitm
  CHANGING ct_repdata TYPE zgt_tt_docimp_01_rep.

  DATA: lt_pntpr   TYPE /sapsll/pntpr_st,
        lt_tariff  TYPE TABLE OF gty_pr_tariff,
        ls_repdata TYPE zgt_s_docimp_01_rep,
        ls_delitm  TYPE zgt_s_delitm,
        ls_pntpr   TYPE /sapsll/pntpr,
        ls_tariff  TYPE gty_pr_tariff,
        lv_guid_pr TYPE /sapsll/guid_pr,
        lv_tabix   LIKE sy-tabix.

  FIELD-SYMBOLS:
    <ls_tariff>  TYPE gty_pr_tariff,
    <ls_repdata> TYPE zgt_s_docimp_01_rep.

* Get the Product GUIDs
  SELECT mandt grvsy prvsy prtyp guid_pr
    INTO CORRESPONDING FIELDS OF TABLE lt_pntpr
    FROM /sapsll/pntpr
    FOR ALL ENTRIES IN it_delitm
    WHERE grvsy = gv_grvsy
      AND prvsy = it_delitm-prvsy
      AND prtyp = gc_product_type-material.

* Add GUIDs to the basic report data
  LOOP AT it_delitm INTO ls_delitm.
    MOVE-CORRESPONDING ls_delitm TO ls_repdata.
    READ TABLE lt_pntpr INTO ls_pntpr
      WITH TABLE KEY mandt = sy-mandt
                     grvsy = gv_grvsy
                     prvsy = ls_delitm-prvsy
                     prtyp = gc_product_type-material.
    IF sy-subrc EQ 0.
      ls_repdata-guid_pr = ls_pntpr-guid_pr.
    ENDIF.
    APPEND ls_repdata TO ct_repdata.
  ENDLOOP.

* Get the associated Product classifications
  SELECT p~guid_pr p~stcts p~guid_ctsnum p~datab n~ccngn AS impcn
    INTO CORRESPONDING FIELDS OF TABLE lt_tariff
    FROM /sapsll/prcts AS p
    JOIN /sapsll/ctsnum AS n ON p~guid_ctsnum = n~guid_ctsnum
    FOR ALL ENTRIES IN ct_repdata
    WHERE guid_pr = ct_repdata-guid_pr
      AND p~stcts = gv_stcts
      AND enduse = gc_tariff_enduse-standard.

* Remove any duplicate entries, due to re-classification
  IF sy-subrc EQ 0.
    SORT lt_tariff BY guid_pr ASCENDING datab DESCENDING.
    LOOP AT lt_tariff ASSIGNING <ls_tariff>.
      lv_tabix = sy-tabix.
      IF <ls_tariff>-guid_pr = lv_guid_pr.
        DELETE lt_tariff INDEX lv_tabix.
      ELSE.
        lv_guid_pr = <ls_tariff>-guid_pr.
      ENDIF.
    ENDLOOP.
  ENDIF.

*---------------------------------------------------------------------
* Populate each item in the results list
  SORT ct_repdata BY delnum delitm.

  LOOP AT ct_repdata ASSIGNING <ls_repdata>.

*   Add the Product Classification
    READ TABLE lt_tariff INTO ls_tariff
      WITH KEY guid_pr = <ls_repdata>-guid_pr
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      <ls_repdata>-guid_ctsnum = ls_tariff-guid_ctsnum.
      <ls_repdata>-impcn = ls_tariff-impcn.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " items_enhance1

*&---------------------------------------------------------------------*
*&      Form  DOCS_DETERMINE
*&---------------------------------------------------------------------*
*       Determine the required documents for Import Declaration
*----------------------------------------------------------------------*
*      -->IT_REPDATA  Enhanced Delivery Item data, with GUIDs
*      <--CT_ALV      Entries to appear in the Results List
*----------------------------------------------------------------------*
FORM docs_determine
  USING    it_repdata TYPE zgt_tt_docimp_01_rep
  CHANGING ct_alv     TYPE zgt_tt_docimp_01_alv.

  DATA: ls_repdata  TYPE zgt_s_docimp_01_rep,
        lv_delnum   TYPE /sapsll/acw_inb_del_no,
        lv_datstamp TYPE tzntstmps.

* Get Country Groups for relevant countries
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_ctygpa
    FROM /sapsll/ctygpa
    FOR ALL ENTRIES IN it_repdata
    WHERE lndab = it_repdata-cucoo
      AND datbi GE sy-datlo.
  IF sy-subrc EQ 0.
    CALL FUNCTION '/SAPSLL/CONVERT_DATE_TO_STAMP'
      EXPORTING
        iv_date            = sy-datlo
      IMPORTING
        ev_timestamp       = lv_datstamp
      EXCEPTIONS
        no_time_zone       = 1
        time_zone_notfound = 2
        format_error       = 3
        no_date_provided   = 4.
    IF sy-subrc EQ 0.
      DELETE gt_ctygpa WHERE datab GT lv_datstamp.
    ENDIF.
    SORT gt_ctygpa BY lndab.
  ENDIF.

* Get Measure-related documents
  SELECT d~ccngn d~stctm d~mstid i~guid_ctsmai mscid ctygr cntry datbi datab papty
    INTO CORRESPONDING FIELDS OF TABLE gt_ctsmxx
    FROM /sapsll/ctsmac AS c
    JOIN /sapsll/ctsmnd AS d ON c~guid_ctsmnd = d~guid_ctsmnd
    JOIN /sapsll/ctsmai AS i ON d~stctm = i~stctm AND d~mstid = i~mstid
  FOR ALL ENTRIES IN it_repdata
    WHERE d~ccngn = it_repdata-impcn
      AND d~stctm IN (gv_stctm_tlegsu, gv_stctm_tcots)
      AND indei IN (gc_indei-import, gc_indei-expimp)
      AND mscid IN s_mscid
      AND datbi GE sy-datlo
      AND datab LE sy-datlo
      AND papty GT ' '.
  IF sy-subrc EQ 0.
    SORT gt_ctsmxx BY ccngn ctygr cntry papty.
    DELETE ADJACENT DUPLICATES FROM gt_ctsmxx COMPARING ctygr cntry papty.
    SORT gt_ctsmxx BY ccngn ctygr cntry.
  ENDIF.

* Main loop through Open Delivery Items
* --------------------------------------------------------------------

  LOOP AT it_repdata INTO ls_repdata.

* Add header-level document(s) if required
    IF ls_repdata-delnum NE lv_delnum.
      PERFORM header_entries_create
        USING    ls_repdata
        CHANGING ct_alv.
    ENDIF.
    lv_delnum = ls_repdata-delnum.

* Item-level documents
    PERFORM item_entries_create
      USING    ls_repdata
      CHANGING ct_alv.

  ENDLOOP.

ENDFORM.                    " docs_determine

*&---------------------------------------------------------------------*
*&      Form  HEADER_ENTRIES_CREATE
*&---------------------------------------------------------------------*
*       Create one or more Results List entries at header level
*----------------------------------------------------------------------*
*      -->IS_DELITM    First item in current Delivery
*      <--CT_HDR_ITEMS Header entries
*----------------------------------------------------------------------*
FORM header_entries_create
  USING    is_repdata TYPE zgt_s_docimp_01_rep
  CHANGING ct_alv    TYPE zgt_tt_docimp_01_alv.

  DATA: ls_alv    TYPE zgt_s_docimp_01_alv,
        ls_docimp TYPE zgt_docimp,
        ls_cucpat TYPE /sapsll/cucpat.

* --------------------------------------------------------------------
* Document for Waybill (Previous Document)
* --------------------------------------------------------------------
  CLEAR ls_alv.
  MOVE is_repdata-delnum TO ls_alv-delnum.
  MOVE 0 TO ls_alv-delitm. " header

* Check if Previous Document already exists in Voorafsetje table
  SELECT SINGLE * FROM zgt_docimp INTO ls_docimp
    WHERE delnum = ls_alv-delnum
      AND delitm = ls_alv-delitm
      AND qaldoc = gc_doc_type-suma.

  IF sy-subrc EQ 0.
* Entry exists - display with relevant status
    MOVE-CORRESPONDING ls_docimp TO ls_alv.
*E227- Set status inactive
**********************************************************************
    IF ls_docimp-deact EQ abap_true.
      ls_alv-errsta = gc_inactive.
      APPEND ls_alv TO ct_alv.
    ELSE.
**********************************************************************
      IF ls_alv-papno IS INITIAL OR ls_alv-flvdo IS INITIAL.
        ls_alv-errsta = gc_msg_status-warning.
        APPEND ls_alv TO ct_alv.
      ELSE.
        IF p_fldec IS INITIAL.
          ls_alv-errsta = gc_msg_status-success.
          APPEND ls_alv TO ct_alv.
        ENDIF.
      ENDIF.

    ENDIF.
  ELSE.
* No existing entry - propose potential entry, status "red"
    IF is_repdata-mtrbc IS NOT INITIAL.
* - Determine Document Category & Type based on MoT
      CASE is_repdata-mtrbc.
        WHEN '1'. " Sea
          READ TABLE gt_cucpat_tran INTO ls_cucpat
            WITH KEY docat = gc_doc_category-trar
            BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_alv-qaldoc = ls_cucpat-qaldoc.
            ls_alv-papty  = ls_cucpat-papty.
          ENDIF.
        WHEN '3'. " Road
          READ TABLE gt_cucpat_tran INTO ls_cucpat
            WITH KEY docat = gc_doc_category-trck
            BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_alv-qaldoc = ls_cucpat-qaldoc.
            ls_alv-papty  = ls_cucpat-papty.
          ENDIF.
        WHEN '4'. " Air
          READ TABLE gt_cucpat_tran INTO ls_cucpat
            WITH KEY docat = gc_doc_category-awb
            BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_alv-qaldoc = ls_cucpat-qaldoc.
            ls_alv-papty  = ls_cucpat-papty.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
      MOVE is_repdata-waybill TO ls_alv-papno.
      ls_alv-errsta = gc_msg_status-error.
      APPEND ls_alv TO ct_alv.

* No MoT, so can't propose Document Category or Type
    ELSE.
      ls_alv-papno = TEXT-i05.
      ls_alv-errsta = gc_msg_status-undefined.
      APPEND ls_alv TO ct_alv.
    ENDIF.
  ENDIF.
* --------------------------------------------------------------------

ENDFORM.                    " header_entries_create

*&---------------------------------------------------------------------*
*&      Form  ITEM_ENTRIES_CREATE
*&---------------------------------------------------------------------*
*       Create one or more Results List entries at item level
*----------------------------------------------------------------------*
*      -->IS_DELITM  Current delivery item data
*      <--CT_ALV     Results table
*----------------------------------------------------------------------*
FORM item_entries_create
  USING    is_repdata TYPE zgt_s_docimp_01_rep
  CHANGING ct_alv     TYPE zgt_tt_docimp_01_alv.

  CONSTANTS: lc_guid_init TYPE /sapsll/guid_16 VALUE '00000000000000000000000000000000'.

  DATA: lt_llns_ped TYPE /sapsll/llns_pedigree_t,
        lt_alv      TYPE zgt_tt_docimp_01_alv,
        lt_res      TYPE gty_t_cdwlm,
        ls_alv      TYPE zgt_s_docimp_01_alv,
        ls_llns_ped TYPE /sapsll/llns_pedigree_s,
        ls_pedigree TYPE gty_pedigree,
        ls_cusb2    TYPE gty_cusb2,
        ls_ctygpa   TYPE /sapsll/ctygpa,
        ls_docimp   TYPE zgt_docimp,
        ls_cucpat   TYPE /sapsll/cucpat,
        ls_ctsmxx   TYPE gty_ctsmxx,
        lv_count    TYPE i,
        lv_pref_ok  TYPE boolean,
        lv_rcode    LIKE sy-subrc,
        lv_found    TYPE xfeld.

* Handle unclassified product
  IF is_repdata-guid_ctsnum IS INITIAL.
    CLEAR ls_alv.
    MOVE-CORRESPONDING is_repdata TO ls_alv.
    ls_alv-errsta = gc_msg_status-undefined.
    ls_alv-papno = TEXT-i04.
    APPEND ls_alv TO ct_alv.
    RETURN.
  ENDIF.

* Handle product without Country of Origin
  IF is_repdata-cucoo IS INITIAL.
    CLEAR ls_alv.
    MOVE-CORRESPONDING is_repdata TO ls_alv.
    ls_alv-errsta = gc_msg_status-undefined.
    ls_alv-papno = TEXT-i06.
    APPEND ls_alv TO ct_alv.
    RETURN.
  ENDIF.

  PERFORM get_doc_ref_data USING    is_repdata
                           CHANGING ct_alv
                                    lv_found
                                    lt_res.

*  IF lv_found EQ abap_true.
*    RETURN.
*  ENDIF.
* --------------------------------------------------------------------
* Documents required by Tariff Measures
* --------------------------------------------------------------------

  LOOP AT gt_ctsmxx INTO ls_ctsmxx
    WHERE ccngn = is_repdata-impcn.

* - Check if Document already exists in Voorafsetje table
    SELECT SINGLE * FROM zgt_docimp INTO ls_docimp
      WHERE delnum = is_repdata-delnum
        AND delitm = is_repdata-delitm
        AND papty = ls_ctsmxx-papty.

* - If found, show in Results List (if all doc's selected)
    IF sy-subrc EQ 0.
      CLEAR ls_alv.

* --- Avoid for duplicate entries
      READ TABLE ct_alv WITH KEY
        delnum = is_repdata-delnum
        delitm = is_repdata-delitm
        papty  = ls_ctsmxx-papty
        TRANSPORTING NO FIELDS.

* --- Show in Results List if not a duplicate
      IF sy-subrc NE 0.
        MOVE-CORRESPONDING ls_ctsmxx TO ls_alv.
        MOVE-CORRESPONDING is_repdata TO ls_alv.
        MOVE-CORRESPONDING ls_docimp TO ls_alv.
        READ TABLE lt_res WITH KEY ccngn = is_repdata-impcn old_papty = ls_alv-papty ASSIGNING FIELD-SYMBOL(<ls_res>).
        IF sy-subrc EQ 0.
          ls_alv-papty = <ls_res>-new_papty.
        ENDIF.
        IF ls_alv-papno IS INITIAL OR ls_alv-flvdo IS INITIAL.
          ls_alv-errsta = gc_msg_status-warning.
          APPEND ls_alv TO ct_alv.
        ELSE.
          IF p_fldec IS INITIAL.
            ls_alv-errsta = gc_msg_status-success.
            APPEND ls_alv TO ct_alv.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
* - Otherwise, create worklist entry as proposal

* Find Document for Country of Origin
      IF ls_ctsmxx-cntry = is_repdata-cucoo.
        PERFORM measures_doc_add
          USING    ls_ctsmxx
                   is_repdata
          CHANGING ls_alv
                   lv_rcode.
        IF lv_rcode EQ 0.
          READ TABLE lt_res WITH KEY ccngn = is_repdata-impcn old_papty = ls_alv-papty ASSIGNING <ls_res>.
          IF sy-subrc EQ 0.
            ls_alv-papty = <ls_res>-new_papty.
          ENDIF.
          APPEND ls_alv TO ct_alv.
        ENDIF.
      ENDIF.

* Find Document for Country Group
      IF ls_ctsmxx-ctygr IS NOT INITIAL.
        LOOP AT gt_ctygpa INTO ls_ctygpa
          WHERE lndab = is_repdata-cucoo
            AND ctygr = ls_ctsmxx-ctygr.
          IF ls_ctsmxx-ctygr = ls_ctygpa-ctygr.
            PERFORM measures_doc_add
              USING    ls_ctsmxx
                       is_repdata
              CHANGING ls_alv
                       lv_rcode.
            IF lv_rcode EQ 0.
* - Avoid duplicates, in case same doc found for Country
              READ TABLE ct_alv
                WITH KEY delnum = ls_alv-delnum
                         delitm = ls_alv-delitm
                         papty  = ls_alv-papty
                TRANSPORTING NO FIELDS.
              IF sy-subrc NE 0.
                READ TABLE lt_res WITH KEY ccngn = is_repdata-impcn old_papty = ls_alv-papty ASSIGNING <ls_res>.
                IF sy-subrc EQ 0.
                  ls_alv-papty = <ls_res>-new_papty.
                ENDIF.
                APPEND ls_alv TO ct_alv.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDLOOP.

  lt_alv = ct_alv.

  LOOP AT ct_alv INTO ls_alv WHERE delnum EQ is_repdata-delnum
                             AND   delitm EQ is_repdata-delitm
                             GROUP BY ( mstid = ls_alv-mstid )
                             REFERENCE INTO DATA(lo_grp).
    LOOP AT GROUP lo_grp ASSIGNING FIELD-SYMBOL(<ls_grp>) WHERE guid_docimp IS NOT INITIAL.
      DELETE lt_alv WHERE delnum EQ is_repdata-delnum
                    AND   delitm EQ is_repdata-delitm
                    AND   mstid  EQ <ls_grp>-mstid
                    AND   guid_docimp EQ lc_guid_init.
    ENDLOOP.
  ENDLOOP.

  ct_alv = lt_alv.

  LOOP AT ct_alv INTO ls_alv WHERE delnum EQ is_repdata-delnum
                             AND   delitm EQ is_repdata-delitm
                             GROUP BY ( papty = ls_alv-papty )
                             REFERENCE INTO DATA(lo_grp2).
    LOOP AT GROUP lo_grp2 ASSIGNING FIELD-SYMBOL(<ls_grp2>) WHERE guid_docimp IS NOT INITIAL.
      DELETE lt_alv WHERE delnum EQ is_repdata-delnum
                    AND   delitm EQ is_repdata-delitm
                    AND   papty  EQ <ls_grp2>-papty
                    AND   guid_docimp EQ lc_guid_init.
    ENDLOOP.
  ENDLOOP.

  ct_alv = lt_alv.

* --------------------------------------------------------------------
* Preference Certificate
* --------------------------------------------------------------------

* Check if Pref.Doc already exists in Voorafsetje table
  SELECT SINGLE * FROM zgt_docimp INTO ls_docimp
    WHERE delnum = is_repdata-delnum
      AND delitm = is_repdata-delitm
      AND qaldoc = gc_doc_type-cuip.
  IF sy-subrc EQ 0.
    CLEAR ls_alv.
    MOVE-CORRESPONDING is_repdata TO ls_alv.
    MOVE-CORRESPONDING ls_docimp TO ls_alv.
* - E227 Set status Inactive
    IF ls_docimp-deact EQ abap_true.
      ls_alv-errsta = gc_inactive.
      APPEND ls_alv TO ct_alv.
* - E227 Set status Inactive
    ELSEIF ls_alv-papno IS INITIAL OR ls_alv-flvdo IS INITIAL.
      ls_alv-errsta = gc_msg_status-warning.
      APPEND ls_alv TO ct_alv.
    ELSE.
      IF p_fldec IS INITIAL.
        ls_alv-errsta = gc_msg_status-success.
        APPEND ls_alv TO ct_alv.
      ENDIF.
    ENDIF.

* If no existing entry, determine if Preference applicable
  ELSE.

* - Check if Pedigree entries already stored
    READ TABLE gt_pedigree WITH KEY
      guid_ctsnum = is_repdata-guid_ctsnum
      TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

* - Get the Tariff "Pedigree" (all levels)
      CALL FUNCTION '/SAPSLL/LLNS_GET_PEDIGREE'
        EXPORTING
          iv_stcts              = gv_stcts
          iv_guid_ctsxxx        = is_repdata-guid_ctsnum
*         IV_TSTMP              = .  " assume "today"
        IMPORTING
          et_pedigree           = lt_llns_ped
        EXCEPTIONS
          stcts_not_found       = 1
          date_not_valid        = 2
          number_not_found      = 3
          no_pedigree_available = 4
          wrong_sort_rule       = 5.
      IF sy-subrc EQ 0.

* - Find any Preference Duty rates for the Tariff
        SELECT *
          APPENDING CORRESPONDING FIELDS OF TABLE gt_lc_cusb2
          FROM /sapsll/lc_cusb2
          FOR ALL ENTRIES IN lt_llns_ped
          WHERE tlc_aart BETWEEN 'B200' AND 'B299'
            AND guid_htsnum = lt_llns_ped-guid_ctsxxx
            AND lgreg = gv_lgreg
            AND datbi > sy-datlo.
        IF sy-subrc EQ 0.
          SORT gt_lc_cusb2 BY lgreg guid_htsnum ctygr_oo cucoo datab.
          DELETE ADJACENT DUPLICATES FROM gt_lc_cusb2
            COMPARING lgreg guid_htsnum ctygr_oo cucoo datab.
        ENDIF.

* - Add the additional entries to the global table
        LOOP AT lt_llns_ped INTO ls_llns_ped.
          ls_pedigree-guid_ctsnum = is_repdata-guid_ctsnum.
          ls_pedigree-strcn = lv_count.
          ls_pedigree-guid_ctsxxx = ls_llns_ped-guid_ctsxxx.
          ls_pedigree-ccngn = ls_llns_ped-ccngn.
          lv_count = lv_count + 1.
          INSERT ls_pedigree INTO TABLE gt_pedigree.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Set excludion tab data
    DATA(lt_lc_cusb2_ctyex) = VALUE gty_t_cusb2( FOR ls_row IN gt_lc_cusb2 WHERE ( ctyex EQ abap_true ) ( ls_row ) ).

    IF lt_lc_cusb2_ctyex IS NOT INITIAL.
      SELECT guid_lc_cusb
        INTO TABLE @DATA(lt_lc_cusxc)
        FROM /sapsll/lc_cusxc
        FOR ALL ENTRIES IN @lt_lc_cusb2_ctyex
        WHERE guid_lc_cusb = @lt_lc_cusb2_ctyex-guid_lc_cusb2.

      IF sy-subrc NE 0.
        CLEAR lt_lc_cusb2_ctyex.
      ENDIF.
    ENDIF.

* - Find Preference rate for Product / Country or Origin
    SORT gt_lc_cusb2 BY guid_htsnum cucoo.
    LOOP AT gt_pedigree INTO ls_pedigree
      WHERE guid_ctsnum = is_repdata-guid_ctsnum.
      READ TABLE gt_lc_cusb2 INTO ls_cusb2
      WITH KEY lgreg = gv_lgreg
               guid_htsnum = ls_pedigree-guid_ctsxxx
               cucoo = is_repdata-cucoo
               BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR ls_alv.
        MOVE-CORRESPONDING is_repdata TO ls_alv.
        ls_alv-qaldoc = gc_doc_type-cuip.
        ls_alv-errsta = gc_msg_status-error.
        READ TABLE gt_cucpat_pref INTO ls_cucpat
          WITH KEY prfnw = ls_cusb2-prfnw
          BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_alv-papty = ls_cucpat-papty.
        ENDIF.
        APPEND ls_alv TO ct_alv.
        lv_pref_ok = gc_true.
        EXIT.
      ENDIF.
    ENDLOOP.

* - Find Preference rate for Product / Country Group
    IF lv_pref_ok IS INITIAL.
      SORT gt_lc_cusb2 BY guid_htsnum ctygr_oo.
      LOOP AT gt_pedigree INTO ls_pedigree
        WHERE guid_ctsnum = is_repdata-guid_ctsnum.
        LOOP AT gt_ctygpa INTO ls_ctygpa
          WHERE lndab = is_repdata-cucoo.
          READ TABLE gt_lc_cusb2 INTO ls_cusb2
            WITH KEY lgreg = gv_lgreg
                     guid_htsnum = ls_pedigree-guid_ctsxxx
                     ctygr_oo = ls_ctygpa-ctygr
            BINARY SEARCH.
          IF sy-subrc EQ 0.

            READ TABLE lt_lc_cusxc INTO DATA(ls_lc_cusxc) WITH KEY guid_lc_cusb = ls_cusb2-guid_lc_cusb2.
            IF sy-subrc EQ 0.
              CONTINUE.
            ENDIF.

            CLEAR ls_alv.
            MOVE-CORRESPONDING is_repdata TO ls_alv.
            ls_alv-qaldoc = gc_doc_type-cuip.
            ls_alv-errsta = gc_msg_status-error.
            READ TABLE gt_cucpat_pref INTO ls_cucpat
              WITH KEY prfnw = ls_cusb2-prfnw
              BINARY SEARCH.
            IF sy-subrc EQ 0.
              ls_alv-papty = ls_cucpat-papty.
            ENDIF.
            APPEND ls_alv TO ct_alv.
            lv_pref_ok = gc_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " item_entries_create

*&---------------------------------------------------------------------*
*&      Form  MEASURES_DOC_ADD
*&---------------------------------------------------------------------*
*       Create a results list entry, if final checks satisfied
*----------------------------------------------------------------------*
*      -->IS_CTSMXX   Measures structure
*      -->IS_DELITM   Delivery Item data returned from ERP
*      -->IS_DELITEM  Enhanced Delivery Item data
*      <--CS_ALV      Results List entry
*      <--CV_RCODE    Return Code > 0 = "Create entry"
*----------------------------------------------------------------------*
FORM measures_doc_add
  USING    is_ctsmxx  TYPE gty_ctsmxx
           is_repdata TYPE zgt_s_docimp_01_rep
  CHANGING cs_alv     TYPE zgt_s_docimp_01_alv
           cv_rcode   LIKE sy-subrc.

  DATA: ls_papty_docus TYPE gty_papty_docus.

  CONSTANTS:
        lc_anti_dumping TYPE /sapsll/mscid VALUE 'D'.

  cv_rcode = 0.

* Don't flag anti-dumping unless indicated
  IF is_ctsmxx-mscid = lc_anti_dumping.
    IF ( is_repdata-coadi IS INITIAL )
      OR ( is_repdata-coadi+1(3) = '999' ).
      cv_rcode = 4.
      RETURN.
    ENDIF.
  ENDIF.

* Only accept if Document or License determination is set
  READ TABLE gt_papty_docus INTO ls_papty_docus
    WITH KEY  papty = is_ctsmxx-papty.
  IF sy-subrc NE 0.
    cv_rcode = 4.
    RETURN.
  ENDIF.

* Populate the Results List entry
  CLEAR cs_alv.
  MOVE-CORRESPONDING is_repdata TO cs_alv.
  MOVE-CORRESPONDING is_ctsmxx TO cs_alv.
  cs_alv-qaldoc = ls_papty_docus-qaldoc.
  cs_alv-errsta = gc_msg_status-error.

ENDFORM.                    " measures_doc_add

*&---------------------------------------------------------------------*
*&      Form  ITEMS_ENHANCE2
*&---------------------------------------------------------------------*
*       Complete the ALV structure
*----------------------------------------------------------------------*
*      <--CT_ALV  ALV results structure
*----------------------------------------------------------------------*
FORM items_enhance2
  CHANGING ct_alv TYPE zgt_tt_docimp_01_alv.

  DATA: lt_qaldoc_txt  TYPE TABLE OF dd07v,
        lt_mscid_txt   TYPE TABLE OF dd07v,
        lt_prt         TYPE /sapsll/prt_upd_st,
        lt_t005t       TYPE /sapsll/t005t_st,
        lt_ctsstxt     TYPE SORTED TABLE OF gty_ctsstxt
                            WITH NON-UNIQUE KEY mstid,
        lt_cucpatt     TYPE /sapsll/cucpatt_t,
        ls_crit_cucptt TYPE /sapsll/cucpatt_crit_s,
        ls_cucpatt     TYPE /sapsll/cucpatt,
        ls_prt         TYPE /sapsll/prt,
        ls_t005t       TYPE t005t,
        ls_ctstxt      TYPE gty_ctsstxt,
        ls_dd07v       TYPE dd07v.

  FIELD-SYMBOLS:
        <ls_alv> TYPE zgt_s_docimp_01_alv.
**********************************************************************

  LOOP AT ct_alv ASSIGNING <ls_alv>
    WHERE ponum IS INITIAL.
    LOOP AT ct_alv ASSIGNING FIELD-SYMBOL(<ls_po>)
      WHERE delnum EQ <ls_alv>-delnum
      AND   ponum  IS NOT INITIAL.
      <ls_alv>-ponum = <ls_po>-ponum.
      EXIT.
    ENDLOOP.
  ENDLOOP.

*---------------------------------------------------------------------
* Generate sorted "look-up" tables for description data
* --------------------------------------------------------------------

* Get the Document Category descriptions
  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = '/SAPSLL/QALDOC'
      langu         = sy-langu
      texts_only    = 'X'
    TABLES
      dd07v_tab     = lt_qaldoc_txt
    EXCEPTIONS
      illegal_input = 1.
  IF sy-subrc EQ 0.
    SORT lt_qaldoc_txt BY ddlanguage domvalue_l.
  ENDIF.

* Get the document descriptions
  macro_append_range sy-mandt ls_crit_cucptt-mandt.
  macro_append_range sy-langu ls_crit_cucptt-langu.
  macro_append_range gv_cuscs_papty ls_crit_cucptt-cuscs.
  macro_append_range gc_doc_type-cuid ls_crit_cucptt-qaldoc.
  macro_append_range gc_doc_type-cuie ls_crit_cucptt-qaldoc.
  macro_append_range gc_doc_type-cuii ls_crit_cucptt-qaldoc.
  macro_append_range gc_doc_type-cuip ls_crit_cucptt-qaldoc.
  macro_append_range gc_doc_type-cuis ls_crit_cucptt-qaldoc.
  macro_append_range gc_doc_type-suma ls_crit_cucptt-qaldoc.
  CALL FUNCTION '/SAPSLL/CUCPATT_DB_RNG_READ'
    EXPORTING
      is_crit        = ls_crit_cucptt
      is_debug       = gs_debug
    IMPORTING
      et_result      = lt_cucpatt
    EXCEPTIONS
      no_data        = 1
      open_sql_error = 4.
  IF sy-subrc EQ 0.
    SORT lt_cucpatt BY mandt langu cuscs qaldoc papty.
  ENDIF.

* Get the associated Product descriptions
  SELECT guid_pr langu prtxt
    INTO CORRESPONDING FIELDS OF TABLE lt_prt
    FROM /sapsll/prt AS t
    FOR ALL ENTRIES IN ct_alv
    WHERE guid_pr = ct_alv-guid_pr
      AND langu = sy-langu.

* Get the Country names
  SELECT spras land1 landx
    INTO CORRESPONDING FIELDS OF TABLE lt_t005t
    FROM t005t
    FOR ALL ENTRIES IN ct_alv
    WHERE spras = sy-langu
      AND land1 = ct_alv-cucoo.

* Get the Measure Type descriptions
  SELECT mstid text1
    INTO CORRESPONDING FIELDS OF TABLE lt_ctsstxt
    FROM /sapsll/ctsstx AS x
    JOIN /sapsll/ctsstxt AS t ON t~guid_ctsstx = x~guid_ctsstx
    JOIN /sapsll/ctsmai AS i ON i~guid_ctsmai = x~stxtk
    FOR ALL ENTRIES IN ct_alv
    WHERE i~mstid = ct_alv-mstid
      AND langu = sy-langu.
  IF sy-subrc EQ 0.
    DELETE ADJACENT DUPLICATES FROM lt_ctsstxt.
  ENDIF.

* Get the Measure Category descriptions
  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = '/SAPSLL/MSCID'
      langu         = sy-langu
      texts_only    = 'X'
    TABLES
      dd07v_tab     = lt_mscid_txt
    EXCEPTIONS
      illegal_input = 1.
  IF sy-subrc EQ 0.
    SORT lt_mscid_txt BY ddlanguage domvalue_l.
  ENDIF.

* --------------------------------------------------------------------
* Finishing loop - add Status icon and descriptions to each record
* --------------------------------------------------------------------

  SORT ct_alv BY delnum delitm.

  LOOP AT ct_alv ASSIGNING <ls_alv>.

*   Add the Product Description
    READ TABLE lt_prt INTO ls_prt
      WITH KEY guid_pr = <ls_alv>-guid_pr
               langu   = sy-langu
      BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_alv>-prtxt = ls_prt-prtxt.
    ENDIF.

*   Add the Country name
    IF <ls_alv>-cucoo IS NOT INITIAL.
      READ TABLE lt_t005t INTO ls_t005t
      WITH TABLE KEY spras = sy-langu
                     land1 = <ls_alv>-cucoo.
      IF sy-subrc EQ 0.
        <ls_alv>-landx = ls_t005t-landx.
      ENDIF.
    ELSE.
      CLEAR <ls_alv>-landx.
    ENDIF.

* - Add the Qualifier description
    IF <ls_alv>-qaldoc IS NOT INITIAL.
      READ TABLE lt_qaldoc_txt INTO ls_dd07v
        WITH KEY ddlanguage = sy-langu
                 domvalue_l = <ls_alv>-qaldoc
        BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_alv>-qaldoc_txt = ls_dd07v-ddtext.
      ENDIF.
    ENDIF.

* - Add the Document Type description
    IF <ls_alv>-papty IS NOT INITIAL.
      IF <ls_alv>-qaldoc IS INITIAL.
        READ TABLE lt_cucpatt INTO ls_cucpatt
          WITH KEY mandt = sy-mandt
                   langu = sy-langu
                   cuscs = gv_cuscs_papty
                   papty = <ls_alv>-papty.
        IF sy-subrc EQ 0.
          <ls_alv>-papty_txt = ls_cucpatt-text1.
        ENDIF.
      ELSE.
        READ TABLE lt_cucpatt INTO ls_cucpatt
          WITH TABLE KEY mandt = sy-mandt
                         langu = sy-langu
                         cuscs = gv_cuscs_papty
                         qaldoc = <ls_alv>-qaldoc
                         papty = <ls_alv>-papty.
        IF sy-subrc NE 0.
          READ TABLE lt_cucpatt INTO ls_cucpatt
          WITH KEY mandt = sy-mandt
                   langu = sy-langu
                   cuscs = gv_cuscs_papty
                   papty = <ls_alv>-papty.
        ENDIF.
        IF sy-subrc EQ 0.
          <ls_alv>-papty_txt = ls_cucpatt-text1.
        ENDIF.
      ENDIF.
    ENDIF.

* - Add the Measure Type description
    IF <ls_alv>-mstid IS NOT INITIAL.
      READ TABLE lt_ctsstxt INTO ls_ctstxt
        WITH TABLE KEY mstid = <ls_alv>-mstid.
      IF sy-subrc = 0.
        <ls_alv>-mstid_txt = ls_ctstxt-text1.
      ENDIF.
    ENDIF.

* - Add the Measure Category description
    IF <ls_alv>-mscid IS NOT INITIAL.
      READ TABLE lt_mscid_txt INTO ls_dd07v
        WITH KEY ddlanguage = sy-langu
                 domvalue_l = <ls_alv>-mscid
        BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_alv>-mscid_txt = ls_dd07v-ddtext.
      ENDIF.
    ENDIF.

* - Determine and add the Status icon
    CASE <ls_alv>-errsta.
      WHEN 'E'.
        macro_create_icon 'ICON_LED_RED' TEXT-i01 <ls_alv>-status.
      WHEN 'S'.
        macro_create_icon 'ICON_LED_GREEN' TEXT-i03 <ls_alv>-status.
      WHEN 'W'.
        macro_create_icon 'ICON_LED_YELLOW' TEXT-i02 <ls_alv>-status.
*-------------------------------------------------------------------- E227 - Set status deactivated
      WHEN 'I'.
        macro_create_icon 'ICON_NO_STATUS' TEXT-i07 <ls_alv>-status.
      WHEN OTHERS.
        macro_create_icon 'ICON_LED_RED' TEXT-i01 <ls_alv>-status.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " items_enhance2

*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV_STRUCTURE
*&---------------------------------------------------------------------*
*       Prepare to display the data
*&---------------------------------------------------------------------*
*      <--CT_FIELDCAT Field Catalogue
*      <--CT_SORT     Sorting criteria
*      <--CS_LAYOUT   ALV layout
*----------------------------------------------------------------------*
FORM build_alv_structure
  TABLES   ct_fieldcat TYPE lvc_t_fcat
           ct_sort     TYPE lvc_t_sort
  CHANGING cs_layout   TYPE lvc_s_layo.

  DATA: lt_fields    TYPE TABLE OF fieldinfo,
        ls_fieldinfo TYPE fieldinfo,
        ls_fieldcat  TYPE lvc_s_fcat,
        celltab      TYPE lvc_t_styl,
        ls_celltab   TYPE lvc_s_styl,
*        ls_sort      TYPE lvc_s_sort,
        lv_col_pos   LIKE sy-tabix VALUE 0.
*        lv_entries   TYPE i,
*        lv_entrynum  TYPE i.

  FREE: ct_fieldcat, ct_sort, cs_layout.

* Specify layout
  cs_layout-cwidth_opt = gc_true.
  cs_layout-zebra = gc_true.
  cs_layout-no_rowmark = gc_true.
  cs_layout-stylefname = 'CELLTAB'.
  cs_layout-edit = gc_false.

* Specify sorting criteria
*  CLEAR ls_sort.

* Get field definitiions
  CALL FUNCTION 'DDIC_DATA_DESCRIPTION_SCAN'
    EXPORTING
      i_structure      = 'ZGT_S_DOCIMP_01_ALV'
    TABLES
      t_fieldinfo      = lt_fields
    EXCEPTIONS
      table_not_exists = 1.
  IF sy-subrc NE 0.
    MESSAGE ID syst-msgid TYPE 'E' NUMBER syst-msgno
      WITH syst-msgv1 syst-msgv2 syst-msgv3 syst-msgv4.
  ENDIF.

* Set up progress counter fields
*  DESCRIBE TABLE lt_fields LINES lv_entries.
*  lv_entrynum = 1.

* Build field catalogue
  LOOP AT lt_fields INTO ls_fieldinfo.
    CLEAR ls_celltab.
    ls_celltab-fieldname = ls_fieldinfo-fieldname.
    IF ls_fieldinfo-fieldname = 'MKSEL'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE celltab.

    CLEAR ls_fieldcat.
    lv_col_pos              = lv_col_pos + 1.
    ls_fieldcat-col_pos     = lv_col_pos.
    set_fieldcat_params ls_fieldinfo ls_fieldcat.
    CASE ls_fieldinfo-fieldname.
      WHEN 'ERRSTA'
        OR 'GUID_PR'
        OR 'GUID_CTSNUM'
        OR 'GUID_DOCIMP'
        OR 'TABIX'
        OR 'DEACT'. " - E227 Set Status Inactive
        ls_fieldcat-tech = gc_true.
      WHEN 'LANDX'
        OR 'MSCID'
        OR 'MSTID'
        OR 'PAPAD'
        OR 'PSPAT'
        OR 'PAPDE'
        OR 'PAPD2'
        OR 'PAPDT'
        OR 'PAPIT'
        OR 'PAPOF'
        OR 'PAPPC'
        OR 'PAPQU'
*        OR 'PAPTY_TXT'
        OR 'PAPVL'
        OR 'POITM'
        OR 'PONUM'
        OR 'QALDOC_TXT'
        OR 'QUALD'
        OR 'STCTM'.
        ls_fieldcat-no_out = gc_true.
      WHEN 'FLVDO'.
        ls_fieldcat-checkbox = gc_true.
      WHEN 'MKSEL'.
        ls_fieldcat-checkbox = gc_true.
        ls_fieldcat-key = gc_true.
        ls_fieldcat-edit = gc_true.
      WHEN 'MSCID_TXT'.
        ls_fieldcat-coltext = TEXT-a11.
        ls_fieldcat-scrtext_s = TEXT-a12.
        ls_fieldcat-scrtext_m = TEXT-a13.
        ls_fieldcat-scrtext_l = TEXT-a14.
        ls_fieldcat-seltext = TEXT-a11.
        ls_fieldcat-no_out = gc_true.
      WHEN 'MSTID_TXT'.
        ls_fieldcat-coltext = TEXT-a21.
        ls_fieldcat-scrtext_s = TEXT-a22.
        ls_fieldcat-scrtext_m = TEXT-a23.
        ls_fieldcat-scrtext_l = TEXT-a24.
        ls_fieldcat-seltext = TEXT-a24.
        ls_fieldcat-no_out = gc_true.
      WHEN 'LICCU'
        OR 'LICUM'.
        ls_fieldcat-qfieldname = ls_fieldinfo-reffield.
        ls_fieldcat-just = 'R'.
        ls_fieldcat-no_out = gc_true.
      WHEN 'STATUS'.
        ls_fieldcat-just = 'C'.
      WHEN OTHERS.
    ENDCASE.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDLOOP.

ENDFORM.                    " build_alv_structure


*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       Display the report in the ALV grid
*----------------------------------------------------------------------*
FORM data_display
  USING    is_variant  TYPE disvariant
  CHANGING ct_alv TYPE zgt_tt_docimp_01_alv.

  DATA: ls_layout   TYPE lvc_s_layo,
        lt_fieldcat TYPE lvc_t_fcat,
        lt_sort     TYPE lvc_t_sort.

* Build up the field catalogue and layout
  PERFORM build_alv_structure TABLES   lt_fieldcat
                                       lt_sort
                              CHANGING ls_layout.

* Display the grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
      is_layout_lvc            = ls_layout
      it_fieldcat_lvc          = lt_fieldcat
      it_sort_lvc              = lt_sort
      is_variant               = is_variant
    TABLES
      t_outtab                 = ct_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " data_display
*&---------------------------------------------------------------------*
*&      Form  GET_DOC_REF_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_doc_ref_data  USING    is_repdata TYPE zgt_s_docimp_01_rep
                      CHANGING  ct_alv     TYPE zgt_tt_docimp_01_alv
                                cv_found   TYPE xfeld
                                ct_res     TYPE gty_t_cdwlm.
**********************************************************************
  CONSTANTS: lc_papty TYPE /sapsll/papty VALUE 'NONE'.

  DATA: lt_result TYPE /sapsll/dpdata_t,
        lt_cdwlm  TYPE STANDARD TABLE OF /sapsll/cdwlm.

  DATA: ls_alv    TYPE zgt_s_docimp_01_alv,
        ls_ctsmxx TYPE gty_ctsmxx,
        ls_docimp TYPE zgt_docimp.

  DATA: lv_rcode TYPE int4.
**********************************************************************
* Get Certificates CUID
**********************************************************************
  PERFORM get_default_data USING    is_repdata
                                    zcl_gt_default_data=>mc_target_cuid
                           CHANGING lt_result.
**********************************************************************
* Get Declarations (Item) CUIE
**********************************************************************
  PERFORM get_default_data USING    is_repdata
                                    zcl_gt_default_data=>mc_target_cuie
                           CHANGING lt_result.
**********************************************************************
* Get Import papers CUII
**********************************************************************
  PERFORM get_default_data USING    is_repdata
                                    zcl_gt_default_data=>mc_target_cuii
                           CHANGING lt_result.
**********************************************************************
* Get Documents of community nature CUIS
**********************************************************************
  PERFORM get_default_data USING    is_repdata
                                    zcl_gt_default_data=>mc_target_cuis
                           CHANGING lt_result.
**********************************************************************
* Get Preference documents CUIP
**********************************************************************
  PERFORM get_default_data USING    is_repdata
                                    zcl_gt_default_data=>mc_target_cuip
                           CHANGING lt_result.

  IF lt_result IS NOT INITIAL.
    lt_cdwlm = VALUE #( FOR row IN lt_result ( papty = row-dpvaltrg datbi = row-datbi datab = row-datab ) ).
    SELECT mstid, papty FROM /sapsll/cdwlm
      FOR ALL ENTRIES IN @lt_cdwlm
      WHERE stctm EQ @zcl_gt_default_data=>mc_eu
      AND   ccngn EQ @is_repdata-impcn
      AND   papty EQ @lt_cdwlm-papty
      AND   datbi LE @lt_cdwlm-datbi
      AND   datab GE @lt_cdwlm-datab
      INTO TABLE @DATA(lt_base).
    IF sy-subrc EQ 0.
      READ TABLE lt_base INTO DATA(ls_base) INDEX 1.
      IF sy-subrc EQ 0.
        SELECT mstid, ccngn, papty FROM /sapsll/cdwlm
          FOR ALL ENTRIES IN @lt_cdwlm
          WHERE stctm EQ @zcl_gt_default_data=>mc_eu
          AND   mstid EQ @ls_base-mstid
          AND   ccngn EQ @is_repdata-impcn
          AND   datbi LE @lt_cdwlm-datbi
          AND   datab GE @lt_cdwlm-datab
          INTO TABLE @DATA(lt_res).
      ENDIF.
    ENDIF.
    cv_found = abap_true.
  ENDIF.

  IF lt_res IS NOT INITIAL.
    LOOP AT lt_base ASSIGNING FIELD-SYMBOL(<ls_base>).
      LOOP AT lt_res ASSIGNING FIELD-SYMBOL(<ls_res>)
        WHERE papty NE <ls_base>-papty.
        ct_res = VALUE #( ( new_papty = <ls_base>-papty old_papty = <ls_res>-papty ccngn = <ls_res>-ccngn ) ).
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  SORT lt_result BY dpvaltrg.
  DELETE ADJACENT DUPLICATES FROM lt_result COMPARING dpvaltrg.
  " IF document type is 'NONE' =>	Remove any document type on Item level in the Voorafsetje
  READ TABLE lt_result TRANSPORTING NO FIELDS WITH KEY dpvaltrg = lc_papty.
  IF sy-subrc EQ 0.
    DELETE FROM zgt_docimp WHERE delnum = is_repdata-delnum
                           AND   delitm = is_repdata-delitm.
    RETURN.
  ENDIF.

  LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
* - Check if Document already exists in Voorafsetje table
    SELECT SINGLE * FROM zgt_docimp INTO ls_docimp
      WHERE delnum = is_repdata-delnum
        AND delitm = is_repdata-delitm
        AND papty = <ls_result>-dpvaltrg.

* - If found, show in Results List (if all doc's selected)
    IF sy-subrc EQ 0.
      CLEAR ls_alv.

* --- Avoid for duplicate entries
      READ TABLE ct_alv WITH KEY
        delnum = is_repdata-delnum
        delitm = is_repdata-delitm
        papty  = <ls_result>-dpvaltrg
        TRANSPORTING NO FIELDS.

* --- Show in Results List if not a duplicate
      IF sy-subrc NE 0.
        MOVE: <ls_result>-dpvaltrg TO ls_alv-papty.
        "MOVE-CORRESPONDING ls_ctsmxx TO ls_alv.
        MOVE-CORRESPONDING is_repdata TO ls_alv.
        MOVE-CORRESPONDING ls_docimp TO ls_alv.
        IF ls_alv-papno IS INITIAL OR ls_alv-flvdo IS INITIAL.
          ls_alv-errsta = gc_msg_status-warning.
          APPEND ls_alv TO ct_alv.
        ELSE.
          IF p_fldec IS INITIAL.
            ls_alv-errsta = gc_msg_status-success.
            APPEND ls_alv TO ct_alv.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
* - Otherwise, create worklist entry as proposal
      MOVE: <ls_result>-dpvaltrg TO ls_ctsmxx-papty,
            is_repdata-cucoo     TO ls_ctsmxx-cntry.

      PERFORM measures_doc_add
        USING    ls_ctsmxx
                 is_repdata
        CHANGING ls_alv
                 lv_rcode.
      IF lv_rcode EQ 0.
        APPEND ls_alv TO ct_alv.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM get_default_data USING     is_repdata  TYPE zgt_s_docimp_01_rep
                                iv_target   TYPE /sapsll/dpfgen
                      CHANGING  ct_result   TYPE /sapsll/dpdata_t.
**********************************************************************
  DATA: ls_pk_ddata        TYPE /sapsll/dpdata_pk_s.

  DATA: lt_result TYPE /sapsll/dpdata_t.
**********************************************************************
* 1. try to get Default data based on article
**********************************************************************
  ls_pk_ddata-genpro = zcl_gt_default_data=>mc_genpro_cdinl.
  ls_pk_ddata-mandt  = sy-mandt.
  ls_pk_ddata-lgreg  = p_lgreg.
  ls_pk_ddata-dpfcmb = zcl_gt_default_data=>mc_fc_0100.
  ls_pk_ddata-dpkey  = is_repdata-guid_pr.

  ls_pk_ddata-target = iv_target.
  lt_result = zcl_gt_default_data=>get_instance( )->get_multi_default_data( is_pk   = ls_pk_ddata
                                                                            iv_date = sy-datum ).
**********************************************************************
*2. try to get Default data tariff code/country of origin
**********************************************************************
  IF lt_result IS INITIAL.
    ls_pk_ddata-genpro = zcl_gt_default_data=>mc_genpro_cdinl.
    ls_pk_ddata-mandt  = sy-mandt.
    ls_pk_ddata-lgreg  = p_lgreg.
    ls_pk_ddata-dpfcmb = zcl_gt_default_data=>mc_fc_tariff_cucoo.
    CONCATENATE  is_repdata-impcn is_repdata-cucoo INTO ls_pk_ddata-dpkey  RESPECTING BLANKS.

    ls_pk_ddata-target = iv_target.
    lt_result = zcl_gt_default_data=>get_instance( )->get_multi_default_data( is_pk   = ls_pk_ddata
                                                                              iv_date = sy-datum ).
  ENDIF.

**********************************************************************
*   3. try to get Default data tariff code
**********************************************************************
  IF lt_result IS INITIAL.
    ls_pk_ddata-genpro = zcl_gt_default_data=>mc_genpro_cdinl.
    ls_pk_ddata-mandt  = sy-mandt.
    ls_pk_ddata-lgreg  = p_lgreg.
    ls_pk_ddata-dpfcmb = zcl_gt_default_data=>mc_fc_tariff_code.
    ls_pk_ddata-dpkey  = is_repdata-impcn.
    ls_pk_ddata-target = iv_target.
    lt_result = zcl_gt_default_data=>get_instance( )->get_multi_default_data( is_pk   = ls_pk_ddata
                                                                              iv_date = sy-datum ).
  ENDIF.

  APPEND LINES OF lt_result TO ct_result.
ENDFORM.
