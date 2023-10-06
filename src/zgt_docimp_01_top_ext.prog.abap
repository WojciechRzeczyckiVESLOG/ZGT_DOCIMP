*&---------------------------------------------------------------------*
*&  Include           ZGT_DOCIMP_01_TOP
*&---------------------------------------------------------------------*

REPORT  zgt_docimp_01 MESSAGE-ID /sapsll/core_leg.

TYPE-POOLS: rsds, slis.

INCLUDE /sapsll/core_constants.
INCLUDE /sapsll/core_leg_constants.
INCLUDE /sapsll/product_constants.
INCLUDE /sapsll/core_fp_macros.

TABLES: zgt_s_sel,
        zgt_s_docimp_01_upd.

/sapsll/debug_data.

TYPES: BEGIN OF gty_pr_tariff,
         guid_pr     TYPE /sapsll/guid_pr,
         stcts       TYPE /sapsll/stcts,
         guid_ctsnum TYPE /sapsll/guid_16,
         datab       TYPE /sapsll/rptst,
         impcn       TYPE /sapsll/ccncu,
       END OF gty_pr_tariff,

       BEGIN OF gty_pedigree,
         guid_ctsnum TYPE /sapsll/guid_16,
         strcn       TYPE /sapsll/strcn,
         guid_ctsxxx TYPE /sapsll/guid_16,
         ccngn       TYPE /sapsll/ccngn,
       END OF gty_pedigree,

       BEGIN OF gty_cusb2,
         guid_lc_cusb2 TYPE	/sapsll/guid_16,
         lgreg         TYPE /sapsll/lgreg,
         guid_htsnum   TYPE /sapsll/guid_16,
         ctygr_oo      TYPE /sapsll/ctygr_oo,
         cucoo         TYPE /sapsll/cucoo,
         datab         TYPE /sapsll/datab,
         prfnw         TYPE /sapsll/prfnw,
         ctyex         TYPE /sapsll/ctyex,
       END OF gty_cusb2,

       gty_t_cusb2 TYPE STANDARD TABLE OF gty_cusb2 WITH DEFAULT KEY,

       BEGIN OF gty_ctsmxx,
         ccngn       TYPE /sapsll/ccngn,
         stctm       TYPE /sapsll/stctm,
         mstid       TYPE /sapsll/mstid,
         guid_ctsmai TYPE /sapsll/guid_16,
         mscid       TYPE /sapsll/mscid,
         ctygr       TYPE /sapsll/ctygr,
         cntry       TYPE /sapsll/land1,
         datbi       TYPE /sapsll/datbi,
         datab       TYPE /sapsll/datab,
         papty       TYPE /sapsll/papty,
       END OF gty_ctsmxx,

       BEGIN OF gty_cdwlm,
         old_papty TYPE /sapsll/papty,
         new_papty TYPE /sapsll/papty,
         ccngn     TYPE /sapsll/ccngn,
       END OF gty_cdwlm,

       gty_t_cdwlm TYPE STANDARD TABLE OF gty_cdwlm,

       BEGIN OF gty_ctsstxt,
         mstid TYPE /sapsll/mstid,
         text1 TYPE /sapsll/string,
       END OF gty_ctsstxt,

       BEGIN OF gty_cuscs,
         cuscs_papty TYPE /sapsll/cuscs,
         cuscs_papst TYPE /sapsll/cuscs,
         cuscs_papof TYPE /sapsll/cuscs,
         cuscs_quald TYPE /sapsll/cuscs,
       END OF gty_cuscs,

       BEGIN OF gty_papty_docus,
         qaldoc TYPE /sapsll/qaldoc,
         papty  TYPE /sapsll/papty,
         docus  TYPE /sapsll/docus,
       END OF gty_papty_docus.

DATA: gt_repdata      TYPE zgt_tt_docimp_01_rep,
      gt_docimp_01_ui TYPE zgt_tt_docimp_01_alv,
      gt_cucpat_tran  TYPE /sapsll/cucpat_t,
      gt_cucpat_pref  TYPE /sapsll/cucpat_t,
      gt_papty_docus  TYPE STANDARD TABLE OF gty_papty_docus,
      gt_ctygpa       TYPE /sapsll/ctygpa_t,
      gt_lc_cusb2     TYPE TABLE OF gty_cusb2,
      gt_pedigree     TYPE SORTED TABLE OF gty_pedigree
                           WITH UNIQUE KEY guid_ctsnum strcn,
      gt_ctsmxx       TYPE TABLE OF gty_ctsmxx,
      gs_cuscs        TYPE gty_cuscs,
      gs_variant      TYPE disvariant,
      gv_lgreg        TYPE /sapsll/lgreg,
      gv_grvsy        TYPE /sapsll/grvsy,
      gv_stcts        TYPE /sapsll/stcts,
      gv_stctm_tcots  TYPE /sapsll/stctm,
      gv_stctm_tlegsu TYPE /sapsll/stctm,
      gv_cuscs_papty  TYPE /sapsll/cuscs,
      gv_row_id       TYPE lvc_s_roid,
      gv_row_info     TYPE lvc_s_row,
      gv_col_info     TYPE lvc_s_col,
      gv_rfcdest      TYPE rfcdest.

*--> E227 Data Declaration
CONSTANTS: gc_inactive TYPE /sapsll/errsta VALUE 'I',
           gc_cat_cuip TYPE /sapsll/qaldoc VALUE 'CUIP'.
DATA: gt_return     TYPE STANDARD TABLE OF bapiret2.
*<-- E227 Data Declaration

CONSTANTS:
  gc_funcname TYPE rs38l_fnam VALUE 'ZGT_FM_OPEN_IMPORT_DELITMS_GET',
  BEGIN OF gc_variant_handle,
    vas TYPE slis_handl VALUE 'VAS',
  END   OF gc_variant_handle.

*&---------------------------------------------------------------------*
*&    Macro  macro_create_icon
*&---------------------------------------------------------------------*
*     Create the Status icon
*----------------------------------------------------------------------*
DEFINE macro_create_icon.
  PERFORM icon_create
    USING    &1
             &2
    CHANGING &3.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&    Macro  set_fieldcat_params
*&---------------------------------------------------------------------*
*     Transform the field info data into the field catalogue parameters
*----------------------------------------------------------------------*
DEFINE set_fieldcat_params.
  &2-tabname     = '1'.
  &2-datatype    = &1-ddic_type.
  &2-inttype     = &1-type.
  &2-intlen      = &1-leng.
  &2-dd_outlen   = &1-olen.
  &2-decimals    = &1-decs.
  &2-ref_table   = &1-name.
  &2-convexit    = &1-conexit.
  &2-fieldname   = &1-fieldname.
  &2-reptext     = &1-reptext.
  &2-scrtext_s   = &1-scrtext_s.
  &2-scrtext_m   = &1-scrtext_m.
  &2-scrtext_l   = &1-scrtext_l.
  &2-seltext     = &1-scrtext_l.
  &2-domname     = &1-domname.
END-OF-DEFINITION.
