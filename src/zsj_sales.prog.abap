REPORT ZSJ_SALES.
*&---------------------------------------------------------------------*
*& Report  Z_SALES_DEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

TYPE-POOLS: slis.
" for demo"
*BREAK-POINT.
CONSTANTS: c_first_tab TYPE slis_tabname VALUE 'ITAB1',
           c_second_tab TYPE slis_tabname VALUE 'ITAB2',
           c_colpos_order TYPE slis_fieldcat_alv-col_pos VALUE '1',
           c_field_order TYPE slis_fieldcat_alv-fieldname VALUE 'VBELN',
           c_tabname_order TYPE slis_fieldcat_alv-tabname  VALUE 'T_VBAK',
           c_reftab_order TYPE  slis_fieldcat_alv-ref_tabname   VALUE 'VBAK',
           c_reffld_order TYPE  slis_fieldcat_alv-ref_fieldname VALUE 'VBELN',
           c_colpos_po TYPE slis_fieldcat_alv-col_pos       VALUE '2',
           c_field_po TYPE slis_fieldcat_alv-fieldname     VALUE 'BSTNK',
           c_reffld_po TYPE slis_fieldcat_alv-ref_fieldname VALUE 'BSTNK',
           c_colpos_cdat TYPE slis_fieldcat_alv-col_pos       VALUE '3',
           c_field_cdat TYPE slis_fieldcat_alv-fieldname     VALUE 'ERDAT',
           c_reffld_cdat TYPE  slis_fieldcat_alv-ref_fieldname VALUE 'ERDAT',
           c_colpos_cust TYPE slis_fieldcat_alv-col_pos       VALUE '4',
           c_field_cust TYPE slis_fieldcat_alv-fieldname     VALUE 'KUNNR',
           c_reffld_cust TYPE  slis_fieldcat_alv-ref_fieldname VALUE 'KUNNR',
           c_order_low   TYPE vbak-vbeln VALUE '0060000100',
           c_colpos_orditm  TYPE   slis_fieldcat_alv-col_pos       VALUE '1',
          c_field_orditm  TYPE    slis_fieldcat_alv-fieldname     VALUE 'VBELN',
          c_tabname_orditm  TYPE    slis_fieldcat_alv-tabname       VALUE 'T_VBAP',
          c_reftab_orditm TYPE    slis_fieldcat_alv-ref_tabname   VALUE 'VBAP',
          c_colpos_mat  TYPE    slis_fieldcat_alv-col_pos       VALUE '2',
          c_field_mat TYPE    slis_fieldcat_alv-fieldname     VALUE 'MATNR',
          c_colpos_price  TYPE    slis_fieldcat_alv-col_pos       VALUE '3',
          c_field_price TYPE    slis_fieldcat_alv-fieldname     VALUE 'NETPR',
          c_colpos_currency TYPE    slis_fieldcat_alv-col_pos       VALUE '4',
          c_field_currency  TYPE    slis_fieldcat_alv-fieldname     VALUE 'WAERK',
          c_colpos_qty  TYPE    slis_fieldcat_alv-col_pos       VALUE '5',
          c_field_qty TYPE    slis_fieldcat_alv-fieldname     VALUE 'KWMENG',
          c_colpos_unit TYPE    slis_fieldcat_alv-col_pos       VALUE 6,
          c_field_unit  TYPE    slis_fieldcat_alv-fieldname     VALUE 'MEINS'.


* Data declarations.
DATA: BEGIN OF t_vbak OCCURS 0,
        vbeln TYPE vbeln,
        bstnk TYPE vbak-bstnk,
        erdat TYPE vbak-erdat,
        kunnr TYPE vbak-kunnr,
      END OF t_vbak.

DATA: BEGIN OF t_vbap OCCURS 0,
        vbeln  TYPE vbeln,
        matnr  TYPE vbap-matnr,
        netpr  TYPE vbap-netpr,
        waerk  TYPE vbap-waerk,
        kwmeng TYPE vbap-kwmeng,
        meins  TYPE vbap-meins,
      END OF t_vbap.

DATA: t_fieldcatalog1 TYPE slis_t_fieldcat_alv.
DATA: t_fieldcatalog2 TYPE slis_t_fieldcat_alv.
DATA: v_repid         TYPE syrepid.
DATA: wa_layout        TYPE slis_layout_alv.
DATA: v_tabname       TYPE slis_tabname.
DATA: t_events        TYPE slis_t_event.


* start-of-selection event.
START-OF-SELECTION.

  v_repid = sy-repid.

*  BREAK-POINT.

* Get the fieldcatalog for the first block
  PERFORM f_get_fieldcat1 CHANGING t_fieldcatalog1.

* Get the fieldcatalog for the second block
  PERFORM f_get_fieldcat2 CHANGING t_fieldcatalog2.

* Get the data for the first block
  SELECT vbeln bstnk erdat kunnr UP TO 10 ROWS
         INTO TABLE t_vbak
         FROM vbak WHERE vbeln > c_order_low.     "'0060000100'.
  IF sy-subrc = 0.
    SORT t_vbak BY vbeln.
  ENDIF.



* Get the data for the second block
  SELECT vbeln matnr netpr waerk kwmeng meins UP TO 10 ROWS
         INTO TABLE t_vbap
         FROM vbap WHERE vbeln > c_order_low.     "'0060000100'.
  IF sy-subrc = 0.
    SORT t_vbap BY vbeln matnr.
  ENDIF.
* init
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
    EXPORTING
      i_callback_program = v_repid.

* First block
  v_tabname = c_first_tab. "'ITAB1'.
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
    EXPORTING
      is_layout                  = wa_layout
      it_fieldcat                = t_fieldcatalog1
      i_tabname                  = v_tabname
      it_events                  = t_events
    TABLES
      t_outtab                   = t_vbak
    EXCEPTIONS
      program_error              = 1
      maximum_of_appends_reached = 2
      OTHERS                     = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* Second block
  v_tabname = c_second_tab. "'ITAB2'.
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
    EXPORTING
      is_layout                  = wa_layout
      it_fieldcat                = t_fieldcatalog2
      i_tabname                  = v_tabname
      it_events                  = t_events
    TABLES
      t_outtab                   = t_vbap
    EXCEPTIONS
      program_error              = 1
      maximum_of_appends_reached = 2
      OTHERS                     = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*Display
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


*---------------------------------------------------------------------*
*       FORM f_get_fieldcat1                                            *
*---------------------------------------------------------------------*
*       Get the field catalog for the first block                     *
*---------------------------------------------------------------------*
FORM f_get_fieldcat1 CHANGING c_fieldcatalog TYPE slis_t_fieldcat_alv.

  DATA: lwa_fieldcatalog TYPE slis_fieldcat_alv.

* Order number
  lwa_fieldcatalog-col_pos       = c_colpos_order.      "'1'.
  lwa_fieldcatalog-fieldname     = c_field_order.       "'VBELN'.
  lwa_fieldcatalog-tabname       = c_tabname_order.     "'T_VBAK'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_order.      "'VBAK'.
  lwa_fieldcatalog-ref_fieldname = c_reffld_order.      "'VBELN'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Customer purchase order.
  lwa_fieldcatalog-col_pos       = c_colpos_po.     "'2'.
  lwa_fieldcatalog-fieldname     = c_field_po.      "'BSTNK'.
  lwa_fieldcatalog-tabname       = c_tabname_order. "'T_VBAK'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_order.  "'VBAK'.
  lwa_fieldcatalog-ref_fieldname = c_reffld_po.     "'BSTNK'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Creation date.
  lwa_fieldcatalog-col_pos       = c_colpos_cdat.     "'3'.
  lwa_fieldcatalog-fieldname     = c_field_cdat.      "'ERDAT'.
  lwa_fieldcatalog-tabname       = c_tabname_order.   "'T_VBAK'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_order.    "'VBAK'.
  lwa_fieldcatalog-ref_fieldname = c_reffld_cdat.     "'ERDAT'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Customer
  lwa_fieldcatalog-col_pos       = c_colpos_cust.     "'4'.
  lwa_fieldcatalog-fieldname     = c_field_cust.      "'KUNNR'.
  lwa_fieldcatalog-tabname       = c_tabname_order.   "'T_VBAK'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_order.    "'VBAK'.
  lwa_fieldcatalog-ref_fieldname = c_reffld_cust.     "'KUNNR'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

ENDFORM.                    "f_get_fieldcat1

*---------------------------------------------------------------------*
*       FORM f_get_fieldcat2                                            *
*---------------------------------------------------------------------*
*       Get the field catalog for the second block                    *
*---------------------------------------------------------------------*
FORM f_get_fieldcat2 CHANGING c_fieldcatalog TYPE slis_t_fieldcat_alv.

  DATA: lwa_fieldcatalog TYPE slis_fieldcat_alv.

* Order number
  lwa_fieldcatalog-col_pos       = c_colpos_orditm.       "'1'.
  lwa_fieldcatalog-fieldname     = c_field_orditm.        "'VBELN'.
  lwa_fieldcatalog-tabname       = c_tabname_orditm.      "'T_VBAP'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_orditm.       "'VBAP'.
  lwa_fieldcatalog-ref_fieldname = c_field_orditm.        "'VBELN'.

  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Material number
  lwa_fieldcatalog-col_pos       = c_colpos_mat.        "'2'.
  lwa_fieldcatalog-fieldname     = c_field_mat.         "'MATNR'.
  lwa_fieldcatalog-tabname       = c_tabname_orditm.    "'T_VBAP'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_orditm.     "'VBAP'.
  lwa_fieldcatalog-ref_fieldname = c_field_mat.         "'MATNR'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Net price
  lwa_fieldcatalog-col_pos       = c_colpos_price.        "'3'.
  lwa_fieldcatalog-fieldname     = c_field_price.         "'NETPR'.
  lwa_fieldcatalog-tabname       = c_tabname_orditm.      "'T_VBAP'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_orditm.       "'VBAP'.
  lwa_fieldcatalog-ref_fieldname = c_field_price.         "'NETPR'.
  lwa_fieldcatalog-cfieldname    = c_field_currency.      "'WAERK'.
  lwa_fieldcatalog-ctabname      = c_tabname_orditm.      "'T_VBAP'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Currency.
  lwa_fieldcatalog-col_pos       = c_colpos_currency.     "'4'.
  lwa_fieldcatalog-fieldname     = c_field_currency.      "'WAERK'.
  lwa_fieldcatalog-tabname       = c_tabname_orditm.      "'T_VBAP'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_orditm.       "'VBAP'.
  lwa_fieldcatalog-ref_fieldname = c_field_currency.      "'WAERK'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* Quantity
  lwa_fieldcatalog-col_pos       = c_colpos_qty.          "'5'.
  lwa_fieldcatalog-fieldname     = c_field_qty.           "'KWMENG'.
  lwa_fieldcatalog-tabname       = c_tabname_orditm.      "'T_VBAP'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_orditm.       "'VBAP'.
  lwa_fieldcatalog-ref_fieldname = c_field_qty.           "'KWMENG'.
  lwa_fieldcatalog-qfieldname    = c_field_unit.          "'MEINS'.
  lwa_fieldcatalog-qtabname      = c_tabname_orditm.      "'T_VBAP'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.

* UOM
  lwa_fieldcatalog-col_pos       = c_colpos_unit.         "'6'.
  lwa_fieldcatalog-fieldname     = c_field_unit.          "'MEINS'.
  lwa_fieldcatalog-tabname       = c_tabname_orditm.      "'T_VBAP'.
  lwa_fieldcatalog-ref_tabname   = c_reftab_orditm.       "'VBAP'.
  lwa_fieldcatalog-ref_fieldname = c_field_unit.          "'MEINS'.
  APPEND lwa_fieldcatalog TO c_fieldcatalog.
  CLEAR lwa_fieldcatalog.
ENDFORM.                    "f_get_fieldcat2
