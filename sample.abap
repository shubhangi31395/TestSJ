*&---------------------------------------------------------------------*
 REPORT  zartd_r_cts_log_update.
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*
*REPORT z_cts_log_kc.
* Tables
 TABLES:e070,e071.
* Types
 TYPES : BEGIN OF gty_trkorr,
          strkorr TYPE strkorr,
         END OF gty_trkorr,
         BEGIN OF gty_obj_tr,
          strkorr  TYPE strkorr,
          obj_name TYPE trobj_name,
          object   TYPE trobjtype,
          trkorr   TYPE trkorr,
          pgmid    TYPE pgmid,
         END OF gty_obj_tr.

 TYPES:
  gtt_atc_db       TYPE STANDARD TABLE OF zartd_cts_atc,
  gtt_ci_db        TYPE STANDARD TABLE OF zartd_cts_ci,
  gtt_acrts_db     TYPE STANDARD TABLE OF zartd_cts_acrts,
  gtt_slin_db      TYPE STANDARD TABLE OF zartd_cts_slin,
  gtt_trkorr       TYPE STANDARD TABLE OF gty_trkorr,
  gtt_obj_tr       TYPE STANDARD TABLE OF gty_obj_tr,
  gtt_cts_log      TYPE STANDARD TABLE OF zartd_tskcts_log,
  gtt_cts_appr     TYPE STANDARD TABLE OF zartd_cts_appr,
  gtt_tadir        TYPE STANDARD TABLE OF tadir,
  gtt_trdir        TYPE STANDARD TABLE OF trdir,
  gtt_cre_dt       TYPE STANDARD TABLE OF e070create.

* Range Type
 TYPES:
  gtr_trkorr      TYPE RANGE OF trkorr.
* Work Area
 DATA:
  gwa_e070        TYPE e070,
  gwa_e070_tsk    TYPE e070,
  gwa_e071        TYPE e071,
  gwa_atc_db      TYPE zartd_cts_atc,
  gwa_ci_db       TYPE zartd_cts_ci,
  gwa_acrts_db    TYPE zartd_cts_acrts,
  gwa_slin_db     TYPE zartd_cts_slin,
  gwa_obj_tr      TYPE gty_obj_tr.

* Internal table
 DATA:
 gt_tr_project    TYPE STANDARD TABLE OF zartd_tr_project,
 gt_e070a         TYPE STANDARD TABLE OF e070a,
 gt_e070          TYPE e070_t,
 gt_e070_tsk      TYPE e070_t,
 gt_e071          TYPE e071_t,
 gt_atc_db        TYPE gtt_atc_db,
 gt_ci_db         TYPE gtt_ci_db,
 gt_acrts_db      TYPE gtt_acrts_db,
 gt_slin_db       TYPE gtt_slin_db,
 gt_artd_db_tr    TYPE gtt_trkorr,
 gt_obj_tr        TYPE gtt_obj_tr,
 gt_ctslog_db     TYPE gtt_cts_log,
 gt_cts_appr      TYPE gtt_cts_appr,
 gt_tadir         TYPE gtt_tadir,
 gt_trdir         TYPE gtt_trdir,
 gt_cre_dt        TYPE gtt_cre_dt.

* Field Symbol
 FIELD-SYMBOLS : <gwa_cts_log>  TYPE zartd_tskcts_log.
* Global Variables
 DATA:
  lv_tabix        TYPE sytabix,
  gr_task         TYPE gtr_trkorr.

* Reference Object
 DATA
 lo_artd_excp     TYPE REF TO zcx_artd_exception_cls.

 SELECT-OPTIONS: s_trkorr FOR e070-trkorr NO INTERVALS,
                 so_date FOR sy-datum ."DEFAULT sy-datum.



 INITIALIZATION.

 START-OF-SELECTION.
*
* Generate exception Object
   CREATE OBJECT lo_artd_excp.

* Create task range table
   PERFORM f_populate_task CHANGING gr_task.

* Get ARTD DB related data
   PERFORM f_artd_db_data USING gr_task CHANGING gt_atc_db gt_slin_db gt_ci_db gt_acrts_db.
*
* Get Main TRs from ARTD DB
   PERFORM f_get_tr_artd_db USING gt_atc_db gt_slin_db gt_ci_db gt_acrts_db
                      CHANGING gt_artd_db_tr.

* Get Customizing TR
   PERFORM f_get_cust_tr USING gr_task CHANGING gt_artd_db_tr.

* Get Main TR and Task Details header details
   IF gt_artd_db_tr IS NOT INITIAL.
     SELECT * FROM e070 INTO TABLE gt_e070
                FOR ALL ENTRIES IN gt_artd_db_tr WHERE trkorr   = gt_artd_db_tr-strkorr
                                                    OR strkorr  = gt_artd_db_tr-strkorr.

   ENDIF.
   IF sy-subrc <> 0 OR gt_e070 IS INITIAL.
     CALL METHOD lo_artd_excp->update_excp_db_direct
       EXPORTING
         iv_trkorr = 'CTS'
         iv_progid = sy-repid
         iv_text   = 'Data not found in E070'.
     LEAVE PROGRAM.
   ENDIF.

* Get Task details extracted from GT_E070 table and remove the entries from GT_E070
   SORT gt_e070 BY strkorr.
   LOOP AT gt_e070 INTO gwa_e070.
     lv_tabix = sy-tabix.
     IF gwa_e070-strkorr IS NOT INITIAL AND gwa_e070-trstatus = 'R'.
       MOVE gwa_e070 TO gwa_e070_tsk.
       APPEND gwa_e070_tsk TO gt_e070_tsk.
       DELETE gt_e070 INDEX lv_tabix.
     ELSEIF gwa_e070-strkorr IS NOT INITIAL AND gwa_e070-trstatus <> 'R'.
       DELETE gt_e070 INDEX lv_tabix.
     ENDIF.
     CLEAR: gwa_e070,gwa_e070_tsk.
   ENDLOOP.

* Get Task Details
   SELECT * FROM e071 INTO TABLE gt_e071
              FOR ALL ENTRIES IN gt_e070_tsk WHERE trkorr = gt_e070_tsk-trkorr.
   IF sy-subrc <> 0 OR gt_e071 IS INITIAL.
     CALL METHOD lo_artd_excp->update_excp_db_direct
       EXPORTING
         iv_trkorr = 'CTS'
         iv_progid = sy-repid
         iv_text   = 'Data not found in E071'.
     LEAVE PROGRAM.
   ENDIF.
*
* Get data in table which will be used to consolidate data.
   CLEAR gwa_e070.
   SORT gt_e070_tsk BY trkorr.
   LOOP AT gt_e071 INTO gwa_e071.
     READ TABLE gt_e070_tsk INTO gwa_e070_tsk WITH KEY trkorr = gwa_e071-trkorr BINARY SEARCH.
     CHECK sy-subrc = 0.
     gwa_obj_tr-strkorr   = gwa_e070_tsk-strkorr.
     gwa_obj_tr-object    = gwa_e071-object.
     gwa_obj_tr-obj_name  = gwa_e071-obj_name.
     gwa_obj_tr-trkorr    = gwa_e071-trkorr.
     gwa_obj_tr-pgmid     = gwa_e071-pgmid.
     APPEND gwa_obj_tr TO gt_obj_tr.
     CLEAR: gwa_e071,gwa_obj_tr,gwa_e070.
   ENDLOOP.

* Get Approval Details
   SELECT * FROM zartd_cts_appr INTO TABLE gt_cts_appr.
   IF sy-subrc = 0.
     SORT gt_cts_appr BY trkorr.
   ENDIF.

* Get Directory details
   SELECT *
     FROM tadir INTO TABLE gt_tadir
    WHERE srcsystem <> 'SAP'
      AND author    <> 'SAP'
      AND devclass  <> '$TMP'.
   IF sy-subrc = 0.
     SORT gt_tadir BY object obj_name.
* Get type of Program
     SELECT * FROM trdir
        INTO TABLE gt_trdir
   FOR ALL ENTRIES IN gt_tadir WHERE name = gt_tadir-obj_name.
     IF sy-subrc = 0.
       SORT gt_trdir  BY name.
     ENDIF.
   ENDIF.

*    DATA : gt_cre_dt TYPE STANDARD TABLE OF e070create.
   SELECT  * FROM e070create INTO TABLE gt_cre_dt WHERE trkorr  IN s_trkorr.

* Consolidate data
   PERFORM f_consolidate_data USING gt_obj_tr gt_e070 gt_e070_tsk gt_e071 gt_atc_db gt_slin_db gt_ci_db gt_acrts_db
                                    gt_cts_appr gt_tadir gt_trdir gt_cre_dt
                           CHANGING gt_ctslog_db.


*Retain date & time if TR is Released.
*  PERFORM f_date_set CHANGING gt_ctslog_db.

* Changing the Object type
   LOOP AT gt_ctslog_db ASSIGNING <gwa_cts_log> WHERE obj_type = 'REPS' OR obj_type = 'CLSD'.
     IF <gwa_cts_log>-obj_type = 'REPS'.
       <gwa_cts_log>-obj_type = 'PROG'.
       <gwa_cts_log>-pgmid    =  'R3TR'.
     ELSE.
       <gwa_cts_log>-obj_type = 'CLAS'.
       <gwa_cts_log>-pgmid    =  'R3TR'.
     ENDIF.
   ENDLOOP.

* Update CTS LOG Table
   MODIFY zartd_tskcts_log FROM TABLE gt_ctslog_db.
   IF sy-subrc = 0.
*     CALL FUNCTION 'ZARTD_AUDIT_UPDATE' DESTINATION 'SM_SM2CLNT100_BACK'
     CALL FUNCTION 'ZARTD_AUDIT_UPDATE' DESTINATION 'SM_SM2CLNT100_TRUSTED'
       TABLES
         t_tskcts_log = gt_ctslog_db.
     COMMIT WORK.
     WRITE: / 'CTS summary log updated successfully'(001).
   ELSE.
     CALL METHOD lo_artd_excp->update_excp_db_direct
       EXPORTING
         iv_trkorr = 'CTS'
         iv_progid = sy-repid
         iv_text   = 'CTS Log table update failed'.
     LEAVE PROGRAM.
   ENDIF.
*
* Initiate Transfer to Solution Manager System SM2
*  PERFORM f_init_dbdet_trnsf USING gt_ctslog_db.
*
* Add Functionality for feature for Notification & WB Approval
   PERFORM f_addfunc_check_for_features.

**call WB Manual Approval workflow
*  DATA: lwa_ztrcheck  TYPE ztrcheck.
**  lo_artd_excp  type ref to zcx_artd_exception_cls.
*
**  SELECT SINGLE *
**    FROM ztrcheck
**    INTO lwa_ztrcheck
**   WHERE id = 'WBAPPROVAL'.
**  IF sy-subrc = 0 AND lwa_ztrcheck-severity = abap_true.
**    TRY.
**        CALL METHOD zcl_artd_wf_manualappr_tr=>run_chk_for_manuappr.
**      CATCH zcx_artd_exception_cls INTO lo_artd_excp .
**        lo_artd_excp->update_excp_db( iv_trkorr = 'CTS' ).
**    ENDTRY.
**  ENDIF.
*
*  TRY.
*      CALL METHOD zcl_artd_wf_manualappr_tr=>run_chk_for_manuappr
**  EXPORTING
**    it_trkorr =
*          .
*    CATCH zcx_artd_exception_cls INTO lo_artd_excp.
*      lo_artd_excp->update_excp_db( iv_trkorr = 'CTS' ).
*  ENDTRY.

 END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_ARTD_DB_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_artd_db_data USING ir_task      TYPE gtr_trkorr
                  CHANGING ct_atc_db    TYPE gtt_atc_db
                           ct_slin_db   TYPE gtt_slin_db
                           ct_ci_db     TYPE gtt_ci_db
                           ct_acrts_db  TYPE gtt_acrts_db.


* Get ATC Data
*   added so_date
   IF so_date[] IS INITIAL.
     so_date-sign = 'I'.
     so_date-option = 'BT'.
     so_date-low = sy-datum.
     so_date-high = sy-datum.
     APPEND so_date.
   ENDIF.
   SELECT * FROM zartd_cts_atc INTO TABLE ct_atc_db WHERE trkorr IN ir_task OR as4date IN so_date ."*******kchoudhary added so_date
   IF sy-subrc = 0.
     SORT ct_atc_db BY strkorr ASCENDING
                       obj_type ASCENDING
                       object ASCENDING
                       as4date DESCENDING
                       as4time DESCENDING.
     DELETE ADJACENT DUPLICATES FROM ct_atc_db  COMPARING strkorr trkorr obj_type object.
   ENDIF.
* Get SLIN Data
   SELECT * FROM zartd_cts_slin INTO TABLE ct_slin_db WHERE trkorr IN ir_task OR as4date IN so_date."*******kchoudhary added so_date
   IF sy-subrc = 0.
     SORT ct_slin_db BY strkorr ASCENDING
                        obj_type ASCENDING
                        object ASCENDING
                        as4date DESCENDING
                        as4time DESCENDING.
     DELETE ADJACENT DUPLICATES FROM ct_slin_db  COMPARING strkorr  trkorr obj_type object.
   ENDIF.
* Get CI Data
   SELECT * FROM zartd_cts_ci INTO TABLE ct_ci_db WHERE trkorr IN ir_task OR  as4date IN so_date."*******kchoudhary added so_date
   IF sy-subrc = 0.
     SORT ct_ci_db BY strkorr ASCENDING
                       obj_type ASCENDING
                       object ASCENDING
                       as4date DESCENDING
                       as4time DESCENDING.
     DELETE ADJACENT DUPLICATES FROM ct_ci_db  COMPARING strkorr trkorr  obj_type  object.
   ENDIF.
* Ger ACRTS Data
   SELECT * FROM zartd_cts_acrts INTO TABLE ct_acrts_db WHERE trkorr IN ir_task OR  as4date IN so_date ."*******kchoudhary added so_date
   IF sy-subrc = 0.
     SORT ct_acrts_db BY strkorr ASCENDING
                         obj_type ASCENDING
                         object ASCENDING
                         as4date DESCENDING
                         as4time DESCENDING.
     DELETE ADJACENT DUPLICATES FROM ct_acrts_db  COMPARING strkorr trkorr obj_type object.
   ENDIF.


 ENDFORM.                    " F_ARTD_DB_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_TR_ARTD_DB
*&---------------------------------------------------------------------*
* Get Main TR from ARTD DB which will be used to extra details
* from E070 and E071
*----------------------------------------------------------------------*
 FORM f_get_tr_artd_db  USING it_atc_db   TYPE gtt_atc_db
                           it_slin_db     TYPE gtt_slin_db
                           it_ci_db       TYPE gtt_ci_db
                           it_acrts_db    TYPE gtt_acrts_db
                  CHANGING ct_artd_db_tr  TYPE gtt_trkorr.

   DATA: lwa_trkorr TYPE gty_trkorr.
   CLEAR: gwa_atc_db,gwa_slin_db,gwa_ci_db,gwa_acrts_db.

   LOOP AT it_atc_db INTO gwa_atc_db.
     CHECK gwa_atc_db-strkorr IS NOT INITIAL.
     lwa_trkorr-strkorr = gwa_atc_db-strkorr.
     APPEND lwa_trkorr TO ct_artd_db_tr.
   ENDLOOP.
   LOOP AT it_slin_db INTO gwa_slin_db.
     CHECK gwa_slin_db-strkorr IS NOT INITIAL.
     lwa_trkorr-strkorr = gwa_slin_db-strkorr.
     APPEND lwa_trkorr TO ct_artd_db_tr.
   ENDLOOP.
   LOOP AT it_ci_db INTO gwa_ci_db.
     CHECK gwa_ci_db-strkorr IS NOT INITIAL.
     lwa_trkorr-strkorr = gwa_ci_db-strkorr.
     APPEND lwa_trkorr TO ct_artd_db_tr.
   ENDLOOP.
   LOOP AT it_acrts_db INTO gwa_acrts_db.
     CHECK gwa_acrts_db-strkorr IS NOT INITIAL.
     lwa_trkorr-strkorr = gwa_acrts_db-strkorr.
     APPEND lwa_trkorr TO ct_artd_db_tr.
   ENDLOOP.

   SORT ct_artd_db_tr.
   DELETE ADJACENT DUPLICATES FROM ct_artd_db_tr.

 ENDFORM.                    " F_GET_TR_ARTD_DB
*&---------------------------------------------------------------------*
*&      Form  F_CONSOLIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_consolidate_data  USING it_obj_tr    TYPE gtt_obj_tr
                                it_e070      TYPE e070_t
                                it_e070_tsk  TYPE e070_t
                                it_e071      TYPE e071_t
                                it_atc_db    TYPE gtt_atc_db
                                it_slin_db   TYPE gtt_slin_db
                                it_ci_db     TYPE gtt_ci_db
                                it_acrts_db  TYPE gtt_acrts_db
                                it_cts_appr  TYPE gtt_cts_appr
                                it_tadir     TYPE gtt_tadir
                                it_trdir     TYPE gtt_trdir
                                it_cre_dt    TYPE gtt_cre_dt

                       CHANGING ct_ctslog_db TYPE gtt_cts_log.
   DATA:
    lwa_obj_tr       TYPE gty_obj_tr,
    lwa_obj_tr_tmp   TYPE gty_obj_tr,
    lwa_e070         TYPE e070,
    lwa_e070_tsk     TYPE e070,
    lwa_e071         TYPE e071,
    lwa_ctslog       TYPE zartd_tskcts_log,
    lwa_cts_appr     TYPE zartd_cts_appr,
    lv_system        TYPE char10,
    lv_timestamp     TYPE string,
    lwa_cre_dt       TYPE e070create.
   "lwa_trdir        TYPE trdir.

   SORT: it_atc_db   BY trkorr obj_type object,
         it_slin_db  BY trkorr obj_type object,
         it_ci_db    BY trkorr obj_type object,
         it_acrts_db BY trkorr obj_type object.

*
   SORT: it_e070     BY trkorr,
         it_e070_tsk BY trkorr,
         it_obj_tr.

   LOOP AT it_obj_tr INTO lwa_obj_tr.

     lwa_obj_tr_tmp = lwa_obj_tr.

     AT END OF trkorr.
       lwa_ctslog-trkorr   = lwa_obj_tr_tmp-trkorr. "
       lwa_ctslog-obj_type = lwa_obj_tr_tmp-object.
       lwa_ctslog-object   = lwa_obj_tr_tmp-obj_name.
       lwa_ctslog-pgmid    = lwa_obj_tr_tmp-pgmid.
       lwa_ctslog-strkorr     = lwa_obj_tr_tmp-strkorr. "

* Get Current system details
       CONCATENATE sy-sysid sy-mandt INTO lv_system SEPARATED BY '.'.
       CONDENSE lv_system.
       lwa_ctslog-tarsystem  = lv_system.
*
       CLEAR:lwa_e070,lwa_e070_tsk.
       READ TABLE it_e070 INTO lwa_e070 WITH KEY trkorr = lwa_obj_tr_tmp-strkorr BINARY SEARCH.
       IF sy-subrc = 0.
         lwa_ctslog-trfunction = lwa_e070-trfunction.
         lwa_ctslog-trstatus   = lwa_e070-trstatus.
*        lwa_ctslog-strkorr     = lwa_e070-strkorr. "

*>>+ Phani on 03/03/2017. TimeStamp Updation - Main TR.
         IF lwa_ctslog-trstatus = 'R'.
           CONCATENATE lwa_e070-as4date lwa_e070-as4time INTO lv_timestamp.
           lwa_ctslog-tr_rel_timestamp = lv_timestamp.
           CLEAR lv_timestamp.
         ENDIF.

*<<+ Phani on 03/03/2017. TimeStamp Updation - Main TR.
         DATA : lv_sys        TYPE string .
*         Get source system details

         CONCATENATE syst-sysid syst-mandt INTO lv_sys.
         CONDENSE lv_sys.
         lwa_ctslog-sorsystem = lv_sys.
*       lwa_ctslog-tarsystem  = lwa_e070-tarsystem.
       ENDIF.
       READ TABLE it_e070_tsk INTO lwa_e070_tsk WITH KEY trkorr = lwa_obj_tr_tmp-trkorr BINARY SEARCH.
       IF sy-subrc = 0.
         lwa_ctslog-as4user  = lwa_e070_tsk-as4user.
         lwa_ctslog-tsk_chng_date  = lwa_e070_tsk-as4date.
         lwa_ctslog-tsk_chng_time  = lwa_e070_tsk-as4time.
         lwa_ctslog-author   = lwa_e070_tsk-as4user.

       ENDIF.
* Get ATC data
       PERFORM f_get_atc_artd_det USING lwa_obj_tr_tmp it_atc_db CHANGING lwa_ctslog.
*
* Get SLIN data
       PERFORM f_get_slin_artd_det USING lwa_obj_tr_tmp it_slin_db CHANGING lwa_ctslog.
*
* Get CI data
       PERFORM f_get_ci_artd_det USING lwa_obj_tr_tmp it_ci_db CHANGING lwa_ctslog.
*
* Get ACRTS Data
       PERFORM f_get_acrts_artd_det USING lwa_obj_tr_tmp it_acrts_db CHANGING lwa_ctslog.
*
* Get Approval Details
       CLEAR lwa_cts_appr.
       READ TABLE it_cts_appr INTO lwa_cts_appr WITH KEY trkorr = lwa_obj_tr_tmp-strkorr
                                                  BINARY SEARCH.
       IF sy-subrc = 0.
         lwa_ctslog-trstatus_appr = abap_true.
*>>+ Phani on 03/03/2017. TimeStamp Updation - WB/Cust Approval.
         lwa_ctslog-tr_approver       = lwa_cts_appr-as4user.
         lwa_ctslog-tr_appr_timestamp = lwa_cts_appr-appr_timestamp.
*       CONCATENATE lwa_cts_appr-as4date lwa_cts_appr-as4time INTO lv_timestamp.
*       CLEAR lv_timestamp.
*<<+ Phani on 03/03/2017. TimeStamp Updation - WB/Cust Approval.
       ENDIF.
*
* Get Object Repository Details (Package & Author)
       PERFORM get_respositoy_det USING lwa_obj_tr_tmp it_tadir CHANGING lwa_ctslog.
*
** Get Object type
*      CLEAR lwa_trdir.
*      READ TABLE it_trdir INTO lwa_trdir WITH KEY name = lwa_obj_tr_tmp-object
*                                    BINARY SEARCH.
*      IF sy-subrc = 0.
*        lwa_ctslog-subc = lwa_trdir-subc.
*      ENDIF.

       READ TABLE it_cre_dt INTO lwa_cre_dt WITH KEY trkorr = lwa_obj_tr_tmp-strkorr.
       IF sy-subrc = 0.
         lwa_ctslog-tr_crt_date   = lwa_cre_dt-cre_date.
         lwa_ctslog-tr_crt_time   = lwa_cre_dt-cre_time.
         lwa_ctslog-tr_owner      = lwa_cre_dt-cre_user.
       ENDIF.


       APPEND lwa_ctslog TO ct_ctslog_db.
       CLEAR lwa_ctslog.
     ENDAT.


     CLEAR:lwa_obj_tr,lwa_obj_tr_tmp.
   ENDLOOP.
*
 ENDFORM.                    " F_CONSOLIDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_ATC_ARTD_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_get_atc_artd_det USING is_obj_tr TYPE gty_obj_tr
                               it_atc_db TYPE gtt_atc_db
                      CHANGING cs_ctslog TYPE zartd_tskcts_log.

   DATA: lwa_atc_db       TYPE zartd_cts_atc.

   READ TABLE it_atc_db INTO lwa_atc_db WITH KEY trkorr = is_obj_tr-trkorr
                                               obj_type = is_obj_tr-object
                                                 object = is_obj_tr-obj_name
                                                 BINARY SEARCH.
   IF sy-subrc = 0.
     cs_ctslog-atc_prio1 = lwa_atc_db-atc_prio1.
     cs_ctslog-atc_prio2 = lwa_atc_db-atc_prio2.
     cs_ctslog-atc_prio3 = lwa_atc_db-atc_prio3.
     cs_ctslog-atc_prio4 = lwa_atc_db-atc_prio4.
     IF lwa_atc_db-trkorrs IS NOT INITIAL.
       cs_ctslog-trkorrs = lwa_atc_db-trkorrs.
     ENDIF.
     CLEAR lwa_atc_db.
   ENDIF.


 ENDFORM.                    " F_GET_ATC_ARTD_DET
*&---------------------------------------------------------------------*
*&      Form  F_GET_SLIN_ARTD_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_get_slin_artd_det USING is_obj_tr  TYPE gty_obj_tr
                                it_slin_db TYPE gtt_slin_db
                       CHANGING cs_ctslog  TYPE zartd_tskcts_log.

   DATA: lwa_slin_db      TYPE zartd_cts_slin.

   READ TABLE it_slin_db INTO lwa_slin_db WITH KEY trkorr = is_obj_tr-trkorr
                                                 obj_type = is_obj_tr-object
                                                   object = is_obj_tr-obj_name
                                                   BINARY SEARCH.
   IF sy-subrc = 0.
     cs_ctslog-slin_ecount = lwa_slin_db-slin_ecount.
     cs_ctslog-slin_wcount = lwa_slin_db-slin_wcount.
     cs_ctslog-slin_mcount = lwa_slin_db-slin_mcount.
     IF lwa_slin_db-trkorrs IS NOT INITIAL.
       cs_ctslog-trkorrs = lwa_slin_db-trkorrs.
     ENDIF.
     CLEAR lwa_slin_db.
   ENDIF.

 ENDFORM.                    " F_GET_SLIN_ARTD_DET
*&---------------------------------------------------------------------*
*&      Form  F_GET_CI_ARTD_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_get_ci_artd_det USING is_obj_tr  TYPE gty_obj_tr
                              it_ci_db   TYPE gtt_ci_db
                     CHANGING cs_ctslog  TYPE zartd_tskcts_log.

   DATA: lwa_ci_db  TYPE zartd_cts_ci.

   READ TABLE it_ci_db INTO lwa_ci_db WITH KEY trkorr = is_obj_tr-trkorr
                                             obj_type = is_obj_tr-object
                                               object = is_obj_tr-obj_name
                                               BINARY SEARCH.
   IF sy-subrc = 0.
     cs_ctslog-ci_ecount = lwa_ci_db-ci_ecount.
     cs_ctslog-ci_wcount = lwa_ci_db-ci_wcount.
     cs_ctslog-ci_mcount = lwa_ci_db-ci_mcount.
     IF lwa_ci_db-trkorrs IS NOT INITIAL.
       cs_ctslog-trkorrs = lwa_ci_db-trkorrs.
     ENDIF.
     CLEAR lwa_ci_db.
   ENDIF.


 ENDFORM.                    " F_GET_CI_ARTD_DET
*&---------------------------------------------------------------------*
*&      Form  F_GET_ACRTS_ARTD_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_get_acrts_artd_det USING is_obj_tr    TYPE gty_obj_tr
                                 it_acrts_db  TYPE gtt_acrts_db
                        CHANGING cs_ctslog    TYPE zartd_tskcts_log.

   DATA lwa_acrts_db     TYPE zartd_cts_acrts.

   READ TABLE it_acrts_db INTO lwa_acrts_db WITH KEY trkorr = is_obj_tr-trkorr
                                                   obj_type = is_obj_tr-object
                                                     object = is_obj_tr-obj_name
                                                     BINARY SEARCH.
   IF sy-subrc = 0.
     cs_ctslog-acrts_ecount = lwa_acrts_db-acrts_ecount.
     cs_ctslog-acrts_wcount = lwa_acrts_db-acrts_wcount.
     cs_ctslog-acrts_mcount = lwa_acrts_db-acrts_mcount.
     IF lwa_acrts_db-trkorrs IS NOT INITIAL.
       cs_ctslog-trkorrs = lwa_acrts_db-trkorrs.
     ENDIF.
     CLEAR lwa_acrts_db.
   ENDIF.

 ENDFORM.                    " F_GET_ACRTS_ARTD_DET
*&---------------------------------------------------------------------*
*&      Form  F_INIT_DBDET_TRNSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f_init_dbdet_trnsf USING it_ctslog_db TYPE gtt_cts_log.
*
*  DATA: lwa_input            TYPE zztl_bcfxxxl_cts_log_rfc,
*        lwa_output           TYPE zztl_bcfxxxl_cts_log_rfcrespon ,
**        lwa_table_of_cts_log TYPE ztable_of_ZARTD_TSKCTS_LOG,
*        lt_cts_log_rfc       TYPE zztltbc_cts_log_tab,
*        lwa_cts_log_rfc      TYPE zztltbc_cts_log,
*        lr_proxy             TYPE REF TO zartdco_z_cts_log_ws,
*        lwa_ctslog_db        TYPE ZARTD_TSKCTS_LOG,
*        lo_excp              TYPE REF TO cx_root,
*        lv_text              TYPE char100.
*
*  LOOP AT it_ctslog_db INTO lwa_ctslog_db.
*    MOVE-CORRESPONDING lwa_ctslog_db TO lwa_cts_log_rfc.
*    APPEND lwa_cts_log_rfc TO lt_cts_log_rfc.
*  ENDLOOP.
*
*  CREATE OBJECT lr_proxy
*    EXPORTING
*      logical_port_name = 'Z_LP_CTS_LOG_WS'.
*
*  lwa_table_of_cts_log-item = lt_cts_log_rfc.
*
*  lwa_input-t_cts_log = lwa_table_of_cts_log.
*
*  TRY.
*      CALL METHOD lr_proxy->ztl_bcfxxxl_cts_log_rfc
*        EXPORTING
*          input  = lwa_input
*        IMPORTING
*          output = lwa_output.
*    CATCH cx_ai_system_fault INTO lo_excp.
*    CATCH cx_ai_application_fault INTO lo_excp.
*      lv_text = lo_excp->get_text( ).
*      CALL METHOD lo_artd_excp->update_excp_db_direct
*        EXPORTING
*          iv_trkorr = 'CTS'
*          iv_progid = sy-repid
*          iv_text   = lv_text.
*  ENDTRY.
*
*ENDFORM.                    " F_INIT_DBDET_TRNSF
*&---------------------------------------------------------------------*
*&      Form  F_POPULATE_TASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_populate_task CHANGING cr_task TYPE gtr_trkorr.

   DATA: lt_e070     TYPE e070_t,
         lwa_e070    TYPE e070,
         lwa_rng_tr  LIKE LINE OF s_trkorr,
         lwa_rng_tsk LIKE LINE OF cr_task.

* When Request or task is passed
   IF s_trkorr IS INITIAL.
     EXIT.
   ENDIF.
* Get details from the Input to judge if its task or TR
   SELECT * FROM e070 INTO TABLE lt_e070 WHERE trkorr  IN s_trkorr
                                            OR strkorr IN s_trkorr.
   IF sy-subrc <> 0.
     CALL METHOD lo_artd_excp->update_excp_db_direct
       EXPORTING
         iv_trkorr = 'CTS'
         iv_progid = sy-repid
         iv_text   = 'Data not found in E070'.
     LEAVE PROGRAM.
   ENDIF.
*
   SORT lt_e070 BY strkorr.
   LOOP AT s_trkorr INTO lwa_rng_tr.
     READ TABLE lt_e070 TRANSPORTING NO FIELDS WITH KEY strkorr = lwa_rng_tr-low BINARY SEARCH.
     IF sy-subrc = 0. " For TR
*       LOOP AT lt_e070 INTO lwa_e070 WHERE ( trstatus = 'R' OR trstatus = 'O' )
*                                       AND strkorr = lwa_rng_tr-low.
       LOOP AT lt_e070 INTO lwa_e070 WHERE strkorr = lwa_rng_tr-low.
         lwa_rng_tsk-sign    = 'I'.
         lwa_rng_tsk-option  = 'EQ'.
         lwa_rng_tsk-low     = lwa_e070-trkorr.
         APPEND lwa_rng_tsk TO cr_task.
       ENDLOOP.
     ELSE.            " For Task
       lwa_rng_tsk-sign    = 'I'.
       lwa_rng_tsk-option  = 'EQ'.
       lwa_rng_tsk-low     = lwa_rng_tr-low.
       APPEND lwa_rng_tsk TO cr_task.
     ENDIF.
   ENDLOOP.


 ENDFORM.                    " F_POPULATE_TASK
*&---------------------------------------------------------------------*
*&      Form  GET_RESPOSITOY_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM get_respositoy_det  USING is_obj_tr TYPE gty_obj_tr
                                it_tadir  TYPE gtt_tadir
                       CHANGING cs_ctslog TYPE zartd_tskcts_log.

   DATA: lwa_tadir        TYPE tadir,
         lv_r3tr_objtype  TYPE trobjtype,
         lv_r3tr_objname  TYPE trobj_name.

   IF is_obj_tr-pgmid = 'R3TR'.
     IF is_obj_tr-object = 'DTEL' OR is_obj_tr-object = 'DOMA'.
       CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
         EXPORTING
           p_limu_objtype = is_obj_tr-object
           p_limu_objname = is_obj_tr-obj_name
         IMPORTING
           p_r3tr_objtype = lv_r3tr_objtype
           p_r3tr_objname = lv_r3tr_objname
         EXCEPTIONS
           no_mapping     = 1
           OTHERS         = 2.
       IF sy-subrc = 0 AND lv_r3tr_objtype = 'DEVC'.
         cs_ctslog-devclass = lv_r3tr_objname.
       ENDIF.
     ELSE.
       CLEAR lwa_tadir.
       READ TABLE it_tadir INTO lwa_tadir WITH KEY object = is_obj_tr-object
                                                 obj_name = is_obj_tr-obj_name
                                            BINARY SEARCH.
       IF sy-subrc = 0.
*     cs_ctslog-author   = lwa_tadir-author.
         cs_ctslog-devclass = lwa_tadir-devclass.
       ENDIF.
     ENDIF.
   ELSEIF is_obj_tr-pgmid = 'LIMU'.
     CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
       EXPORTING
         p_limu_objtype = is_obj_tr-object
         p_limu_objname = is_obj_tr-obj_name
       IMPORTING
         p_r3tr_objtype = lv_r3tr_objtype
         p_r3tr_objname = lv_r3tr_objname
       EXCEPTIONS
         no_mapping     = 1
         OTHERS         = 2.
     IF sy-subrc = 0.
       CLEAR lwa_tadir.
       IF lv_r3tr_objtype = 'DEVC'.
         cs_ctslog-devclass = lv_r3tr_objname.
       ELSE.
         READ TABLE it_tadir INTO lwa_tadir WITH KEY object = lv_r3tr_objtype
                                                   obj_name = lv_r3tr_objname
                                              BINARY SEARCH.
         IF sy-subrc = 0.
           cs_ctslog-devclass = lwa_tadir-devclass.
         ENDIF.
       ENDIF.
     ENDIF.
   ENDIF.
*
 ENDFORM.                    " GET_RESPOSITOY_DET
*&---------------------------------------------------------------------*
*&      Form  F_DATE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CTSLOG_DB  text
*      <--P_GT_CTSLOG_DB  text
*----------------------------------------------------------------------*
 FORM f_date_set CHANGING ct_ctslog_db TYPE gtt_cts_log.

   DATA: lt_ctslog_db TYPE gtt_cts_log,
         lt_tmp TYPE gtt_cts_log,
         lwa_ct_ctslog_db TYPE zartd_tskcts_log,
         lwa_ctslog_db TYPE zartd_tskcts_log.

   lt_tmp[] = ct_ctslog_db[].
   DELETE lt_tmp WHERE trstatus NE 'R'.

   SELECT * FROM zartd_tskcts_log
     INTO TABLE lt_ctslog_db
     FOR ALL ENTRIES IN lt_tmp WHERE trkorr = lt_tmp-trkorr.

   SORT lt_ctslog_db BY trkorr obj_type object.
   LOOP AT ct_ctslog_db INTO lwa_ct_ctslog_db WHERE trstatus = 'R'.
     READ TABLE lt_ctslog_db INTO lwa_ctslog_db
     WITH KEY trkorr = lwa_ct_ctslog_db-trkorr
     obj_type = lwa_ct_ctslog_db-obj_type
     object = lwa_ct_ctslog_db-object BINARY SEARCH."******************kchoudhary

     IF sy-subrc EQ 0.
       lwa_ct_ctslog_db-tsk_chng_date  = lwa_ctslog_db-tsk_chng_date.
       lwa_ct_ctslog_db-tsk_chng_time  = lwa_ctslog_db-tsk_chng_time .
       MODIFY ct_ctslog_db FROM lwa_ct_ctslog_db.
     ENDIF.
   ENDLOOP.
 ENDFORM.                    " F_DATE_SET
*&---------------------------------------------------------------------*
*&      Form  F_ADDFUNC_CHECK_FOR_FEATURES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM f_addfunc_check_for_features .

* Work Area
* Internal Table
   DATA: "lt_ztrcheck    TYPE STANDARD TABLE OF ztrcheck,
         lt_ztrcheck    TYPE STANDARD TABLE OF zartd_fea_config,
         lt_trkorrs     TYPE trkorrs.

* Get Config table
   SELECT * FROM zartd_fea_config INTO TABLE lt_ztrcheck.
*SELECT * FROM ztrcheck INTO TABLE lt_ztrcheck.

   IF sy-subrc <> 0.
     CALL METHOD lo_artd_excp->update_excp_db_direct
       EXPORTING
         iv_trkorr = 'CTS'
         iv_progid = sy-repid
         iv_text   = 'ZTRCHECK not configured'.
     LEAVE PROGRAM.
   ENDIF.

* For WB Approval
   READ TABLE lt_ztrcheck TRANSPORTING NO FIELDS WITH KEY id = 'WRBAPPR'
                                                      active = 'X'.
   IF sy-subrc = 0.
     TRY.
         CALL METHOD zcl_artd_wf_manualappr_tr=>run_chk_for_manuappr
           EXPORTING
             it_trkorr = lt_trkorrs.
       CATCH zcx_artd_exception_cls .
     ENDTRY.
   ENDIF.
*
* For Notification feature
   READ TABLE lt_ztrcheck TRANSPORTING NO FIELDS WITH KEY id = 'NOTIFY'
                                                      active = 'X'.
   IF sy-subrc <> 0.
     EXIT.
   ENDIF.
* Get open TRs for which review errors are zero
   TRY.
       CALL METHOD zcl_artd_utilities=>get_wbtr_with_no_errors
         IMPORTING
           et_trkorrs = lt_trkorrs.
     CATCH zcx_artd_exception_cls.
   ENDTRY.
* Send Notifications for TR retrieved
   IF lt_trkorrs IS NOT INITIAL.
     TRY.
         CALL METHOD zcl_artd_task_check=>mailnotify_wb_review
           EXPORTING
             it_trkorr = lt_trkorrs.
       CATCH zcx_artd_exception_cls.
     ENDTRY.
   ENDIF.
*
 ENDFORM.                    " F_ADDFUNC_CHECK_FOR_FEATURES

*&---------------------------------------------------------------------*
*&      Form  F_GET_CUST_TR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_ARTD_DB_TR  text
*----------------------------------------------------------------------*
 FORM f_get_cust_tr  USING ct_task       TYPE gtr_trkorr
                  CHANGING ct_artd_db_tr TYPE gtt_trkorr.

   DATA: lt_cust  TYPE STANDARD TABLE OF e070,
         lwa_cust TYPE e070,
         lt_e070a TYPE TABLE OF e070a.

* When Task is provided using Selection screen
   IF ct_task IS NOT INITIAL.
     SELECT strkorr FROM e070 APPENDING TABLE ct_artd_db_tr
                                        WHERE trkorr IN ct_task
                                          AND trfunction = 'Q'
                                          AND trstatus   = 'R'.
     IF sy-subrc = 0.
       SORT ct_artd_db_tr BY strkorr.
       DELETE ADJACENT DUPLICATES FROM ct_artd_db_tr COMPARING ALL FIELDS."**********kchoudhary
     ENDIF.
   ELSE.
     SELECT * FROM e070a INTO TABLE lt_e070a
                              WHERE attribute = 'SAP_CTS_PROJECT'
                                AND reference IN ( SELECT cts_id FROM zartd_tr_project ).
     IF sy-subrc = 0 AND lt_e070a IS NOT INITIAL.
       SELECT trkorr FROM e070 APPENDING TABLE ct_artd_db_tr
                            FOR ALL ENTRIES IN lt_e070a
                                         WHERE trkorr     = lt_e070a-trkorr
                                           AND trfunction = 'W'  " Customizing
                                           AND trstatus   = 'D'  " Modifiable
                                           AND strkorr    = space.
     ENDIF.
   ENDIF.


 ENDFORM.                    "f_get_cust_tr
