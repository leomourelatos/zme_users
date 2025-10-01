class ZME_AUTH definition
  public
  final
  create public .

public section.

  class-methods GET_AUTH_PROJECTS
    importing
      !I_UNAME type USR02-BNAME
    returning
      value(R_PSPID) type ZME_PSPID_RNG .
protected section.
private section.
ENDCLASS.



CLASS ZME_AUTH IMPLEMENTATION.


  METHOD get_auth_projects.

    DATA:
      lr_prctr TYPE zme_prctr_rng,
      ls_prctr TYPE zme_prctr_range,
      lr_fkstl TYPE zme_fkstl_rng,
      ls_fkstl TYPE zme_fkstl_range,
      lr_pspid TYPE zme_pspid_rng,
      ls_pspid TYPE zme_pspid_range.

    DATA:
      lt_values TYPE susr_t_usvalues,
      lv_full   TYPE sap_bool.

    CLEAR:
      lr_prctr, lr_prctr[], ls_prctr,
      lr_fkstl, lr_fkstl[], ls_fkstl,
      lr_pspid, lr_pspid[], ls_pspid.

    CLEAR: lv_full, lt_values, lt_values[].

    CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
      EXPORTING
*       NEW_BUFFERING       = 3
        mandant             = sy-mandt
        user_name           = i_uname
        sel_object          = 'Z_PRCTR'
*       NO_TRACE            =
*       OPTIMIZE            =
*       RESPECT_DISABLEMNT_4_AUTH_CHK       =
*       SACF_SCENARIO       = ' '
      IMPORTING
        fully_authorized    = lv_full
      TABLES
        values              = lt_values
*       IT_FILTERS          =
      EXCEPTIONS
        user_name_not_exist = 1
        not_authorized      = 2
        internal_error      = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_full IS INITIAL.
      DELETE lt_values WHERE field <> 'Z_PRCTR'.
      LOOP AT lt_values INTO DATA(ls_values).
        ls_prctr-sign = 'I'.
        IF ls_values-von = '*'.
          ls_prctr-option = 'CP'.
        ELSE.
          IF ls_values-bis IS NOT INITIAL.
            ls_prctr-option = 'BT'.
            ls_prctr-low = ls_values-von.
            ls_prctr-high = ls_values-bis.
          ELSE.
            ls_prctr-option = 'EQ'.
            ls_prctr-low = ls_values-von.
          ENDIF.
        ENDIF.
        COLLECT ls_prctr INTO lr_prctr.
      ENDLOOP.
    ENDIF.






    CLEAR: lv_full, lt_values, lt_values[].

    CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
      EXPORTING
*       NEW_BUFFERING       = 3
        mandant             = sy-mandt
        user_name           = i_uname
        sel_object          = 'Z_FKSTL'
*       NO_TRACE            =
*       OPTIMIZE            =
*       RESPECT_DISABLEMNT_4_AUTH_CHK       =
*       SACF_SCENARIO       = ' '
      IMPORTING
        fully_authorized    = lv_full
      TABLES
        values              = lt_values
*       IT_FILTERS          =
      EXCEPTIONS
        user_name_not_exist = 1
        not_authorized      = 2
        internal_error      = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_full IS INITIAL.
      DELETE lt_values WHERE field <> 'Z_FKSTL'.
      LOOP AT lt_values INTO ls_values.
        ls_fkstl-sign = 'I'.
        IF ls_values-von = '*'.
          ls_fkstl-option = 'CP'.
        ELSE.
          IF ls_values-bis IS NOT INITIAL.
            ls_fkstl-option = 'BT'.
            ls_fkstl-low = ls_values-von.
            ls_fkstl-high = ls_values-bis.
          ELSE.
            ls_fkstl-option = 'EQ'.
            ls_fkstl-low = ls_values-von.
          ENDIF.
        ENDIF.
        COLLECT ls_fkstl INTO lr_fkstl.
      ENDLOOP.
    ENDIF.






    CLEAR: lv_full, lt_values, lt_values[].

    CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
      EXPORTING
*       NEW_BUFFERING       = 3
        mandant             = sy-mandt
        user_name           = i_uname
        sel_object          = 'Z_PROJECT'
*       NO_TRACE            =
*       OPTIMIZE            =
*       RESPECT_DISABLEMNT_4_AUTH_CHK       =
*       SACF_SCENARIO       = ' '
      IMPORTING
        fully_authorized    = lv_full
      TABLES
        values              = lt_values
*       IT_FILTERS          =
      EXCEPTIONS
        user_name_not_exist = 1
        not_authorized      = 2
        internal_error      = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_full IS INITIAL.
      DELETE lt_values WHERE field <> 'Z_PSPID'.
      LOOP AT lt_values INTO ls_values.
        ls_pspid-sign = 'I'.
        IF ls_values-von = '*'.
          ls_pspid-option = 'CP'.
        ELSE.
          IF ls_values-bis IS NOT INITIAL.
            ls_pspid-option = 'BT'.
            ls_pspid-low = ls_values-von.
            ls_pspid-high = ls_values-bis.
          ELSE.
            ls_pspid-option = 'EQ'.
            ls_pspid-low = ls_values-von.
          ENDIF.
        ENDIF.
        COLLECT ls_pspid INTO lr_pspid.
      ENDLOOP.
    ENDIF.



*    BREAK lmourelatos.

    CLEAR: r_pspid, r_pspid[], ls_pspid.

    IF lr_pspid[] IS INITIAL.

      ls_pspid-sign = 'I'.
      ls_pspid-option = 'CP'.
      ls_pspid-low = '*'.
      APPEND ls_pspid TO r_pspid.

    ELSE.

      ls_pspid-sign = 'I'.
      ls_pspid-option = 'EQ'.
      SELECT pspid FROM zps_project_list
        INTO ls_pspid-low
        WHERE prctr IN lr_prctr
        AND   fkstl IN lr_fkstl
        AND   pspid IN lr_pspid.
        APPEND ls_pspid TO r_pspid.
      ENDSELECT.

    ENDIF.




*  SELECT * FROM cepc.
*     INTO @DATA(ls_cepc).
*
*    AUTHORITY-CHECK OBJECT 'Z_PRCTR'
*      ID 'ACTVT' DUMMY
*      ID 'Z_PRCTR' FIELD ls_cepc-prctr.
*
*    IF sy-subrc = 0.
*
*      SELECT * FROM csks
*        INTO @DATA(ls_csks)
*        WHERE prctr = @ls_cepc-prctr
*        AND   datbi = '99991231'.
*
*        SELECT posid FROM prps
*          INTO @DATA(lv_posid)
*          WHERE fkstl = @ls_csks-kostl
*          AND   stufe = '1'
*          AND   posid NOT LIKE '%-ΣΥΜΒ%'
*          AND   posid NOT LIKE '%-Σ%'.
*
*          IF sy-subrc <> 0.
*
*            BREAK lmourelatos.
*
*          ELSEIF sy-subrc = 0.
*
*            AUTHORITY-CHECK OBJECT 'Z_PROJECT'
*              ID 'ACTVT' DUMMY
*              ID 'Z_PSPID' FIELD lv_posid.
*
*            IF sy-subrc = 0.
*
*              lr_pspid-sign = 'I'.
*              lr_pspid-option = 'EQ'.
*              lr_pspid-low = lv_posid.
*
*              APPEND lr_pspid TO r_pspid.
*
*            ELSE.
*
*              BREAK lmourelatos.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDSELECT.
*
*      ENDSELECT.
*
*    ENDIF.
*
*  ENDSELECT.

  ENDMETHOD.
ENDCLASS.
