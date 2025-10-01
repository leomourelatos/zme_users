class ZME_USER definition
  public
  final
  create public .

public section.

  class-methods NEW_USER_NOTIFICATION
    importing
      !I_USERNAME type UNAME
      !I_MODE type CHAR1
      !I_BIRTH_NAME type AD_NAME2_P optional
      !I_MANUAL type CHAR1 optional
    exporting
      !E_CONTENT_ID type SO_OBJ_DES
      !E_MAIL_SUBJ type STRING
      !E_RETURN type BAPIRETURN
    changing
      !T_SOLIX type SOLIX_TAB optional
    returning
      value(R_CONTENTS) type SWFTLISTI1 .
  class-methods AD_READ .
  class-methods AD_CHECK
    importing
      !I_GIVENNAME type ZSTRING default 'Test'
      !I_SN type ZSTRING default 'RFQ_03' .
  class-methods AD_CREATE
    importing
      !I_GIVENNAME type ZSTRING default 'Test'
      !I_SN type ZSTRING default 'RFQ_03' .
  class-methods AD_ENABLE
    importing
      !I_GIVENNAME type ZSTRING default 'Test'
      !I_SN type ZSTRING default 'RFQ_03' .
  class-methods AD_DISABLE
    importing
      !I_GIVENNAME type ZSTRING default 'Test'
      !I_SN type ZSTRING default 'RFQ_03' .
  class-methods AD_CHANGEPSW
    importing
      !I_GIVENNAME type ZSTRING default 'Test'
      !I_SN type ZSTRING default 'RFQ_03' .
protected section.
private section.

  class-data LT_LDAP type LDAPETAB .
  class-data LS_LDAP type LDAPE .
  class-data LT_ATTRS type LDAP_ATII_T .
  class-data LS_ATTRS type LDAP_ATII .
  class-data LT_VALS type LDAP_VALI_T .
  class-data LS_VALS type LDAP_VALI .

  class-methods FILL_LDAP_TABLE
    importing
      !I_NAME type LDAP_ATTR
      !I_TYP type LDAP_OPER
      !I_VAL type LDAP_VAL
    exporting
      !T_ATTRS type LDAP_ATII_T
      !T_VALS type LDAP_VALI_T .
ENDCLASS.



CLASS ZME_USER IMPLEMENTATION.


  METHOD ad_changepsw.

    DATA:
      lv_displayname       TYPE ldap_val,
      lv_givenname         TYPE ldap_val,
      lv_sn                TYPE ldap_val,
      lv_distinguishedname TYPE ldap_val,
      lv_samaccountname    TYPE ldap_val,
      lv_userprincipalname TYPE ldap_val,
      ls_server_err        TYPE REF TO cx_ldap_client_open_server_err,
      dn                   TYPE ldapdefs-dn.

    CLEAR:
      lv_displayname,
      lv_givenname,
      lv_sn,
      lv_distinguishedname,
      lv_samaccountname,
      lv_userprincipalname,
      ls_server_err,
      dn.

    lv_givenname = i_givenname.

    lv_sn = i_sn.

    lv_displayname = i_givenname && |_| && i_sn.

    lv_distinguishedname = |CN=| && lv_displayname && |,| && TEXT-ou1.

    lv_samaccountname = lv_displayname.
    CONDENSE lv_samaccountname NO-GAPS.

    lv_userprincipalname = lv_samaccountname && |@ithakicentral.gr|.

    dn = |CN=| && lv_displayname && |,| && TEXT-ou1.

    CLEAR:
      lt_attrs, lt_attrs[],
      lt_vals,  lt_vals[].

    DATA: lv_password_raw    TYPE string VALUE '"1Q2w3e4r5t!"',
          lv_utf16_xstring   TYPE xstring,
          lv_utf16_string    TYPE string,
          lt_values          TYPE STANDARD TABLE OF string,
          lv_password_base64 TYPE ldap_val.
*      ls_entry TYPE ldapentry,
*      lt_entries TYPE STANDARD TABLE OF ldapentry.

    " Convert to UTF-16LE
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = lv_password_raw
        mimetype = 'text/plain; charset=utf-16le'
      IMPORTING
        buffer   = lv_utf16_xstring.
    lv_utf16_string = lv_utf16_xstring.

    " Now Base64 encode it
    lv_password_base64 = cl_http_utility=>encode_base64( lv_utf16_string ).

*APPEND lv_password_base64 TO lt_values.


    zme_user=>fill_ldap_table( EXPORTING i_name  = 'UNICODEPWD'
                                         i_typ   = '2'
                                         i_val   = lv_password_base64
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
    ).

    CALL FUNCTION 'LDAP_MODIFY'
      EXPORTING
        dn           = dn
        mode         = 'C'
      IMPORTING
        ldaprc_exc   = ls_server_err
      TABLES
        attrs_in     = lt_attrs
        values_in    = lt_vals
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        param_error  = 3
        ldap_failure = 4
        hexval_error = 5
        not_alive    = 6
        other_error  = 7
        OTHERS       = 8.

    IF sy-subrc <> 0 OR ls_server_err IS NOT INITIAL.

      BREAK lmourelatos.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


  METHOD ad_check.

    DATA:
      lv_displayname       TYPE ldap_val,
      lv_givenname         TYPE ldap_val,
      lv_sn                TYPE ldap_val,
      lv_distinguishedname TYPE ldap_val,
      lv_samaccountname    TYPE ldap_val,
      lv_userprincipalname TYPE ldap_val,
      ls_server_err        TYPE REF TO cx_ldap_client_open_server_err,
      dn                   TYPE ldapdefs-dn.

    CLEAR:
      lv_displayname,
      lv_givenname,
      lv_sn,
      lv_distinguishedname,
      lv_samaccountname,
      lv_userprincipalname,
      ls_server_err,
      dn.

    lv_givenname = i_givenname.

    lv_sn = i_sn.

    lv_displayname = i_givenname && |_| && i_sn.

    lv_distinguishedname = |CN=| && lv_displayname && |,| && TEXT-ou1.

    lv_samaccountname = lv_displayname.
    CONDENSE lv_samaccountname NO-GAPS.

    lv_userprincipalname = lv_samaccountname && |@ithakicentral.gr|.

    dn = |CN=| && lv_displayname && |,| && TEXT-ou1.

    CLEAR:
      lt_attrs, lt_attrs[],
      lt_vals,  lt_vals[].

    CALL FUNCTION 'LDAP_SIMPLEBIND'
      EXPORTING
        serverid     = 'ITHAKIDC3'
        usr          = lv_userprincipalname
        pwd          = '1Q2w3e4r5t!'
*       USR_STRING   =
*       PWD_STRING   =
*       WAIT_TIME    = 0
* IMPORTING
*       LDAPRC       =
* CHANGING
*       HOLDSESS     = 0
      EXCEPTIONS
        no_authoriz  = 1
        config_error = 2
        nomore_conns = 3
        ldap_failure = 4
        not_alive    = 5
        other_error  = 6
        OTHERS       = 7.
    IF sy-subrc <> 0.

      BREAK lmourelatos.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD ad_create.

    DATA:
      lv_displayname       TYPE ldap_val,
      lv_givenname         TYPE ldap_val,
      lv_sn                TYPE ldap_val,
      lv_distinguishedname TYPE ldap_val,
      lv_samaccountname    TYPE ldap_val,
      lv_userprincipalname TYPE ldap_val,
      ls_server_err        TYPE REF TO cx_ldap_client_open_server_err,
      dn                   TYPE ldapdefs-dn.

    CLEAR:
      lv_displayname,
      lv_givenname,
      lv_sn,
      lv_distinguishedname,
      lv_samaccountname,
      lv_userprincipalname,
      ls_server_err,
      dn.

    lv_givenname = i_givenname.

    lv_sn = i_sn.

    lv_displayname = i_givenname && |_| && i_sn.

    lv_distinguishedname = |CN=| && lv_displayname && |,| && TEXT-ou1.

    lv_samaccountname = lv_displayname.
    CONDENSE lv_samaccountname NO-GAPS.

    lv_userprincipalname = lv_samaccountname && |@ithakicentral.gr|.

    dn = |CN=| && lv_displayname && |,| && TEXT-ou1.

    CLEAR:
      lt_attrs, lt_attrs[],
      lt_vals,  lt_vals[].

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'CN'
                                         i_typ   = '0'
                                         i_val   = lv_displayname
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'DISPLAYNAME'
                                         i_typ   = '0'
                                         i_val   = lv_displayname
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'GIVENNAME'
                                         i_typ   = '0'
                                         i_val   = lv_givenname
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'NAME'
                                         i_typ   = '0'
                                         i_val   = lv_displayname
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'OBJECTCLASS'
                                         i_typ   = '0'
                                         i_val   = 'top'
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'OBJECTCLASS'
                                         i_typ   = '0'
                                         i_val   = 'person'
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'OBJECTCLASS'
                                         i_typ   = '0'
                                         i_val   = 'organizationalPerson'
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'OBJECTCLASS'
                                         i_typ   = '0'
                                         i_val   = 'user'
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'SAMACCOUNTNAME'
                                         i_typ   = '0'
                                         i_val   = lv_samaccountname
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'SN'
                                         i_typ   = '0'
                                         i_val   = lv_sn
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

*    zme_user=>fill_ldap_table( EXPORTING i_name  = 'USERACCOUNTCONTROL'
*                                         i_typ   = '0'
*                                         i_val   = '544'
*                               IMPORTING
*                                         t_attrs = lt_attrs
*                                         t_vals  = lt_vals
*                              ).

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'USERPRINCIPALNAME'
                                         i_typ   = '0'
                                         i_val   = lv_userprincipalname
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
                              ).

    CALL FUNCTION 'LDAP_ADD'
      EXPORTING
        dn           = dn
        mode         = 'C'
*       ldap_modify  = abap_true
      IMPORTING
        ldaprc_exc   = ls_server_err
      TABLES
        attrs_in     = lt_attrs
        values_in    = lt_vals
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        param_error  = 3
        ldap_failure = 4
        hexval_error = 5
        not_alive    = 6
        other_error  = 7
        OTHERS       = 8.

    IF sy-subrc <> 0 OR ls_server_err IS NOT INITIAL.

      BREAK lmourelatos.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


  METHOD AD_DISABLE.

    DATA:
      lv_displayname       TYPE ldap_val,
      lv_givenname         TYPE ldap_val,
      lv_sn                TYPE ldap_val,
      lv_distinguishedname TYPE ldap_val,
      lv_samaccountname    TYPE ldap_val,
      lv_userprincipalname TYPE ldap_val,
      ls_server_err        TYPE REF TO cx_ldap_client_open_server_err,
      dn                   TYPE ldapdefs-dn.

    CLEAR:
      lv_displayname,
      lv_givenname,
      lv_sn,
      lv_distinguishedname,
      lv_samaccountname,
      lv_userprincipalname,
      ls_server_err,
      dn.

    lv_givenname = i_givenname.

    lv_sn = i_sn.

    lv_displayname = i_givenname && |_| && i_sn.

    lv_distinguishedname = |CN=| && lv_displayname && |,| && TEXT-ou1.

    lv_samaccountname = lv_displayname.
    CONDENSE lv_samaccountname NO-GAPS.

    lv_userprincipalname = lv_samaccountname && |@ithakicentral.gr|.

    dn = |CN=| && lv_displayname && |,| && TEXT-ou1.

    CLEAR:
      lt_attrs, lt_attrs[],
      lt_vals,  lt_vals[].

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'USERACCOUNTCONTROL'
                                         i_typ   = '2'
                                         i_val   = '546'
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
    ).

    CALL FUNCTION 'LDAP_MODIFY'
      EXPORTING
        dn           = dn
        mode         = 'C'
      IMPORTING
        ldaprc_exc   = ls_server_err
      TABLES
        attrs_in     = lt_attrs
        values_in    = lt_vals
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        param_error  = 3
        ldap_failure = 4
        hexval_error = 5
        not_alive    = 6
        other_error  = 7
        OTHERS       = 8.

    IF sy-subrc <> 0 OR ls_server_err IS NOT INITIAL.

      BREAK lmourelatos.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.


  ENDMETHOD.


  METHOD AD_ENABLE.

    DATA:
      lv_displayname       TYPE ldap_val,
      lv_givenname         TYPE ldap_val,
      lv_sn                TYPE ldap_val,
      lv_distinguishedname TYPE ldap_val,
      lv_samaccountname    TYPE ldap_val,
      lv_userprincipalname TYPE ldap_val,
      ls_server_err        TYPE REF TO cx_ldap_client_open_server_err,
      dn                   TYPE ldapdefs-dn.

    CLEAR:
      lv_displayname,
      lv_givenname,
      lv_sn,
      lv_distinguishedname,
      lv_samaccountname,
      lv_userprincipalname,
      ls_server_err,
      dn.

    lv_givenname = i_givenname.

    lv_sn = i_sn.

    lv_displayname = i_givenname && |_| && i_sn.

    lv_distinguishedname = |CN=| && lv_displayname && |,| && TEXT-ou1.

    lv_samaccountname = lv_displayname.
    CONDENSE lv_samaccountname NO-GAPS.

    lv_userprincipalname = lv_samaccountname && |@ithakicentral.gr|.

    dn = |CN=| && lv_displayname && |,| && TEXT-ou1.

    CLEAR:
      lt_attrs, lt_attrs[],
      lt_vals,  lt_vals[].

    zme_user=>fill_ldap_table( EXPORTING i_name  = 'USERACCOUNTCONTROL'
                                         i_typ   = '2'
                                         i_val   = '544'
                               IMPORTING
                                         t_attrs = lt_attrs
                                         t_vals  = lt_vals
    ).

    CALL FUNCTION 'LDAP_MODIFY'
      EXPORTING
        dn           = dn
        mode         = 'C'
      IMPORTING
        ldaprc_exc   = ls_server_err
      TABLES
        attrs_in     = lt_attrs
        values_in    = lt_vals
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        param_error  = 3
        ldap_failure = 4
        hexval_error = 5
        not_alive    = 6
        other_error  = 7
        OTHERS       = 8.

    IF sy-subrc <> 0 OR ls_server_err IS NOT INITIAL.

      BREAK lmourelatos.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.


  ENDMETHOD.


  METHOD ad_read.

    DATA:
      ls_ldapusers    TYPE zme_ldapusers,
      lt_ldapusers    TYPE STANDARD TABLE OF zme_ldapusers WITH NON-UNIQUE DEFAULT KEY,
      ls_ldapusers_hd TYPE zme_ldapusers_hd,
      lt_ldapusers_hd TYPE STANDARD TABLE OF zme_ldapusers_hd WITH NON-UNIQUE DEFAULT KEY.

    DATA:
      lv_disable.

    CLEAR:
      lv_disable.

    CLEAR: lt_ldap, lt_ldap[].

    CALL FUNCTION 'LDAP_READ'
      EXPORTING
        base         = TEXT-ou1
*       BASE_STRING  =
        scope        = 2
        filter       = '(&(objectclass=user))'
*       FILTER_STRING       =
*       TIMEOUT      =
*       attributes   = attributes_ldap
      IMPORTING
*       LDAPRC       =
        entries      = lt_ldap
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        ldap_failure = 3
        not_alive    = 4
        other_error  = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_ldap INTO DATA(wa_ldap).

      CLEAR lv_disable.

      READ TABLE wa_ldap-attributes INTO DATA(ls_attr) WITH KEY name = 'SAMACCOUNTNAME'.
      IF sy-subrc = 0.
        READ TABLE ls_attr-vals INTO DATA(ls_val) INDEX 1.
        IF sy-subrc = 0.
          ls_ldapusers-dn = ls_val-val.
        ENDIF.
      ENDIF.

      READ TABLE wa_ldap-attributes INTO ls_attr WITH KEY name = 'USERACCOUNTCONTROL'.
      IF sy-subrc = 0.
        READ TABLE ls_attr-vals INTO ls_val INDEX 1.
        IF sy-subrc = 0.
          IF ls_val-val = '2' OR
             ls_val-val = '514' OR
             ls_val-val = '546' OR
             ls_val-val = '66050' OR
             ls_val-val = '66082' OR
             ls_val-val = '262658' OR
             ls_val-val = '262690' OR
             ls_val-val = '328194' OR
             ls_val-val = '328226'.
            lv_disable = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      LOOP AT wa_ldap-attributes INTO DATA(wa_attr).

        ls_ldapusers-name       = wa_attr-name.
        ls_ldapusers-typ        = wa_attr-typ.
        ls_ldapusers-operation  = wa_attr-operation.

        LOOP AT wa_attr-vals INTO DATA(wa_val).

          ls_ldapusers-val = wa_val-val.
          ls_ldapusers-buzei = sy-tabix.

          APPEND ls_ldapusers TO lt_ldapusers.
          MOVE-CORRESPONDING ls_ldapusers TO ls_ldapusers_hd.
          ls_ldapusers_hd-disable = lv_disable.
          COLLECT ls_ldapusers_hd INTO lt_ldapusers_hd.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    CLEAR: lt_ldap, lt_ldap[].

    CALL FUNCTION 'LDAP_READ'
      EXPORTING
        base         = TEXT-ou2
*       BASE_STRING  =
        scope        = 2
        filter       = '(&(objectclass=user))'
*       FILTER_STRING       =
*       TIMEOUT      =
*       attributes   = attributes_ldap
      IMPORTING
*       LDAPRC       =
        entries      = lt_ldap
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        ldap_failure = 3
        not_alive    = 4
        other_error  = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_ldap INTO wa_ldap.

      CLEAR lv_disable.

      READ TABLE wa_ldap-attributes INTO ls_attr WITH KEY name = 'SAMACCOUNTNAME'.
      IF sy-subrc = 0.
        READ TABLE ls_attr-vals INTO ls_val INDEX 1.
        IF sy-subrc = 0.
          ls_ldapusers-dn = ls_val-val.
        ENDIF.
      ENDIF.

      READ TABLE wa_ldap-attributes INTO ls_attr WITH KEY name = 'USERACCOUNTCONTROL'.
      IF sy-subrc = 0.
        READ TABLE ls_attr-vals INTO ls_val INDEX 1.
        IF sy-subrc = 0.
          IF ls_val-val = '2' OR
             ls_val-val = '514' OR
             ls_val-val = '546' OR
             ls_val-val = '66050' OR
             ls_val-val = '66082' OR
             ls_val-val = '262658' OR
             ls_val-val = '262690' OR
             ls_val-val = '328194' OR
             ls_val-val = '328226'.
            lv_disable = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      LOOP AT wa_ldap-attributes INTO wa_attr.

        ls_ldapusers-name       = wa_attr-name.
        ls_ldapusers-typ        = wa_attr-typ.
        ls_ldapusers-operation  = wa_attr-operation.

        LOOP AT wa_attr-vals INTO wa_val.

          ls_ldapusers-val = wa_val-val.
          ls_ldapusers-buzei = sy-tabix.

          APPEND ls_ldapusers TO lt_ldapusers.
          MOVE-CORRESPONDING ls_ldapusers TO ls_ldapusers_hd.
          ls_ldapusers_hd-disable = lv_disable.
          COLLECT ls_ldapusers_hd INTO lt_ldapusers_hd.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_ldapusers_hd INTO ls_ldapusers_hd.
      SELECT SINGLE * FROM zme_ldapusers_hd
        INTO @DATA(l_ldapusers_hd)
        WHERE dn = @ls_ldapusers_hd-dn.
      IF sy-subrc = 0.
        UPDATE zme_ldapusers_hd FROM ls_ldapusers_hd.
      ELSE.
        INSERT zme_ldapusers_hd FROM ls_ldapusers_hd.
      ENDIF.
      DELETE FROM zme_ldapusers WHERE dn = ls_ldapusers_hd-dn.
    ENDLOOP.

    COMMIT WORK.

    SELECT * FROM zme_ldapusers_hd INTO ls_ldapusers_hd.
      READ TABLE lt_ldapusers_hd INTO l_ldapusers_hd WITH KEY dn = ls_ldapusers_hd-dn.
      IF sy-subrc <> 0.
        ls_ldapusers_hd-disable = 'X'.
        UPDATE zme_ldapusers_hd FROM ls_ldapusers_hd.
      ENDIF.
    ENDSELECT.

    LOOP AT lt_ldapusers INTO ls_ldapusers.
      INSERT zme_ldapusers FROM ls_ldapusers.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_ldap_table.

    CLEAR:
      ls_attrs,
      ls_vals.

    READ TABLE t_attrs INTO DATA(s_attr) WITH KEY text = i_name.
    IF sy-subrc = 0.
      s_attr-text = i_name.
      s_attr-num1 = i_typ.
      s_attr-num2 = s_attr-num2 + 1.
      MODIFY t_attrs FROM s_attr INDEX sy-tabix.
    ELSE.
      ls_attrs-text = i_name.
      ls_attrs-num1 = i_typ.
      ls_attrs-num2 = 1.
      APPEND ls_attrs TO t_attrs.
    ENDIF.

    ls_vals-text = i_val.
    APPEND ls_vals TO t_vals.

  ENDMETHOD.


  METHOD new_user_notification.

    APPEND TEXT-h01 TO r_contents."<p style='margin-top:0cm;margin-right:0cm;margin-bottom:8.0pt;margin-left:0cm;font-size:11.0pt;font-family:"Aptos",sans-serif;'>
    APPEND i_birth_name TO r_contents.

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND TEXT-h02 TO r_contents."<br>

    IF i_mode = 'N'.
      APPEND 'Ο κωδικός σου για την πρόσβαση στο' TO r_contents.
      IF sy-sysid = 'MED'.
        APPEND '<strong> Test SAP </strong>' TO r_contents.
      ELSEIF sy-sysid = 'MEP'.
        APPEND '<strong> SAP </strong>' TO r_contents.
      ENDIF.
      APPEND 'έχει δημιουργηθεί. Το username σου είναι το' TO r_contents.
    ELSEIF i_mode = 'I'.
      APPEND 'Ο Κωδικος σου για την πρόσβαση στο' TO r_contents.
      IF sy-sysid = 'MED'.
        APPEND '<strong> Test SAP </strong>' TO r_contents.
      ELSEIF sy-sysid = 'MEP'.
        APPEND '<strong> SAP </strong>' TO r_contents.
      ENDIF.
      APPEND 'έχει αρχικοποιηθεί. Το username σου είναι το' TO r_contents.
    ENDIF.

    APPEND TEXT-h03 TO r_contents."<strong>
    APPEND TEXT-h15 TO r_contents."&quot;
    APPEND i_username TO r_contents.
    APPEND TEXT-h15 TO r_contents."&quot;
    APPEND TEXT-h04 TO r_contents."</strong>

    APPEND 'και το προσωρινό σου password είναι'  TO r_contents.

    APPEND TEXT-h03 TO r_contents."<strong>
    APPEND TEXT-h15 TO r_contents."&quot;
    APPEND 'initiaL#11' TO r_contents.
    APPEND TEXT-h15 TO r_contents."&quot;
    APPEND TEXT-h04 TO r_contents."</strong>

    APPEND '(προσοχή στα κεφαλαία). Κατά την πρώτη εισαγωγή θα χρειαστεί να εισάγεις νέο password.' TO r_contents.
    APPEND ' Αυτό θα πρέπει να είναι τουλάχιστον 10 χαρακτήρες, να περιέχει τουλάχιστον 1 κεφαλαίο, τουλάχιστον 1 αριθμό και τουλάχιστον 1 σύμβολο.' TO r_contents.

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND TEXT-h02 TO r_contents."<br>

    IF i_manual = 'X'.
      APPEND 'Συνημμένα θα βρεις οδηγίες για την εγκατάσταση της εφαρμογής τοπικά στον υπολογιστή που θα χρησιμοποιείς.' TO r_contents.

      APPEND TEXT-h02 TO r_contents."<br>

      APPEND TEXT-h02 TO r_contents."<br>
    ENDIF.

    APPEND 'Για όποια απορία ή τυχόν πρόβλημα αντιμετωπίσεις κατά την χρήση μπορείς να στείλεις email στη διεύθυνση' TO r_contents.
    APPEND TEXT-h05 && TEXT-h11 && 'support.sap@mesogeos.gr' && TEXT-h06 && ' support.sap@mesogeos.gr' && TEXT-h07 TO r_contents."<a href=" && mailto: && _____ && "> _____ && </a>

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND 'Στη διάθεση σας για οποιαδήποτε διευκρίνηση.' TO r_contents.

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND TEXT-h02 TO r_contents."<br>

    APPEND TEXT-h02 TO r_contents."<br>

*    APPEND TEXT-h08 TO r_contents."<img height="44" src="data:image/png;base64,
*    APPEND TEXT-h09 TO r_contents."" alt="image" width="150">

*    DATA(lv_mr_api) = cl_mime_repository_api=>if_mr_api~get_api( ).
*
*    CALL METHOD lv_mr_api->get
*      EXPORTING
*        i_url              = '/SAP/PUBIC/MES.bpm'
**       i_check_authority  = 'X'              " X Check Authorization, '' No Authorization Check
*      IMPORTING
*        e_is_folder        = DATA(is_folder)                 " X - Object is folder, '' - Otherwise file
*        e_content          = DATA(lv_content)                 " Object Contents
**       e_content_last_changed =                  " Change Time (UTC) of Content
**       e_mime_type        =                  " MIME Type
*        e_loio             = DATA(l_loio)                 " Technical Object Key Incl. MIME-GUID
**  CHANGING
**       c_language         =                  " Object Language
*      EXCEPTIONS
*        parameter_missing  = 1                " Parameter missing or is initial
*        error_occured      = 2                " Unspecified Error Occurred
*        not_found          = 3                " Object not found
*        permission_failure = 4                " Missing Authorization
*        OTHERS             = 5.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    DATA:l_obj_len TYPE so_obj_len.
*    l_obj_len = xstrlen( lv_content ).
*    DATA(lv_graphic_length) = xstrlen( lv_content ).
*    DATA gr_xstr TYPE xstring.
*    DATA l_offset TYPE i.
*    DATA l_length TYPE i.
*    DATA lt_solix TYPE solix_tab.
*    DATA ls_solix TYPE solix.
*    DATA l_filename TYPE mime_text.
*    DATA l_content_id TYPE mime_cntid.
*
*    CLEAR gr_xstr.
*    gr_xstr = lv_content(l_obj_len).
*    l_offset = 0.
*    l_length = 255.
*    CLEAR lt_solix[].
*    WHILE l_offset < lv_graphic_length.
*      DATA(l_diff) = lv_graphic_length - l_offset.
*      IF l_diff > l_length.
*        ls_solix-line = gr_xstr+l_offset(l_length).
*      ELSE.
*        ls_solix-line = gr_xstr+l_offset(l_diff).
*      ENDIF.
*      APPEND ls_solix TO lt_solix.
*      ADD l_length TO l_offset.
*    ENDWHILE.
*
*
*    l_filename = 'MES.bmp'.
*    l_content_id = 'MES.bmp'.
*    DATA lo_mime_helper TYPE REF TO cl_gbt_multirelated_service.
*    CREATE OBJECT lo_mime_helper.
*    CALL METHOD lo_mime_helper->add_binary_part
*      EXPORTING
*        content      = lt_solix
*        filename     = l_filename
*        extension    = 'BMP'
*        description  = 'MES'
*        content_type = 'image/bmp'
*        length       = l_obj_len
*        content_id   = l_content_id.




*    DATA:
*      lv_img         TYPE xstring,
*      ls_attachment  TYPE crms_email_image,
*      ls_image_url   TYPE crms_email_image_url,
*      lt_attachments TYPE crmt_email_image,
*      lt_image_urls  TYPE crmt_email_image_url.
*
*    CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
*      EXPORTING
*        p_object       = 'GRAPHICS'
*        p_name         = '1000_FI_EL'
*        p_id           = 'BMAP'
*        p_btype        = 'BCOL'
*      RECEIVING
*        p_bmp          = lv_img
*      EXCEPTIONS
*        not_found      = 1
*        internal_error = 2
*        OTHERS         = 3.
*    CHECK sy-subrc = 0.
*
*    ls_attachment-content_id = '1000_FI_EL'.
*    ls_attachment-content_bin = lv_img.
*    ls_attachment-is_embedded = abap_true.
*    APPEND ls_attachment TO lt_attachments.
*
*    ls_image_url-content_id = '1000_FI_EL'.
*    ls_image_url-url = cl_ssf_xsf_utilities=>mime_url_for_bds_graphic(
*      p_object = 'GRAPHICS'
*      p_name = '1000_FI_EL'
*      p_id = 'BMAP'
*      p_btype = 'BCOL' ) .
*    APPEND ls_image_url TO lt_image_urls.
*
*    DATA:
*      lv_body       TYPE crmt_email_mime_struc,
*      lv_body_html TYPE string.
*
*
*    CALL METHOD cl_crm_email_utility=>get_body_part_from_editor
*      EXPORTING
*        iv_rewrite_cid = abap_true
*        it_image_url   = lt_image_urls
*        it_attachment  = lt_attachments
*      IMPORTING
*        et_mime_data   = lv_body
*      CHANGING
*        cv_html        = lv_body_html
*      EXCEPTIONS
*        input_error    = 1
*        OTHERS         = 2.
*
*    BREAK lmourelatos.
*
*    APPEND TEXT-h02 TO r_contents."<br>
**    APPEND '<img alt="image/bmp" src="cid:1000_FI_EL" />' TO r_contents.
*    APPEND '<img src="cid:BMAP" />' TO r_contents.
*    APPEND TEXT-h02 TO r_contents."<br>

    APPEND '<span style="color: rgb(0, 112, 192);border: 0px;font-size: 11pt;">' TO r_contents.
    APPEND TEXT-h03 TO r_contents."<strong>
    APPEND 'Μουρελάτος Λεωνίδας' TO r_contents.
    APPEND TEXT-h04 TO r_contents."</strong>
    APPEND TEXT-h02 TO r_contents."<br>
    APPEND 'Αιόλου 67 - 10559 Αθήνα' TO r_contents.
    APPEND TEXT-h02 TO r_contents."<br>
    APPEND 'Τηλ.: +30 210 3837748 (117)' TO r_contents.
    APPEND TEXT-h02 TO r_contents."<br>
    APPEND 'Κιν.: +30 694 3100297' TO r_contents.
    APPEND TEXT-h02 TO r_contents."<br>
    APPEND 'Email: ' TO r_contents.
    APPEND TEXT-h10 TO r_contents."<u>
    APPEND TEXT-h05 && TEXT-h11 && 'leo.mourelatos@mesogeos.gr' && TEXT-h06 && 'leo.mourelatos@mesogeos.gr' && TEXT-h07 TO r_contents."<a href=" && mailto: && _____ && "> _____ && </a>
    APPEND TEXT-h13 TO r_contents."</u>
    APPEND TEXT-h02 TO r_contents."<br>

    APPEND 'Web: ' TO r_contents.
    APPEND TEXT-h05 && 'http://mesogeos.gr' && TEXT-h06 && 'mesogeos.gr' && TEXT-h07 TO r_contents."<a href=" && _____ && "> _____ && </a>

    APPEND TEXT-h18 TO r_contents."</span>
    APPEND TEXT-h14 TO r_contents."</p>

  ENDMETHOD.
ENDCLASS.
