*&---------------------------------------------------------------------*
*& Report ZME_USER_NOTIFICATIONS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zme_user_notifications.

PARAMETERS:
  p_uname  LIKE suid_st_bname-bname,
  new_user RADIOBUTTON GROUP a DEFAULT 'X',
  ini_user RADIOBUTTON GROUP a,
  file_in  TYPE zstring.

DATA:
  p_mode,
  gt_contents   TYPE swftlisti1,
  gs_return     TYPE bapireturn,
  gt_userreturn TYPE bapiret2_tab,
  gs_bapiaddr3  TYPE bapiaddr3,
  gt_bapiadsmtp TYPE bapiadsmtp_t,
  gs_bapiadsmtp TYPE bapiadsmtp,
  gt_mail_not   TYPE zme_proc_notif_tbl,
  gs_mail_not   TYPE zme_proc_notif,
  it_data       TYPE solix_tab,
  filelength    TYPE i,
  lv_filein.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_in.
  PERFORM f4_path_and_file CHANGING file_in.

START-OF-SELECTION.

  IF new_user = 'X'.
    p_mode = 'N'.
  ELSEIF ini_user = 'X'.
    p_mode = 'I'.
  ENDIF.

  CLEAR:
    gt_mail_not[],
    it_data[],
    filelength,
    lv_filein.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = p_uname
    IMPORTING
      address  = gs_bapiaddr3
    TABLES
      return   = gt_userreturn
      addsmtp  = gt_bapiadsmtp.

  READ TABLE gt_bapiadsmtp INTO gs_bapiadsmtp WITH KEY std_no = 'X'.

  IF sy-subrc = 0.

    CLEAR:
      gt_mail_not, gt_mail_not[],
      gs_mail_not.

    SELECT recipient acct_name sapuser FROM zme_notification
      INTO ( gs_mail_not-mail_type, gs_mail_not-acct_name, gs_mail_not-sapuser )
      WHERE mailtype = 'USER'
      AND   itype = ' '
      AND   matkl = ' '
      AND   recipient = 'FROM'.

      APPEND gs_mail_not TO gt_mail_not.
    ENDSELECT.

    CLEAR gs_mail_not.
    gs_mail_not-mail_type = 'TO'.
    gs_mail_not-acct_name = gs_bapiadsmtp-e_mail.
    gs_mail_not-sapuser = ' '.
    APPEND gs_mail_not TO gt_mail_not.

    IF file_in IS NOT INITIAL.

      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = file_in
          filetype                = 'BIN'
          has_field_separator     = abap_false
          read_by_line            = abap_true
          dat_mode                = abap_false
          codepage                = space
          ignore_cerr             = abap_true
        IMPORTING
          filelength              = filelength
        CHANGING
          data_tab                = it_data
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19.

      lv_filein = 'X'.

      DATA:
        lv_drive         TYPE string,
        lv_extension     TYPE string,
        lv_name          TYPE string,
        lv_name_with_ext TYPE string,
        lv_path          TYPE string.


      TRY.
          cl_bcs_utilities=>split_path(
            EXPORTING
              iv_path = file_in
            IMPORTING
              ev_path = lv_path
              ev_name = lv_name_with_ext
          ).
        CATCH cx_bcs INTO DATA(bcs).
      ENDTRY.

*  CATCH cx_bcs. " BCS: General Exceptions

      CALL METHOD cl_bcs_utilities=>split_name
        EXPORTING
          iv_name      = lv_name_with_ext
*         iv_delimiter = GC_DOT
        IMPORTING
          ev_name      = lv_name
          ev_extension = lv_extension.

      DATA:
        att_name TYPE so_obj_des.

      att_name = lv_name.

    ENDIF.


    DATA(lt_contents) = zme_user=>new_user_notification( EXPORTING i_username   = p_uname
                                                                   i_birth_name = gs_bapiaddr3-birth_name
                                                                   i_mode       = p_mode
                                                                   i_manual     = lv_filein
                                                         IMPORTING
                                                                   e_return     = gs_return
                                                        ).

    zme_mail=>mail_sent( EXPORTING mail_subj    = 'SAP Username'
                                   t_contents   = lt_contents
                                   att_name     = att_name
                                   t_attach     = it_data
                                   i_mail_not   = ' '
                                   t_mail_notif = gt_mail_not
                        ).

  ENDIF.
*&---------------------------------------------------------------------*
*& Form f4_path_and_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- FILE_IN
*&---------------------------------------------------------------------*
FORM f4_path_and_file CHANGING ly_file.


  DATA path LIKE ibipparms-path.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = path.

  ly_file = path.


ENDFORM.
