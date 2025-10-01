*&---------------------------------------------------------------------*
*& Report ZME_MM_USER_DEFAULTS_V2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zme_mm_user_defaults_v2.

PARAMETERS: uname TYPE sy-uname.
PARAMETERS: uname_n TYPE sy-uname.

DATA:
  lt_esduscom TYPE STANDARD TABLE OF esduscom WITH NON-UNIQUE DEFAULT KEY,
  l_esduscom  TYPE STANDARD TABLE OF esduscom WITH NON-UNIQUE DEFAULT KEY.

DATA:
  BEGIN OF int_action,
    mepo                TYPE esdus_s-action VALUE 'MEPO',
    migo_adpic_toggler  TYPE esdus_s-action VALUE 'MIGO_ADPIC_TOGGLER',
    migo_detail         TYPE esdus_s-action VALUE 'MIGO_DETAIL',
    migo_detail_toggler TYPE esdus_s-action VALUE 'MIGO_DETAIL_TOGGLER',
    migo_firstline      TYPE esdus_s-action VALUE 'MIGO_FIRSTLINE',
    migo_header         TYPE esdus_s-action VALUE 'MIGO_HEADER',
    migo_header_toggler TYPE esdus_s-action VALUE 'MIGO_HEADER_TOGGLER',
    migo_tips           TYPE esdus_s-action VALUE 'MIGO_TIPS',
    migo_tree           TYPE esdus_s-action VALUE 'MIGO_TREE',
    migo_tv_toggler     TYPE esdus_s-action VALUE 'MIGO_TV_TOGGLER',
    miro                TYPE esdus_s-action VALUE 'MIRO',
    purchaseorder       TYPE esdus_s-action VALUE 'PurchaseOrder',
    purchaserequisition TYPE esdus_s-action VALUE 'PurchaseRequisition',
  END   OF int_action,
  lv_action TYPE esdus_s-action.

START-OF-SELECTION.

  DO 13 TIMES VARYING lv_action FROM int_action-mepo NEXT int_action-migo_adpic_toggler.
    CLEAR: l_esduscom, l_esduscom[].

    CALL FUNCTION 'ES_READ_USER_SETTINGS'
      EXPORTING
        iaction = lv_action
        iuname  = uname
*       IDB     =
*       ILIKE   =
      TABLES
        iesdus  = l_esduscom.

    APPEND LINES OF l_esduscom TO lt_esduscom.
    CLEAR: l_esduscom, l_esduscom[].
  ENDDO.

  IF uname_n IS NOT INITIAL.

    CALL FUNCTION 'ES_DELETE_USER_SETTINGS'
      EXPORTING
*       iaction   = 'PurchaseRequisition'
*       ielement  =
*       iactive   =
        iuname    = uname_n
        isave     = 'X'
*  TABLES
*       iesdus    =
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

*    BREAK lmourelatos.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT lt_esduscom INTO DATA(ls_esduscom).
      CALL FUNCTION 'ES_APPEND_USER_SETTINGS'
        EXPORTING
          iaction  = ls_esduscom-action
          ielement = ls_esduscom-element
          iactive  = ls_esduscom-active
          iuname   = uname_n
          isave    = 'X'
*    TABLES
*         iesdus   = lt_esduscom.
        .
    ENDLOOP.
  ENDIF.

*  BREAK lmourelatos.
