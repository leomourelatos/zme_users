REPORT.

PARAMETERS uname TYPE uname.
SELECTION-SCREEN BEGIN OF SCREEN 1001.
  PARAMETERS dummy.
SELECTION-SCREEN END OF SCREEN 1001.

START-OF-SELECTION.

  cl_framework_mm=>get_instance( IMPORTING ex_instance = DATA(framework) ).

  sy-uname = uname.

  " Class CL_PERSONALIZATION_MM
  DATA(personalization) = framework->get_personalization( ).

  " For the object type if_purchase_requisition=>c_objtyp = 'BUS2105' (Purchase Requisition), the
  " proposer class will be FUNCTION-POOL=MEPROP\CLASS=LCL_REQ_PROPOSER. You MUST indicate the
  " DDIC structure MEREQ_PROP which is hardcoded in LCL_REQ_PROPOSER.
  CONSTANTS po_doc TYPE mepo_mfs_application VALUE 'MMPUR_PO_DOC'. " From the constants in LMEGUICON
  DATA(l_proposer) = cl_proposer_mm=>create( im_objecttype      = EXACT #( if_purchase_requisition=>c_objtyp )
                                             im_structure       = 'MEREQ_PROP'           ##NO_TEXT
                                             im_mfs_application = po_doc
                                             im_postfix         = '_X' ).

  framework->add_proposer( im_objecttype = EXACT #( if_purchase_requisition=>c_objtyp )
                           im_proposer   = l_proposer ).
  personalization->append( im_name   = 'ReqProposer'          ##NO_TEXT
                           im_object = l_proposer ).
  DATA(esdus_stream) = NEW cl_esdus_connector_mm_2( im_action_id = 'PurchaseRequisition' ) ##NO_TEXT. " ESDUS-ACTION
  esdus_stream->if_reader_mm~read_root( personalization ).

  " Function module 'MEPERS_START_ITEM_PROP' must be called inside a screen sequence because
  " in l_popup->send() (CL_WINDOW_MM), there is "PERFORM set_screen IN PROGRAM (prog) USING old_dyn"
  " (PROG = '') which does a SET SCREEN and later, the end of program produces a short dump DYNPRO_NOT_FOUND.
  CALL SELECTION-SCREEN 1001.

  ASSERT 1 = 1.

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = '1001'.
    DATA(l_save) = EXACT mmpur_bool( abap_false ).
    CALL FUNCTION 'MEPERS_START_ITEM_PROP'
      EXPORTING
        im_objecttype = EXACT string40( if_purchase_requisition=>c_objtyp )
      IMPORTING
        ex_savesetup  = l_save.
    IF l_save = abap_true.
      " Transfer screen input to global data in function group of ES_SAVE_USER_SETTINGS.
      " This doesn't transfer "deletions" of 'PurchaseRequisition' personal settings which are not displayed.
      " Don't use ESDUS_STREAM->HANDLE_SAVE_REQUEST( ), which would delete them.

      sy-uname = uname.
      esdus_stream->if_writer_mm~write_root( personalization ).
      CALL FUNCTION 'ES_SAVE_USER_SETTINGS'.
      COMMIT WORK.
    ENDIF.

    SUPPRESS DIALOG.
    LEAVE TO SCREEN 0.
  ENDIF.
