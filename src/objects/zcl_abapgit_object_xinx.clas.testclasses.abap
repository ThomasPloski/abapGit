class ltcl_xinx_handler definition final for testing
  duration short
  risk level harmless.

  private section.

    constants:
      co_xinx_name            type ddobjectid value 'X01',
      co_xinx_fsi             type ddobjectid value 'XFS',
      co_database_table_name  type tabname value 'DDTFO_TABWXINX',
      co_default_package      type devclass value 'TEST_XINX_ABAPGIT',
      co_activation_online    type ddmode value 'O',
      co_activation_transport type ddmode value 'T'.

    data:
      mo_cut    type ref to cl_abapgit_object_xinx,
      mo_table  type ref to lcl_table_with_xinx.

    methods:
      test_existing_xinx_exists for testing raising cx_static_check,
      test_xinx_not_exists for testing raising cx_static_check,
      test_active_xinx_active for testing raising cx_static_check,
      test_inactive_xinx_inactive for testing raising cx_static_check,
      test_changed_by for testing raising cx_static_check,
      test_delete for testing raising cx_static_check,

      test_serialize for testing raising cx_static_check,
      test_deserialize for testing raising cx_static_check,

      setup,
      teardown.

endclass.


class ltcl_xinx_handler implementation.

  method setup.
    data: ls_dd12v   type dd12v,
          lv_objname type sobj_name.

    mo_table = new lcl_table_with_xinx( co_database_table_name ).

    ls_dd12v = value #( sqltab = co_database_table_name
                        indexname = co_xinx_name
                        isextind = abap_true
                        as4user = sy-uname  ).
    " given
    mo_table->create_activate_table( ).
    mo_table->create_secondary_index( is_dd12v = ls_dd12v  iv_fieldname = 'CHAR1'  ).
    mo_table->activate( co_xinx_name ).

    " given
    CONCATENATE co_database_table_name co_xinx_name into lv_objname SEPARATED BY space.
    data(ls_item) = value if_abapgit_definitions=>ty_item( obj_type  = 'XINX'
                                                           obj_name  = lv_objname
                                                           devclass  = '$TMP'
                                                           inactive  = abap_false
                                                           srcsystem = 'XYZ' ).
    mo_cut = new cl_abapgit_object_xinx( is_item = ls_item    iv_language = 'E' ).
    mo_cut->mo_files = new cl_abapgit_objects_files( is_item = ls_item ).
  endmethod.

  method test_deserialize.

    data: ls_dd12v   type dd12v,
          lt_dd17v   type dd17vtab,
          properties type rswsourcet.


    properties = value #(
    ( `{`)
    ( ` "formatVersion": "1",`)
    ( ` "header":{`)
    ( `  "description": "deserialize",`)
    ( `  "originalLanguage": "en"` )
    ( ` },`)
    ( ` "textIndex": true,"uniqueIndex": true,"indexFields":`)
    ( ` [{"fieldName": "CHAR1"`)
    ( ` },`)
    ( ` {"fieldName": "CHAR3"`)
    ( `}] `)
    ( `}`) ).

    data(json_as_xstring) = cl_aff_content_handler_factory=>get_handler_for_plain_text( )->serialize( properties ).
    mo_cut->mo_files->add_raw( iv_ext = 'json' iv_data = json_as_xstring ).

    mo_cut->if_abapgit_object~deserialize(
      iv_package = '$TMP'
      iv_transport = ''
      io_xml     = value #( )
      iv_step    = if_abapgit_object=>gc_step_id-abap
      ii_log     = new cl_abapgit_log( ) ).

    " test updated data
    clear: ls_dd12v, lt_dd17v.
    call function 'DDIF_INDX_GET'
      exporting
        name          = co_database_table_name
        id            = co_xinx_name
        state         = 'M'
      importing
        dd12v_wa      = ls_dd12v
      tables
        dd17v_tab     = lt_dd17v
      exceptions
        illegal_input = 1
        others        = 2.
    cl_abap_unit_assert=>assert_equals( exp = 'N'  act = ls_dd12v-as4local ).
    cl_abap_unit_assert=>assert_equals( exp = 'F'  act = ls_dd12v-full_text ).
    cl_abap_unit_assert=>assert_equals( exp = 2  act = lines( lt_dd17v ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'CHAR3'  act = lt_dd17v[ 2 ]-fieldname ).

  endmethod. " test_deserialize

  method test_serialize.
    data: lv_objname type sobj_name,
          lv_string type string,
          lv_xinx_name type if_aff_obj=>obj_name.
    data settings_serialize type ref to if_aff_settings_serialize.

    " given
    CONCATENATE co_database_table_name co_xinx_name into lv_objname SEPARATED BY space.
    final(item) = value if_abapgit_definitions=>ty_item(
        obj_type  = 'XINX'
        obj_name  = lv_objname
        devclass  = co_default_package
        inactive  = abap_true
        srcsystem = 'XYZ' ).

    mo_cut->mo_files = new cl_abapgit_objects_files( item ).

    lv_string = '{#  "formatVersion": "1",#   "header": {#    "description": "",# ' &&
                ' "originalLanguage": "en"#  },#  "textIndex": false,#  "uniqueIndex": false,# ' &&
                ' "indexFields": [# {#   "fieldName": "CHAR1"#  }#  ]#}# '.
    "test
    final(empty_files) = mo_cut->mo_files->get_files(  ).
    cl_abap_unit_assert=>assert_initial( empty_files ).

    mo_cut->if_abapgit_object~serialize( ii_log = new cl_abapgit_log(  ) io_xml = new cl_abapgit_xml_output(  ) ).
    final(lt_act_files) = mo_cut->mo_files->get_files(  ).
    final(lv_json) = cl_abap_codepage=>convert_from( lt_act_files[ 1 ]-data ).

    "verify
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_act_files ) ).
    cl_abap_unit_assert=>assert_char_cp(
      exporting
        act              =  lv_json
        exp              =  '{*'
    ).

  endmethod. " test_serialize

  method test_active_xinx_active.
    "test
    data(lv_is_active) = mo_cut->if_abapgit_object~is_active( ).
    "verify
    cl_abap_unit_assert=>assert_true( act = lv_is_active  ).
  endmethod. " test_active_xinx_active

  method test_inactive_xinx_inactive.
" TODO! : in case of an inactive XINX, should 'exiests' method find the inactive version?
    data: ls_dd12v   type dd12v,
          lv_objname type sobj_name.
    " given
    ls_dd12v = value #( sqltab = co_database_table_name
                        indexname = co_xinx_fsi
                        isextind = abap_true
                        fuzzy_search_indx = abap_true
                        as4user = sy-uname  ).
    mo_table->create_secondary_index( is_dd12v = ls_dd12v  iv_fieldname = 'ALPHAC'  ).
    mo_table->activate( ).

    " given
    CONCATENATE co_database_table_name co_xinx_fsi into lv_objname SEPARATED BY space.
    data(lo_cut) = new cl_abapgit_object_xinx(
        is_item = value #(
            obj_type  = 'XINX'
            obj_name  = lv_objname
            devclass  = co_default_package
            inactive  = abap_false
            srcsystem = 'XYZ' )
           iv_language = 'E' ).

    "test
    " data(lv_exists) = lo_cut->if_abapgit_object~exists( ).
    data(lv_is_active) = lo_cut->if_abapgit_object~is_active( ).
    "verify
    " cl_abap_unit_assert=>assert_true( act = lv_exists  ).
    cl_abap_unit_assert=>assert_false( act = lv_is_active  ).

  endmethod. " test_inactive_xinx_inactive

  method test_changed_by.
    "test
    data(lv_user) = mo_cut->if_abapgit_object~changed_by( ).
    "verify
    cl_abap_unit_assert=>assert_equals( act = lv_user  exp = sy-uname ).
  endmethod. " test_changed_by

  method test_delete.

  endmethod. " test_delete

  method test_existing_xinx_exists.
    "test
    data(lv_exist) = mo_cut->if_abapgit_object~exists( ).
    "verify
    cl_abap_unit_assert=>assert_true( lv_exist ).
  endmethod. " test_existing_xinx_exists

  method test_xinx_not_exists.
    " given
    data(lo_cut) = new cl_abapgit_object_xinx(
        is_item = value #(
            obj_type  = 'XINX'
            obj_name  = 'SOMETABLE99  IDX'
            devclass  = co_default_package
            inactive  = abap_true
            srcsystem = 'XYZ' )
        iv_language = 'E' ).

    "test
    data(lv_user) = lo_cut->if_abapgit_object~changed_by( ).
    data(lv_exist) = lo_cut->if_abapgit_object~exists( ).

    "verify
    cl_abap_unit_assert=>assert_initial( lv_user ).
    cl_abap_unit_assert=>assert_false( lv_exist ).
  endmethod. " test_xinx_not_exists






  method teardown.
    mo_table->delete_table( ).
  endmethod.

endclass.
