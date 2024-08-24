class lcl_table_with_xinx definition final.

  PUBLIC SECTION.
    methods:
      constructor
        importing iv_tabname type tabname,
      create_activate_table
        importing iv_doma_conv type abap_bool default abap_false
                  iv_act_mode  type dcmode default 'O',
      create_secondary_index
        importing is_dd12v     type dd12v
                  iv_fieldname type fieldname,
      delete_table,
      activate
        importing iv_idx_id type indexid optional.

  private section.

    constants:
      co_test_domain  type domname value 'ZPL_DOMA',
      co_test_dtype   type domname value 'ZPL_DTEL',
      co_2ndidx_name  type indexid value 'S01'.

    data:
      mv_tabname  type tabname,
      mt_ddfields type standard table of ddfield.

    types:
      begin of s_field,
        field_name type fieldname,
        is_key     type keyflag,
        data_type  type datatype_d,
        length     type ddleng,
        decimals   type decimals,
        not_null   type notnull,
      end of s_field,
      t_field type standard table of s_field with default key.

endclass.


class lcl_table_with_xinx implementation.

  method create_activate_table.

    data: ls_dd01v  type dd01v,
          ls_dd02v  type dd02v,
          ls_dd09l  type dd09l,
          ls_dd04l  type dd04l,
          lv_act_rc type sysubrc,
          lt_dd04l  type standard table of dd04l,
          lt_gentab type standard table of dcgentb,
          lt_deltab type standard table of dcdeltb,
          lt_dd07v  type standard table of dd07v with empty key,
          lt_dd03p  type standard table of dd03p with default key.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""" create domain
    ls_dd01v = value #( domname   = co_test_domain
                        datatype  = 'CHAR'
                        leng      = 20
                        outputlen = 20 ).
    if iv_doma_conv = abap_true.
      ls_dd01v-convexit = 'ALPHA'.
    endif.

    call function 'DD_DOMA_PUT'
      exporting
        dd01v_wa            = ls_dd01v
        domain_name         = ls_dd01v-domname
        prid                = -1
      tables
        dd07v_tab           = lt_dd07v
      exceptions
        illegal_value       = 1
        op_failure          = 2
        object_inconsistent = 3
        others              = 99.

    cl_abap_unit_assert=>assert_subrc( sy-subrc ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""" create data type
    ls_dd04l = value #( rollname = co_test_dtype
                        domname  = co_test_domain ).

    call function 'DD_DTEL_PUT'
      exporting
        dd04l_wa            = ls_dd04l
        prid                = -1
        rollname            = ls_dd04l-rollname
      tables
        dd04t_tab           = lt_dd04l
      exceptions
        illegal_value       = 1
        object_inconsistent = 2
        db_access_failure   = 3
        others              = 99.

    cl_abap_unit_assert=>assert_subrc( sy-subrc ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""" create table
    " table header
    ls_dd02v-tabname    = mv_tabname.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-mainflag   = 'X'.
    ls_dd02v-langdep    = 'X'.
    ls_dd02v-clidep     = 'X'.
    ls_dd02v-exclass    = '4'.
    ls_dd02v-ddlanguage = 'E'.
    ls_dd02v-ddtext     = 'TEST'.

    " technical settings
    ls_dd09l-tabname  = mv_tabname.
    ls_dd09l-tabkat   = '0'.
    ls_dd09l-tabart   = 'APPL0'.
    ls_dd09l-pufferung = 'X'.
    ls_dd09l-roworcolst = 'C'.

    " table fields
    data(lt_fields) = value t_field(
                     ( field_name = 'KEY1'   is_key = abap_true  data_type = 'CLNT' length = 3 )
                     ( field_name = 'KEY2'   is_key = abap_true  data_type = 'CHAR' length = 10 )
                     ( field_name = 'CHAR1'  is_key = abap_false data_type = 'CHAR' length = 3 )
                     ( field_name = 'CHAR2'  is_key = abap_false data_type = 'CHAR' length = 20 )
                     ( field_name = 'SSTR1'  is_key = abap_false data_type = 'SSTR' length = 1024 )
                     ( field_name = 'ALPHAC' is_key = abap_false data_type = 'CHAR' length = 20 )
                     ( field_name = 'AINTV'  is_key = abap_false data_type = 'INT4' length = 10 )
                     ( field_name = 'STEXT'  is_key = abap_false data_type = 'STRG' )
                     ( field_name = 'RSTR1'  is_key = abap_false data_type = 'RSTR' )
                     ( field_name = 'AINTV2' is_key = abap_false data_type = 'INT4' length = 10 )
                     ( field_name = 'CHAR3'  is_key = abap_false data_type = 'CHAR' length = 3 )
                     ( field_name = 'ALPHA2' is_key = abap_false data_type = 'CHAR' length = 20 )     ).

    loop at lt_fields reference into data(lr_field).

      if sy-tabix = 6 or sy-tabix = 12.
        append value #( tabname   = mv_tabname
                        fieldname = lr_field->field_name
                        position  = sy-tabix
                        keyflag   = lr_field->is_key
                        rollname  = ls_dd04l-rollname
                        datatype  = lr_field->data_type
                        leng      = lr_field->length
                        decimals  = lr_field->decimals
                        notnull   = cond #( when lr_field->is_key = abap_true then abap_true else lr_field->not_null ) ) to lt_dd03p.
      else.
        append value #( tabname   = mv_tabname
                        fieldname = lr_field->field_name
                        position  = sy-tabix
                        keyflag   = lr_field->is_key
                        datatype  = lr_field->data_type
                        leng      = lr_field->length
                        decimals  = lr_field->decimals
                        notnull   = cond #( when lr_field->is_key = abap_true then abap_true else lr_field->not_null ) ) to lt_dd03p.
      endif.

      append value #( fieldname = lr_field->field_name
                      position = sy-tabix
                      keyflag = lr_field->is_key
                      datatype = lr_field->data_type
                      leng =  lr_field->length
                      decimals = lr_field->decimals
                      nullable = cond #( when lr_field->is_key = abap_true then abap_false else abap_true )  ) to mt_ddfields.

    endloop.

    " Creation of table TABNAME
    call function 'DDIF_TABL_PUT'
      exporting
        name              = mv_tabname  " Name of the Table to be Written
        dd02v_wa          = ls_dd02v    " Table Header
        dd09l_wa          = ls_dd09l    " Technical Settings of the Table
      tables
        dd03p_tab         = lt_dd03p    " Table Fields
      exceptions
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        others            = 6.
    if sy-subrc <> 0.
      cl_abap_unit_assert=>assert_subrc( msg = |table creation failed -> DDIF_TABL_PUT: Exception | && sy-subrc ) ##NO_TEXT.
    endif.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""" mass activation
    lt_gentab = value #( ( name = co_test_domain type = 'DOMA' )
                         ( name = co_test_dtype  type = 'DTEL' )
                         ( name = mv_tabname   type = 'TABL' ) ).

    call function 'DD_MASS_ACT_C3'
      exporting
        ddmode         = iv_act_mode
        inactive       = ' '
        version        = 'M'
        device         = 'T'
        write_log      = 'X'
        t_on           = ' '
        parallel       = 'X'
        prid           = -1
      importing
        act_rc         = lv_act_rc
      tables
        gentab         = lt_gentab
        deltab         = lt_deltab
      exceptions
        access_failure = 1
        no_objects     = 2
        locked         = 3
        internal_error = 4
        others         = 5.

    cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_number_between( lower = 0 upper = 4 number = lv_act_rc ).

  endmethod.

  method create_secondary_index.
    data: lv_idx_name type indexid,
          ls_dd12v    type dd12v,
          lt_dd17v    type standard table of dd17v with empty key.

    ls_dd12v = is_dd12v.

    if ls_dd12v-indexname is initial.
      lv_idx_name = co_2ndidx_name.
      ls_dd12v-indexname =  co_2ndidx_name.
    else.
      lv_idx_name = ls_dd12v-indexname.
    endif.

    lt_dd17v = value #( ( ddlanguage = 'E' sqltab = ls_dd12v-sqltab indexname = ls_dd12v-indexname position = '0001' fieldname = iv_fieldname ) ).

    call function 'DDIF_INDX_PUT'
      exporting
        name              = mv_tabname
        id                = lv_idx_name
        dd12v_wa          = ls_dd12v
      tables
        dd17v_tab         = lt_dd17v
      exceptions
        indx_not_found    = 1
        name_inconsistent = 2
        indx_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        others            = 99.

    if sy-subrc <> 0.
      cl_abap_unit_assert=>assert_subrc( msg = |DDIF_INDX_PUT: Exception | && sy-subrc ) ##NO_TEXT.
    endif.
  endmethod.

  method delete_table.
    data wa type tadir.

    call function 'DD_TABL_DEL'
      exporting
        tabname   = mv_tabname
        del_state = 'M'.    "Delete all versions
* remove tadir entry as well
    wa-pgmid     = 'R3TR'.
    wa-object    = 'TABL'.
    wa-obj_name  = mv_tabname.

    delete tadir from wa.

    if ( sy-subrc = 0 ).
      call function 'DB_COMMIT'.
    endif.

* remove tadir entry as well
    wa-pgmid     = 'R3TR'.
    wa-object    = 'TABL'.
    wa-obj_name  = mv_tabname.

    delete tadir from wa.

    if ( sy-subrc = 0 ).
      call function 'DB_COMMIT'.
    endif.

    call function 'DDIF_OBJECT_DELETE'
      exporting
        type = 'TABL'
        name = mv_tabname
      exceptions
        others = 1.

    call function 'DD_DTEL_DEL'
      exporting
        rollname   = co_test_dtype
        del_state  = 'M'    "Delete all versions
        prid       = -1.

    call function 'DD_DOMA_DEL'
      exporting
        domname   = co_test_domain
        del_state = 'M'    "Delete all versions
        prid      = -1.

    commit work.
  endmethod.

  method constructor.
    mv_tabname = iv_tabname.
  endmethod.

  method activate.

    data: tmprc     type sy-subrc,
          lv_rc     type sy-subrc,
          lt_gentab type standard table of dcgentb,
          lt_deltab type standard table of dcdeltb.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    if iv_idx_id is initial.
       lt_gentab = value #( ( name = mv_tabname   type = 'TABL' )  ).
    else.
       lt_gentab = value #( ( name = mv_tabname   type = 'TABL' )
                            ( name = mv_tabname   indx = iv_idx_id    type = 'XINX' )  ).
    endif.

    call function 'DD_MASS_ACT_C3'
      exporting
        ddmode         = 'O'
        inactive       = ' '
        version        = 'M'
        device         = 'T'
        write_log      = 'X'
        t_on           = ' '
        parallel       = 'X'
        prid           = -1
      importing
        act_rc         = tmprc
      tables
        gentab         = lt_gentab
        deltab         = lt_deltab
      exceptions
        access_failure = 1
        no_objects     = 2
        locked         = 3
        internal_error = 4
        others         = 5.
    lv_rc = sy-subrc.

    cl_abap_unit_assert=>assert_subrc( exp = 0 act = lv_rc ).
    cl_abap_unit_assert=>assert_equals( act = tmprc exp = 0 msg = `expect activation to be sccuess` ) ##NO_TEXT.

  endmethod.

endclass.
