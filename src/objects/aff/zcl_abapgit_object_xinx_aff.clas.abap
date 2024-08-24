CLASS zcl_abapgit_object_xinx_aff DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: zif_abapgit_object~changed_by REDEFINITION,
             zif_abapgit_object~is_active redefinition,
             zif_abapgit_object~get_deserialize_steps REDEFINITION.
  PROTECTED SECTION.

      methods get_metadata redefinition.

  PRIVATE SECTION.

    data mo_handler type ref to cl_dd_xinx_handler .
ENDCLASS.



CLASS zcl_abapgit_object_xinx_aff IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.
    DATA lt_names   TYPE STANDARD TABLE OF string.

    TRY.
        mo_handler = cl_xinx_aff_object_handler=>get_ddic_handler(  object_key = CONV #( ms_item-obj_name ) ).
        rv_user = mo_handler->get_header( )-as4user.
      CATCH cx_aff_root cx_dd_xinx_handler .
         clear rv_user.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND if_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    data: lv_table_name type tabname,
          lv_index_id   type indexid,
          lt_names      type standard table of string.

    split ms_item-obj_name at space into table lt_names.
    delete lt_names where table_line is initial.
    lv_table_name = lt_names[ 1 ].
    lv_index_id =  lt_names[ 2 ].

    select single as4local from DD12L where sqltab = @lv_table_name and indexname = @lv_index_id into @final(lv_as4local).

    if lv_as4local = 'A'.
        rv_active = abap_true.
    else.
        rv_active = abap_false.
    endif.
  ENDMETHOD.


  method get_metadata.
    rs_metadata = super->get_metadata( ).
    rs_metadata-ddic = abap_true.
  endmethod.

ENDCLASS.
