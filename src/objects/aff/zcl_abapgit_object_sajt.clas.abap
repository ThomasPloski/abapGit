CLASS zcl_abapgit_object_sajt DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_object~changed_by
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SAJT IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    CONSTANTS c_table_name TYPE tabname VALUE 'APJ_W_JT_ROOT'.

    SELECT SINGLE lst_ch_user_acct
      FROM (c_table_name)
      INTO rv_user
      WHERE job_template_name = ms_item-obj_name
        AND job_template_version = 'I'.

    IF rv_user IS INITIAL.
      SELECT SINGLE lst_ch_user_acct
        FROM (c_table_name)
        INTO rv_user
        WHERE job_template_name = ms_item-obj_name
          AND job_template_version = 'A'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
