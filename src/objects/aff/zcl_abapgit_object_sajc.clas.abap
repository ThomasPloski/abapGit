class ZCL_ABAPGIT_OBJECT_SAJC definition
  public
  inheriting from ZCL_ABAPGIT_OBJECT_COMMON_AFF
  final
  create public .

public section.

  methods ZIF_ABAPGIT_OBJECT~CHANGED_BY
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SAJC IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    CONSTANTS c_table_name TYPE tabname VALUE 'APJ_W_JCE_ROOT'.

    SELECT SINGLE lst_ch_user_acct
      FROM (c_table_name)
      INTO rv_user
      WHERE job_catalog_entry_name = ms_item-obj_name
        AND job_catalog_entry_version = 'I'.

    IF rv_user IS INITIAL.
      SELECT SINGLE lst_ch_user_acct
        FROM (c_table_name)
        INTO rv_user
        WHERE job_catalog_entry_name = ms_item-obj_name
          AND job_catalog_entry_version = 'A'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
