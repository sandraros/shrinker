********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_DEMO_ABAP2XLSX_LICEN
*
********************************************************************************
INTERFACE ZIF_SHRINKER_DEMO_ABAP2XLSX_WH PUBLIC.

    TYPES:
      BEGIN OF ty_cell,
        name    TYPE c LENGTH 10, "AAA1234567"
        style   TYPE i,
        type    TYPE c LENGTH 9,
        formula TYPE string,
        value   TYPE string,
      END OF ty_cell .

    DATA:
      cells TYPE STANDARD TABLE OF ty_cell .

    METHODS get_cells
      IMPORTING
        !i_row   TYPE i
        !i_index TYPE i .
ENDINTERFACE.
