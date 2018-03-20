*&---------------------------------------------------------------------*
*& Report  Z_UPLOAD_EXCEL_TO_INTERNAL_TAB
*& AUTOR: David Rueda barrón
*&---------------------------------------------------------------------*
REPORT  z_upload_excel_to_internal_tab.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
PARAMETERS: p_file   TYPE localfile OBLIGATORY,
            p_row    TYPE i         OBLIGATORY,
            p_column TYPE i         OBLIGATORY.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*
CONSTANTS: c_fs_wa       TYPE lvc_fname VALUE '-',
           c_ext_xls     TYPE string    VALUE '*.xls'.

*&---------------------------------------------------------------------*
*& Structure
*&---------------------------------------------------------------------*
TYPES: type_excel_tab TYPE  STANDARD TABLE OF alsmex_tabline.

*&---------------------------------------------------------------------*
*& Internal Tables
*&---------------------------------------------------------------------*
DATA: ti_excel_tab TYPE type_excel_tab.

*&---------------------------------------------------------------------*
*& Reference Variables
*&---------------------------------------------------------------------*
DATA: t_dyn_tab   TYPE REF TO data,
      x_dyn_wa    TYPE REF TO data.

*&---------------------------------------------------------------------*
*& Field Symbols
*&---------------------------------------------------------------------*
FIELD-SYMBOLS : <FS_TABLE> TYPE STANDARD TABLE,
                <FS_WA>    TYPE ANY.

*&---------------------------------------------------------------------*
*& At Selection-Screen
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_open_file.  "Ayuda Busqueda, navegador de archivos

*&---------------------------------------------------------------------*
*& Start-of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM upload_excel_to_itab USING  p_file
                                      p_row
                                      p_column
                               CHANGING ti_excel_tab.



  IF ti_excel_tab IS NOT INITIAL.

    PERFORM create_dynamic_itab_wa USING ti_excel_tab.
    PERFORM fill_data              USING ti_excel_tab.
    PERFORM display_output         USING ti_excel_tab.

  ELSE.
    MESSAGE text-s01 TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F4_OPEN_FILE
*&---------------------------------------------------------------------*
FORM f4_open_file.

  DATA: lt_filetable    TYPE filetable,
        lx_filetable    TYPE file_table,
        lv_return_code  TYPE i,
        lv_window_title TYPE string.

  lv_window_title = text-001.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title
      default_extension       = c_ext_xls
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_return_code
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_filetable INTO lx_filetable INDEX 1.
  p_file = lx_filetable-filename.

ENDFORM.                    " F4_OPEN_FILE
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL_TO_ITAB
*&---------------------------------------------------------------------*
FORM upload_excel_to_itab USING pi_file   TYPE localfile
                                pi_row    TYPE i
                                pi_column TYPE i
                          CHANGING TO_EXCEL_TAB TYPE type_excel_tab.

  REFRESH: TO_EXCEL_TAB.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = pi_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = pi_column
      i_end_row               = pi_row
    TABLES
      intern                  = TO_EXCEL_TAB
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " UPLOAD_EXCEL_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_ITAB_WA
*&---------------------------------------------------------------------*
FORM create_dynamic_itab_wa USING TI_EXCEL_TAB TYPE type_excel_tab.

  DATA: lr_datdr  TYPE REF TO cl_abap_datadescr,
        lr_struc  TYPE REF TO cl_abap_structdescr,
        gw_comp   TYPE abap_componentdescr,
        gt_comp   TYPE abap_component_tab.

  FIELD-SYMBOLS: <FS_EXCEL> TYPE LINE OF type_excel_tab.

  LOOP AT  TI_EXCEL_TAB ASSIGNING <FS_EXCEL> WHERE row EQ '1' .

    lr_datdr ?= cl_abap_datadescr=>describe_by_data( <FS_EXCEL>-VALUE ).
    gw_comp-name = <FS_EXCEL>-VALUE.
    gw_comp-type = lr_datdr.
    APPEND gw_comp  TO gt_comp. CLEAR gw_comp.

  ENDLOOP.

  TRY.
    lr_struc = cl_abap_structdescr=>create( p_components = gt_comp ).
  CATCH cx_sy_struct_creation.
    WRITE: / 'CX_SY_STRUCT_CREATION ERROR'.
  ENDTRY.

  CREATE DATA x_dyn_wa TYPE HANDLE lr_struc.
  ASSIGN x_dyn_wa->*  TO <FS_WA>.
  CREATE DATA t_dyn_tab LIKE STANDARD TABLE OF <FS_WA>.
  ASSIGN t_dyn_tab->* TO <FS_TABLE>.

ENDFORM.                    " CREATE_DYNAMIC_ITAB_WA
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
FORM fill_data USING TI_EXCEL_TAB TYPE type_excel_tab.

  FIELD-SYMBOLS: <FS_EXCEL> TYPE LINE OF type_excel_tab,
                 <FS_C>     TYPE LINE OF type_excel_tab,
                 <FS_FIELD> TYPE ANY.

  LOOP AT ti_excel_tab ASSIGNING <FS_EXCEL> WHERE row NE '1' . " 1 -> cabecera
*  ¿Que columna estamos tratando ?
   READ TABLE ti_excel_tab ASSIGNING <FS_C> WITH KEY ROW = '1' COL = <FS_EXCEL>-COL.
   CHECK ( SY-SUBRC EQ 0 ).

   READ TABLE <FS_TABLE> ASSIGNING <FS_WA> INDEX ( <FS_EXCEL>-ROW - 1 ).
   IF ( SY-SUBRC <> 0 ).
     APPEND INITIAL LINE TO <FS_TABLE>  ASSIGNING <FS_C>.
   ENDIF.

   ASSIGN COMPONENT <FS_C>-VALUE OF STRUCTURE <FS_TABLE> TO <FS_FIELD>.
   CHECK ( Sy-SUBRC EQ 0 ).

*  Realizar las conversiones de datos necesarias ahora
   <FS_FIELD> = <FS_EXCEL>-VALUE.

   ENDLOOP.

ENDFORM.                    " FILL_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM display_output USING TI_EXCEL_TAB TYPE type_excel_tab.

  DATA: lo_table     TYPE REF TO cl_salv_table,
        lo_functions TYPE REF TO cl_salv_functions_list,
        lo_column    TYPE REF TO cl_salv_column,
        lo_columns   TYPE REF TO cl_salv_columns.

  DATA: lv_short     TYPE scrtext_s,
        lv_medium    TYPE scrtext_m,
        lv_long      TYPE scrtext_l.

  DATA: wl_ddtext TYPE dd04t-ddtext,
        wl_FNAME  TYPE LVC_FNAME.

  FIELD-SYMBOLS: <FS_EXCEL> TYPE LINE OF type_excel_tab.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_table
        CHANGING
          t_table      = <FS_TABLE>.
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.


  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).

  lo_columns = lo_table->get_columns( ).

  lo_columns->set_optimize( abap_true ).

   LOOP AT  TI_EXCEL_TAB ASSIGNING <FS_EXCEL>.

    CLEAR: wl_ddtext, wl_FNAME .

    wl_FNAME = <FS_EXCEL>-VALUE.
    TRANSLATE wl_FNAME TO UPPER CASE.

    SELECT SINGLE ddtext
      INTO wl_ddtext
      FROM dd04t
      WHERE rollname   = wl_FNAME
        AND ddlanguage = sy-langu
        AND as4local   = 'A'.

    lv_short  = wl_ddtext+0(10).
    lv_medium = wl_ddtext+0(20).
    lv_long   = wl_ddtext.


      TRY.
          lo_column = lo_columns->get_column( wl_FNAME  ).
          lo_column->set_short_text( lv_short ).
          lo_column->set_medium_text( lv_medium ).
          lo_column->set_long_text( lv_long ).
        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
      ENDTRY.


  ENDLOOP.

  lo_table->display( ).

ENDFORM.                    " DISPLAY_OUTPUT
