*&---------------------------------------------------------------------*
*& Report  Z_UPLOAD_EXCEL_ITAB
*& AUTOR: David Rueda barrón
*&---------------------------------------------------------------------*
*& Subir datos de un fichero excel XLS a tabla interna
*& Este código solamente funciona con ficheros exceL XLS
*&
*& Debemos conocer de antemano el formato del fichero excel.
*& para este ejemplo el formato es el siguiente
*&  - 1º Linea del excel es la cabecera
*&  - Las columas y su orden en el  excel son:
*&       lifnr - Proveedor
*&       matnr - Material
*&       esokz - Tipo Registro Info
*&       netpr - Precio
*&       peinh - Cantidad Base
*&       waers - moneda
*&       bprme - UM Precio
*&---------------------------------------------------------------------*

REPORT z_upload_excel_itab.

*&---------------------------------------------------------------------*
*&  Tipos y estructuras globales
*&---------------------------------------------------------------------*
TYPES: BEGIN OF type_datos,
         lifnr TYPE ekko-lifnr, "Proveedor
         matnr TYPE ekpo-matnr, "Material
         esokz TYPE eine-esokz, "Tipo Registro Info
         netpr TYPE eine-netpr, "Precio
         peinh TYPE eine-peinh, "Cantidad Base
         waers TYPE eine-waers, "moneda
         bprme TYPE eine-bprme, "UM Precio
       END OF type_datos.

TYPES type_t_datos TYPE STANDARD TABLE OF type_datos.

*&---------------------------------------------------------------------*
*& Variable globales
*&---------------------------------------------------------------------*
DATA tg_datos TYPE type_t_datos.

DATA gr_table TYPE REF TO cl_salv_table.
DATA cx_salv  TYPE REF TO cx_salv_msg.

DATA gr_msg TYPE string.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
PARAMETERS: p_file   TYPE rlgrap-filename OBLIGATORY.

*&---------------------------------------------------------------------*
*& At Selection-Screen
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM set_filepath CHANGING  p_file.  "Ayuda Busqueda, navegador de archivos


*&---------------------------------------------------------------------*
*& Start-of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM upload_excel_it USING    p_file
                          CHANGING tg_datos.

  TRY.
      cl_salv_table=>factory(
         IMPORTING
           r_salv_table = gr_table
         CHANGING
           t_table      = tg_datos ).

      gr_table->display( ).
    CATCH cx_salv_msg INTO cx_salv.
*     gestionamos las excepciones que puedan suceder
      gr_msg = cx_salv->get_text( ).
      MESSAGE gr_msg TYPE 'E'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&      Form  set_filepath
*&---------------------------------------------------------------------*
* Este FORM sirve para el explorador en el que se selecciona el archivo
*----------------------------------------------------------------------*
FORM set_filepath  CHANGING po_ruta TYPE rlgrap-filename.

  CONSTANTS: c_ext_exl   TYPE string     VALUE '*.XLS'.

  DATA: lt_filetable TYPE filetable,
        lx_filetable TYPE file_table,
        wl_sel_text  TYPE string,
        lv_rc        TYPE i.

  CLEAR po_ruta.

  wl_sel_text = text-s01.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wl_sel_text
      default_extension       = c_ext_exl
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lt_filetable INTO lx_filetable INDEX 1.
    CHECK sy-subrc EQ 0.
    po_ruta = lx_filetable-filename.
  ENDIF.


ENDFORM.                    " SET_FILEPATH
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL_IT
*&---------------------------------------------------------------------*
*& Subir un archivo excel a una tabla interna
*&---------------------------------------------------------------------
FORM upload_excel_it USING    pi_ruta TYPE rlgrap-filename
                     CHANGING to_file TYPE type_t_datos.


  TYPES: BEGIN OF type_excel,
           lifnr(10) TYPE c, "Proveedor
           matnr(18) TYPE c, "Material
           esokz(1)  TYPE c, "Tipo Registro Info
           netpr(13) TYPE c, "Precio
           peinh(5)  TYPE c, "Cantidad Base
           waers(3)  TYPE c, "moneda
           bprme(3)  TYPE c, "UM Precio
         END OF type_excel.

  DATA: tl_exc TYPE STANDARD TABLE OF type_excel.
  DATA: it_raw TYPE truxs_t_text_data.


  FIELD-SYMBOLS:  <fs_exc>  TYPE type_excel,
                  <fs_file> TYPE LINE OF type_t_datos.

  REFRESH: to_file, tl_exc.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*     i_line_header        = 'X'
      i_tab_raw_data       = it_raw       " WORK TABLE
      i_filename           = pi_ruta
    TABLES
      i_tab_converted_data = tl_exc[]    "ACTUAL DATA
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF ( sy-subrc <> 0 ).
    MESSAGE text-e02  TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.

    DELETE  tl_exc INDEX 1.                    "eliminar la cabecera

    LOOP AT tl_exc ASSIGNING <fs_exc>.

      APPEND INITIAL LINE TO to_file ASSIGNING <fs_file>.

      PERFORM conversion_sap_format USING <fs_exc>-lifnr CHANGING <fs_file>-lifnr.
      PERFORM conversion_sap_format USING <fs_exc>-matnr CHANGING <fs_file>-matnr.
      PERFORM conversion_sap_format USING <fs_exc>-esokz CHANGING <fs_file>-esokz.
      PERFORM conversion_sap_format USING <fs_exc>-waers CHANGING <fs_file>-waers.

      PERFORM conversion_sap_unit USING <fs_exc>-bprme CHANGING <fs_file>-bprme.

      PERFORM conversion_sap_num USING <fs_exc>-netpr CHANGING <fs_file>-netpr.
      PERFORM conversion_sap_num USING <fs_exc>-peinh CHANGING <fs_file>-peinh.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPLOAD_EXCEL_IT

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SAP_FORMAT
*&---------------------------------------------------------------------*
FORM conversion_sap_format  USING    pi_output
                            CHANGING po_output.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pi_output
    IMPORTING
      output = po_output.

ENDFORM.                    " CONVERSION_SAP_FORMAT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SAP_UNIT
*&---------------------------------------------------------------------*
FORM conversion_sap_unit  USING    pi_output
                          CHANGING po_output.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input          = pi_output
      language       = sy-langu
    IMPORTING
      output         = po_output
    EXCEPTIONS
      unit_not_found = 1.

  IF sy-subrc <> 0.
    CLEAR po_output.
  ENDIF.

ENDFORM.                    " CONVERSION_SAP_FORMAT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SAP_NUM
*&---------------------------------------------------------------------*
FORM conversion_sap_num  USING    pi_output
                          CHANGING po_output.

  CALL FUNCTION 'MOVE_CHAR_TO_NUM'
    EXPORTING
      chr             = pi_output
    IMPORTING
      num             = po_output
    EXCEPTIONS
      convt_no_number = 1
      convt_overflow  = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    CLEAR po_output.
  ENDIF.

ENDFORM.                    " CONVERSION_SAP_FORMAT
