*&---------------------------------------------------------------------*
*& Report  Z_DOWNLOAD_EXCEL_TO_FILE
*& AUTOR: David Rueda Barrón
*&---------------------------------------------------------------------*
*& Descarga el contenido de la tabla SPFLI a un fichero local EXCEL
*&
*&---------------------------------------------------------------------*

REPORT Z_DOWNLOAD_EXCEL_TO_FILE.

*&---------------------------------------------------------------------*
*& Start-of-Selection
*&---------------------------------------------------------------------*
TYPES: BEGIN OF type_spfli,
  carrid     TYPE spfli-carrid,    "compañía aérea
  connid     TYPE spfli-connid,    "Código de conexión de vuelo directo
  countryfr  TYPE spfli-countryfr, "Clave de país
  cityfrom   TYPE spfli-cityfrom,  "Ciudad de salida
  airpfrom   TYPE spfli-airpfrom,  "Aeropuerto de salida
  countryto  TYPE spfli-countryto, "Clave de país
  cityto     TYPE spfli-cityto,    "Ciudad de llegada
  airpto     TYPE spfli-airpto,    "Aeropuerto de destino
END OF type_spfli.

TYPES  TYPE_T_spfli TYPE STANDARD TABLE OF type_spfli.

DATA ti_spfli TYPE TYPE_T_spfli.
DATA wg_file TYPE string.

*&---------------------------------------------------------------------*
*& Start-of-Selection
*&---------------------------------------------------------------------*
start-of-selection.

  SELECT carrid
         connid
         countryfr
         cityfrom
         airpfrom
         countryto
         cityto
   INTO CORRESPONDING FIELDS OF TABLE ti_spfli
    FROM spfli.

  PERFORM get_filename  CHANGING wg_file.
  PERFORM download_to_exc  USING wg_file ti_spfli.

*---------------------------------------------------------------------*
*      Form  GET_FILENAME
*---------------------------------------------------------------------*
*     Abrir navegador para seleccionar fichero
*----------------------------------------------------------------------*
FORM get_filename  CHANGING p_file TYPE string.

  DATA w_file LIKE rlgrap-filename.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name     = w_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

  p_file = w_file.

ENDFORM.                      "GET_FILENAME

*---------------------------------------------------------------------*
*      Form  DOWNLOAD_TO_EXC
*---------------------------------------------------------------------*
FORM download_to_exc  USING Pi_file  TYPE STRING
                            ti_spfli TYPE type_t_spfli.

  TYPES: BEGIN OF tl_name,
    fldname(11) TYPE c,
   END OF tl_name.

  DATA: tl_name  TYPE STANDARD TABLE OF tl_name.

  DATA: wl_file TYPE  rlgrap-filename.

  FIELD-SYMBOLS:  <FL> TYPE tl_name.

  wl_file = Pi_file.

* Cabecera con el nombre de las columnas del EXCEL
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'compañía aérea'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Código de conexión de vuelo directo'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Clave de país'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Ciudad de salida'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Aeropuerto de salida'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Clave de país'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Ciudad de llegada'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Ciudad de llegada'.
  APPEND INITIAL LINE TO tl_name ASSIGNING <FL>.
  <FL>-fldname =  'Aeropuerto de destino'.


  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
    EXPORTING
      file_name                 = wl_file
      data_sheet_name           = 'SPFLI'
      password_option           = 0
    TABLES
      data_tab                  = ti_spfli
      fieldnames                = tl_name
    EXCEPTIONS
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_filename          = 6
      invalid_pivot_fields      = 7
      download_problem          = 8
      OTHERS                    = 9.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(yv01) WITH 'File does not exist'.
    WHEN 2.
      MESSAGE e000(yv01) WITH 'Filename expected'.
    WHEN 3.
      MESSAGE e000(yv01) WITH 'Communication error'.
    WHEN 4.
      MESSAGE e000(yv01) WITH 'OLE object method error'.
    WHEN 5.
      MESSAGE e000(yv01) WITH 'OLE object property error'.
    WHEN 6.
      MESSAGE e000(yv01) WITH 'Invalid filename'.
    WHEN 7.
      MESSAGE e000(yv01) WITH 'Invalid pivot fields'.
    WHEN 8.
      MESSAGE e000(yv01) WITH 'Download problem'.
    WHEN 9.
      MESSAGE e000(yv01) WITH 'Other problem'.
  ENDCASE.


ENDFORM.                    " DOWNLOAD_TO_EXC
