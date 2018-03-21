*&----------------------------------------------------------------------*
*& PROGRAMA.............:  z_abap best practices_1
*& AUTOR................:  David rueda Barrón
*& FECHA................:                                               *
*& TRANSACCION..........:                                               *
*& ORDEN DE TRANSPORTE..:                                               *
*& EMPRESA..............:                                               *
*&----------------------------------------------------------------------*
*& DESCRIPCION:                                                         *
*&  Programa 1 practicas de optimizacion y best practices ABAP
*&----------------------------------------------------------------------*
*& Modificador  Fecha   MARCA Motivo                                    *
*& ----------- -------- ----- ----------------------------------------- *
*& XXXXXXXX    DD.MM.AA  @01  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
*&                                                                      *
*&----------------------------------------------------------------------*

REPORT  z_abap best practices_1.

TABLES: vbak.

*&---------------------------------------------------------------------*
*&  ESTRUCTURAS GLOBALES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF type_data,
         vbeln     TYPE vbak-vbeln, "Nº de pedido
         audat     TYPE vbak-audat, "FEcha de pedido
         auart     TYPE vbak-auart, "Tipo de pedido SAP
         vkorg     TYPE vbak-vkorg, "Org. de ventas
         vtweg     TYPE vbak-vtweg, "Canal
         spart     TYPE vbak-spart, "Sector
         kunnr     TYPE vbak-kunnr, "Cliente
         name1     TYPE kna1-name1, "Nombre cliente
         lfstk     TYPE vbuk-lfstk, "status entrega
         kwmeng    TYPE vbap-kwmeng, "Cantidad
         netwr     TYPE vbap-netwr, "Importe
         descr(50) TYPE c,
       END OF type_data.

TYPES: type_t_data TYPE STANDARD TABLE OF type_data.

*&---------------------------------------------------------------------*
*&  DATOS GLOBALES
*&---------------------------------------------------------------------*
DATA ti_data TYPE type_t_data.



*&---------------------------------------------------------------------*
*& Parámetros de Selección
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block01 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_vbeln FOR vbak-vbeln,
                s_kunnr FOR vbak-kunnr.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK block01.

*&---------------------------------------------------------------------*
*& Validaciones de Pantalla
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

** Obtencion de los Datos
  DATA wa_return TYPE c.
  PERFORM f_carga_datos CHANGING ti_data
                                 wa_return.

  PERFORM obtenr_Mensajes.

** Lisatado de los datos
  PERFORM cargar_alv USING ti_data.


  WRITE: 'OK programa terminado' COLOR 5.
  WRITE: 'Regrese al menu principal' COLOR 5.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_CARGA_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TI_DATA  text
*----------------------------------------------------------------------*
FORM f_carga_datos  CHANGING ti_data TYPE type_t_data
                             pi_return.

  TYPES: BEGIN OF type_vbap,
           vbeln  TYPE vbap-vbeln,
           posnr  TYPE vbap-posnr,
           matnr  TYPE vbap-matnr,
           kwmeng TYPE vbap-kwmeng,
           netwr  TYPE vbap-netwr,
         END OF type_vbap.

  DATA tmp_data TYPE type_t_data.
  DATA tmp_pos  TYPE STANDARD TABLE OF type_vbap.

  DATA wl_cantidad(15) TYPE c.
  DATA wl_importe(15) TYPE c.

  DATA wl_variable TYPE c.

  FIELD-SYMBOLS: <fs_tmp_data> TYPE LINE OF type_t_data,
                 <fs_data_alv> TYPE LINE OF type_t_data,
                 <fs_pos>      TYPE type_vbap.

  REFRESH ti_data.

  SELECT vbak~vbeln
         vbak~audat
         vbak~auart
         vbak~vkorg
         vbak~vtweg
         vbak~spart
         vbak~kunnr
         vbuk~lfstk
    INTO CORRESPONDING FIELDS OF TABLE tmp_data
    FROM vbak
    INNER JOIN vbuk
      ON vbak~vbeln EQ vbuk~vbeln
    WHERE vbak~vbeln IN s_vbeln
      AND vbak~kunnr IN s_kunnr.

  DELETE tmp_data WHERE lfstk <> 'C'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE tmp_pos
    FROM vbap
    FOR ALL ENTRIES IN tmp_data
    WHERE vbeln EQ tmp_data-vbeln..

  WAIT UP TO 200 SECONDS.

  LOOP AT tmp_data ASSIGNING <fs_tmp_data>.

    APPEND INITIAL LINE TO ti_data ASSIGNING <fs_data_alv>.
    MOVE-CORRESPONDING <fs_tmp_data> TO <fs_data_alv>.

    SELECT SINGLE kunnr
                  land1
                  name1
                  name2
                  ort01
                  pstlz
                  regio
      INTO CORRESPONDING FIELDS OF  <fs_data_alv>
      FROM kna1
      WHERE kunnr = <fs_data_alv>-kunnr.




    LOOP AT tmp_pos ASSIGNING <fs_pos>.

      <fs_data_alv>-kwmeng = <fs_data_alv>-kwmeng + <fs_pos>-kwmeng .
      <fs_data_alv>-netwr = <fs_data_alv>-kwmeng + <fs_pos>-netwr .

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <fs_pos>-matnr
        IMPORTING
          output       = <fs_pos>-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      WRITE <fs_data_alv>-kwmeng TO wl_cantidad.
      WRITE <fs_data_alv>-netwr  TO wl_importe.

    ENDLOOP.

    IF <fs_data_alv>-netwr > 1000.
      <fs_data_alv>-descr = 'Avisar al cliente'.
    ELSE.

    ENDIF.

  ENDLOOP.

  .

ENDFORM.                    " F_CARGA_DATOS
*&---------------------------------------------------------------------*
*&      Form  CARGAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_DATA  text
*----------------------------------------------------------------------*
FORM cargar_alv  USING ti_data TYPE type_t_data.


  SET TITLEBAR 'TITULO_9010'.

* Objeto de la clase CL_SALV_TABLE
  DATA obj_alv_table TYPE REF TO cl_salv_table.

  DATA cx_salv      TYPE REF TO cx_salv_msg.
  DATA cx_exist     TYPE REF TO cx_salv_existing.
  DATA cx_data_err  TYPE REF TO cx_salv_data_error.
  DATA cx_not_found TYPE REF TO cx_salv_not_found.

  DATA: gr_columns TYPE REF TO cl_salv_columns_table,
        gr_column  TYPE REF TO cl_salv_column_table.

* Instanciamos la clase con la tabla que contiene los datos
  cl_salv_table=>factory( IMPORTING r_salv_table = obj_alv_table
                          CHANGING t_table       = ti_data ).

  obj_alv_table->set_screen_status( pfstatus      = 'ZZSTANDARD'  "Nuestro STATUS GUI
                                   report        = sy-repid
                                   set_functions = obj_alv_table->c_functions_all ).

  gr_columns ?= obj_alv_table->get_columns( ).
  gr_columns->set_optimize( abap_true ).   "Optimizar automa. abcho de TODAS las columnas


  gr_column  ?= gr_columns->get_column( 'KUNNR' ).
  gr_column->set_short_text( 'Cliente' ).
  gr_column->set_medium_text( 'Cliente' ).
  gr_column->set_long_text( 'Cliente'  ).

  gr_column  ?= gr_columns->get_column( 'KWMENG' ).
  gr_column->set_short_text( 'CantTot' ).
  gr_column->set_medium_text( 'Cantidad tot.' ).
  gr_column->set_long_text( 'Cantidad total' ).

  gr_column  ?= gr_columns->get_column( 'NETWR' ).
  gr_column->set_short_text( 'TotImp' ).
  gr_column->set_medium_text( 'Total imp' ).
  gr_column->set_long_text( 'Total importe' ).

* Lanzamos el ALV
  obj_alv_table->display( ).

ENDFORM.                    " CARGAR_ALV
*&---------------------------------------------------------------------*
*&      Form  F_READ_TABLE
*&---------------------------------------------------------------------*
*   Leemos un registro de la tabla y llamamos a la transaccion VA03
*----------------------------------------------------------------------*
FORM f_read_table  USING ti_data TYPE type_t_data.

  DATA wl_data TYPE LINE OF type_t_data.

  READ TABLE ti_data INDEX 5 INTO wl_data.

  IF sy-subrc EQ 0.
    SET PARAMETER ID 'TO_ID'        FIELD wl_data-vbeln.
    SET PARAMETER ID 'BYTOPI'       FIELD wl_data-kunnr.
    SET PARAMETER ID 'TRM_EXT_CALL' FIELD abap_true.
    CALL TRANSACTION 'VL03' AND SKIP FIRST SCREEN.

  ENDIF.

ENDFORM.                    " F_READ_TABLE

*&---------------------------------------------------------------------*
*&      Form  F_QUITAR_CEROS
*&---------------------------------------------------------------------*
*   Quita rceros para salidas por pantalla
*----------------------------------------------------------------------*
FORM f_quitar_ceros  CHANGING ti_data TYPE type_t_data.

  DATA wl_data TYPE LINE OF type_t_data.

  LOOP AT ti_data INTO  wl_data.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_data-kunnr
      IMPORTING
        output = wl_data-kunnr.
    .


  ENDLOOP.

ENDFORM.                    " F_READ_TABLE
*&---------------------------------------------------------------------*
*&      Form  F_REDONDEAR_DECIMALES
*&---------------------------------------------------------------------*
*   REdondear a 1 los decimales del importe
*----------------------------------------------------------------------*
FORM f_redondear_decimales  CHANGING ti_data TYPE type_t_data.

  DATA wl_subrc TYPE sy-subrc.
  DATA wl_data TYPE LINE OF type_t_data.

  LOOP AT ti_data INTO  wl_data.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = 1
        input    = wl_data-netwr
      IMPORTING
        output   = wl_data-netwr.

    IF ( sy-subrc EQ 0 ).

    ENDIF.

  ENDLOOP.

  .
ENDFORM.                    " F_READ_TABLE


*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_DATOS  text
*----------------------------------------------------------------------*
FORM eliminar_unicos CHANGING ti_data TYPE type_t_data.

*  DATA: p_datos2 LIKE p_datos OCCURS 0 WITH HEADER LINE,
*        v_cont TYPE i.
*
*  p_datos2[] = p_datos[].
*  v_cont = 0.
*
*  SORT p_datos2 BY augbl.
*
*  LOOP AT p_datos2.
*
*    ADD 1 TO v_cont.
*    AT END OF augbl.
*      IF v_cont < 2.
*        DELETE p_datos WHERE augbl = p_datos2-augbl.
*      ENDIF.
*      v_cont = 0.
*    ENDAT.
*
*  ENDLOOP.

ENDFORM.                    " eliminar_unicos
*&---------------------------------------------------------------------*
*       F_direccion_cliente
*----------------------------------------------------------------------*
*      -->P_T_DATOS  text
*----------------------------------------------------------------------*
FORM f_direccion_cliente  USING pi_kunnr
                                pi_addrn
                          CHANGING ti_data TYPE type_t_data.

  DATA wl_adrnr TYPE kna1-adrnr.
  DATA wl_addr1 TYPE szadr_addr1_complete..

  CALL FUNCTION 'ADDR_GET_COMPLETE'
    EXPORTING
      addrnumber     = wl_adrnr
*     ADDRHANDLE     =
*     ARCHIVE_HANDLE =
*     IV_CURRENT_COMM_DATA          = 'X'
    IMPORTING
      addr1_complete = wl_addr1
* EXCEPTIONS
*     PARAMETER_ERROR               = 1
*     ADDRESS_NOT_EXIST             = 2
*     INTERNAL_ERROR = 3
*     WRONG_ACCESS_TO_ARCHIVE       = 4
*     OTHERS         = 5
    .

  IF sy-subrc <> 0.
    MESSAGE e208(00) WITH 'Direccion NO encontrada'.
  ELSE.
    MESSAGE s208(00) WITH 'Direccion encontrada'.
  ENDIF.

ENDFORM.                    " eliminar_unicos
*&---------------------------------------------------------------------*
*&      Form  OBTENR_VUELOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM obtenr_Mensajes .

  TYPES:
    BEGIN OF ty_t100,
      arbgb TYPE t100-arbgb,
      msgnr TYPE t100-msgnr,
      text  TYPE t100-text,
    END   OF ty_t100.

  DATA: t_ids       TYPE STANDARD TABLE OF t100-msgnr.
  DATA: t_t100_all  TYPE STANDARD TABLE OF t100.
  DATA: t_t100      TYPE STANDARD TABLE OF ty_t100.

  APPEND  '001' TO t_ids.
  APPEND  '002' TO t_ids.

  IF t_ids IS NOT INITIAL.
    SELECT  arbgb
            msgnr
*            text
      INTO TABLE t_t100
      FROM t100
      FOR ALL ENTRIES IN t_ids
      WHERE arbgb LIKE '0%'
      AND   msgnr = t_ids-table_line.
  ENDIF.


ENDFORM.                    " OBTENR_VUELOS
