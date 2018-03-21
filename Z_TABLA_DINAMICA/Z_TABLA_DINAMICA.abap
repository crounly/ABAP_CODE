*&---------------------------------------------------------------------*
*& Report  Z_TABLAS_DINAMICAS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_tablas_dinamicas.


*&---------------------------------------------------------------------*
*& Internal Tables y variables globales
*&---------------------------------------------------------------------*
DATA wa_dref   TYPE REF TO data.
DATA tl_dtabla TYPE REF TO data.

FIELD-SYMBOLS: <tl_dtabla> TYPE STANDARD TABLE,
               <wa_dtable> TYPE any,
               <FS_WA_DINAMICO> TYPE any,
               <FS_CAMPO> TYPE any.

*&---------------------------------------------------------------------*
*& variables ALV
*&--------------------------------------------------------------------*
DATA gr_table TYPE REF TO cl_salv_table.
DATA cx_salv  TYPE REF TO cx_salv_msg.

DATA gr_msg TYPE string.
*&---------------------------------------------------------------------*
*& Parámetros de Selección
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block01 WITH FRAME TITLE text-s01.
PARAMETERS: p_view   TYPE dd02v-tabname OBLIGATORY.
PARAMETERS: p_mand   TYPE MANDT.
SELECTION-SCREEN END OF BLOCK block01.

*&---------------------------------------------------------------------*
*& Start-of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT COUNT( * ) UP TO 1 ROWS FROM dd02l "Comprobar que la tabla existe en la base de datos
    WHERE tabname  EQ p_view.

  IF ( sy-subrc EQ 0 ).

    CREATE DATA wa_dref TYPE (p_view).
    ASSIGN wa_dref->* TO <wa_dtable>.
    IF ( sy-subrc EQ 0 ).

      CREATE DATA tl_dtabla TYPE STANDARD TABLE OF (p_view).
      ASSIGN tl_dtabla->* TO <tl_dtabla>.
      IF ( sy-subrc EQ 0 ).

        SELECT * INTO CORRESPONDING FIELDS OF TABLE <tl_dtabla>
        FROM (p_view).

        IF ( sy-subrc EQ 0 ).

*         Y asi recorremos una tabla dinamica.
*         Un puntero aputna al registro de la tabla <FS_WA_DINAMICO>
*         Otro apunta al campo de la tabla <FS_CAMPO>
*         Como son tipo ANY , al asignarse toman la forma de la estructura, tabla o campo que apuntan
          LOOP AT <tl_dtabla> ASSIGNING <FS_WA_DINAMICO>.

            ASSIGN COMPONENT 'MANDT' OF STRUCTURE <FS_WA_DINAMICO> TO <FS_CAMPO>.
            IF ( sy-subrc EQ 0 ).
              <FS_CAMPO> = p_mand.
            ENDIF.


          ENDLOOP.

          TRY.
              cl_salv_table=>factory(
                IMPORTING
                  r_salv_table = gr_table
                CHANGING
                  t_table      = <tl_dtabla> ).

              gr_table->display( ).
            CATCH cx_salv_msg INTO cx_salv.
* gestionamos las excepciones que puedan suceder
              gr_msg = cx_salv->get_text( ).
              MESSAGE gr_msg TYPE 'E'.
          ENDTRY.




        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.
