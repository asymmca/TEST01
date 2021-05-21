CLASS yxx_statistik_2001_team DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yxx_statistik_2001_interface .

*    TYPES:
*      ty_t_ausgabe   TYPE STANDARD TABLE OF zpa_ausgabe,
*      ty_erw_ausgabe TYPE STANDARD TABLE OF zpa_ausgabe_pers.

    class-data it_objec TYPE objec_t .
    DATA gt_data TYPE zpa_tt_ausgabe .
    DATA gt_data_pers TYPE zpa_tt_ausgabe_pers.
*    DATA it_0001 TYPE pa0001 .
    DATA gt_2001 TYPE p2001_tab .
    DATA gt_0001 TYPE p0001_tab.
    DATA gt_0002 TYPE p0002_tab.
    METHODS constructor
      IMPORTING
        !iv_flag1 TYPE boole
        !iv_flag2 TYPE boole
        !iv_check TYPE boole
        !iv_orgeh TYPE orgeh
        !iv_endda TYPE endda
        !iv_begda TYPE begda .
    METHODS process_data .
    METHODS get_data
      EXPORTING
        !rt_data TYPE yttdaten .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gv_begda TYPE begda.
    DATA gv_endda TYPE endda.
    DATA it_0002 TYPE pa0002 .
    DATA it_2001 TYPE pa2001 .
    DATA gt_objec TYPE objec_t.
    DATA go_db TYPE REF TO yxx_statistik_2001_db .
    METHODS get_pers_orge
      IMPORTING
        it_objec TYPE objec_t.
    METHODS get_org.
    METHODS set_pers_tab
      IMPORTING
        is_objec TYPE objec.
    METHODS set_orgeh_tab
      IMPORTING
        is_objec TYPE objec.
    METHODS process_data_pernr.
ENDCLASS.



CLASS YXX_STATISTIK_2001_TEAM IMPLEMENTATION.


  METHOD constructor.
    gv_begda = iv_begda.
    gv_endda = iv_endda.
    IF go_db IS NOT BOUND.
      go_db = NEW yxx_statistik_2001_db( iv_flag1 = iv_flag1
                                         iv_flag2 = iv_flag2
                                         iv_check = iv_check
                                         iv_orgeh = iv_orgeh
                                         iv_endda = iv_endda
                                         iv_begda = iv_begda ).
    ENDIF.
* 0001 0002 nur Einzeldatensätze 2001 gt_ IDEE Daten_verarbeiten Loop über 1 und 2 und Mapping anhand PERNR wer wer ist

    get_org(  ).

    "" Daten für alle It nach außen holen

  ENDMETHOD.


  METHOD get_data.

  ENDMETHOD.


  METHOD get_org.
    DATA(lt_objec) = go_db->get_objec( ).
    DELETE lt_objec WHERE otype = 'S'.
    LOOP AT lt_objec INTO DATA(ls_objec).

      me->set_orgeh_tab( is_objec = ls_objec ).
      me->set_pers_tab( is_objec = ls_objec ).
    ENDLOOP.
    me->get_pers_orge( it_objec = gt_objec ).
  ENDMETHOD.


  METHOD get_pers_orge.
    LOOP AT gt_objec INTO DATA(ls_objec).
      DATA lt_pernr TYPE STANDARD TABLE OF hrpernr.
      CALL FUNCTION 'HRCM_ORGUNIT_EMPLOYEE_LIST_GET'
        EXPORTING
          plvar              = ls_objec-plvar
          otype              = ls_objec-otype
          objid              = ls_objec-objid
          begda              = gv_begda
          endda              = gv_endda
*         path_id            = space
        TABLES
          pernr_table        = lt_pernr
        EXCEPTIONS
          path_error         = 1
          root_error         = 2
          no_employees_found = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*Aufruf Datenverarbeitungsmethoden / Berechnung
* me->get_person_data( it_pernr = lt_pernr ).
* METHOD get_person_data.
*   Loop at it_pernr into ls_pernr.
*       LOOP AT 2001 into lt_2001 where gt_2001 .
*           LOOP at 1 into 1 where pernr.
*               loop at 2 into 2 where pernr.
*           endloop
*         endloop
*     endloop
*
* endloop.
* ENDMETHOD.

    ENDLOOP.
  ENDMETHOD.


  METHOD process_data.
* LOOP ÜBER IT
*@TODO:
* WRAPPER METHODE LOOP AT ÜBER PERSONALDATEN -> AUSGABE TABELLEN BEFÜLLEN
* ÜBERLEGEN WIE MAN DIE METHODE PROGRAMMIERT 2 oder mehr?
*
* Return gt_
*    go_db->gt_0001.
  ENDMETHOD.


  METHOD process_data_pernr.

  ENDMETHOD.


  METHOD set_orgeh_tab.
*    IF is_objec-otype = 'O' AND is_objec-objid

*    READ TABLE gt_objec WITH KEY objid = is_objec-objid TRANSPORTING NO FIELDS.
*    IF sy-subrc <> 0.
*      APPEND is_objec TO gt_objec.
*    ENDIF.
    TRY.
        DATA(ls_data) = gt_objec[ objid = is_objec-objid ].

      CATCH cx_root.
        IF is_objec-otype = 'O'.
          APPEND is_objec TO gt_objec.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD set_pers_tab.

    DATA: lt_2001 TYPE p2001_tab,
          ls_0002 TYPE p0002,
          ls_0001 TYPE p0001.
    IF is_objec-otype = 'P'.
* and
      go_db->get_pernr( EXPORTING is_objec = is_objec
                        IMPORTING rt_2001 = lt_2001
                                  rs_0002 = ls_0002
                                  rs_0001 = ls_0001 ).
      APPEND LINES OF lt_2001 TO gt_2001.
      APPEND ls_0001 TO gt_0001.
      APPEND ls_0002 TO gt_0002.


*      gt_data

    endif.
* AdHoc Tabellen erstellen und darüber Loopen anstatt monster gt_XXXX
* 1 + 2 + 2001 je person
    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_average_absent_org.
* Möglichkeit? /FW
* LOOP AT gt_2001 into DATA(ls_2001).
*
* BERECHNUNG
* Möglichkeit 2?
*DATA(bla) = go_db->get_2001( ).
    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_average_absent_person.

    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_count_analyzed_persons.

    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_count_persons_absent.

    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_count_persons_absent_restr.

    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_count_persons_wo_absent.

    ENDMETHOD.


    METHOD yxx_statistik_2001_interface~get_person.

    ENDMETHOD.
ENDCLASS.
