CLASS zcl_cur_exch_rate_prov DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_cur_exch_rate TYPE TABLE OF zce_cur_exch_rate.
    INTERFACES: if_rap_query_provider, if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_cur_exch_rate
        IMPORTING
          iv_top           TYPE i
          iv_skip          TYPE i
        EXPORTING
          et_cur_exch_rate TYPE tt_cur_exch_rate.
ENDCLASS.

CLASS zcl_cur_exch_rate_prov IMPLEMENTATION.
  METHOD get_cur_exch_rate.
    CONSTANTS: lc_destination_name TYPE string VALUE 'exchangerateapi', " Name from BTP Cockpit
               lc_endpoint         TYPE string VALUE 'https://v6.exchangerate-api.com/v6/1e5fe44669f088b9c1bace61/latest/USD'.
    TYPES: BEGIN OF ys_conversion_rates,
             curr TYPE waers,
             rate TYPE ukurs_curr,
           END OF ys_conversion_rates,
           BEGIN OF ys_json,
             result           TYPE string,
             conversion_rates TYPE REF TO data,
           END OF ys_json.
    DATA: lo_destination TYPE REF TO if_http_destination,
          lo_http_client TYPE REF TO if_web_http_client,
          lv_url         TYPE string,
          lv_response    TYPE string,
          lv_status      TYPE i.
    DATA: ls_json        TYPE ys_json.

    TRY.
        " 1. Retrieve the HTTP destination object
*        lo_destination = cl_http_destination_provider=>create_by_cloud_destination(
*            i_name = lc_destination_name
*            i_authn_mode = if_a4c_cp_service=>service_specific
*        ).
        lo_destination = cl_http_destination_provider=>create_by_url( lc_endpoint ).

        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(lv_content_type) = lo_response->get_content_type( ).
        DATA(ls_status) = lo_response->get_status( ).
        DATA(lv_text) = lo_response->get_text( ).
      CATCH cx_web_http_client_error INTO DATA(lx_client_error).
        " Handle HTTP client communication errors
        " ...
      CATCH cx_http_dest_provider_error INTO DATA(lx_provider_error).
        "handle exception
    ENDTRY.

    "Convert the data from JSON to ABAP using the XCO Library; output the data
    DATA: lo_json        TYPE REF TO /ui2/cl_json.

    "Create JSON parser
    CREATE OBJECT lo_json.
    "Parse the JSON
    lo_json->deserialize(
       EXPORTING
         json = lv_text
       CHANGING
         data = ls_json ).

    " Convert to internal table
    FIELD-SYMBOLS: <ls_rates> TYPE any.
    ASSIGN ls_json-conversion_rates->* TO <ls_rates>.

    IF <ls_rates> IS ASSIGNED.
      DATA(lo_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <ls_rates> ) ).
      DATA(lv_start) = iv_SKIP + 1.
      DATA(lv_end) = iv_SKIP + iv_TOP.
      LOOP AT lo_descr->components ASSIGNING FIELD-SYMBOL(<ls_component>)
        FROM lv_start TO lv_end.
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_rates>
          TO FIELD-SYMBOL(<ls_value>).
        IF  sy-subrc      =  0
        AND <ls_value>->* IS NOT INITIAL.
          APPEND VALUE #(
            fcurr = 'USD'
            tcurr = <ls_component>-name
            ukurs = CONV #( <ls_value>->* )
          ) TO et_cur_exch_rate.
        ENDIF.
      ENDLOOP.
    ENDIF.

*    "Variables for http_client and client_proxy
*    DATA:
*      lo_http_client  TYPE REF TO if_web_http_client,
*      lo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy,
*      lo_request      TYPE REF TO /iwbep/if_cp_request_read_list,
*      lo_response     TYPE REF TO /iwbep/if_cp_response_read_lst.
*
*    " 1. Get the destination of remote system; Create http client
*    DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
*                             comm_scenario  = 'Z_OUTBOUND_ODATA_CSCEN_###'
**                             comm_system_id = '<Comm System Id>'
**                             service_id     = ''
*                           ).
*    lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
*
*    "2. create client proxy
*    lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
*      EXPORTING
*         is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
*                                             proxy_model_id      = 'Z###_MODEL_TRAVELS'
*                                             proxy_model_version = '###1' )
*        io_http_client             = lo_http_client
*        iv_relative_service_root   = '/sap/opu/odata/sap/Z_C_TRAVEL_API_O2_###'  " = the service endpoint in the service binding in PRV' ).
*       ).
*    ASSERT lo_http_client IS BOUND .
*    " 3. Navigate to the resource and create a request for the read operation
*    lo_request = lo_client_proxy->create_resource_for_entity_set( 'SIMPLE_TRAVEL' )->create_request_for_read( ).
*    lo_request->set_top( 50 )->set_skip( 0 ).
*    " 4. Execute the request and retrieve the business data
*    lo_response = lo_request->execute( ).
*    lo_response->get_business_data( IMPORTING et_business_data = et_cur_exch_rate ).
*    " Handle remote Exception
  ENDMETHOD.

  METHOD if_rap_query_provider~select.
    DATA lt_cur_exch_rate TYPE tt_cur_exch_rate.
    DATA(lv_top)     = io_request->get_paging( )->get_page_size( ).
    DATA(lv_skip)    = io_request->get_paging( )->get_offset( ).
*    DATA(requested_fields)  = io_request->get_requested_elements( ).
*    DATA(sort_order)    = io_request->get_sort_elements( ).

    TRY.
        get_cur_exch_rate(
          EXPORTING
            iv_top            = CONV i( lv_top )
            iv_skip           = CONV i( lv_skip )
          IMPORTING
            et_cur_exch_rate  = lt_cur_exch_rate ) .
        io_response->set_total_number_of_records( lines( lt_cur_exch_rate ) ).
        io_response->set_data( lt_cur_exch_rate ).

      CATCH cx_root INTO DATA(exception).
        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).
    ENDTRY.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    DATA business_data TYPE tt_cur_exch_rate.

*    TRY.
    get_cur_exch_rate(
      EXPORTING
        iv_top           = 1
        iv_skip          = 0
      IMPORTING
        et_cur_exch_rate = business_data ).
    out->write( business_data ).
*      CATCH cx_root INTO DATA(exception).
*        out->write( cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_text( ) ).
*    ENDTRY.
  ENDMETHOD.

ENDCLASS.
