*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

*CLASS lcl_program_load IMPLEMENTATION.
*
*  METHOD select_prog.
*
*    DATA ls_load_status TYPE ty_s_load_status.
*    DATA: BEGIN OF ls_trdir_partial,
*            name TYPE trdir-name,
*          END OF ls_trdir_partial.
*    DATA l_utimstp TYPE tzntimestp.
*    DATA l_stimstp TYPE tzntimestp.
*
*    DATA(lt_rng_prog) = it_rng_prog.
*    WHILE lt_rng_prog IS NOT INITIAL.
*      DATA(lt_small_rng_prog) = VALUE ty_t_rng_prog( ( LINES OF lt_rng_prog FROM 1 TO 100 ) ).
*      DELETE lt_rng_prog FROM 1 TO 100.
*      SELECT trdir~name, repoload~sdat, repoload~stime, repoload~udat, repoload~utime
*        FROM trdir
*          LEFT OUTER JOIN repoload
*            ON trdir~name = repoload~progname
*        WHERE trdir~name IN @lt_small_rng_prog
*          AND trdir~subc NE 'I'
*          AND ( repoload~udat IS NULL OR repoload~udat IN @it_rng_udat )
*          AND ( repoload~sdat IS NULL OR repoload~sdat IN @it_rng_sdat )
*        APPENDING TABLE @kit_load_status.
*    ENDWHILE.
*
*    IF i_nevergen = abap_false.
*      DELETE kit_load_status WHERE sdat IS INITIAL.
*    ENDIF.
*
*    LOOP AT kit_load_status INTO ls_load_status.
*      IF ls_load_status-sdat IS INITIAL.
*        ls_load_status-stat = 'NO_LOAD'.
*      ELSE.
*        CONCATENATE ls_load_status-udat ls_load_status-utime INTO l_utimstp.
*        CONCATENATE ls_load_status-sdat ls_load_status-stime INTO l_stimstp.
*        IF l_utimstp LT l_stimstp.
*          ls_load_status-stat = 'INVALID_LOAD'.
*        ELSE.
*          ls_load_status-stat = 'VALID_LOAD'.
*        ENDIF.
*      ENDIF.
*      MODIFY kit_load_status FROM ls_load_status TRANSPORTING stat.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*
*  METHOD get_progs.
*    et_load_status = kit_load_status.
*  ENDMETHOD.
*
*
*  METHOD get_number_of_progs.
*    DESCRIBE TABLE kit_load_status LINES e_number.
*  ENDMETHOD.
*
*
*  METHOD filter_prog.
*
*    DATA ls_load_status TYPE ty_s_load_status.
*    DATA g_do_generate TYPE flag.
*
*    LOOP AT kit_load_status INTO ls_load_status.
*
*      CLEAR g_do_generate.
*      IF ls_load_status-stat = 'NO_LOAD' AND nevergen = 'X'.
*        g_do_generate = 'X'.
*      ELSEIF ls_load_status-stat = 'INVALID_LOAD' AND toberege = 'X'.
*        g_do_generate = 'X'.
*      ELSEIF ls_load_status-stat = 'VALID_LOAD' AND generatd = 'X'.
*        g_do_generate = 'X'.
*      ENDIF.
*
*      IF g_do_generate IS INITIAL.
*        DELETE kit_load_status.
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDMETHOD.
*
*
*  METHOD gen_progs.
*    DATA ls_load_status TYPE ty_s_load_status.
*    DATA l_commit_count TYPE i.
*
*    LOOP AT kit_load_status INTO ls_load_status.
*      GENERATE REPORT ls_load_status-progname MESSAGE ls_load_status-gen_message.
*      ls_load_status-gen_result = sy-subrc.
*      MODIFY kit_load_status FROM ls_load_status.
*      RAISE EVENT program_generated EXPORTING progname = ls_load_status-progname
*                                              subrc    = sy-subrc
*                                              counter  = sy-tabix
*                                              total    = lines( kit_load_status ).
*      " the following is needed to avoid PXA_NO_FREE_SPACE dump
*      " (see note 302500 Transaction SGEN terminates with PXA_NO_FREE_SPACE)
*      " when there are many GENERATE REPORT executed
*      ADD 1 TO l_commit_count.
*      IF i_commit_frequency > 0 AND l_commit_count = i_commit_frequency.
*        COMMIT WORK.
*        l_commit_count = 0.
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.
*
*
*  METHOD invalidate.
*    DATA ls_load_status TYPE ty_s_load_status.
*
*    ls_load_status-sdat = sy-datum.
*    ls_load_status-stime = sy-uzeit.
*    ls_load_status-stat = 'INVALID_LOAD'.
*    MODIFY kit_load_status FROM ls_load_status TRANSPORTING sdat stime stat
*          WHERE stat <> ls_load_status-stat.
*    ku_date = ls_load_status-sdat.
*    ku_time = ls_load_status-stime.
*
*    IF i_test IS INITIAL.
*      LOOP AT kit_load_status INTO ls_load_status.
*        TRY.
*            ku_program = ls_load_status-progname.
*            EXEC SQL.
*              UPDATE repoload
*                    SET sdat = :ku_date
*                        stime = :ku_time
*                    WHERE prog = :ku_program
*            ENDEXEC.
*            DATA exc_ref TYPE REF TO cx_sy_native_sql_error.
*          CATCH cx_sy_native_sql_error INTO exc_ref.
*            DATA error_text TYPE string.
*            error_text = exc_ref->get_text( ).
*        ENDTRY.
*      ENDLOOP.
*      COMMIT WORK.
*    ENDIF.
*
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS lcl_uuid IMPLEMENTATION.
*
*  METHOD class_constructor.
*
*    uuid_generator = cl_uuid_factory=>create_system_uuid( ).
*
*  ENDMETHOD.
*
*
*  METHOD get_c26.
*
*    result = uuid_generator->create_uuid_c26( ).
*
*  ENDMETHOD.
*
*
*  METHOD get_x16.
*
*    result = uuid_generator->create_uuid_x16( ).
*
*  ENDMETHOD.
*
*ENDCLASS.
