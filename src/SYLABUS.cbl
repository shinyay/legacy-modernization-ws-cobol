      *****************************************************************
      * シラバス管理システム - メインプログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLABUS.
       AUTHOR. SHINYAY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USER-CHOICE           PIC 9 VALUE 0.
       01 WS-EXIT-FLAG            PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-CONTROL.
           PERFORM UNTIL WS-EXIT-FLAG = 1
               PERFORM DISPLAY-MAIN-MENU
               PERFORM PROCESS-MENU-CHOICE
           END-PERFORM.

           DISPLAY "シラバス管理システムを終了します。".
           STOP RUN.

       DISPLAY-MAIN-MENU.
           DISPLAY " ".
           DISPLAY "************************************************".
           DISPLAY "*         シラバス管理システム                  *".
           DISPLAY "************************************************".
           DISPLAY "* 1. シラバス登録                              *".
           DISPLAY "* 2. シラバス更新                              *".
           DISPLAY "* 3. シラバス削除                              *".
           DISPLAY "* 4. シラバス照会                              *".
           DISPLAY "* 5. シラバス一覧表示                          *".
           DISPLAY "* 6. 各種レポート生成                          *".
           DISPLAY "* 9. 終了                                      *".
           DISPLAY "************************************************".
           DISPLAY " ".
           DISPLAY "選択してください (1-6, 9):" WITH NO ADVANCING.
           ACCEPT WS-USER-CHOICE.

       PROCESS-MENU-CHOICE.
           EVALUATE WS-USER-CHOICE
               WHEN 1
                   PERFORM CALL-SYLLABUS-REGISTER
               WHEN 2
                   PERFORM CALL-SYLLABUS-UPDATE
               WHEN 3
                   PERFORM CALL-SYLLABUS-DELETE
               WHEN 4
                   PERFORM CALL-SYLLABUS-QUERY
               WHEN 5
                   PERFORM CALL-SYLLABUS-LIST
               WHEN 6
                   PERFORM CALL-REPORT-GENERATE
               WHEN 9
                   MOVE 1 TO WS-EXIT-FLAG
               WHEN OTHER
                   DISPLAY "無効な選択です。再試行してください。"
           END-EVALUATE.

       CALL-SYLLABUS-REGISTER.
           DISPLAY "シラバス登録プログラムを呼び出します...".
           CALL "SYLREG"
           ON EXCEPTION
               DISPLAY "エラー: シラバス登録プログラムの呼び出しに失敗しました。"
           END-CALL.

       CALL-SYLLABUS-UPDATE.
           DISPLAY "シラバス更新プログラムを呼び出します...".
           CALL "SYLUPD"
           ON EXCEPTION
               DISPLAY "エラー: シラバス更新プログラムの呼び出しに失敗しました。"
           END-CALL.

       CALL-SYLLABUS-DELETE.
           DISPLAY "シラバス削除プログラムを呼び出します...".
           CALL "SYLDEL"
           ON EXCEPTION
               DISPLAY "エラー: シラバス削除プログラムの呼び出しに失敗しました。"
           END-CALL.

       CALL-SYLLABUS-QUERY.
           DISPLAY "シラバス照会プログラムを呼び出します...".
           CALL "SYLQRY"
           ON EXCEPTION
               DISPLAY "エラー: シラバス照会プログラムの呼び出しに失敗しました。"
           END-CALL.

       CALL-SYLLABUS-LIST.
           DISPLAY "シラバス一覧表示プログラムを呼び出します...".
           CALL "SYLLST"
           ON EXCEPTION
               DISPLAY "エラー: シラバス一覧表示プログラムの呼び出しに失敗しました。"
           END-CALL.

       CALL-REPORT-GENERATE.
           DISPLAY "レポート生成プログラムを呼び出します...".
           CALL "SYLRPT"
           ON EXCEPTION
               DISPLAY "エラー: レポート生成プログラムの呼び出しに失敗しました。"
           END-CALL.
