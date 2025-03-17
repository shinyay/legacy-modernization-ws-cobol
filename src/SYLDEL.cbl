*****************************************************************
      * シラバス管理システム - シラバス削除プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLDEL.
       AUTHOR. SHINYAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYLLABUS-FILE
               ASSIGN TO "syllabus.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SYL-COURSE-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD SYLLABUS-FILE.
           COPY "copybooks/SYLFILE.cpy".

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX VALUE "00".
          88 WS-FILE-SUCCESS       VALUE "00".
          88 WS-FILE-DUP           VALUE "22".
          88 WS-FILE-NOT-FOUND     VALUE "23".

       01 WS-SEARCH-COURSE-ID     PIC X(6).
       01 WS-CONFIRMATION         PIC X VALUE "N".
          88 WS-CONFIRM-YES       VALUE "Y" "y".
          88 WS-CONFIRM-NO        VALUE "N" "n".

       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       SCREEN SECTION.
       01 DELETE-SEARCH-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス削除画面".
           05 LINE 3 COLUMN 1 VALUE "削除する科目コードを入力してください: ".
           05 LINE 3 COLUMN 40 PIC X(6) USING WS-SEARCH-COURSE-ID.

       01 DELETE-CONFIRM-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス削除確認".
           05 LINE 3 COLUMN 1 VALUE "科目コード: ".
           05 LINE 3 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 4 COLUMN 1 VALUE "科目名: ".
           05 LINE 4 COLUMN 15 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 5 COLUMN 1 VALUE "学部学科コード: ".
           05 LINE 5 COLUMN 20 PIC X(4) FROM SYL-DEPARTMENT-ID.
           05 LINE 6 COLUMN 1 VALUE "教員ID: ".
           05 LINE 6 COLUMN 15 PIC X(5) FROM SYL-TEACHER-ID.
           05 LINE 8 COLUMN 1 VALUE "このシラバスを削除します。よろしいですか？ (Y/N): ".
           05 LINE 8 COLUMN 50 PIC X USING WS-CONFIRMATION.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILE.
           IF WS-FILE-SUCCESS
               PERFORM UNTIL WS-EXIT
                   PERFORM DELETE-SYLLABUS-PROCESS
                   PERFORM CHECK-CONTINUE
               END-PERFORM
           ELSE
               DISPLAY "シラバスファイルが見つかりません。"
           END-IF.

           PERFORM CLOSE-FILE.
           GOBACK.

       OPEN-FILE.
           OPEN I-O SYLLABUS-FILE.
           IF WS-FILE-NOT-FOUND
               DISPLAY "エラー: シラバスファイルが見つかりません。"
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF.

       CLOSE-FILE.
           CLOSE SYLLABUS-FILE.

       DELETE-SYLLABUS-PROCESS.
           PERFORM SEARCH-SYLLABUS.
           IF WS-FILE-SUCCESS
               PERFORM CONFIRM-DELETION
               IF WS-CONFIRM-YES
                   PERFORM DELETE-SYLLABUS-RECORD
               ELSE
                   DISPLAY "削除がキャンセルされました。"
               END-IF
           END-IF.

       SEARCH-SYLLABUS.
           DISPLAY DELETE-SEARCH-SCREEN.
           ACCEPT DELETE-SEARCH-SCREEN.

           MOVE WS-SEARCH-COURSE-ID TO SYL-COURSE-ID.
           READ SYLLABUS-FILE
               KEY IS SYL-COURSE-ID
               INVALID KEY
                   DISPLAY "エラー: 科目コード " SYL-COURSE-ID
                           " は存在しません。"
                   MOVE "23" TO WS-FILE-STATUS
           END-READ.

       CONFIRM-DELETION.
           MOVE "N" TO WS-CONFIRMATION.
           DISPLAY DELETE-CONFIRM-SCREEN.
           ACCEPT DELETE-CONFIRM-SCREEN.

       DELETE-SYLLABUS-RECORD.
           DELETE SYLLABUS-FILE
               INVALID KEY
                   DISPLAY "エラー: レコードの削除に失敗しました。"
           END-DELETE.

           IF WS-FILE-SUCCESS
               DISPLAY "シラバスが正常に削除されました。"
           END-IF.

       CHECK-CONTINUE.
           DISPLAY " ".
           DISPLAY "続けて削除しますか？ (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
