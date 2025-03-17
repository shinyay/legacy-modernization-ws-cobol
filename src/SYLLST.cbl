*****************************************************************
      * シラバス管理システム - シラバス一覧表示プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLLST.
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
          88 WS-FILE-NOT-FOUND     VALUE "23".
          88 WS-EOF               VALUE "10".

       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       01 WS-LIST-OPTION          PIC 9 VALUE 0.
       01 WS-DEPARTMENT-FILTER    PIC X(4) VALUE SPACES.
       01 WS-TEACHER-FILTER       PIC X(5) VALUE SPACES.
       01 WS-SEMESTER-FILTER      PIC X(2) VALUE SPACES.

       01 WS-PAGE-CONTROL.
          05 WS-RECORDS-PER-PAGE   PIC 99 VALUE 10.
          05 WS-CURRENT-PAGE       PIC 999 VALUE 1.
          05 WS-TOTAL-RECORDS      PIC 999 VALUE 0.
          05 WS-TOTAL-PAGES        PIC 999 VALUE 0.
          05 WS-LINE-COUNT         PIC 99 VALUE 1.
          05 WS-PAGE-OPTION        PIC X VALUE "N".
             88 WS-NEXT-PAGE        VALUE "N" "n".
             88 WS-PREV-PAGE        VALUE "P" "p".
             88 WS-EXIT-LIST        VALUE "X" "x".

       01 WS-KEY-PRESSED          PIC X.

       SCREEN SECTION.
       01 LIST-OPTION-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス一覧表示".
           05 LINE 3 COLUMN 1 VALUE "表示オプションを選択してください:".
           05 LINE 5 COLUMN 1 VALUE "1. 全シラバス".
           05 LINE 6 COLUMN 1 VALUE "2. 学部学科別".
           05 LINE 7 COLUMN 1 VALUE "3. 教員別".
           05 LINE 8 COLUMN 1 VALUE "4. 開講学期別".
           05 LINE 10 COLUMN 1 VALUE "選択 (1-4): ".
           05 LINE 10 COLUMN 15 PIC 9 USING WS-LIST-OPTION.

       01 DEPARTMENT-FILTER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "学部学科別シラバス一覧".
           05 LINE 3 COLUMN 1 VALUE "学部学科コードを入力してください: ".
           05 LINE 3 COLUMN 35 PIC X(4) USING WS-DEPARTMENT-FILTER.

       01 TEACHER-FILTER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "教員別シラバス一覧".
           05 LINE 3 COLUMN 1 VALUE "教員IDを入力してください: ".
           05 LINE 3 COLUMN 30 PIC X(5) USING WS-TEACHER-FILTER.

       01 SEMESTER-FILTER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "開講学期別シラバス一覧".
           05 LINE 3 COLUMN 1 VALUE "開講学期を入力してください (例: 01=春前期): ".
           05 LINE 3 COLUMN 45 PIC X(2) USING WS-SEMESTER-FILTER.

       01 SYLLABUS-LIST-HEADER.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス一覧表示".
           05 LINE 2 COLUMN 1 VALUE "ページ:".
           05 LINE 2 COLUMN 10 PIC ZZ9 FROM WS-CURRENT-PAGE.
           05 LINE 2 COLUMN 15 VALUE "/".
           05 LINE 2 COLUMN 17 PIC ZZ9 FROM WS-TOTAL-PAGES.
           05 LINE 3 COLUMN 1 VALUE "======================================================".
           05 LINE 4 COLUMN 1 VALUE "科目コード 科目名                     学部  教員ID 学期".
           05 LINE 5 COLUMN 1 VALUE "======================================================".

       01 SYLLABUS-LIST-ITEM.
           05 LINE WS-LINE-COUNT COLUMN 1 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE WS-LINE-COUNT COLUMN 10 PIC X(25) FROM SYL-COURSE-NAME.
           05 LINE WS-LINE-COUNT COLUMN 36 PIC X(4) FROM SYL-DEPARTMENT-ID.
           05 LINE WS-LINE-COUNT COLUMN 42 PIC X(5) FROM SYL-TEACHER-ID.
           05 LINE WS-LINE-COUNT COLUMN 48 PIC X(2) FROM SYL-SEMESTER.

       01 SYLLABUS-LIST-FOOTER.
           05 LINE 17 COLUMN 1 VALUE "======================================================".
           05 LINE 19 COLUMN 1 VALUE "N=次ページ, P=前ページ, X=終了: ".
           05 LINE 19 COLUMN 35 PIC X USING WS-PAGE-OPTION.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILE.
           IF WS-FILE-SUCCESS
               PERFORM LIST-OPTIONS-PROCESS
               PERFORM COUNT-RECORDS
               PERFORM CALCULATE-PAGES
               PERFORM DISPLAY-LIST-PROCESS
           ELSE
               DISPLAY "シラバスファイルが見つかりません。"
           END-IF.

           PERFORM CLOSE-FILE.
           GOBACK.

       OPEN-FILE.
           OPEN INPUT SYLLABUS-FILE.
           IF WS-FILE-NOT-FOUND
               DISPLAY "エラー: シラバスファイルが見つかりません。"
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF.

       CLOSE-FILE.
           CLOSE SYLLABUS-FILE.

       LIST-OPTIONS-PROCESS.
           DISPLAY LIST-OPTION-SCREEN.
           ACCEPT LIST-OPTION-SCREEN.

           EVALUATE WS-LIST-OPTION
               WHEN 1
                   CONTINUE
               WHEN 2
                   PERFORM GET-DEPARTMENT-FILTER
               WHEN 3
                   PERFORM GET-TEACHER-FILTER
               WHEN 4
                   PERFORM GET-SEMESTER-FILTER
               WHEN OTHER
                   DISPLAY "無効な選択です。全シラバスを表示します。"
                   MOVE 1 TO WS-LIST-OPTION
           END-EVALUATE.

       GET-DEPARTMENT-FILTER.
           DISPLAY DEPARTMENT-FILTER-SCREEN.
           ACCEPT DEPARTMENT-FILTER-SCREEN.

       GET-TEACHER-FILTER.
           DISPLAY TEACHER-FILTER-SCREEN.
           ACCEPT TEACHER-FILTER-SCREEN.

       GET-SEMESTER-FILTER.
           DISPLAY SEMESTER-FILTER-SCREEN.
           ACCEPT SEMESTER-FILTER-SCREEN.

       COUNT-RECORDS.
           MOVE 0 TO WS-TOTAL-RECORDS.
           MOVE LOW-VALUES TO SYL-COURSE-ID.

           START SYLLABUS-FILE KEY >= SYL-COURSE-ID
               INVALID KEY
                   MOVE "23" TO WS-FILE-STATUS
           END-START.

           IF WS-FILE-SUCCESS
               PERFORM READ-AND-COUNT-RECORDS
           END-IF.

       READ-AND-COUNT-RECORDS.
           PERFORM UNTIL WS-EOF
               READ SYLLABUS-FILE NEXT RECORD
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       IF RECORD-MATCHES-FILTER
                           ADD 1 TO WS-TOTAL-RECORDS
                       END-IF
               END-READ
           END-PERFORM.

           MOVE "00" TO WS-FILE-STATUS.

       RECORD-MATCHES-FILTER.
           EVALUATE WS-LIST-OPTION
               WHEN 1
                   MOVE "Y" TO WS-CONTINUE-FLAG
               WHEN 2
                   IF SYL-DEPARTMENT-ID = WS-DEPARTMENT-FILTER
                       MOVE "Y" TO WS-CONTINUE-FLAG
                   ELSE
                       MOVE "N" TO WS-CONTINUE-FLAG
                   END-IF
               WHEN 3
                   IF SYL-TEACHER-ID = WS-TEACHER-FILTER
                       MOVE "Y" TO WS-CONTINUE-FLAG
                   ELSE
                       MOVE "N" TO WS-CONTINUE-FLAG
                   END-IF
               WHEN 4
                   IF SYL-SEMESTER = WS-SEMESTER-FILTER
                       MOVE "Y" TO WS-CONTINUE-FLAG
                   ELSE
                       MOVE "N" TO WS-CONTINUE-FLAG
                   END-IF
           END-EVALUATE.

           IF WS-CONTINUE
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       CALCULATE-PAGES.
           DIVIDE WS-TOTAL-RECORDS BY WS-RECORDS-PER-PAGE
               GIVING WS-TOTAL-PAGES REMAINDER WS-LINE-COUNT.

           IF WS-LINE-COUNT > 0
               ADD 1 TO WS-TOTAL-PAGES
           END-IF.

           IF WS-TOTAL-PAGES = 0
               MOVE 1 TO WS-TOTAL-PAGES
           END-IF.

       DISPLAY-LIST-PROCESS.
           PERFORM UNTIL WS-EXIT-LIST
               MOVE 1 TO WS-CURRENT-PAGE
               MOVE "N" TO WS-PAGE-OPTION

               PERFORM PAGE-NAVIGATION
           END-PERFORM.

       PAGE-NAVIGATION.
           EVALUATE WS-PAGE-OPTION
               WHEN "N" "n"
                   IF WS-CURRENT-PAGE < WS-TOTAL-PAGES
                       ADD 1 TO WS-CURRENT-PAGE
                   END-IF
               WHEN "P" "p"
                   IF WS-CURRENT-PAGE > 1
                       SUBTRACT 1 FROM WS-CURRENT-PAGE
                   END-IF
               WHEN "X" "x"
                   MOVE "X" TO WS-PAGE-OPTION
                   EXIT PARAGRAPH
           END-EVALUATE.

           PERFORM DISPLAY-CURRENT-PAGE.

           DISPLAY SYLLABUS-LIST-FOOTER.
           ACCEPT SYLLABUS-LIST-FOOTER.

       DISPLAY-CURRENT-PAGE.
           DISPLAY SYLLABUS-LIST-HEADER.

           MOVE LOW-VALUES TO SYL-COURSE-ID.
           START SYLLABUS-FILE KEY >= SYL-COURSE-ID
               INVALID KEY
                   MOVE "23" TO WS-FILE-STATUS
           END-START.

           IF WS-FILE-SUCCESS
               MOVE 6 TO WS-LINE-COUNT
               MOVE 0 TO WS-LINE-COUNT

               PERFORM READ-AND-PROCESS-RECORDS
           END-IF.

       READ-AND-PROCESS-RECORDS.
           MOVE 0 TO WS-LINE-COUNT.
           MOVE "00" TO WS-FILE-STATUS.

           PERFORM UNTIL WS-EOF OR WS-LINE-COUNT >= WS-RECORDS-PER-PAGE
               READ SYLLABUS-FILE NEXT RECORD
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       PERFORM PROCESS-RECORD-FOR-DISPLAY
               END-READ
           END-PERFORM.

           MOVE "00" TO WS-FILE-STATUS.

       PROCESS-RECORD-FOR-DISPLAY.
           IF RECORD-MATCHES-FILTER
               IF WS-LINE-COUNT < ((WS-CURRENT-PAGE * WS-RECORDS-PER-PAGE) -
                                   WS-RECORDS-PER-PAGE)
                   ADD 1 TO WS-LINE-COUNT
               ELSE
                   ADD 1 TO WS-LINE-COUNT
                   COMPUTE WS-LINE-COUNT = WS-LINE-COUNT + 5
                   DISPLAY SYLLABUS-LIST-ITEM
               END-IF
           END-IF.
