*****************************************************************
      * シラバス管理システム - シラバス照会プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLQRY.
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

       01 WS-SEARCH-COURSE-ID     PIC X(6).
       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       01 WS-KEY-PRESSED          PIC X.

       SCREEN SECTION.
       01 QUERY-SEARCH-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス照会画面".
           05 LINE 3 COLUMN 1 VALUE "照会する科目コードを入力してください: ".
           05 LINE 3 COLUMN 40 PIC X(6) USING WS-SEARCH-COURSE-ID.

       01 SYLLABUS-DETAIL-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス詳細".
           05 LINE 3 COLUMN 1 VALUE "科目コード: ".
           05 LINE 3 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 4 COLUMN 1 VALUE "科目名: ".
           05 LINE 4 COLUMN 15 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 5 COLUMN 1 VALUE "学部学科コード: ".
           05 LINE 5 COLUMN 20 PIC X(4) FROM SYL-DEPARTMENT-ID.
           05 LINE 6 COLUMN 1 VALUE "教員ID: ".
           05 LINE 6 COLUMN 15 PIC X(5) FROM SYL-TEACHER-ID.
           05 LINE 7 COLUMN 1 VALUE "開講学期: ".
           05 LINE 7 COLUMN 15 PIC X(2) FROM SYL-SEMESTER.
           05 LINE 8 COLUMN 1 VALUE "単位数: ".
           05 LINE 8 COLUMN 15 PIC 9 FROM SYL-CREDITS.
           05 LINE 10 COLUMN 1 VALUE "授業概要: ".
           05 LINE 11 COLUMN 5 PIC X(60) FROM SYL-DESCRIPTION(1:60).
           05 LINE 12 COLUMN 5 PIC X(60) FROM SYL-DESCRIPTION(61:60).
           05 LINE 13 COLUMN 5 PIC X(60) FROM SYL-DESCRIPTION(121:60).
           05 LINE 14 COLUMN 5 PIC X(20) FROM SYL-DESCRIPTION(181:20).
           05 LINE 16 COLUMN 1 VALUE "学習目標: ".
           05 LINE 17 COLUMN 5 PIC X(60) FROM SYL-OBJECTIVES(1:60).
           05 LINE 18 COLUMN 5 PIC X(40) FROM SYL-OBJECTIVES(61:40).
           05 LINE 20 COLUMN 1 VALUE "続けるには任意のキーを押してください...".
           05 LINE 20 COLUMN 40 PIC X TO WS-KEY-PRESSED.

       01 WEEK-PLAN-DETAIL-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "授業計画".
           05 LINE 2 COLUMN 1 VALUE "科目コード: ".
           05 LINE 2 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 2 COLUMN 25 VALUE "科目名: ".
           05 LINE 2 COLUMN 35 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 4 COLUMN 1 VALUE "授業計画:".
           05 LINE 6 COLUMN 1 VALUE "第1週: ".
           05 LINE 6 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(1).
           05 LINE 7 COLUMN 1 VALUE "第2週: ".
           05 LINE 7 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(2).
           05 LINE 8 COLUMN 1 VALUE "第3週: ".
           05 LINE 8 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(3).
           05 LINE 9 COLUMN 1 VALUE "第4週: ".
           05 LINE 9 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(4).
           05 LINE 10 COLUMN 1 VALUE "第5週: ".
           05 LINE 10 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(5).
           05 LINE 11 COLUMN 1 VALUE "第6週: ".
           05 LINE 11 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(6).
           05 LINE 12 COLUMN 1 VALUE "第7週: ".
           05 LINE 12 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(7).
           05 LINE 13 COLUMN 1 VALUE "第8週: ".
           05 LINE 13 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(8).
           05 LINE 14 COLUMN 1 VALUE "第9週: ".
           05 LINE 14 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(9).
           05 LINE 15 COLUMN 1 VALUE "第10週: ".
           05 LINE 15 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(10).
           05 LINE 16 COLUMN 1 VALUE "第11週: ".
           05 LINE 16 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(11).
           05 LINE 17 COLUMN 1 VALUE "第12週: ".
           05 LINE 17 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(12).
           05 LINE 18 COLUMN 1 VALUE "第13週: ".
           05 LINE 18 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(13).
           05 LINE 19 COLUMN 1 VALUE "第14週: ".
           05 LINE 19 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(14).
           05 LINE 20 COLUMN 1 VALUE "第15週: ".
           05 LINE 20 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(15).
           05 LINE 22 COLUMN 1 VALUE "続けるには任意のキーを押してください...".
           05 LINE 22 COLUMN 40 PIC X TO WS-KEY-PRESSED.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILE.
           IF WS-FILE-SUCCESS
               PERFORM UNTIL WS-EXIT
                   PERFORM QUERY-SYLLABUS-PROCESS
                   PERFORM CHECK-CONTINUE
               END-PERFORM
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

       QUERY-SYLLABUS-PROCESS.
           PERFORM SEARCH-SYLLABUS.
           IF WS-FILE-SUCCESS
               PERFORM DISPLAY-SYLLABUS-DETAIL
               PERFORM DISPLAY-WEEK-PLAN
           END-IF.

       SEARCH-SYLLABUS.
           DISPLAY QUERY-SEARCH-SCREEN.
           ACCEPT QUERY-SEARCH-SCREEN.

           MOVE WS-SEARCH-COURSE-ID TO SYL-COURSE-ID.
           READ SYLLABUS-FILE
               KEY IS SYL-COURSE-ID
               INVALID KEY
                   DISPLAY "エラー: 科目コード " SYL-COURSE-ID
                           " は存在しません。"
                   MOVE "23" TO WS-FILE-STATUS
           END-READ.

       DISPLAY-SYLLABUS-DETAIL.
           DISPLAY SYLLABUS-DETAIL-SCREEN.
           ACCEPT SYLLABUS-DETAIL-SCREEN.

       DISPLAY-WEEK-PLAN.
           DISPLAY WEEK-PLAN-DETAIL-SCREEN.
           ACCEPT WEEK-PLAN-DETAIL-SCREEN.

       CHECK-CONTINUE.
           DISPLAY " ".
           DISPLAY "続けて照会しますか？ (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
