*****************************************************************
      * シラバス管理システム - シラバス登録プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLREG.
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

       01 WS-EOF-FLAG             PIC X VALUE "N".
          88 WS-EOF               VALUE "Y".

       01 WS-FUNCTION-CODE        PIC X.
       01 WS-PARAM-1              PIC X(50).
       01 WS-PARAM-2              PIC X(50).
       01 WS-RESULT               PIC X(200).
       01 WS-RETURN-CODE          PIC 9.

       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       SCREEN SECTION.
       01 SYLLABUS-INPUT-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス登録画面".
           05 LINE 3 COLUMN 1 VALUE "科目コード (例: CS1001): ".
           05 LINE 3 COLUMN 28 PIC X(6) USING SYL-COURSE-ID.
           05 LINE 4 COLUMN 1 VALUE "科目名: ".
           05 LINE 4 COLUMN 28 PIC X(30) USING SYL-COURSE-NAME.
           05 LINE 5 COLUMN 1 VALUE "学部学科コード: ".
           05 LINE 5 COLUMN 28 PIC X(4) USING SYL-DEPARTMENT-ID.
           05 LINE 6 COLUMN 1 VALUE "教員ID: ".
           05 LINE 6 COLUMN 28 PIC X(5) USING SYL-TEACHER-ID.
           05 LINE 7 COLUMN 1 VALUE "開講学期 (例: 01=春前期): ".
           05 LINE 7 COLUMN 28 PIC X(2) USING SYL-SEMESTER.
           05 LINE 8 COLUMN 1 VALUE "単位数: ".
           05 LINE 8 COLUMN 28 PIC 9 USING SYL-CREDITS.
           05 LINE 10 COLUMN 1 VALUE "授業概要: ".
           05 LINE 10 COLUMN 28 PIC X(50) USING SYL-DESCRIPTION.
           05 LINE 11 COLUMN 28 PIC X(50) USING SYL-DESCRIPTION(51:50).
           05 LINE 12 COLUMN 28 PIC X(50) USING SYL-DESCRIPTION(101:50).
           05 LINE 13 COLUMN 28 PIC X(50) USING SYL-DESCRIPTION(151:50).
           05 LINE 15 COLUMN 1 VALUE "学習目標: ".
           05 LINE 15 COLUMN 28 PIC X(50) USING SYL-OBJECTIVES.
           05 LINE 16 COLUMN 28 PIC X(50) USING SYL-OBJECTIVES(51:50).

       01 WEEK-PLAN-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "授業計画登録画面".
           05 LINE 2 COLUMN 1 VALUE "科目コード: ".
           05 LINE 2 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 2 COLUMN 25 VALUE "科目名: ".
           05 LINE 2 COLUMN 35 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 4 COLUMN 1 VALUE "各週の授業計画を入力してください:".
           05 LINE 6 COLUMN 1 VALUE "第1週: ".
           05 LINE 6 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(1).
           05 LINE 7 COLUMN 1 VALUE "第2週: ".
           05 LINE 7 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(2).
           05 LINE 8 COLUMN 1 VALUE "第3週: ".
           05 LINE 8 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(3).
           05 LINE 9 COLUMN 1 VALUE "第4週: ".
           05 LINE 9 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(4).
           05 LINE 10 COLUMN 1 VALUE "第5週: ".
           05 LINE 10 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(5).
           05 LINE 11 COLUMN 1 VALUE "第6週: ".
           05 LINE 11 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(6).
           05 LINE 12 COLUMN 1 VALUE "第7週: ".
           05 LINE 12 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(7).
           05 LINE 13 COLUMN 1 VALUE "第8週: ".
           05 LINE 13 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(8).
           05 LINE 14 COLUMN 1 VALUE "第9週: ".
           05 LINE 14 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(9).
           05 LINE 15 COLUMN 1 VALUE "第10週: ".
           05 LINE 15 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(10).
           05 LINE 16 COLUMN 1 VALUE "第11週: ".
           05 LINE 16 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(11).
           05 LINE 17 COLUMN 1 VALUE "第12週: ".
           05 LINE 17 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(12).
           05 LINE 18 COLUMN 1 VALUE "第13週: ".
           05 LINE 18 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(13).
           05 LINE 19 COLUMN 1 VALUE "第14週: ".
           05 LINE 19 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(14).
           05 LINE 20 COLUMN 1 VALUE "第15週: ".
           05 LINE 20 COLUMN 10 PIC X(30) USING SYL-WEEK-PLAN(15).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILE.
           PERFORM UNTIL WS-EXIT
               PERFORM INITIALIZE-SYLLABUS-RECORD
               PERFORM INPUT-SYLLABUS-DATA
               PERFORM INPUT-WEEK-PLAN-DATA
               PERFORM WRITE-SYLLABUS-RECORD
               PERFORM CHECK-CONTINUE
           END-PERFORM.

           PERFORM CLOSE-FILE.
           GOBACK.

       OPEN-FILE.
           OPEN I-O SYLLABUS-FILE.
           IF WS-FILE-NOT-FOUND
               OPEN OUTPUT SYLLABUS-FILE
               CLOSE SYLLABUS-FILE
               OPEN I-O SYLLABUS-FILE
           END-IF.

       CLOSE-FILE.
           CLOSE SYLLABUS-FILE.

       INITIALIZE-SYLLABUS-RECORD.
           INITIALIZE SYLLABUS-RECORD.

       INPUT-SYLLABUS-DATA.
           DISPLAY SYLLABUS-INPUT-SCREEN.
           ACCEPT SYLLABUS-INPUT-SCREEN.

           MOVE "C" TO WS-FUNCTION-CODE.
           MOVE SYL-COURSE-ID TO WS-PARAM-1.
           MOVE SPACES TO WS-PARAM-2.

           CALL "SYLCOM" USING WS-FUNCTION-CODE, WS-PARAM-1,
                              WS-PARAM-2, WS-RESULT, WS-RETURN-CODE.

           IF WS-RETURN-CODE = 1
               DISPLAY WS-RESULT
               PERFORM INPUT-SYLLABUS-DATA
           END-IF.

       INPUT-WEEK-PLAN-DATA.
           DISPLAY WEEK-PLAN-SCREEN.
           ACCEPT WEEK-PLAN-SCREEN.

       WRITE-SYLLABUS-RECORD.
           WRITE SYLLABUS-RECORD
               INVALID KEY
                   DISPLAY "エラー: 科目コード " SYL-COURSE-ID
                           " はすでに存在します。"
           END-WRITE.

       CHECK-CONTINUE.
           DISPLAY " ".
           DISPLAY "続けて登録しますか？ (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
