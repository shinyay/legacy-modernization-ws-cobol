*****************************************************************
      * シラバス管理システム - シラバス更新プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLUPD.
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

       01 WS-SEARCH-COURSE-ID     PIC X(6).
       01 WS-UPDATE-OPTION        PIC 9 VALUE 0.

       SCREEN SECTION.
       01 SEARCH-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス更新画面".
           05 LINE 3 COLUMN 1 VALUE "更新する科目コードを入力してください: ".
           05 LINE 3 COLUMN 40 PIC X(6) USING WS-SEARCH-COURSE-ID.

       01 UPDATE-MENU-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "シラバス更新メニュー".
           05 LINE 2 COLUMN 1 VALUE "科目コード: ".
           05 LINE 2 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 2 COLUMN 25 VALUE "科目名: ".
           05 LINE 2 COLUMN 35 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 4 COLUMN 1 VALUE "更新する項目を選択してください:".
           05 LINE 6 COLUMN 1 VALUE "1. 科目名".
           05 LINE 7 COLUMN 1 VALUE "2. 学部学科コード".
           05 LINE 8 COLUMN 1 VALUE "3. 教員ID".
           05 LINE 9 COLUMN 1 VALUE "4. 開講学期".
           05 LINE 10 COLUMN 1 VALUE "5. 単位数".
           05 LINE 11 COLUMN 1 VALUE "6. 授業概要".
           05 LINE 12 COLUMN 1 VALUE "7. 学習目標".
           05 LINE 13 COLUMN 1 VALUE "8. 授業計画".
           05 LINE 14 COLUMN 1 VALUE "9. 保存して終了".
           05 LINE 16 COLUMN 1 VALUE "選択 (1-9): ".
           05 LINE 16 COLUMN 15 PIC 9 USING WS-UPDATE-OPTION.

       01 UPDATE-COURSE-NAME-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "科目名更新".
           05 LINE 3 COLUMN 1 VALUE "現在の科目名: ".
           05 LINE 3 COLUMN 20 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 5 COLUMN 1 VALUE "新しい科目名: ".
           05 LINE 5 COLUMN 20 PIC X(30) USING SYL-COURSE-NAME.

       01 UPDATE-DEPARTMENT-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "学部学科コード更新".
           05 LINE 3 COLUMN 1 VALUE "現在の学部学科コード: ".
           05 LINE 3 COLUMN 25 PIC X(4) FROM SYL-DEPARTMENT-ID.
           05 LINE 5 COLUMN 1 VALUE "新しい学部学科コード: ".
           05 LINE 5 COLUMN 25 PIC X(4) USING SYL-DEPARTMENT-ID.

       01 UPDATE-TEACHER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "教員ID更新".
           05 LINE 3 COLUMN 1 VALUE "現在の教員ID: ".
           05 LINE 3 COLUMN 20 PIC X(5) FROM SYL-TEACHER-ID.
           05 LINE 5 COLUMN 1 VALUE "新しい教員ID: ".
           05 LINE 5 COLUMN 20 PIC X(5) USING SYL-TEACHER-ID.

       01 UPDATE-SEMESTER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "開講学期更新".
           05 LINE 3 COLUMN 1 VALUE "現在の開講学期: ".
           05 LINE 3 COLUMN 20 PIC X(2) FROM SYL-SEMESTER.
           05 LINE 5 COLUMN 1 VALUE "新しい開講学期 (例: 01=春前期): ".
           05 LINE 5 COLUMN 35 PIC X(2) USING SYL-SEMESTER.

       01 UPDATE-CREDITS-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "単位数更新".
           05 LINE 3 COLUMN 1 VALUE "現在の単位数: ".
           05 LINE 3 COLUMN 20 PIC 9 FROM SYL-CREDITS.
           05 LINE 5 COLUMN 1 VALUE "新しい単位数: ".
           05 LINE 5 COLUMN 20 PIC 9 USING SYL-CREDITS.

       01 UPDATE-DESCRIPTION-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "授業概要更新".
           05 LINE 3 COLUMN 1 VALUE "現在の授業概要: ".
           05 LINE 4 COLUMN 5 PIC X(50) FROM SYL-DESCRIPTION(1:50).
           05 LINE 5 COLUMN 5 PIC X(50) FROM SYL-DESCRIPTION(51:50).
           05 LINE 6 COLUMN 5 PIC X(50) FROM SYL-DESCRIPTION(101:50).
           05 LINE 7 COLUMN 5 PIC X(50) FROM SYL-DESCRIPTION(151:50).
           05 LINE 9 COLUMN 1 VALUE "新しい授業概要: ".
           05 LINE 10 COLUMN 5 PIC X(50) USING SYL-DESCRIPTION(1:50).
           05 LINE 11 COLUMN 5 PIC X(50) USING SYL-DESCRIPTION(51:50).
           05 LINE 12 COLUMN 5 PIC X(50) USING SYL-DESCRIPTION(101:50).
           05 LINE 13 COLUMN 5 PIC X(50) USING SYL-DESCRIPTION(151:50).

       01 UPDATE-OBJECTIVES-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "学習目標更新".
           05 LINE 3 COLUMN 1 VALUE "現在の学習目標: ".
           05 LINE 4 COLUMN 5 PIC X(50) FROM SYL-OBJECTIVES(1:50).
           05 LINE 5 COLUMN 5 PIC X(50) FROM SYL-OBJECTIVES(51:50).
           05 LINE 7 COLUMN 1 VALUE "新しい学習目標: ".
           05 LINE 8 COLUMN 5 PIC X(50) USING SYL-OBJECTIVES(1:50).
           05 LINE 9 COLUMN 5 PIC X(50) USING SYL-OBJECTIVES(51:50).

       01 UPDATE-WEEK-PLAN-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "授業計画更新".
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
               PERFORM SEARCH-SYLLABUS
               IF WS-FILE-SUCCESS
                   PERFORM UPDATE-SYLLABUS-LOOP
                   PERFORM REWRITE-SYLLABUS-RECORD
               END-IF
               PERFORM CHECK-CONTINUE
           END-PERFORM.

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

       SEARCH-SYLLABUS.
           DISPLAY SEARCH-SCREEN.
           ACCEPT SEARCH-SCREEN.

           MOVE WS-SEARCH-COURSE-ID TO SYL-COURSE-ID.
           READ SYLLABUS-FILE
               KEY IS SYL-COURSE-ID
               INVALID KEY
                   DISPLAY "エラー: 科目コード " SYL-COURSE-ID
                           " は存在しません。"
                   MOVE "00" TO WS-FILE-STATUS
           END-READ.

       UPDATE-SYLLABUS-LOOP.
           MOVE 0 TO WS-UPDATE-OPTION.
           PERFORM UNTIL WS-UPDATE-OPTION = 9
               DISPLAY UPDATE-MENU-SCREEN
               ACCEPT UPDATE-MENU-SCREEN

               EVALUATE WS-UPDATE-OPTION
                   WHEN 1
                       PERFORM UPDATE-COURSE-NAME
                   WHEN 2
                       PERFORM UPDATE-DEPARTMENT
                   WHEN 3
                       PERFORM UPDATE-TEACHER
                   WHEN 4
                       PERFORM UPDATE-SEMESTER
                   WHEN 5
                       PERFORM UPDATE-CREDITS
                   WHEN 6
                       PERFORM UPDATE-DESCRIPTION
                   WHEN 7
                       PERFORM UPDATE-OBJECTIVES
                   WHEN 8
                       PERFORM UPDATE-WEEK-PLAN
                   WHEN 9
                       EXIT PERFORM
                   WHEN OTHER
                       DISPLAY "無効な選択です。再試行してください。"
               END-EVALUATE
           END-PERFORM.

       UPDATE-COURSE-NAME.
           DISPLAY UPDATE-COURSE-NAME-SCREEN.
           ACCEPT UPDATE-COURSE-NAME-SCREEN.

       UPDATE-DEPARTMENT.
           DISPLAY UPDATE-DEPARTMENT-SCREEN.
           ACCEPT UPDATE-DEPARTMENT-SCREEN.

       UPDATE-TEACHER.
           DISPLAY UPDATE-TEACHER-SCREEN.
           ACCEPT UPDATE-TEACHER-SCREEN.

       UPDATE-SEMESTER.
           DISPLAY UPDATE-SEMESTER-SCREEN.
           ACCEPT UPDATE-SEMESTER-SCREEN.

       UPDATE-CREDITS.
           DISPLAY UPDATE-CREDITS-SCREEN.
           ACCEPT UPDATE-CREDITS-SCREEN.

       UPDATE-DESCRIPTION.
           DISPLAY UPDATE-DESCRIPTION-SCREEN.
           ACCEPT UPDATE-DESCRIPTION-SCREEN.

       UPDATE-OBJECTIVES.
           DISPLAY UPDATE-OBJECTIVES-SCREEN.
           ACCEPT UPDATE-OBJECTIVES-SCREEN.

       UPDATE-WEEK-PLAN.
           DISPLAY UPDATE-WEEK-PLAN-SCREEN.
           ACCEPT UPDATE-WEEK-PLAN-SCREEN.

       REWRITE-SYLLABUS-RECORD.
           REWRITE SYLLABUS-RECORD
               INVALID KEY
                   DISPLAY "エラー: レコードの更新に失敗しました。"
           END-REWRITE.
           DISPLAY "シラバス情報が正常に更新されました。".

       CHECK-CONTINUE.
           DISPLAY " ".
           DISPLAY "続けて更新しますか？ (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
