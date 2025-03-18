# COBOL 開発コンテナのサンプル

これは Visual Studio Code 開発コンテナを使用した COBOL 開発を実演するサンプルプロジェクトです。このプロジェクトには、ウェルカムメッセージと現在の日付を表示するシンプルな COBOL プログラムが含まれています。また、典型的なレガシー COBOL アプリケーションのパターンを示す包括的なシラバス管理システムも含まれています。

## 前提条件

- [Visual Studio Code](https://code.visualstudio.com/)
- [Docker](https://www.docker.com/)
- [Dev Containers 拡張機能](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

## 始め方

1. このリポジトリをクローンします：
   ```bash
   git clone https://github.com/yourusername/hello-devcontainer-with-cobol.git
   cd hello-devcontainer-with-cobol
   ```

2. Visual Studio Code でプロジェクトを開きます：
   ```bash
   code .
   ```

3. VS Code から「コンテナで再度開く」というプロンプトが表示されたら、クリックします。
   - または、`F1`を押して、「Dev Containers: コンテナで再度開く」と入力して Enter を押します

4. 開発コンテナのビルドと起動を待ちます。初回は数分かかる場合があります。

## アプリケーションのビルドと実行

開発コンテナ内で、VS Code タスクを使用してアプリケーションをビルドおよび実行できます：

### VS Code コマンドパレット (Ctrl+Shift+P または Cmd+Shift+P) を使用する場合：
1. アプリケーションをビルドする：
   - コマンドパレットを開いて「Tasks: ビルドタスクの実行」と入力するか、`Ctrl+Shift+B`を押します

2. アプリケーションを実行する：
   - コマンドパレットを開く
   - 「Tasks: タスクの実行」と入力
   - 「run」を選択

### VS Code でターミナルを使用する場合：
1. アプリケーションをビルドする：
   ```bash
   make
   ```

2. アプリケーションを実行する：
   ```bash
   make run
   ```

3. ビルド成果物をクリーンする：
   ```bash
   make clean
   ```

## プロジェクト構造

- `src/hello.cob`：ウェルカムメッセージと現在の日付を表示するシンプルな COBOL プログラム
- `src/SYLABUS.cbl`：シラバス管理システムのメインプログラム（コントローラー）
- `src/SYLREG.cbl`：シラバス登録プログラム
- `src/SYLUPD.cbl`：シラバス更新プログラム
- `src/SYLDEL.cbl`：シラバス削除プログラム
- `src/SYLQRY.cbl`：シラバス照会プログラム
- `src/SYLLST.cbl`：シラバス一覧プログラム
- `src/SYLRPT.cbl`：シラバスレポート生成プログラム
- `src/SYLCOM.cbl`：共通ルーチン（サブプログラム）
- `src/copybooks/`：レコード定義を含むディレクトリ
  - `SYLFILE.cpy`：シラバスファイルのレコード定義
  - `DEPFILE.cpy`：学部ファイルのレコード定義
  - `TEAFILE.cpy`：教員ファイルのレコード定義
- `Makefile`：ビルド設定
- `.devcontainer/`：開発コンテナ設定ファイル
- `bin/`：ビルド出力ディレクトリ（ビルド中に作成される）

# シラバス管理システム

シラバス管理システムは、古典的な COBOL エンタープライズアプリケーションのパターンとテクニックを実証する包括的なアプリケーションです。このサンプルは、教育機関、金融サービス、保険会社でよく見られる従来のレガシーシステムの多くの機能を再現しています。

## システムアーキテクチャ

システムはモジュール設計による階層的なプログラム構造を採用しています — 各プログラムが特定の機能に特化する伝統的な COBOL 開発アプローチです：

### 主要モジュール

1. **SYLABUS.cbl** - メインコントローラープログラム
   - エントリーポイントとメインメニューとして機能
   - 他の専門モジュールへの呼び出しを調整
   - メニューナビゲーションのユーザー入力を処理

2. **SYLREG.cbl** - シラバス登録プログラム
   - CRUDの作成操作を実装
   - データ入力用の画面インターフェースを提供
   - SYLCOMの呼び出しを通じて入力データを検証
   - インデックスファイルに新しいシラバスレコードを書き込み

3. **SYLUPD.cbl** - シラバス更新プログラム
   - CRUDの更新操作を実装
   - 既存のシラバスレコードの変更を可能にする
   - フィールドごとの更新オプションを提供
   - レコードの取得と更新に動的ファイルアクセスモードを使用

4. **SYLDEL.cbl** - シラバス削除プログラム
   - CRUDの削除操作を実装
   - 削除前の確認を提供
   - インデックスファイルからシラバスレコードを削除
   - 削除エラーケースを処理

5. **SYLQRY.cbl** - シラバス照会プログラム
   - CRUDの読み取り操作を実装
   - 特定のシラバスに関する詳細情報を取得して表示
   - 基本情報と週間計画の詳細の両方を表示
   - ページ分割を使用した画面指向の表示を使用

6. **SYLLST.cbl** - シラバス一覧プログラム
   - フィルタリングオプション付きのリスト機能を提供
   - 複数のレコードを表示するためのページ分割を実装
   - 学部、教員、学期ベースのフィルタリングを提供
   - 動的なレコードカウントと表示を使用

7. **SYLRPT.cbl** - レポート生成プログラム
   - シラバスデータから整形されたレポートを作成
   - 複数のレポートタイプとフィルタリングオプションを提供
   - 印刷用の順次テキストファイルに出力
   - ヘッダー、詳細、要約セクションを含む

8. **SYLCOM.cbl** - 共通ユーティリティサブプログラム
   - 他のモジュール用の共有機能を提供
   - データ検証ルーチンを実装
   - 日付処理とフォーマットサービスを提供
   - 柔軟な操作のためにパラメータ受け渡しを使用

## 実演されるCOBOLの主要機能

### 1. テキストベースUIのためのスクリーンセクション

アプリケーションは、従来の端末アプリケーションに典型的なテキストベースのインターフェースを作成するために、COBOLのスクリーンセクションを広範囲に使用しています：

```cobol
SCREEN SECTION.
01 SYLLABUS-INPUT-SCREEN.
    05 BLANK SCREEN.
    05 LINE 1 COLUMN 1 VALUE "シラバス登録画面".
    05 LINE 3 COLUMN 1 VALUE "科目コード (例: CS1001): ".
    05 LINE 3 COLUMN 28 PIC X(6) USING SYL-COURSE-ID.
```

これは以下を実証しています：
- BLANK SCREENによる端末画面のクリア
- LINEとCOLUMN句を使用した絶対位置指定
- 入出力機能を持つフィールド定義
- フォームベースのデータ入力システム

### 2. ファイル操作と永続性

アプリケーションはデータ永続性のためのインデックスファイル処理を示しています：

```cobol
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SYLLABUS-FILE
        ASSIGN TO "syllabus.dat"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS SYL-COURSE-ID
        FILE STATUS IS WS-FILE-STATUS.
```

主な特徴：
- INDEXEDオーガナイゼーション（メインフレームシステムのVSAMファイルに類似）
- 順次アクセスとランダムアクセスの両方のためのDYNAMICアクセスモード
- 主キー定義（SYL-COURSE-ID）
- FILE STATUSを使用したエラー処理

### 3. 複雑なデータ構造

システムはデータ構造定義のためにcopybookを使用します：

- **SYLFILE.cpy**：コース情報と週間計画を含むシラバスレコード構造を定義
- **DEPFILE.cpy**：学部レコード構造を定義
- **TEAFILE.cpy**：教員レコード構造を定義

これらの構造は以下を含む可能性があります：
- グループ項目による入れ子階層
- テーブル処理のためのOCCURS句（週間計画実装で見られる）
- 複数のプログラム間で再利用可能な定義

### 4. 制御フローとプログラムロジック

アプリケーションは洗練されたCOBOL制御構造を示しています：

```cobol
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
```

主要なプログラミングパターン：
- 複数方向分岐のためのEVALUATE文（switch/caseに類似）
- 手続き呼び出しのためのPERFORM文
- PERFORM UNTILによる条件ループ
- セクションと段落の構成

### 5. サブプログラム通信

システムはプログラム間通信を実証しています：

```cobol
CALL "SYLCOM" USING WS-FUNCTION-CODE, WS-PARAM-1,
               WS-PARAM-2, WS-RESULT, WS-RETURN-CODE.
```

特徴：
- 静的サブプログラム呼び出し
- 参照によるパラメータ渡し
- リターンコード処理
- ファンクションコードベースの操作選択

### 6. エラー処理メカニズム

システム全体に堅牢なエラー処理が実装されています：

```cobol
01 WS-FILE-STATUS           PIC XX VALUE "00".
   88 WS-FILE-SUCCESS       VALUE "00".
   88 WS-FILE-DUP           VALUE "22".
   88 WS-FILE-NOT-FOUND     VALUE "23".
```

主要な技術：
- 読みやすいステータスチェックのためのレベル88条件名
- 明示的なエラーメッセージ表示
- ファイル操作における例外処理
- ステータスコードの伝播

## CRUD実装

システムは完全なCRUD（作成、読み取り、更新、削除）実装を提供します：

1. **作成**：SYLREG.cbl - 新しいシラバスエントリの登録
   - すべてのフィールドのデータ入力画面
   - 大規模データセット用のマルチスクリーン入力
   - レコード書き込み前の検証

2. **読み取り**：SYLQRY.cbl - シラバス情報の詳細照会
   - キーベースのレコード検索
   - 総合的な情報のマルチスクリーン表示
   - 複雑なデータ間のナビゲーション

3. **更新**：SYLUPD.cbl - 既存のシラバスエントリの変更
   - フィールドごとの更新
   - 変更前の現在のデータ表示
   - 完全または選択的な更新

4. **削除**：SYLDEL.cbl - シラバスエントリの削除
   - キーによるレコード選択
   - 削除前の確認
   - 操作後のステータス報告

5. **一覧**：SYLLST.cbl - シラバスエントリのフィルタリングされた一覧
   - 複数のフィルタリングオプション
   - ページ分割表示
   - 要約情報

6. **レポート**：SYLRPT.cbl - フォーマット済みレポート
   - 複数のレポートタイプ
   - フィルタリングと選択基準
   - 印刷用のフォーマット済み出力

## ビジネスドメイン

アプリケーションは以下の主要エンティティを持つ教育機関のシラバス管理システムをモデル化しています：

1. **コース/シラバス**：以下を含むコア教育提供物：
   - 識別情報（コースコード）
   - 説明情報（名前、説明）
   - スケジュール情報（学期、単位）
   - 週間教育計画

2. **学部**：学術組織単位

3. **教員**：コースに関連する教員メンバー

このドメインモデルは、COBOLアプリケーションが、オブジェクト指向モデルではなく、ファイル構造とプログラムロジックを通じてビジネスエンティティと関係をどのように表現するかを示しています。

## 結論

シラバス管理システムは、以下の特性を持つ古典的なCOBOLアプリケーションアーキテクチャの模範です：

1. 専門化されたプログラムによるモジュラーアーキテクチャ
2. テキストベースユーザーインターフェース用のスクリーンセクション
3. データ永続性のためのインデックスファイル操作
4. copybookを使用した複雑なデータ構造
5. 構造化された制御フローによる手続き型プログラミングパラダイム
6. サブプログラム呼び出しメカニズム
7. 総合的なエラー処理
8. 完全なCRUD実装

このサンプルは、教育、金融、保険、政府部門など、さまざまな業界で重要なビジネス運用を行っている多くのレガシーCOBOLアプリケーションで見られるパターンとテクニックを効果的に実証しています。

## 機能

サンプル COBOL プログラムは以下を実演します：
- 基本的な COBOL プログラム構造
- 日付処理
- フォーマット済み出力表示

## VS Code COBOL 開発機能

この開発コンテナには、以下の VS Code 拡張機能による強化された COBOL 開発サポートが付属しています：

- **COBOL 言語サポート** (bitlang.cobol)：基本的な COBOL 構文ハイライトと言語サポート
- **Z Open Editor** (ibm.zopeneditor)：IBM による高度な COBOL 編集機能
- **COBOL 言語サポート** (broadcom.cobol-language-support)：追加の COBOL 言語機能
- **Code Spell Checker** (streetsidesoftware.code-spell-checker)：コメントや文字列のスペルエラーを検出
- **Background Copy** (kainino.backgroundcopy)：COPY ステートメントの処理を改善

### COBOL 固有の設定

環境は COBOL に最適化された設定で事前構成されています：

- **カラムルーラー**：列 6、7、72（標準 COBOL カード形式）の視覚ガイド
- **インデント**：スペース（タブではない）による単一スペースインデント
- **保存時のフォーマット**：ファイル保存時の自動コードフォーマット
- **セマンティックハイライト**：より良いコード読みやすさのための強化された構文ハイライト
- **空白の可視化**：正確なフォーマットのためにすべての空白文字が表示される
- **区分フォーマット**：COBOL 区分の自動配置とフォーマット

### エディタ機能

- 入力、貼り付け、保存時の自動フォーマット
- スマートな COBOL 対応のインデント
- より正確な COBOL 補完のための単語ベースの提案を無効化
- COBOL フォーマットを維持するための追加のスペースの保持
- より良いコード構造の視覚化のための段落インジケータ

## インストール

1. `.devcontainer` ディレクトリを作成します：
```shell
mkdir -p .devcontainer
```

2. `.devcontainer` に以下のファイルを作成します：

### Dockerfile
COBOL 開発環境をセットアップするコンテナ定義：
- ベースイメージとして Ubuntu 22.04 スリムを使用
- イメージサイズを最小化するマルチステージビルド
- GnuCOBOL コンパイラと必要なライブラリをインストール
- 重要な開発ツール（git、curl）を含む
- 必要なコンポーネントのみを含めることでコンテナサイズを最適化

### compose.yaml
コンテナオーケストレーション用の Docker Compose 設定：
- ローカルの Dockerfile からコンテナをビルド
- ソースコードアクセス用のワークスペースディレクトリをマウント
- リソース制限を設定（メモリ：1GB、CPU シェア）
- `sleep infinity` でコンテナを実行し続ける
- 適切なシグナルハンドリングのための init プロセスを有効化

### devcontainer.json
VS Code 開発コンテナ設定：
- コンテナ名とワークスペースの場所を指定
- Docker Compose 設定と統合
- 構文ハイライト用の COBOL 拡張機能をインストール
- より良い開発体験のためのターミナル設定を構成
- 開発用のリモートユーザとして root を設定

3. VS Code は開発コンテナ設定を自動的に検出します

## 参考資料

## ライセンス

[MIT ライセンス](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)の下でリリースされています

## 作者

- GitHub: <https://github.com/shinyay>
- Twitter: <https://twitter.com/yanashin18618>
- Mastodon: <https://mastodon.social/@yanashin>
