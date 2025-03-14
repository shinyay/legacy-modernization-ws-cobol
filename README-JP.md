# COBOL 開発コンテナのサンプル

これは Visual Studio Code 開発コンテナを使用した COBOL 開発を実演するサンプルプロジェクトです。このプロジェクトには、ウェルカムメッセージと現在の日付を表示するシンプルな COBOL プログラムが含まれています。

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

- `src/hello.cob`：メイン COBOL ソースファイル
- `Makefile`：ビルド設定
- `.devcontainer/`：開発コンテナ設定ファイル
- `bin/`：ビルド出力ディレクトリ（ビルド中に作成される）

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
