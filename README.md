# jsai2024-FarmDeepDataAnalysisEnvironment

農業情報学会（JSAI）2024年度年次大会での研究発表「大規模言語モデルと連携した農場ディープデータの解析環境の設計と労働時間分析への適用」に関連するプログラムとデータ

## 概要

このプロジェクトは、大規模言語モデルを活用して農場のディープデータを解析する環境の設計・開発に関する成果をまとめたものです。農場に蓄積する多種多様なデータを効率的かつ効果的に分析する環境の構築を目的としています。分析環境は主にEmacsとそのパッケージおよびR言語とそのパッケージを用います。

## 構成

- `.emacs.d/`: Emacsの設定ファイル
  - `init.el`: データの前処理スクリプト
  - `my-functions.el`: モデルの学習スクリプト
- `R_for_example/`: 発表資料で用いたコードとデータ
- `bin/`: スクリプト
  - `ai.R`: LLM（OpenAI GPT-3.5-turbo）へのAPIのRスクリプト
  - `ai.sh`: API（ai.R）のラッパーのシェルスクリプト
- `doc/`: 発表資料で用いたプレゼンファイルなど
- `figure/`: 発表資料で用いた図の画像ファイル

## インストール方法

1. このリポジトリをクローンします：

```sh
git clone https://github.com/masaei-sato/jsai2024-FarmDeepDataAnalysisEnvironment.git
```

2. Emacsのインストール：

- Windows:
  - [公式サイト](https://www.gnu.org/software/emacs/download.html#windows)からインストーラをダウンロードし、実行します。
- macOS:
  - Homebrewを使用する場合: `brew install emacs`
  - または[公式サイト](https://emacsformacosx.com/)からダウンロードしてインストールします。
- Linux (Ubuntu/Debian):

```sh
sudo apt update
sudo apt install emacs
```

3. Rのインストール：

- Windows:
  - [CRAN](https://cran.r-project.org/bin/windows/base/)からインストーラをダウンロードし、実行します。
- macOS:
  - Homebrewを使用する場合: `brew install r`
  - または[CRAN](https://cran.r-project.org/bin/macosx/)からダウンロードしてインストールします。
- Linux (Ubuntu/Debian):

```sh
sudo apt update
sudo apt install r-base
```

4. 必要なRパッケージのインストール：

Rを起動し、以下のコマンドを実行します：

```R
install.packages(c("httr", "jsonlite"))
```

5. Emacsの設定：

このリポジトリの `.emacs.d` ディレクトリを、ホームディレクトリにコピーまたはシンボリックリンクを作成します。Emacsを起動すると、必要なパッケージ（ESS, transient, yasnippet等）が自動でダウンロードされインストールされるようになっています。

6. OpenAI APIキーの取得と設定：

a. OpenAIのアカウント作成：

- [OpenAIのウェブサイト](https://openai.com/)にアクセスし、アカウントを作成します。
- アカウント作成には有効なメールアドレスが必要です。

b. APIキーの取得：

- OpenAIアカウントにログインした後、ダッシュボードにアクセスします。
- 「API Keys」または「Create new secret key」のセクションを探します。
- 新しいAPIキーを生成します。
- 生成されたキーを安全な場所に保存してください。このキーは再表示できません。

c. 環境変数の設定：

- OpenAI APIキーを環境変数 `OPENAI_API_KEY` に設定します。
- 具体的には、`.Renviron` ファイルに次の行を含めます：

```sh
OPENAI_API_KEY="your_api_key"
```

注意：APIキーは秘密情報です。GitHubなどの公開リポジトリにアップロードしたり、他人と共有したりしないでください。

## 重要な注意事項
1. 動作環境について：
- 本プログラムはDebian 12で動作を確認しました。
- Windows，macOSでの動作は未確認です。

2. API利用料金について：

- OpenAI APIの使用には料金が発生します。料金は使用量に応じて課金されます。
- 現在の料金体系と詳細については、[OpenAIの価格ページ](https://openai.com/pricing)で確認してください。
- 料金は予告なく変更される可能性があります。

3. 利用者の責任：

- このプロジェクトを利用してOpenAI APIを使用する場合、発生する料金はすべて利用者の責任となります。
- APIの使用量と料金を定期的に確認し、予期せぬ高額請求を避けるため、必要に応じて利用制限を設定することをお勧めします。

4. セキュリティ：

- APIキーは個人情報と同様に扱ってください。第三者との共有や、公開リポジトリへのアップロードは絶対に避けてください。
- 不正使用を防ぐため、定期的にAPIキーを更新することをお勧めします。

このプロジェクトを使用する前に、上記の点を十分に理解し、自己責任のもとでAPIを利用してください。OpenAIのサービス利用規約と料金ポリシーをよく読み、同意した上で使用してください。

プロジェクト管理者は、本プロジェクトのプログラム使用および個々のユーザーのAPI使用に起因する料金や損害について一切の責任を負いません。

## 使用方法

1. Emacsの起動：

ターミナルで `emacs` コマンドを実行するか、GUI版Emacsを起動します。

2. EmacsでRを実行：

- まずコードを記述するファイルを開きます。
- そのファイル内で `M-x R` を入力してRを起動します。

3. コードの作成：

- プログラムコードを作成します。
- `C-c p` を入力するとtransientが起動し、キー操作で実行したいコマンドを選択します。

## 連絡先
プロジェクトに関する質問やフィードバックがある場合は、以下の方法で連絡してください：

名前: 佐藤正衛
メール: sato.masaei269@naro.go.jp
GitHub: https://gist.github.com/masaei-sato
