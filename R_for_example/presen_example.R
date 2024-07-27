## プレゼンテーションファイル用の例題
## Created: 2024-5-21(Tue)
## Revised: Time-stamp: <2024-05-21 18:56:10 masaei>
## 作成者：佐藤正衛 <masaei@affrc.go.jp>

## 例題
## * 要件定義
## 以下のテンプレートを使用して，
## 1. データの読み込み：./data/ディレクトリから気象のcsvファイルを読み込む関数を実装する
## 2. 読み込んだ気象データから月ごとの平均気温，最高気温，最低気温を計算する関数を実装する
## 3. 月ごとの気温データをグラフにプロットする関数を実装する
## * 関数名: myFunction
## ** 関数の目的:
## 関数は，〇〇を行い，〇〇を返す。

## ** 入力:
## 関数に必要な入力は以下の通り：
## - 入力1: （具体的なデータ型や形式など）
## - 入力2: （具体的なデータ型や形式など）

## ** 出力:
## 関数が返す出力情報は以下の通り：
## - 出力1: （具体的なデータ型や形式など）
## - 出力2: （具体的なデータ型や形式など）

## ** 動作:
## 関数は以下の手順で動作する：
## 1. ステップ1の説明
## 2. ステップ2の説明
## 3. ...

## ** 例外処理:
## 関数は以下の異常動作やエラーに対応する：
## - エラー1が発生した場合，〇〇する
## - エラー2が発生した場合，〇〇する

## ** 副作用:
## 関数の実行による副作用は以下の通り：
## - 外部状態Aが変更されると，〇〇の影響を及ぼす

## ** パフォーマンス:
## 関数が〇〇に影響を及ぼす場合，以下の最適化を考慮する：
## - 最適化手法1: 〇〇
## - 最適化手法2: 〇〇

## ** 制約:
## 関数の利用時には以下の制約条件に留意する：
## - 制約条件1: 〇〇
## - 制約条件2: 〇〇

## ** 入力例（関数の使用例）:
## - 入力1: 例示1
## - 入力2: 例示2

## ** 出力例（出力結果の形式の具体例）:
## - 出力1: 例示1
## - 出力2: 例示2



## * 関数名: read_weather_data
## 関数の目的:
## データの読み込みを行い，気象データを返す。

## 入力:
## - 入力1: directory_path (character), データのディレクトリパス
## - 入力2: file_name (character), ファイル名

## 出力:
## - 出力1: data.frame, 読み込んだ気象データ

## 動作:
## 1. 指定されたディレクトリから指定されたファイルを読み込む。

# Function to read weather data from a CSV file located in the specified directory path
# and with the given file name
read_weather_data <- function(directory_path, file_name) {
    # Read the CSV file by combining the directory path and file name
    data <- read.csv(paste0(directory_path, "/", file_name))
    return(data)
}

#roxygen2
## * read_weather_data
## ** roxygen2
##' Function to read weather data from a CSV file located in the specified directory path
##' and with the given file name.
##' 
##' @title Read Weather Data
##' @author [Your Name]
##' @param directory_path Path to the directory where the CSV file is located
##' @param file_name Name of the CSV file to be read
##' @return The data read from the CSV file
##' @examples
##' read_weather_data("path/to/directory", "weather_data.csv")
## ** function
read_weather_data <- function(directory_path, file_name) {
    # Read the CSV file by combining the directory path and file name
    data <- read.csv(paste0(directory_path, "/", file_name))
    return(data)
}



## * 関数名: calculate_monthly_weather_stats
## 関数の目的:
## 月ごとの平均気温，最高気温，最低気温を計算する。

## 入力:
## - 入力1: weather_data (data.frame), 気象データ

## 出力:
## - 出力1: data.frame, 月ごとの平均気温，最高気温，最低気温データ

## 動作:
## 1. 気象データを月ごとにグループ化し，平均気温，最高気温，最低気温を計算する。

calculate_monthly_weather_stats <- function(weather_data) {
    library(dplyr)
    library(lubridate)
    
    weather_data$Date <- ymd(weather_data$Date)
    stats <- weather_data %>%
        mutate(month = month(Date)) %>%
        group_by(month) %>%
        summarize(mean_temp = mean(Temp), max_temp = max(Temp), min_temp = min(Temp))
    
    return(stats)
}

## * 関数名: plot_monthly_weather_data
## 関数の目的:
## 月ごとの気温データをグラフにプロットする。

## 入力:
## - 入力1: weather_stats (data.frame), 月ごとの気温統計データ

## 出力:
## - 出力1: グラフ

## 動作:
## 1. 月ごとの平均気温，最高気温，最低気温を折れ線グラフでプロットする。

plot_monthly_weather_data <- function(weather_stats) {
    library(ggplot2)
    
    ggplot(weather_stats, aes(x = month, y = mean_temp, color = "Mean Temp")) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = max_temp, color = "Max Temp")) +
    geom_point(aes(y = min_temp, color = "Min Temp")) +
    scale_color_manual(values = c("Mean Temp" = "blue", "Max Temp" = "red", "Min Temp" = "green")) +
    labs(title = "Monthly Weather Data", x = "Month", y = "Temperature")
}

## 使用例:
## - 入力1: weather_data <- read_weather_data("./data/", "weather_data.csv")
## - 入力2: monthly_stats <- calculate_monthly_weather_stats(weather_data)
## - 入力3: plot_monthly_weather_data(monthly_stats)


## * 関数名: myFunction
## ** 関数の目的:
## 関数は，〇〇を行い，〇〇を返す。

## ** 入力:
## 関数に必要な入力は以下の通り：
## - 入力1: （具体的なデータ型や形式など）
## - 入力2: （具体的なデータ型や形式など）

## ** 出力:
## 関数が返す出力情報は以下の通り：
## - 出力1: （具体的なデータ型や形式など）
## - 出力2: （具体的なデータ型や形式など）

## ** 動作:
## 関数は以下の手順で動作する：
## 1. ステップ1の説明
## 2. ステップ2の説明
## 3. ...

## ** 例外処理:
## 関数は以下の異常動作やエラーに対応する：
## - エラー1が発生した場合，〇〇する
## - エラー2が発生した場合，〇〇する

## ** 副作用:
## 関数の実行による副作用は以下の通り：
## - 外部状態Aが変更されると，〇〇の影響を及ぼす

## ** パフォーマンス:
## 関数が〇〇に影響を及ぼす場合，以下の最適化を考慮する：
## - 最適化手法1: 〇〇
## - 最適化手法2: 〇〇

## ** 制約:
## 関数の利用時には以下の制約条件に留意する：
## - 制約条件1: 〇〇
## - 制約条件2: 〇〇

## ** 入力例（関数の使用例）:
## - 入力1: 例示1
## - 入力2: 例示2

## ** 出力例（出力結果の形式の具体例）:
## - 出力1: 例示1
## - 出力2: 例示2
