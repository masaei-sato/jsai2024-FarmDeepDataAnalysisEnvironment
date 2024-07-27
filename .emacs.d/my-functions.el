;;; my-functions.el --- 自己定義した関数をまとめたファイル  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Masaei Sato

;; Author: Masaei Sato <masaei@affrc.go.jp>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 自分で定義した関数をまとめたファイル。よく使う操作を関数化しておく。

;;; Code:
;;;; transient menu
;; transientのメニューを定義する
(require 'transient)
(require 'ess)
(require 'ess-view-data)

(setq transient-align-variable-pitch t) ;日本語の文字幅修正

;;;;; メタtransientメニュー
;; TODO: メニューのヘルプ文を書く。各モードのキーバインド
(transient-define-prefix my-transient-menus ()
  "Select Transient Menu"
  ["Menus"
   ("a" "AI Menu" ai-transient-menu)
   ("d" "Rパッケージ開発" ess-r-package-transient-menu)
   ("h" "Help" help-transient-menu)
   ("i" "Interface" interface-transient-menu)
   ("Q" "Quit" transient-quit-all)
   ]
  )
(global-set-key (kbd "C-c P") 'my-transient-menus) ;RET
;;;;; Help用のtransient
(transient-define-prefix help-transient-menu ()
  "Help Menu"
  [["Info"
    ("i" "info" info)
    ]
   ["File"
    ("t" "linux tools" (lambda () (interactive) (find-file "~/help/linux.org")))
    ("e" "emacs" (lambda () (interactive) (find-file "~/help/emacs.org")))
    ]
   ])
;;;;; Interface用のtransient
(transient-define-prefix interface-transient-menu ()
  "Interface Menu"
  [["Company"
    ("cc" "company" company-mode)
    ("cg" "company" global-company-mode)
    ]
   ["Misc"
    ("ln" "行番号" linum-mode)
    ("hl" "行ハイライト" global-hl-line-mode)
    ("mm" "ミニマップ(in progモード)" minimap-mode)
    ;; ("mo" "ミニモードライン" mini-modeline-mode)
    ]
   ["Theme"
    ("tt" "テーマ選択" load-theme)
    ("tr" "テーマリセット" disable-theme)
    ]
   ])
;;;;; Rパッケージ開発用のtransient
(transient-define-prefix ess-r-package-transient-menu ()
  [
   "R devtools menu."
   ["Dev"
    ("b" "Build" (lambda () (interactive) (ess-r-devtools-build)))
    ("c" "check" (lambda () (interactive) (ess-r-devtools-check-package)))
    ("d" "document" (lambda () (interactive) (ess-r-devtools-document-package)))
    ("l" "load" (lambda () (interactive) (ess-r-devtools-load-package)))
    ("t" "test" (lambda () (interactive) (ess-r-devtools-test-package)))
    ("u" "unload" (lambda () (interactive) (ess-r-devtools-unload-package)))
    ]
   ["Install"
    ("i" "install-package" (lambda () (interactive) (ess-r-devtools-install-package)))
    ("I" "install-github" (lambda () (interactive) (ess-r-devtools-install-github)))
    ]
   ["Create"
    ("C" "Create" (lambda () (interactive) (ess-r-devtools-create-package)))
    ]
   ["Excute"
    ("A" "Ask excute-command:" (lambda () (interactive) (ess-r-devtools-execute-command)))
    ("E" "excute-command:" (lambda () (interactive) (ignore)))
    ]
   ["AI"
    ("ai" "AI" ai-transient-menu)    
    "ess-r"
    ("r" "ess-r" ess-r-transient-menu)
    ]
   ["Misc"
    ("W" "check-with-winbuilder" (lambda () (interactive) (ess-r-devtools-check-with-winbuilder)))
    "mode"
    ("mt" "ess-r-package-mode" (lambda () (interactive) (ess-r-package-mode)))
    ]
   ])

;;;;; ess-r-mode用のtransient
(transient-define-prefix ess-r-transient-menu ()
  [
   "R ess-r menu."
   ["Narrowing"
    ("nf" "Narrow function" (lambda () (interactive) (ess-narrow-to-defun-or-para)))
    ("nn" "Narrow region" narrow-to-region)
    ("nw" "Widen"   (lambda () (interactive) (widen)))
    ("hs" "hs-toggle" (lambda () (interactive) (hs-toggle-hiding)))
    ]
   ["Edit"
    ("i" "Indent/Format" (lambda () (interactive) (format-all-buffer)))
    ("D" "dump object" (lambda () (interactive) (ess-dump-object-into-edit-buffer (symbol-name (symbol-at-point)))))
    "gptstudio"
    "gpttools"
    ]
   ["AI"
    ;; ("ai" "AI" (lambda () (interactive) ()))
    ("ai" "AI" ai-transient-menu)    
    ]
   ["View"
    ;; listviewerパッケージの関数を呼び出す
    ("Vl" "view list" (lambda () (interactive) (insert "listviewer::jsonedit()")))
    ("VD1" "view data" (lambda () (interactive) (insert "View()")))
    ("VD2" "view datatable" (lambda () (interactive) (insert "DT::datatable()")))
    ("VD3" "view datatable" (lambda () (interactive) (insert "formattable::formattable(iris)")))
    ]
   [
    "obj mngmt"
    ;; ("rd" "Rdired" (lambda () (interactive) (ess-rdired)))
    ("rd" "Rdired" ess-rdired)
    ("vd" "view-data-print" (lambda () (interactive) (ess-view-data-print)))
    "save"
    ("sf" "save function"  my/append-region-to-my-miscfunctions-r)
    ]
   ["pkg dev"
    ;; ("rD" "Rutils devtools" rutils-devtools)
    ("d" "devtools" (lambda () (interactive) (ess-r-package-transient-menu)))
    ]
;;;;;; Menu
   ["Menu"
    ("mt" "top menu" my-transient-menus)
    ]
;;;;;; Help   
   ["Help"
    ("hi" "info:ESS" (lambda () (interactive) (info "ess")))
    ("hcv" "help on obj" ess-display-help-on-object)
    ("hp" "pkg index" ess-display-package-index)
    ]
   ])

;;;;; ess-view-data用のtransient
(transient-define-prefix ess-view-data-transient-menu ()
  "ess-view-data menu"
  [
    "ess-view-data"
    ;; ("p" "print" (lambda () (interactive) (ess-view-data-print)))
    ("c" "count" (lambda () (interactive) (ess-view-data-count)))
    ("s" "summary" (lambda () (interactive) (ess-view-data-summarise)))
    ("f" "filter" (lambda () (interactive) (ess-view-data-filter)))
    ("r" "reset" (lambda () (interactive) (ess-view-data-reset)))
    ("l" "select" (lambda () (interactive) (ess-view-data-select)))
    ("m" "mutate" (lambda () (interactive) (ess-view-data-mutate)))
    ("g" "group_by" (lambda () (interactive) (ess-view-data-group_by)))
    ("o" "sort" (lambda () (interactive) (ess-view-data-sort)))
    ("u" "unique" (lambda () (interactive) (ess-view-data-unique)))
    ]
  )

;;;;; 各モードで同じキーバインディングを使うためのマイナーモード
;;;;;; org-mode用のtransientメニュー
;; ;; org-mode-mapにキーバインドを追加
;; (define-key org-mode-map (kbd "C-c p") 'org-mode-transient-menu)

;; ;; マイナーモードを定義してorg-mode-hookに追加する
;; (define-minor-mode my-org-transient-menus-mode
;;   "A minor mode for org-mode transient menus."
;;   :init-value nil
;;   :lighter " org-transient"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c p") 'org-mode-transient-menu)
;;             map))

;; (add-hook 'org-mode-hook 'my-org-transient-menus-mode)

;;;;;; ess-view-data用のtransientメニュー
;; ess-view-data-mode-mapにキーバインドを追加
(define-key ess-view-data-mode-map (kbd "C-c p") 'ess-view-data-transient-menu)

;; マイナーモードを定義してess-view-data-mode-hookに追加する
(define-minor-mode my-ess-view-data-transient-menus-mode
  "A minor mode for ess-view-data-mode transient menus."
  :init-value nil
  :lighter " ess-view-data-transient"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") 'ess-view-data-transient-menu)
            map))

(add-hook 'ess-view-data-mode-hook 'my-ess-view-data-transient-menus-mode)

;;;;;; ess-mode(ess-r-mode用, ess-r-package-mode用)のtransientメニュー
(define-minor-mode my-ess-transient-menus-mode
  "A minor mode to toggle between ESS transient menus."
  :init-value nil
  :lighter " ESS-Transient"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") 
              (lambda () (interactive)
                (if (bound-and-true-p ess-r-package-mode)
                    (ess-r-package-transient-menu)
                  (ess-r-transient-menu))))
            map))

(defun my-ess-enable-transient-menus ()
  (my-ess-transient-menus-mode 1))

(add-hook 'ess-r-mode-hook 'my-ess-enable-transient-menus)



;;;; AI
;; 参考 https://zenn.dev/megeton/articles/b5bff41f35af9a
;; 2024-1-31(Wed)
;;;;; ai-call
;; ai用のtransientメニュー(ai-transient-menu)で選択した指示文をRスクリプト(ai.R)を使ってChatGPTに投げる関数
;; ai.Rのラッパー[[~/bin/ai.sh][ai.sh]]を用いる
(defun my-ai-call (prompt)
  (interactive)
  (let* ((text (buffer-substring (region-beginning) (region-end)))
         (command (concat "~/bin/ai.sh" " " (shell-quote-argument (format prompt text)))))
  ;; (let* ((text (buffer-substring (region-beginning) (region-end)))
  ;;        (command (if (eq system-type 'windows-nt)
  ;;                     (concat "C:/Users/masaei/AppData/Local/Programs/Git/bin/bash.exe ~/bin/ai.sh" " " (shell-quote-argument (format prompt text)))
  ;;                   (concat "~/bin/ai.sh" " " (shell-quote-argument (format prompt text))))))
    (save-excursion (insert (shell-command-to-string command)))))
;;;;; ai用のtransientメニュー
;; transient メニュー形式で指示を選択する
;; https://zenn.dev/megeton/articles/06cb8d002603db
(transient-define-prefix ai-transient-menu ()
  "選択範囲を ChatGPT に投げる"
  [
;;;;;; 校正 [C]orrection
   ["校正"
    ("ca" "学術的文書"         (lambda () (interactive) (my-ai-call "%s\n上の文章を学術的文章のスタイルで校正してください。改行を挿入後に校正した結果だけを回答してください。クォーテションで囲む必要もありません。")))
    ("cb" "ビジネス文書" (lambda () (interactive) (my-ai-call "%s\n上の文章をビジネス文章のスタイルで校正してください。改行を挿入後に校正した結果だけを回答してください。クォーテションで囲む必要もありません。")))
    ("cg" "一般" (lambda () (interactive) (my-ai-call "%s\n上の文章を校正してください。改行を挿入後に校正した結果だけを回答してください。クォーテションで囲む必要もありません。")))
    ]
;;;;;; 翻訳 [T]ranslation
   ["翻訳"
    ("te" "→英語"       (lambda () (interactive) (my-ai-call "「%s」を英語に翻訳してください")))
    ("tj" "→日本語"     (lambda () (interactive) (my-ai-call "「%s」を日本語に翻訳してください")))
    ]
;;;;;; プログラミング [P]rogramming
   ["プログラミング"
    ("pp" "Pythonへ変換"       (lambda () (interactive) (my-ai-call "%s\n上のコードをPythonに変換してください")))
    ("pr" "Rへ変換"        (lambda () (interactive) (my-ai-call "%s\n上のコードをRに変換してください。コードはorg-modeドキュメントで読みやすくするため，適切な場所で改行してください。")))
    ]
;;;;;; Rプログラミング [R] Programming
   ;; Github Copilot Chatを参考に
   ;; [[file:~/help/AI_IoT.org::*Github Copilot Chat][Github Copilot Chat]]
   ["Rプログラミング"
    ("rd" "要件定義"     my/insert-snippet-ess-r-reqdef-template)
    ("rp" "要件からコード提案"     (lambda () (interactive) (insert "\n#提案コード\n") (my-ai-call "要件定義：\n%s\n上の要件定義にもとづき，Rのコードを提案してください。インデントは半角スペース2つを基準にしてください。")))
    ("re" "コードの内容説明"     (lambda () (interactive) (insert "\n#説明\n") (my-ai-call "%s\n上のRのコードの仕組みや関数の内容を説明してください。")))
    ("rf" "コードの修正提案"     (lambda () (interactive) (insert "\n#修正提案\n") (my-ai-call "%s\n上のRのコードの問題点に対して修正方法を提案してください。")))
    ("rc" "関数にコメント付与"     (lambda () (interactive) (insert "\n#コメント\n") (my-ai-call "%s\n上のR関数内のコードにコメント文を付けてください。まとまった処理には，その処理全体に関する説明も付けてください。コメントを追加する以外のコード自体の変更は行わないでください。コメントは英語で書いてください。")))
    ("ro" "roxygen2"     (lambda () (interactive) (insert "\n#roxygen2\n") (my-ai-call "%s\n上のR関数にroxygen2文書を加筆してください。見本を参考にしてください。\n
文章は英語で書いてください。\n
見本：
## * <function name>
## ** roxygen2
##' <Description>
##' 
##' <Details>
##' @title Add function
##' @author [Author Name]
##' @param <Description of param1>
##' @param <Description of param2>
##' @return <Description of return value>
##' @examples
##' <a few examples>
## ** function
my_add <- function(x, y) {
  z <- x + y
  return(z)
}

なお，次の条件を満たすこと。\n
- roxygen2の記述は，「##'」でコメントアウトする(記号#を二重にする。)\n
- roxygen2の一行目は，Descriptionを記述する\n
- roxygen2の二行目は，空行を挿入する\n
- roxygen2の三行目は，Detailsを記述する\n
- @title,@authorを加える\n
- 追加したroxygen2文章の一行前に「## * <function name>」，その次の行に「## ** roxygen2」を, roxygen2文章と関数定義の間の行に 「## ** function」を挿入(これらはroxygen2文ではないため，記号「'」は付けない。)\n
- 関数内に記載されたコメント文は削除してはならない\n
- ")))
    ("rt" "単体テスト"     (lambda () (interactive) (my-ai-call "%s\n上のR関数について単体テストのコードを提案してください。テストコードには説明用コメント文を丁寧に付けてください。コメントは英語で。")))
    ("rr" "リファクタリング"     (lambda () (interactive) (my-ai-call "%s\n上のR関数をリファクタリングしてください。")))
    ]
;;;;;; AI [A]I
   ["AI org-ai"
    ("Ac" "チャット開始" (lambda () (interactive) (insert "ai")))
    ("Ar" "選択範囲で質問"         (lambda () (interactive) (my-ai-call "%s")))
    ;; ("As" "model変更"         org-ai-switch-chat-model)
    ]
   ])

;;;; 関連する関数
;;;;; ai-save-region-instruction-result-to-kill-ring
;; 2024-1-31(Wed)
;; model，パラメータはR関数で固定しているので，変更はファイルを直接編集すること
;; function defined in R. [[file:~/bin/ai.R]]
(defun my/ai-save-region-instruction-result-to-kill-ring (start end)
  "Run ai.sh shell script with selected instruction region as argument and save output to kill-ring."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (output (shell-command-to-string (concat "~/bin/ai.sh " (shell-quote-argument region-text)))))
    (kill-new output)
    ;; 選択リージョンを解除する
    (deactivate-mark)
    (message "Output from ai.sh saved to kill-ring.")))

;;;;; append-region-to-my-miscfunctions-r
;; 2024-2-23(Fri)
;; ESSで作成したR関数をmy_miscfunctions.Rに追加する
(defun my/append-region-to-my-miscfunctions-r ()
  "Append the selected region to my_miscfunctions.R.
   If my_miscfunctions.R does not exist, it will be created."
  (interactive)
  (unless (use-region-p)
    (error "No region selected"))
  (let ((file-name (concat (file-name-directory (buffer-file-name)) "my_miscfunctions.R"))
        (content (buffer-substring-no-properties (region-beginning) (region-end))))
    (if (file-writable-p file-name)
        (with-temp-buffer
          (insert content "\n") ; Ensure there is a newline after the appended content
          ;; Create the file if it doesn't exist, or append to it if it does.
          (append-to-file (point-min) (point-max) file-name)
          (message "Appended to %s" file-name))
      (error "Cannot write to file: %s" file-name))))


;;;;; insert-snippet-ess-r-reqdef-template
;; 2024-2-22(Thu)
;; ess-r-modeでreqdefスニペット(関数の要件定義テンプレート)を挿入する
(defun my/insert-snippet-ess-r-reqdef-template ()
  "Inserts the 'reqdef' snippet for ess-r-mode."
  (interactive)
  ;; Specify the snippet directory and snippet name
  (let ((snippet-dir (expand-file-name "~/.emacs.d/snippets/ess-r-mode/pkgDev"))
        (snippet-name "reqdef")
        (mode 'ess-r-mode))
    ;; Ensure YASnippet is loaded and initialized
    (unless (featurep 'yasnippet)
      (require 'yasnippet)
      (yas-global-mode 1)) ;; Ensure YASnippet global mode is enabled
    ;; Add the snippet directory to yas-snippet-dirs if not already present
    (unless (member snippet-dir yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snippet-dir)
      (yas-reload-all)) ;; Reload all snippets after adding new directory
    ;; Ensure the current buffer is in the correct mode for the snippet
    (unless (eq major-mode mode)
      (error "This function is intended for use with %s" mode))
    ;; Find and expand the snippet
    (if-let ((snippet (yas-lookup-snippet snippet-name mode)))
        (yas-expand-snippet (yas--template-content snippet))
      (message "Snippet '%s' not found in %s for mode %s" snippet-name snippet-dir mode))))

;;;;; org-ai-create-roxygen2
;; Rパッケージ開発のためのroxygen2ドキュメントを作成する
(defun org-ai-create-roxygen2 (start end)
  "Ask ChatGPT to create a roxygen2 documentation for a selected function.
`START' is the buffer position of the start of the function.
`END' is the buffer position of the end of the function."
  (interactive "r")

  ;; Get the function name
  (let ((function-name (if (use-region-p)
                           (buffer-substring-no-properties start end)
                         (read-string "Enter the function name: "))))
    
    ;; Generate the roxygen2 template
    (let ((text-prompt-fn (lambda (code) (format "

Create a roxygen2 documentation for the function `%s`.

```r
%s
```
" function-name code)))
          (buffer-with-selected-code (current-buffer))
          (file-name (buffer-file-name))
          (output-buffer (get-buffer-create "*org-ai-create-roxygen2*"))
          (win-config (current-window-configuration)))
      
      ;; Call org-ai-with-input-or-spoken-text to interact with ChatGPT
      (org-ai-with-input-or-spoken-text "Create roxygen2 documentation: " function-name
        (org-ai--output-to-buffer start end text-prompt-fn output-buffer
                                  :show-output-buffer t
                                  :callback (lambda ()
                                              (progn
                                                (with-current-buffer output-buffer
                                                  ;; Ensure buffer ends with a newline
                                                  (goto-char (point-max))
                                                  (unless (eq (char-before) ?\n) (insert ?\n))
                                                  ;; Mark the whole buffer
                                                  (push-mark)
                                                  (push-mark (point-max) nil t)
                                                  (goto-char (point-min)))
                                                (org-ai--diff-and-patch-buffers buffer-with-selected-code output-buffer file-name)
                                                (set-window-configuration win-config))))))))

;;; my-functions.el ends here
