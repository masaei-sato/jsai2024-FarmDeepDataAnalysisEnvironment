;;; my-functions.el --- JSAI2024研究発表用のemacs設定ファイル  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Masaei Sato

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

;; Define your own functions in this file.
;; Place this file in the directory ~/.emacs.d/.
;; Add the following line to init.el so that this file is loaded.
;; (load "my-functions.el")

;;; Code:
;;;; パッケージ管理
;;;;; Melpa
;; Emacsのパッケージを提供するリポジトリMelpaを利用するための設定
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; パッケージアーカイブの情報を初期化または更新
(unless package-archive-contents
  (package-refresh-contents))

;;;;; use-package
;; use-packageとは、パッケージの設定を簡潔に記述するためのマクロ
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use-packageの初期化
(eval-when-compile
  (require 'use-package))

;; オプション: use-packageによるパッケージの自動インストールを有効化
(setq use-package-always-ensure t)

;;;;; melpaからインストールするパッケージ
;;;;;; transient
;; transientは、Emacsのコマンドラインインターフェースを拡張するためのライブラリ
(use-package transient
  :ensure t)
(setq transient-align-variable-pitch t) ;日本語の文字幅修正

;;;;;; ESS(Emacs Speaks Statistics)
;; ESSは、統計解析ソフトウェアRやSAS、Stataなどの統計ソフトウェアをEmacs上で利用するためのパッケージ
(use-package ess
  :ensure t)

;;;;;; org-ai
;; org-aiは、Emacsのorg-modeでGPT-3やGPT-4を利用するためのパッケージ
(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

;;;;;; yasnippet
;; yasnippetは、テンプレートを利用してコードを効率的に書くためのパッケージ
;; ess-r-modeで要件定義テンプレートを挿入するために利用する
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;;; 自己定義関数
;;;;; yas-helm-insert-snippet
;; helmでスニペットを挿入する。migemoも有効にする。プレビュー機能あり
;; 2024-2-10(Sat)
;; (setq helm-migemo-mode t)
(defun my/yas-helm-insert-snippet ()
  "Use helm to insert a snippet for the current major mode, with a preview of the snippet content."
  (interactive)
  ;; Save current buffer and cursor position
  (let* ((original-buffer (current-buffer))
         (original-pos (point))
         (snippets (yas--all-templates (yas--get-snippet-tables)))
         (candidates (mapcar (lambda (template)
                               (cons (format "%s\t%s" (yas--template-name template) (yas--template-key template))
                                     template))
                             snippets)))
    (helm :sources (helm-build-sync-source "YASnippet"
                     :candidates candidates
                     :migemo t
                     :action (lambda (template)
                               ;; Switch back to the original buffer and restore cursor position
                               (switch-to-buffer original-buffer)
                               (goto-char original-pos)
                               ;; Now, expand the snippet
                               (yas-expand-snippet (yas--template-content template)))
                     :persistent-action (lambda (template)
                                          ;; Preview the snippet content in a temporary buffer
                                          (with-output-to-temp-buffer "*YASnippet Preview*"
                                            (princ (yas--template-content template))))
                     :persistent-help "Preview snippet")
          :buffer "*helm yas-snippet*")))

;;;;; ai-call
;; ai用のtransientメニュー(ai-transient-menu)で選択した指示文をRスクリプト(ai.R)を使ってChatGPTに投げる関数
;; ai.Rのラッパー[[~/bin/ai.sh][ai.sh]]を用いる
(defun my-ai-call (prompt)
  (interactive)
  (let* ((text (buffer-substring (region-beginning) (region-end)))
         (command (concat "~/bin/ai.sh" " " (shell-quote-argument (format prompt text)))))
    (save-excursion (insert (shell-command-to-string command)))))

;;;;; ai-save-region-instruction-result-to-kill-ring
;; 選択リージョンの指示文をRスクリプト(ai.R)を使ってChatGPTに投げて結果をkill-ringに保存する関数
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

;;;;; org-ai-create-roxygen2
;; org-aiを使って選択した関数のroxygen2ドキュメントを作成する関数
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


;;;; transient menu
;; transientのメニューを作成する
;; メニュー形式で指示を選択する
;; 参考: https://zenn.dev/megeton/articles/06cb8d002603db

;;;;; トップメニュー
(transient-define-prefix my-transient-menus ()
  "Select Transient Menu"
  ["Menus"
   ("a" "AI Menu" ai-transient-menu)
   ("h" "Help" help-transient-menu)
   ;; ("o" "Org Mode" org-mode-transient-menu)
   ("r" "Rパッケージ開発" rutils-devtools)   ;; Rパッケージ開発. rutilsパッケージ
   ("Q" "Quit" transient-quit-all)
   ]
  )
(global-set-key (kbd "C-c P") 'my-transient-menus) ;ショートカットキーの設定

;;;;; ai用のtransientメニュー
(transient-define-prefix ai-transient-menu ()
  "選択範囲を ChatGPT に投げる"
  [
;;;;;; プログラミング [P]rogramming
   ["プログラミング"
    ("pp" "Pythonへ変換"       (lambda () (interactive) (my-ai-call "%s\n上のコードをPythonに変換してください")))
    ("pr" "Rへ変換"        (lambda () (interactive) (my-ai-call "%s\n上のコードをRに変換してください。コードはorg-modeドキュメントで読みやすくするため，適切な場所で改行してください。")))
    ]
;;;;;; Rプログラミング [R] Programming
   ;; Github Copilot Chatを参考にメニューを作成
   ;; [[file:~/help/AI_IoT.org::*Github Copilot Chat][Github Copilot Chat]]
   ["Rプログラミング"
    ("rd" "要件定義"     my/insert-snippet-ess-r-reqdef-template)
    ("rp" "要件からコード提案"     (lambda () (interactive) (my-ai-call "要件定義：\n%s\n上の要件定義にもとづき，Rのコードを提案してください。インデントは半角スペース2つを基準にしてください。")))
    ("re" "コードの内容説明"     (lambda () (interactive) (my-ai-call "%s\n上のRのコードの仕組みや関数の内容を説明してください。")))
    ("rf" "コードの修正提案"     (lambda () (interactive) (my-ai-call "%s\n上のRのコードの問題点に対して修正方法を提案してください。")))
    ("rc" "関数にコメント付与"     (lambda () (interactive) (my-ai-call "%s\n上のR関数内のコードにコメント文を付けてください。まとまった処理には，その処理全体に関する説明も付けてください。コメントを追加する以外のコード自体の変更は行わないでください。コメントは英語で書いてください。")))
    ("ro" "roxygen2"     (lambda () (interactive) (my-ai-call "%s\n上のR関数にroxygen2文書を加筆してください。見本を参考にしてください。\n
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

;;;;; Help用のtransient
(transient-define-prefix help-transient-menu ()
  "Help Menu"
  [["Info"
    ("i" "info" info)
    ]
   ])
;;;;; org-mode用のtransient
(transient-define-prefix org-mode-transient-menu ()
  "org-mode menu"
  [  "【org-mode menu】"
;;;;;; AI [A]I
   ["AI org-ai"
    ("Ac" "チャット開始" (lambda () (interactive) (insert "ai")))
    ("Ar" "選択範囲で質問"         (lambda () (interactive) (my-ai-call "%s")))
    ("As" "model変更"         org-ai-switch-chat-model)
    ]
;;;;;; Help   
   ["Help"
    ("hi" "info:Org Guide" (lambda () (interactive) (info "(orgguide)")))
    ]
   ])

;;;;; ess-mode用のtransient
(transient-define-prefix ess-transient-menu ()
  "ess menu"
  [
   ["Narrowing"
    ("nf" "Narrow function" (lambda () (interactive) (ess-narrow-to-defun-or-para)))
    ("nn" "Narrow region" narrow-to-region)
    ("nw" "Widen"   (lambda () (interactive) (widen)))
    ("hs" "hs-toggle" (lambda () (interactive) (hs-toggle-hiding)))
    ]
   ["Edit"
    ("i" "Indent" (lambda () (interactive) (format-all-buffer)))
    ]
   ["AI"
    ("ai" "AI" ai-transient-menu)    
    ]
   [
    "obj mngmt"
    ("rd" "Rdired" (lambda () (interactive) (ess-rdired)))
    ("vd" "view-data-print" (lambda () (interactive) (ess-view-data-print)))
    "save"
    ("sf" "save function"  my/append-region-to-my-miscfunctions-r)
    ]
   ["pkg dev"
    ("rD" "Rutils devtools" rutils-devtools)
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

;;;;; 各モードで同じキーバインディングを使うためのマイナーモード
;;;;;; org-mode用のtransientメニュー
;; org-mode-mapにキーバインドを追加
(define-key org-mode-map (kbd "C-c p") 'org-mode-transient-menu)

;; マイナーモードを定義してorg-mode-hookに追加する
(define-minor-mode my-org-transient-menus-mode
  "A minor mode for org-mode transient menus."
  :init-value nil
  :lighter " org-transient"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") 'org-mode-transient-menu)
            map))

(add-hook 'org-mode-hook 'my-org-transient-menus-mode)

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

;;;;;; ess-mode用のtransientメニュー
;; ess-mode-mapにキーバインドを追加
(define-key ess-mode-map (kbd "C-c p") 'ess-transient-menu)

;; マイナーモードを定義してess-mode-hookに追加する
(define-minor-mode my-ess-transient-menus-mode
  "A minor mode for ess-mode transient menus."
  :init-value nil
  :lighter " ess-transient"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") 'ess-transient-menu)
                map))

(add-hook 'ess-mode-hook 'my-ess-transient-menus-mode)

;;;; end

(provide 'my-functions)
;;; my-functions.el ends here
