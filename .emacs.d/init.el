;; -*- mode: emacs-lisp; coding: utf-8 -*-
;; Revised: Time-stamp: <2024-05-18 09:24:18 masaei>
;; Author: Masaei Sato <masaei@affrc.go.jp>

;;; 1. 初期設定
;;;; 1-1 pathを通す
(setq load-path (append '("~/.emacs.d/lib") load-path))
(setq load-path (append '("~/.emacs.d/lib/edraw") load-path))
;; xfce4のランチャー起動ではパスが通っていない場合があるので，その対応
(let ((my-bin-path (expand-file-name "~/bin")))
  (setenv "PATH" (concat my-bin-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-bin-path))

;;;; 1-2 日本語環境設定
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;;;; bash
;; ;; WindowsとLinuxの区別をしてシェルを設定する関数
;; (defun set-shell-path ()
;;   (if (eq system-type 'windows-nt)
;;       ;; Windowsの場合
;;       (setq explicit-shell-file-name "C:/Users/masaei/AppData/Local/Programs/Git/bin/bash.exe")
;;     ;; Linuxの場合
;;     (setq explicit-shell-file-name "/bin/bash")))

;; ;; シェルを設定する関数を呼び出す
;; (set-shell-path)

;; ;; シェルのパスが正しく設定されたか確認するメッセージを表示
;; (message "Using shell: %s" explicit-shell-file-name)

;;; 2. パッケージ管理の設定
;;;; 2-1 straight
;; https://github.com/radian-software/straight.el
;; 2022-7-2(Sat)
;; https://nukosuke.hatenablog.jp/entry/straight-el
;; straight.el自身のインストールと初期設定を行ってくれる
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; 2-2 package.elの初期設定
(require 'package)
(setq package-archives
      '(
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ))
;; 初期化
(package-initialize)
;; パッケージがインストール/更新されていないならインストール/更新
(unless package-archive-contents
  (package-refresh-contents))

;;;; 2-3 インストールするパッケージ my/favorite-packages
(defvar my/favorite-packages
  '(
;;;;; pkg management
   browse-kill-ring 
   exec-path-from-shell
   gnu-elpa-keyring-update 
   use-package
;;;;; org-mode
   ob-async org org-ac org-agenda-property
   org-attach-screenshot org-bookmark-heading
   org-contrib
;;;;; transient
   transient rutils
;;;;; snippet
   yasnippet yasnippet-snippets
;;;;; ai, LLM, chatgpt
    org-ai 
;;;;; R
   ess ess-view-data
   poly-R poly-markdown poly-noweb polymode
   flycheck
   format-all
;;;;; misc
   color-theme-modern smartparens which-key hlinum migemo dash minimap hiwin cp5022x
;;;;; end
    ))

;;;; 2-4 my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (pkg my/favorite-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; 3. 全般的な設定
;;;; 3-1 見た目に関する設定
(global-font-lock-mode t)
(setq visible-bell t)
(show-paren-mode 1)
(blink-cursor-mode 0)
(bar-cursor-mode 1)
(setq-default line-spacing 1) ;行間
(setq x-select-enable-clipboard t)
(add-hook 'text-mode-hook 'turn-on-window-margin-mode); 2018-3-16(Fri)
(setq nobreak-char-display nil)                      ; 全角スペースのアンダーラインを非表示

;; 選択中のウィンドウに色を付ける
(require 'hiwin)
;; (hiwin-activate)
;;   (set-face-background 'hiwin-face "gray25")
(hiwin-deactivate)

;; fringeの行の折り返し時に付く矢印の色を変更する。(行頭のみ。行末(right-curly-arrow)はそのまま。)
;; 参照：emacs info 14.14 Window Fringes
;; 2021-9-14(Tue)
(defface my-custom-curly-face
  '((t (:foreground "black")))
  "Face for fringe curly bitmaps."
  :group 'basic-faces)
(set-fringe-bitmap-face 'left-curly-arrow 'my-custom-curly-face)

;;;; 3-2 theme-framework
;; themeを変更するときは M-x load-theme → load-only-theme(themeを全てリセットしてからloadする)
;; (load-theme 'robin-hood t)
;; https://www.reddit.com/r/emacs/comments/30b67j/how_can_you_reset_emacs_to_the_default_theme/
;; 2020-7-15(Wed)
(defun load-only-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (call-interactively 'load-theme))

;;;; 3-3 key bind
;;;;; 設定
;; C-h を backspace として使う等。
(keyboard-translate ?\C-h ?\C-?)
;(keyboard-translate ?\177 ?\^h)
;(keyboard-translate ?\^h ?\177)
(global-set-key "\C-h" nil)
(global-set-key (kbd "C-c SPC") 'set-mark-command)
(global-set-key (kbd "C-c @") 'set-mark-command)
(global-set-key [delete] 'delete-char)
(global-set-key [muhenkan] 'other-window)
(global-set-key [S-muhenkan] 'previous-multiframe-window)
(global-set-key [Hangul_Hanja] 'other-window)
;(global-set-key [muhenkan] [RET])
(global-unset-key "\C-z")
(define-key global-map "\C-zl" 'toggle-truncate-lines)
(setq text-mode-hook 'turn-off-auto-fill)
(global-set-key "\C-zf" 'auto-fill-mode)
(setq caps-key 'control)
(global-set-key "\M-R" 'set-justification-right)
;; [f1] help
;; [f2] ???
;; [f3] defining kbd macro
(global-set-key [f4] 'list-bookmarks)
;; (global-set-key [f5] 'newsticker-show-news)→newstickerは使わない
(global-set-key [f6] 'list-buffers) ;2016-6-16(Thu)追加
;; [f7] undefined
;; [f8] ez-insert
;; [f9] undefined
;; [f10,f11,f12] 使われている
(global-set-key [f12] 'tool-bar-mode)
(global-set-key [S-f12] 'menu-bar-mode)
(global-set-key [?\C-\S-n] 'forward-paragraph) ;Control + Shift + n
(global-set-key [?\C-\S-p] 'backward-paragraph) ;Control + Shift + p
(global-set-key [C-f12] 'elscreen-toggle-display-tab)
(global-set-key [M-f12] 'hide-mode-line-mode)
(global-set-key (kbd "M-C-,") 'markerpen-mark-region)
(global-set-key (kbd "M-C-<") 'markerpen-clear-region)
(global-set-key "\C-c\C-xl" 'org-toggle-link-display)
;; C-c C-pの設定
(eval-after-load "ess-mode"
  '(define-key ess-mode-map (kbd "C-c C-p") nil))
(eval-after-load "outshine"
  '(define-key outline-minor-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading))

;;;;; unset
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c C-x C-c") 'save-buffers-kill-emacs)

;;;;; 分割したウィンドウ間の移動
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)
 
;;;; 3-4 初期フレームの設定
;; (setq default-frame-alist
;;      (append (list 
;; ;                    '(foreground-color . "black")
;; ;                    '(background-color . "LemonChiffon")
;; ;                    '(background-color . "gray")
;; ;                    '(border-color . "black")
;; ;                    '(mouse-color . "white")
;; ;                    '(cursor-color . "black")
;; ;                    '(font . "ipa-gothic 16") ???error
;;                     '(width . 90)
;;                     '(height . 30)
;;                     '(top . 0)
;;                     '(left . 0))
;;               default-frame-alist))
;
;;;;; window-system
(when window-system
  (tool-bar-mode -1) ;メニューのアイコン 1 or -1
  (menu-bar-mode -1) ;メニューの文字  1 or -1
  (scroll-bar-mode -1)
  )

;;;; 3-5 ファイル名，拡張子とモードの関連付け
(setq auto-mode-alist
      (append '(
		("\\.sdic$" . html-mode)
		;; ("\\.tex$" . yatex-mode)
		;; ("\\.html$" . yahtml-mode)
		;; ("\\.bib$" . bibtex-mode)
		("\\.org$" . org-mode)
		;; ("\\.txt$" . org-mode)
		;; ("\\.lgr$" . ledger-mode)
		;; ("\\.ledger$" . ledger-mode)
		;; ("\\.ledger$" . ledger-mode)
		;; ("\\.mod\\'" . gmpl-mode)
;		("\\.txt$" . text-mode)
		)
              auto-mode-alist)
      )

;;;; 3-6 リージョンを削除できるように
;; delete-regionしなくてもC-dで削除できる
(delete-selection-mode t)

;;;; 3-7 shortcut key ショートカットキー
(global-set-key [(control ?=)] (lambda () (interactive) (text-scale-increase 1))) ; hs-toggle-hidingとかぶるためC-+ から C-=へ変更
(global-set-key [(control ?-)] (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key [(control ?0)] (lambda () (interactive) (text-scale-increase 0)))

;;;; 3-8 スクロール
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)  ;; これでスクロールがよりスムーズになります

;;; 4. フォント font
;;;; 4-1 全般
;; (set-frame-font "さざなみゴシック-12" nil t)
(set-frame-font "ＭＳ ゴシック-12")
;; (set-frame-font "MeiryoKe_console-12")
;; (set-frame-font "VL ゴシック-10")
;; (set-frame-font "MigMix 2M-12" nil t)

;;;; 4-2 cp5022x.el
;; 2019-6-10(Mon)
;; MewでISO-2022-JPなのに丸数字つかってるメールを読む方法
;; http://hkoie.livedoor.blog/archives/55763366.html
(require 'cp5022x)

;;; 5. 日本語関連
;;;; 5-1 mozc
;; mozcを使うとき
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
;; (require 'mozc)
;; (setq default-input-method "japanese-mozc")
;; (prefer-coding-system 'utf-8) 
;;; 6. 入力支援
;;;; 6-1 comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)
(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
;;;; 6-2 yasnippet
(require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"
;;         "~/.emacs.d/yasnippets"
;;         ))
;; るびきち #20 http://emacs.rubikitch.com/sd1512-dabbrev-hippie-skeleton-yasnippet/
;; るびきち #21
(yas-global-mode 1)
;; スニペット名をidoで選択する
(setq yas-prompt-functions '(yas-ido-prompt))
;; 既存スニペットを挿入する
;; (define-key yas-minor-mode-map (kbd "C-c i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-c i i") 'my/yas-helm-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-c i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-c i v") 'yas-visit-snippet-file)
;; ivy-yasnippet(snippetのpreview)のための設定 2020-1-28(Tue)
(define-key yas-minor-mode-map (kbd "C-c i p") 'ivy-yasnippet)
(define-key yas-minor-mode-map (kbd "C-c i r") 'yas-reload-all)
;; yasnippetの補完キー変更
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-i") nil)
(define-key yas-minor-mode-map (kbd "C-S-i") 'yas-expand)

;;;; 6-3 outshine
;;;;; 1) outshine初期設定
;; https://github.com/tj64/outshine
;; 2016-12-31(Sat)
;; org以外でorg-modeのように構造化
(require 'outshine)
(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'ess-mode-hook 'outline-minor-mode)
(add-hook 'ess-mode-hook 'outshine-mode)
(add-hook 'message-mode-hook 'outline-minor-mode)
(add-hook 'sh-mode-hook 'outline-minor-mode)
(add-hook 'sh-mode-hook 'outshine-mode)
(setq outshine-use-speed-commands t)
(setq outshine-startup-folded-p t)

;;;;; 2) outshine customize style
;; http://www.modernemacs.com/post/outline-ivy/
(require 'dash)
(defun -add-font-lock-kwds (FONT-LOCK-ALIST)
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
                `(,rgx (0 (progn
                            (compose-region (match-beginning 1) (match-end 1)
                                            ,(concat "\t" (list uni-point)))
                            nil))))
              FONT-LOCK-ALIST)))

(defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial '-add-font-lock-kwds
                                (symbol-value font-locks)))))))

(defconst emacs-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^;;;\\) "          ?■)
    ("\\(^;;;;\\) "         ?○)
    ("\\(^;;;;;\\) "        ?★)
    ("\\(^;;;;;;\\) "       ?▷)))

(defconst lisp-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^;; \\*\\) "          ?■)
    ("\\(^;; \\*\\*\\) "       ?○)
    ("\\(^;; \\*\\*\\*\\) "    ?★)
    ("\\(^;; \\*\\*\\*\\*\\) " ?▷)))

(defconst ess-outlines-font-lock-alist ;2021-2-9(Tue)
  '(("\\(^## \\*\\) "          ?■)
    ("\\(^## \\*\\*\\) "       ?○)
    ("\\(^## \\*\\*\\*\\) "    ?★)
    ("\\(^## \\*\\*\\*\\*\\) " ?▷)))

(defconst sh-script-outlines-font-lock-alist ;2022-5-12(Thu)
  '(("\\(^#' \\*\\) "          ?■)
    ("\\(^#' \\*\\*\\) "       ?○)
    ("\\(^#' \\*\\*\\*\\) "    ?★)
    ("\\(^#' \\*\\*\\*\\*\\) " ?▷)))

(add-font-locks
 '((emacs-outlines-font-lock-alist emacs-lisp-mode-hook)
   ;; (lisp-outlines-font-lock-alist clojure-mode-hook hy-mode-hook)
   (ess-outlines-font-lock-alist ess-mode-hook)
   (sh-script-outlines-font-lock-alist sh-script-mode-hook)
   ))


;;; 7. ESS Emacs Speaks Statistics
;;;; 初期設定
(if (eq system-type 'windows-nt)
    (setq inferior-ess-r-program "C:/Program Files/R/R-4.4.0/bin/R.exe"))
(require 'ess-site)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(autoload 'R "ess-site" "start R" t)


;; (require 'ess-smart-underscore)
;; パッケージはremoved on 2019-8-29(Thu)
;; ESS info を見て以下の設定
(define-key ess-r-mode-map "_" #'ess-insert-assign)
(define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)

;; indent levelをformat-all-bufferにあわせる
(defun my-ess-mode-hook ()
  (setq-local ess-indent-offset 2))

(add-hook 'ess-mode-hook 'my-ess-mode-hook)

;;;; バッファ表示の制御
;; ESS info： 3.5 Controlling buffer display 
(setq display-buffer-alist
      `(("*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1) 			;-1
         (window-width . 0.33)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.5)
	 (window-height . 0.25)
         (reusable-frames . nil))
        ("*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.5)
         (reusable-frames . nil))))
;;;; 出力用バッファの表示のさせ方
;; 2020-1-30(Thu)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;;;; Rdired
;; 2020-2-5(Wed)
(with-eval-after-load 'ess-rdired
(define-key ess-rdired-mode-map "P" 'ess-rdired-plot)
(define-key ess-rdired-mode-map "n" 'forward-to-indentation)
(define-key ess-rdired-mode-map "p" 'backward-to-indentation)
)

;;;; indent-region-function with styler
;; 2022-6-1(Wed)
;; https://github.com/emacs-ess/ESS/issues/947
;;;;; ess-indent-region-with-styler
(defun ess-indent-region-with-styler (beg end)
  "Format region of code R using styler::style_text()."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string ;; how to avoid this double matching?
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
	(buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({options(styler.colored_print.vertical = FALSE);styler::style_text(text = \"\n%s\", reindention = styler::specify_reindention(regex_pattern = \"###\", indention = 0), indent_by = 4)})\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      ;; (skip-chars-backward "\n")
      (let ((end (point)))
	(goto-char (point-min))
	(goto-char (1+ (point-at-eol)))
	(setq string (buffer-substring-no-properties (point) end))
	))
    (delete-region beg end)
    (insert string)
    (delete-char -1)
    ))
;;;;; load
(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook
            (lambda ()
            (set (make-local-variable 'indent-region-function)
               'ess-indent-region-with-styler))))
;;;; ess-r-modeに自作メニューを表示
;; 2024-3-12(Tue)
(defun insert-r-command (command)
  "Inserts an R command into the current buffer."
  (interactive "sR Command: ")
  (insert command))

;;;;; my-R Menu
(easy-menu-define
 my-r-menu
 ess-r-mode-map ;; もしess-r-mode-mapが未定義なら、ess-mode-mapを試してください。
 "My R Tools"
 '("My-R-Tools"
;;;;; FarmDeepdata Menu
   ("FarmDeepdata"
    ("File"
     ["Load Dataset" (insert-r-command "load('path/to/file.RData')\n") t]
     ["Save Dataset" (insert-r-command "save(my_data, file='path/to/file.RData')\n") t])
    ("Plot"
     ["Basic Plot" (insert-r-command "plot(x, y)\n") t]
     ["Histogram" (insert-r-command "hist(data)\n") t])
    ("Help"
     ["Help on function" (insert-r-command "?function_name\n") t]
     ["Example for function" (insert-r-command "example(function_name)\n") t]))
;;;;; general Menu
   ("Plots"
    ["Scatterplot" (insert-r-command "plot(x, y)\n") t]
    ["Histogram" (insert-r-command "hist(data)\n") t]
    ["Boxplot" (insert-r-command "boxplot(data)\n") t])
   ("Summaries"
    ["Summary" (insert-r-command "summary(data)\n") t]
    ["Describe" (insert-r-command "describe(data)\n") t])
   ("Modeling"
    ["Linear Regression" (insert-r-command "lm(y ~ x, data=data)\n") t]
    ["Logistic Regression" (insert-r-command "glm(y ~ x, family=binomial, data=data)\n") t])
   "--"
   ["Other Command" (insert-r-command nil) t]))

;;;;; end
(defun my-ess-r-mode-hook ()
  (easy-menu-add my-r-menu))

(add-hook 'ess-r-mode-hook 'my-ess-r-mode-hook)



;;; 8. my-functions.el
;; Transient, 自己定義した関数，LLMを利用したAIタスクの設定
(load-file "~/.emacs.d/my-functions.el")

;; end of file
