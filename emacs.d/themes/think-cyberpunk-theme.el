;;; think-cyberpunk-theme.el --- Cyberpunk Color Theme

;; Copyright 2012-2014, Nicholas M. Van Horn

;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: color theme cyberpunk
;; Version: 20140630.1800
;; X-Original-Version: 1.10

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;; "and he'd still see the matrix in his sleep, bright lattices of logic
;; unfolding across that colorless void..."
;; William Gibson, Neuromancer.

;;; Commentary:

;; This theme is a port of the overtone/emacs-live theme of the same name
;; (https://github.com/overtone/emacs-live). The original theme was
;; designed for use with the color-theme package. This theme adopts the
;; new built-in theme support deftheme. Additionally, this
;; theme strives to offer as many mode-specific customizations as
;; possible, with further tweaks that suit my fancy.

(deftheme think-cyberpunk "The Cyberpunk color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Cyberpunk palette
      (think-cyberpunk-fg "#dcdccc")
      (think-cyberpunk-bg-1 "#2b2b2b")
      (think-cyberpunk-bg-05 "#383838")
      (think-cyberpunk-bg "#000000")
      (think-cyberpunk-bg+1 "#4f4f4f")
      (think-cyberpunk-bg+2 "#5f5f5f")
      (think-cyberpunk-bg+3 "#6f6f6f")
      (think-cyberpunk-red+1 "#dca3a3")
      (think-cyberpunk-red "#ff0000")
      (think-cyberpunk-red-1 "#8b0000")
      (think-cyberpunk-red-2 "#8b0000")
      (think-cyberpunk-red-3 "#9c6363")
      (think-cyberpunk-red-4 "#8c5353")
      (think-cyberpunk-red-5 "#7F073F")
      (think-cyberpunk-pink "#ff69b4")
      (think-cyberpunk-pink-1 "#ff1493")
      (think-cyberpunk-pink-2 "#cd1076")
      (think-cyberpunk-orange-2 "#FF6400")
      (think-cyberpunk-orange-1 "#ff8c00") ;; DarkOrange
      (think-cyberpunk-orange "#ffa500")
      (think-cyberpunk-yellow "#ffff00")
      (think-cyberpunk-yellow-1 "#FBDE2D")
      (think-cyberpunk-yellow-2 "#d0bf8f")
      (think-cyberpunk-yellow-3 "#D8FA3C")
      (think-cyberpunk-yellow-4 "#E9C062")
      (think-cyberpunk-yellow-5 "#ffd700")
      (think-cyberpunk-green-2 "#006400")
      (think-cyberpunk-green-1 "#2e8b57")
      (think-cyberpunk-green "#00ff00")
      (think-cyberpunk-green+1 "#61CE3C")
      (think-cyberpunk-green+2 "#9fc59f")
      (think-cyberpunk-green+3 "#afd8af")
      (think-cyberpunk-green+4 "#bfebbf")
      (think-cyberpunk-cyan "#93e0e3")
      (think-cyberpunk-blue+1 "#94bff3")
      (think-cyberpunk-blue "#0000ff")    ;; blue
      (think-cyberpunk-blue-1 "#7b68ee")  ;; medium slate blue
      (think-cyberpunk-blue-2 "#6a5acd")  ;; slate blue
      (think-cyberpunk-blue-3 "#add8e6")  ;; light blue
      (think-cyberpunk-blue-4 "#b2dfee")  ;; LightBlue2
      (think-cyberpunk-blue-5 "#4c83ff")
      (think-cyberpunk-blue-6 "#96CBFE")
      (think-cyberpunk-blue-7 "#00ffff")
      (think-cyberpunk-blue-8 "#4F94CD")
      (think-cyberpunk-magenta "#dc8cc3")
      (think-cyberpunk-black "#000000")
      (think-cyberpunk-black-2 "#0C1021")
      (think-cyberpunk-black-3 "#0A0A0A")
      (think-cyberpunk-gray "#d3d3d3")
      (think-cyberpunk-gray-2 "#8B8989")
      (think-cyberpunk-gray-3 "#919191")
      (think-cyberpunk-gray-4 "#999999")
      (think-cyberpunk-gray-5 "#333333")
      (think-cyberpunk-gray-6 "#1A1A1A")
      (think-cyberpunk-gray-7 "#4D4D4D")
      (think-cyberpunk-gray-8 "#262626")
      (think-cyberpunk-white "#ffffff")
      (think-cyberpunk-white-2 "#F8F8F8")
      (think-cyberpunk-white-3 "#fffafa"))

 (custom-theme-set-faces
   'think-cyberpunk
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,think-cyberpunk-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,think-cyberpunk-yellow-2 :underline t :weight normal))))
   `(blue ((,class (:foreground ,think-cyberpunk-blue))))
   `(bold ((,class (:bold t))))
   `(bold-italic ((,class (:bold t))))
   `(border-glyph ((,class (nil))))
   `(buffers-tab ((,class (:background ,think-cyberpunk-black-2 :foreground ,think-cyberpunk-white-2))))

   ;;; basic coloring
   `(default ((,class (:foreground ,think-cyberpunk-gray :background ,think-cyberpunk-black))))
   `(cursor ((,class (:background ,think-cyberpunk-fg))))
   `(escape-glyph-face ((,class (:foreground ,think-cyberpunk-red))))
   ;; `(fringe ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-bg+1))))
   `(header-line ((,class (:foreground ,think-cyberpunk-yellow
                                       :background ,think-cyberpunk-bg-1
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,think-cyberpunk-gray-5))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,think-cyberpunk-green))))
   `(compilation-error-face ((,class (:foreground ,think-cyberpunk-red-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(compilation-info-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(compilation-info ((,class (:foreground ,think-cyberpunk-green+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,think-cyberpunk-green))))
   `(compilation-line-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(compilation-line-number ((,class (:foreground ,think-cyberpunk-yellow))))
   `(compilation-message-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(compilation-warning-face ((,class (:foreground ,think-cyberpunk-yellow-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink-1))))
   `(grep-error-face ((,class (:foreground ,think-cyberpunk-red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-red))))
   `(grep-match-face ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink-1))))
   `(match ((,class (:background ,think-cyberpunk-black :foreground ,think-cyberpunk-pink-1))))


   ;;; multiple-cursors
   `(mc/cursor-face ((,class (:inverse-video nil, :background ,think-cyberpunk-pink :foreground ,think-cyberpunk-black))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink-1))))
   `(isearch-fail ((,class (:background ,think-cyberpunk-red-1))))
   
   `(lazy-highlight ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-yellow))))
   `(query-replace ((,class (:background ,think-cyberpunk-gray-5))))
   `(Highline-face ((,class (:background ,think-cyberpunk-green-1))))
   `(italic ((,class (nil))))
   `(left-margin ((,class (nil))))
   `(toolbar ((,class (nil))))
   `(underline ((,class (:underline nil))))
   `(text-cursor ((,class (:background ,think-cyberpunk-yellow :foreground ,think-cyberpunk-black))))

   `(menu ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-bg))))
   `(minibuffer-prompt ((,class (:foreground ,think-cyberpunk-green+1 :background ,think-cyberpunk-black))))
   `(mode-line
     ((,class (:foreground ,think-cyberpunk-blue-5
                           :background ,think-cyberpunk-gray-5
                           :box (:line-width -1 :color ,think-cyberpunk-blue-5)))))
   ;; `(mode-line-buffer-id ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,think-cyberpunk-gray-7
                           :background ,think-cyberpunk-gray-6
                           :box (:line-width -1 :color ,think-cyberpunk-blue-5)))))
   `(region ((,class (:background ,think-cyberpunk-red-5))))
   `(secondary-selection ((,class (:background ,think-cyberpunk-bg+2))))
   `(trailing-whitespace ((,class (:background ,think-cyberpunk-red))))
   `(vertical-border ((,class (:foreground ,think-cyberpunk-gray-5 :background ,think-cyberpunk-black))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,think-cyberpunk-orange-2))))
   `(font-lock-comment-face ((,class (:foreground ,think-cyberpunk-gray-2 :italic t))))
   ;; `(font-lock-comment-delimiter-face ((,class (:foreground ,think-cyberpunk-green)))) 
   `(font-lock-constant-face ((,class (:foreground ,think-cyberpunk-blue-5))))
   ;; `(font-lock-doc-face ((,class (:foreground ,think-cyberpunk-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,think-cyberpunk-orange-1))))
   `(font-lock-function-name-face ((,class (:foreground ,think-cyberpunk-pink-1))))
   `(font-lock-keyword-face ((,class (:foreground ,think-cyberpunk-yellow-3))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,think-cyberpunk-gray-3))))
   `(font-lock-string-face ((,class (:foreground ,think-cyberpunk-green+1))))
   `(font-lock-type-face ((,class (:foreground ,think-cyberpunk-blue-8))))
   `(font-lock-variable-name-face ((,class (:foreground ,think-cyberpunk-yellow-3))))
   `(font-lock-warning-face ((,class (:foreground ,think-cyberpunk-pink))))
   `(font-lock-reference-face ((,class (:foreground ,think-cyberpunk-gray))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,think-cyberpunk-yellow-4))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,think-cyberpunk-red))))


   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(gui-element ((,class (:background ,think-cyberpunk-gray-5 :foreground ,think-cyberpunk-blue-6))))
   


   ;;; newsticker
   ;; These are currently placeholders that probably look terrible.
   ;; Someone who uses newsticker is welcome to change these
   `(newsticker-date-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(newsticker-default-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,think-cyberpunk-green+3))))
   `(newsticker-extra-face ((,class (:foreground ,think-cyberpunk-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,think-cyberpunk-green))))
   `(newsticker-new-item-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,think-cyberpunk-red))))
   `(newsticker-old-item-face ((,class (:foreground ,think-cyberpunk-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,think-cyberpunk-green))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,think-cyberpunk-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,think-cyberpunk-red))))
   `(newsticker-treeview-old-face ((,class (:foreground ,think-cyberpunk-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,think-cyberpunk-yellow))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,think-cyberpunk-fg))))
   `(ack-file ((,class (:foreground ,think-cyberpunk-blue))))
   `(ack-line ((,class (:foreground ,think-cyberpunk-yellow))))
   `(ack-match ((,class (:foreground ,think-cyberpunk-orange :background ,think-cyberpunk-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(font-latex-string ((,class (:foreground ,think-cyberpunk-green))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))
   `(font-latex-sectioning-0 ((,class (:foreground ,think-cyberpunk-blue :background ,think-cyberpunk-black :scale 1.5))))
   `(font-latex-sectioning-1 ((,class (:foreground ,think-cyberpunk-blue :background ,think-cyberpunk-black :scale 1.5))))

   ;; auto-complete
   `(ac-completion-face ((,class (:background ,think-cyberpunk-gray-2 :underline t))))
   `(ac-candidate-face ((,class (:background ,think-cyberpunk-gray-4 :foreground ,think-cyberpunk-black))))
   `(ac-selection-face ((,class (:background ,think-cyberpunk-pink-1 :foreground ,think-cyberpunk-black))))
   `(popup-tip-face ((,class (:background ,think-cyberpunk-gray-5 :foreground ,think-cyberpunk-white))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,think-cyberpunk-black-3))))
   `(popup-scroll-bar-background-face ((,class (:background ,think-cyberpunk-gray-5))))
   `(popup-isearch-match ((,class (:background ,think-cyberpunk-black :foreground ,think-cyberpunk-pink-1))))

   `(window-number-face ((,class (:background ,think-cyberpunk-gray-6 :foreground ,think-cyberpunk-blue-5))))

   ;; company-mode
   `(company-tooltip ((,class (:background ,think-cyberpunk-gray-2 :foreground ,think-cyberpunk-yellow))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,think-cyberpunk-blue))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,think-cyberpunk-blue))))
   `(company-tooltip-selection ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink-1))))
   `(company-tooltip-annotation ((,class (:foreground ,think-cyberpunk-black-3))))
   `(company-scrollbar-fg ((,class (:background ,think-cyberpunk-black-3))))
   `(company-scrollbar-bg ((,class (:background ,think-cyberpunk-gray-5))))
   `(company-preview ((,class (:foreground ,think-cyberpunk-gray :background ,think-cyberpunk-pink-1))))

   ;; diff
   `(diff-added ((,class (:foreground ,think-cyberpunk-green))))
   `(diff-changed ((,class (:foreground ,think-cyberpunk-yellow))))
   `(diff-removed ((,class (:foreground ,think-cyberpunk-red))))
   `(diff-header ((,class (:background ,think-cyberpunk-bg+2))))
   `(diff-file-header ((,class (:background ,think-cyberpunk-bg+2 :foreground ,think-cyberpunk-fg :bold t))))

   ;; ediff
   `(ediff-current-diff-Ancestor ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-pink))))
   `(ediff-current-diff-A ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-bg-05))))
   `(ediff-current-diff-B ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-bg+1))))
   `(ediff-current-diff-C ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-bg+2))))
   `(ediff-even-diff-Ancestor ((,class (:foreground ,think-cyberpunk-white :background ,think-cyberpunk-bg-05))))
   `(ediff-even-diff-A ((,class (:foreground ,think-cyberpunk-white :background ,think-cyberpunk-bg+1))))
   `(ediff-even-diff-B ((,class (:foreground ,think-cyberpunk-white :background ,think-cyberpunk-bg+2))))
   `(ediff-even-diff-C ((,class (:foreground ,think-cyberpunk-white :background ,think-cyberpunk-bg+3))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink))))
   `(ediff-fine-diff-A ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-blue-5))))
   `(ediff-fine-diff-B ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-blue-5))))
   `(ediff-fine-diff-C ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-blue-5))))
   `(ediff-odd-diff-Ancestor ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-gray-2))))
   `(ediff-odd-diff-A ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-gray-3))))
   `(ediff-odd-diff-B ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-gray-4))))
   `(ediff-odd-diff-C ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-gray))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,think-cyberpunk-green+4 :background ,think-cyberpunk-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,think-cyberpunk-red :background ,think-cyberpunk-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,think-cyberpunk-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,think-cyberpunk-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,think-cyberpunk-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,think-cyberpunk-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,think-cyberpunk-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,think-cyberpunk-red-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,think-cyberpunk-yellow-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,think-cyberpunk-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,think-cyberpunk-orange-2 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,think-cyberpunk-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(erc-keyword-face ((,class (:foreground ,think-cyberpunk-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,think-cyberpunk-red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,think-cyberpunk-green))))
   `(erc-pal-face ((,class (:foreground ,think-cyberpunk-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,think-cyberpunk-orange :background ,think-cyberpunk-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,think-cyberpunk-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,think-cyberpunk-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,think-cyberpunk-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,think-cyberpunk-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,think-cyberpunk-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,think-cyberpunk-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,think-cyberpunk-blue))))
   `(gnus-summary-low-read ((t (:foreground ,think-cyberpunk-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,think-cyberpunk-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,think-cyberpunk-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,think-cyberpunk-blue+1))))
   `(gnus-summary-normal-read ((,class (:foreground ,think-cyberpunk-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,think-cyberpunk-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,think-cyberpunk-fg))))
   `(gnus-summary-selected ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,think-cyberpunk-yellow-2))))
   `(gnus-cite-10 ((,class (:foreground ,think-cyberpunk-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,think-cyberpunk-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,think-cyberpunk-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,think-cyberpunk-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,think-cyberpunk-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,think-cyberpunk-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,think-cyberpunk-green))))
   `(gnus-cite-7 ((,class (:foreground ,think-cyberpunk-red))))
   `(gnus-cite-8 ((,class (:foreground ,think-cyberpunk-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,think-cyberpunk-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,think-cyberpunk-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,think-cyberpunk-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,think-cyberpunk-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,think-cyberpunk-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,think-cyberpunk-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,think-cyberpunk-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,think-cyberpunk-bg+2))))
   `(gnus-signature ((,class (:foreground ,think-cyberpunk-yellow))))
   `(gnus-x ((,class (:background ,think-cyberpunk-fg :foreground ,think-cyberpunk-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,think-cyberpunk-green
                           :background ,think-cyberpunk-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,think-cyberpunk-yellow
                           :background ,think-cyberpunk-bg-1
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,think-cyberpunk-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,think-cyberpunk-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,think-cyberpunk-bg :background ,think-cyberpunk-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,think-cyberpunk-green+4 :background ,think-cyberpunk-bg-1))))

   ;; hl-line-mode
   `(hl-sexp-face ((,class (:background ,think-cyberpunk-gray-5))))
   `(hl-line-face ((,class (:background ,think-cyberpunk-gray-5))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,think-cyberpunk-pink-1 :background ,think-cyberpunk-black))))
   `(ido-only-match ((,class (:foreground ,think-cyberpunk-pink-1 :background ,think-cyberpunk-black))))
   `(ido-subdir ((,class (:foreground ,think-cyberpunk-gray-4 :backgroun ,think-cyberpunk-black))))
   `(ido-indicator ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink-1))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,think-cyberpunk-orange))))
   `(js2-error-face ((,class (:foreground ,think-cyberpunk-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,think-cyberpunk-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,think-cyberpunk-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,think-cyberpunk-green+3))))
   `(js2-function-param-face ((,class (:foreground, think-cyberpunk-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,think-cyberpunk-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,think-cyberpunk-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,think-cyberpunk-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,think-cyberpunk-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,think-cyberpunk-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,think-cyberpunk-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,think-cyberpunk-red+1))))
   `(jabber-activity-face((,class (:foreground ,think-cyberpunk-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,think-cyberpunk-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,think-cyberpunk-green+2 :background ,think-cyberpunk-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,think-cyberpunk-pink-1))))
   `(magit-branch ((,class (:foreground ,think-cyberpunk-yellow-5))))
   `(magit-item-highlight ((,class (:background ,think-cyberpunk-gray-8))))
   `(magit-diff-add ((,class (:foreground ,think-cyberpunk-green))))
   `(magit-diff-del ((,class (:foreground ,think-cyberpunk-red))))
   `(magit-diff-hunk-header ((,class (:foreground ,think-cyberpunk-orange))))

   `(eval-sexp-fu-flash ((,class (:background ,think-cyberpunk-gray-8 :foreground ,think-cyberpunk-pink-2))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,think-cyberpunk-blue-5))))
   `(message-header-other ((,class (:foreground ,think-cyberpunk-green))))
   `(message-header-to ((,class (:foreground ,think-cyberpunk-pink-1 :weight bold))))
   `(message-header-from ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,think-cyberpunk-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,think-cyberpunk-green))))
   `(message-mml ((,class (:foreground ,think-cyberpunk-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,think-cyberpunk-orange))))
   `(mew-face-header-from ((,class (:foreground ,think-cyberpunk-yellow))))
   `(mew-face-header-date ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-header-to ((,class (:foreground ,think-cyberpunk-red))))
   `(mew-face-header-key ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-header-private ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-header-important ((,class (:foreground ,think-cyberpunk-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,think-cyberpunk-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,think-cyberpunk-red))))
   `(mew-face-header-xmew ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,think-cyberpunk-red))))
   `(mew-face-body-url ((,class (:foreground ,think-cyberpunk-orange))))
   `(mew-face-body-comment ((,class (:foreground ,think-cyberpunk-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,think-cyberpunk-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,think-cyberpunk-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,think-cyberpunk-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,think-cyberpunk-red))))
   `(mew-face-mark-review ((,class (:foreground ,think-cyberpunk-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-mark-delete ((,class (:foreground ,think-cyberpunk-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,think-cyberpunk-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-mark-unread ((,class (:foreground ,think-cyberpunk-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,think-cyberpunk-green))))
   `(mew-face-eof-part ((,class (:foreground ,think-cyberpunk-yellow))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,think-cyberpunk-cyan :background ,think-cyberpunk-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,think-cyberpunk-bg :background ,think-cyberpunk-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,think-cyberpunk-bg :background ,think-cyberpunk-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,think-cyberpunk-yellow))))
   `(nav-face-button-num ((,class (:foreground ,think-cyberpunk-cyan))))
   `(nav-face-dir ((,class (:foreground ,think-cyberpunk-green))))
   `(nav-face-hdir ((,class (:foreground ,think-cyberpunk-red))))
   `(nav-face-file ((,class (:foreground ,think-cyberpunk-fg))))
   `(nav-face-hfile ((,class (:foreground ,think-cyberpunk-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background ,think-cyberpunk-black))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,think-cyberpunk-black))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,think-cyberpunk-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,think-cyberpunk-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,think-cyberpunk-bg+1))))

   ;; org-mode
   `(org-document-title ((,class (:foreground ,think-cyberpunk-blue-3 :background ,think-cyberpunk-black :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,think-cyberpunk-blue-3 :background ,think-cyberpunk-black :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,think-cyberpunk-gray-2 :background ,think-cyberpunk-black))))
   `(org-agenda-date-today
     ((,class (:foreground ,think-cyberpunk-orange-2 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,think-cyberpunk-gray-2 :foreground ,think-cyberpunk-black
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,think-cyberpunk-blue-7 :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,think-cyberpunk-green
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :foreground ,think-cyberpunk-orange :weight bold
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:foreground ,think-cyberpunk-pink-1 :height 1.3))))
   `(org-level-2 ((,class (:foreground ,think-cyberpunk-yellow :height 1.2))))
   `(org-level-3 ((,class (:foreground ,think-cyberpunk-blue-5 :height 1.1))))
   `(org-level-4 ((,class (:foreground ,think-cyberpunk-green))))
   `(org-level-5 ((,class (:foreground ,think-cyberpunk-orange))))
   `(org-level-6 ((,class (:foreground ,think-cyberpunk-pink))))
   `(org-level-7 ((,class (:foreground ,think-cyberpunk-green+3))))
   `(org-level-8 ((,class (:foreground ,think-cyberpunk-blue-1))))
   `(org-link ((,class (:foreground ,think-cyberpunk-blue-6 :underline t))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-column ((,class (:background ,think-cyberpunk-yellow-3 :foreground ,think-cyberpunk-black))))
   `(org-column-title ((,class (:background ,think-cyberpunk-bg-1 :underline t :weight bold))))
   `(org-block ((,class (:foreground ,think-cyberpunk-fg :background ,think-cyberpunk-bg-05))))
   `(org-block-begin-line 
     ((,class (:foreground "#008ED1" :background ,think-cyberpunk-bg-1))))
   `(org-block-background ((,class (:background ,think-cyberpunk-bg-05))))
   `(org-block-end-line 
     ((,class (:foreground "#008ED1" :background ,think-cyberpunk-bg-1))))

   ;; `(org-deadline-announce ((,class (:foreground ,think-cyberpunk-red-1))))
   ;; `(org-scheduled ((,class (:foreground ,think-cyberpunk-green+4))))
   ;; `(org-scheduled-previously ((,class (:foreground ,think-cyberpunk-red-4))))
   ;; `(org-scheduled-today ((,class (:foreground ,think-cyberpunk-blue+1))))
   ;; `(org-special-keyword ((,class (:foreground ,think-cyberpunk-yellow-1))))
   ;; `(org-table ((,class (:foreground ,think-cyberpunk-green+2))))
   ;; `(org-time-grid ((,class (:foreground ,think-cyberpunk-orange))))
   ;; `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((,class (:bold t :foreground ,think-cyberpunk-red :weight bold :underline nil))))
   ;; `(org-formula ((,class (:foreground ,think-cyberpunk-yellow-2))))
   ;; `(org-headline-done ((,class (:foreground ,think-cyberpunk-green+3))))
   ;; `(org-hide ((,class (:foreground ,think-cyberpunk-bg-1))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,think-cyberpunk-red-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,think-cyberpunk-green-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,think-cyberpunk-pink-1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,think-cyberpunk-green))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,think-cyberpunk-blue-3))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,think-cyberpunk-orange))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,think-cyberpunk-blue-2))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,think-cyberpunk-gray))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,think-cyberpunk-white))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,think-cyberpunk-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,think-cyberpunk-red-4))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,think-cyberpunk-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,think-cyberpunk-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,think-cyberpunk-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,think-cyberpunk-red))))
   `(rpm-spec-package-face ((,class (:foreground ,think-cyberpunk-red))))
   `(rpm-spec-section-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,think-cyberpunk-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,think-cyberpunk-orange))))
   `(rst-level-2-face ((,class (:foreground ,think-cyberpunk-green+1))))
   `(rst-level-3-face ((,class (:foreground ,think-cyberpunk-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,think-cyberpunk-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,think-cyberpunk-cyan))))
   `(rst-level-6-face ((,class (:foreground ,think-cyberpunk-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,think-cyberpunk-red-3 :background ,think-cyberpunk-black))))
   `(show-paren-match ((,class (:foreground ,think-cyberpunk-black :background ,think-cyberpunk-pink-1))))

   `(naeu-green-face ((,class (:foreground ,think-cyberpunk-green :background ,think-cyberpunk-black))))
   `(naeu-pink-face ((,class (:foreground ,think-cyberpunk-pink-1 :background ,think-cyberpunk-black))))
   `(naeu-blue-face ((,class (:foreground ,think-cyberpunk-blue-1 :background ,think-cyberpunk-black))))
   `(naeu-orange-face ((,class (:foreground ,think-cyberpunk-yellow-1 :background ,think-cyberpunk-black))))
   `(naeu-red-face ((,class (:foreground ,think-cyberpunk-orange :background ,think-cyberpunk-black))))
   `(naeu-grey-face ((,class (:foreground ,think-cyberpunk-gray-7 :background ,think-cyberpunk-black))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,think-cyberpunk-red))))

  ;;; ansi-term
   `(term-color-black ((,class (:foreground ,think-cyberpunk-bg
                                            :background ,think-cyberpunk-bg-1))))
   `(term-color-red ((,class (:foreground ,think-cyberpunk-red-2
                                          :background ,think-cyberpunk-red-4))))
   `(term-color-green ((,class (:foreground ,think-cyberpunk-green
                                            :background ,think-cyberpunk-green+2))))
   `(term-color-yellow ((,class (:foreground ,think-cyberpunk-orange
                                             :background ,think-cyberpunk-yellow))))
   `(term-color-blue ((,class (:foreground ,think-cyberpunk-blue-1
                                           :background ,think-cyberpunk-blue-4))))
   `(term-color-magenta ((,class (:foreground ,think-cyberpunk-magenta
                                              :background ,think-cyberpunk-red))))
   `(term-color-cyan ((,class (:foreground ,think-cyberpunk-cyan
                                           :background ,think-cyberpunk-blue))))
   `(term-color-white ((,class (:foreground ,think-cyberpunk-fg
                                            :background ,think-cyberpunk-bg-1))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,think-cyberpunk-gray-5))))

   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,think-cyberpunk-pink-1 :background ,think-cyberpunk-black))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,think-cyberpunk-bg :foreground ,think-cyberpunk-bg+1))))
   `(whitespace-hspace ((,class (:background ,think-cyberpunk-bg :foreground ,think-cyberpunk-bg+1))))
   `(whitespace-tab ((,class (:background ,think-cyberpunk-bg :foreground ,think-cyberpunk-red))))
   `(whitespace-newline ((,class (:foreground ,think-cyberpunk-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,think-cyberpunk-red :background ,think-cyberpunk-bg))))
   `(whitespace-line ((,class (:background ,think-cyberpunk-bg-05 :foreground ,think-cyberpunk-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,think-cyberpunk-orange :foreground ,think-cyberpunk-orange))))
   `(whitespace-indentation ((,class (:background ,think-cyberpunk-yellow :foreground ,think-cyberpunk-red))))
   `(whitespace-empty ((,class (:background ,think-cyberpunk-yellow :foreground ,think-cyberpunk-red))))
   `(whitespace-space-after-tab ((,class (:background ,think-cyberpunk-yellow :foreground ,think-cyberpunk-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,think-cyberpunk-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,think-cyberpunk-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,think-cyberpunk-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,think-cyberpunk-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,think-cyberpunk-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,think-cyberpunk-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,think-cyberpunk-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,think-cyberpunk-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,think-cyberpunk-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,think-cyberpunk-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,think-cyberpunk-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,think-cyberpunk-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,think-cyberpunk-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,think-cyberpunk-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,think-cyberpunk-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,think-cyberpunk-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,think-cyberpunk-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,think-cyberpunk-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,think-cyberpunk-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,think-cyberpunk-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,think-cyberpunk-green+4))))

   ;; yasnippet
   `(yas/field-highlight-face ((,class (:background ,think-cyberpunk-pink-1 :foreground ,think-cyberpunk-black))))

   ;; enh-ruby-mode enh-ruby-op-face
   `(enh-ruby-op-face ((,class (:foreground ,think-cyberpunk-blue-7))))
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,think-cyberpunk-green+2))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,think-cyberpunk-green+2))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,think-cyberpunk-blue-1))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,think-cyberpunk-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,think-cyberpunk-bg-1 :foreground ,think-cyberpunk-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'think-cyberpunk
   `(ansi-color-names-vector [,think-cyberpunk-bg ,think-cyberpunk-red-2 ,think-cyberpunk-green ,think-cyberpunk-orange
                                          ,think-cyberpunk-blue-1 ,think-cyberpunk-magenta ,think-cyberpunk-cyan ,think-cyberpunk-fg])
   ;; fill-column-indicator
   `(fci-rule-color ,think-cyberpunk-bg-05)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'think-cyberpunk)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; think-cyberpunk-theme.el ends here.
