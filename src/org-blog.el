;;; org-blog.el --- Blog with ox-publish          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ;;; Commentary:

;; Org project configuration my personal blog.

  ;;; Code:

(require 'org)
(require 'ox-publish)
(require 'ox-html)
(require 'org-element)

(defun org-blog-prepare (project-plist)
  "With help from `https://github.com/howardabrams/dot-files'.
  Touch `index.org' to rebuilt it.
  Argument `PROJECT-PLIST' contains information about the current project."
  (let* ((base-directory (plist-get project-plist :base-directory))
         (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
    (with-current-buffer buffer
      (set-buffer-modified-p t)
      (save-buffer 0))
    (kill-buffer buffer)))

(defvar org-blog-head
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/bootstrap.css\"/>
  <link rel=\"stylesheet\" type=\"text/css\" href=\"https://fonts.googleapis.com/css?family=Amaranth|Libre+Baskerville|Bree+Serif|Ubuntu+Mono|Pacifico&subset=latin,greek\"/>")

(defun org-blog-preamble (_plist)
  "Pre-amble for whole blog."
  "<div class=\"banner\">
    <a href=\"/\"> Ramblings from a Corner </a>
  </div>
  <ul class=\"banner-links\">
    <li><a href=\"archive.html\"> Posts </a> </li>
  </ul>
  <hr>")

(defun org-blog-postamble (_plist)
  "Post-amble for whole blog."
  "<footer class=\"footer\">
	<p> Built with
	  <svg id=\"i-heart\" viewBox=\"0 0 32 32\">
    	<path d=\"M4 16 C1 12 2 6 7 4 12 2 15 6 16 8 17 6 21 2 26 4 31 6 31 12 28 16 25 20 16 28 16 28 16 28 7 20 4 16 Z\"/>
	  </svg> in
      <img id=\"i-emacs\" src=\"/assets/images/emacs.svg\">
	</p>
  </footer>
  <script type=\"text/javascript\" src=\"/assets/js/custom.js\"> </script> ")

(defun org-blog-sitemap-format-entry (entry _style project)
  "Format for site map ENTRY, as a string.
  ENTRY is a file name.  STYLE is the style of the sitemap.
  PROJECT is the current project."
  (if (s-starts-with-p "posts/" entry)
      (format "@@html:<span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]]"
              (format-time-string "%h %m, %Y"
                                  (org-publish-find-date entry project))
              entry
              (org-publish-find-title entry project))
    ""))

(defun org-blog-sitemap-function (title list)
  "Return sitemap as a string.
  TITLE is the the title of the site map.  LIST is an internal
  representation for the files to include, as returned by
  `org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
          "\n#+begin_archive\n"
          (mapconcat (lambda (li)
                       (format "@@html:<li>@@ %s @@html:</li>@@"
                               (car li)))
                     (cdr list)
                     "\n")
          "\n#+end_archive\n"))

(setq org-publish-project-alist
      `(("orgfiles"
         :base-directory "~/blog/src/"
         :exclude ".*drafts/.*"
         :base-extension "org"

         :publishing-directory "~/blog/"

         :recursive t
         :preparation-function org-blog-prepare
         :publishing-function org-html-publish-to-html

         :with-toc nil
         :with-title t
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-default-style t
         :html-head-include-scripts nil
         :htmlized-source t
         :html-head-extra ,org-blog-head
         :html-preamble org-blog-preamble
         :html-postamble org-blog-postamble

         :auto-sitemap t
         :sitemap-filename "archive.org"
         :sitemap-title "Blog Posts"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry org-blog-sitemap-format-entry
         :sitemap-function org-blog-sitemap-function

         :html-link-home "/"
         :html-link-up "/")

        ("assets"
         :base-directory "~/blog/src/assets/"
         :base-extension ".*"
         :publishing-directory "~/blog/assets/"
         :publishing-function org-publish-attachment
         :recursive t)

        ("blog" :components ("orgfiles" "assets"))))

(provide 'org-blog)
;;; org-blog.el ends here
