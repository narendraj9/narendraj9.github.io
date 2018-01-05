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
(require 'ox-rss)

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
    <!--<li><a href=\"/archive.xml\"> RSS </a> </li>--!>
    <li><a href=\"/archive.html\"> Posts </a> </li>
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
  <script type=\"text/javascript\" src=\"/assets/js/custom.js\"> </script>
  <script type=\"text/javascript\" src=\"/assets/js/turbolinks.js\"> </script>
")

(defun org-blog-sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (when (s-starts-with-p "posts/" entry)
    (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]] @@html:</span>@@"
            (format-time-string "%h %m, %Y"
                                (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(defun org-blog-sitemap-function (title list)
  "Return sitemap using TITLE and LIST returned by `org-blog-sitemap-format-entry'."
  (concat "#+TITLE: " title "\n\n"
          "\n#+begin_archive\n"
          (mapconcat (lambda (li)
                       (format "@@html:<li>@@ %s @@html:</li>@@" (car li)))
                     (seq-filter #'car (cdr list))
                     "\n")
          "\n#+end_archive\n"))

(defun org-blog-publish-to-html (plist filename pub-dir)
  "Same as `org-html-publish-to-html' but modifies html before finishing."
  (let ((file-path (org-html-publish-to-html plist filename pub-dir)))
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-min))
      (search-forward "<body>")
      (insert (concat "\n<div class=\"content-wrapper container\">\n "
                      "  <div class=\"row\"> <div class=\"col\"> </div> "
                      "  <div class=\"col-sm-6 col-md-8\"> "))
      (goto-char (point-max))
      (search-backward "</body>")
      (insert "\n</div>\n<div class=\"col\"></div></div>\n</div>\n")
      (save-buffer)
      (kill-buffer))
    file-path))


(setq org-publish-project-alist
      `(("orgfiles"
         :base-directory "~/blog/src/"
         :exclude ".*drafts/.*"
         :base-extension "org"

         :publishing-directory "~/blog/"

         :recursive t
         :preparation-function org-blog-prepare
         :publishing-function org-blog-publish-to-html

         :with-toc nil
         :with-title t
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-default-style nil
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
         :sitemap-function org-blog-sitemap-function)

        ("assets"
         :base-directory "~/blog/src/assets/"
         :base-extension ".*"
         :publishing-directory "~/blog/assets/"
         :publishing-function org-publish-attachment
         :recursive t)

        ("rss"
         :base-directory "~/blog/src/"
         :base-extension "org"
         :html-link-home "https://vicarie.in/"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :publishing-directory "~/blog/"
         :publishing-function (org-rss-publish-to-rss)
         :exclude ".*"
         :include ("archive.org")
         :section-numbers nil
         :table-of-contents nil)

        ("blog" :components ("orgfiles" "assets" "rss"))))

(provide 'org-blog)
;;; org-blog.el ends here
