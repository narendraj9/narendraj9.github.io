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

(require 'ox-publish)
(require 'ox-html)

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
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/css/bootstrap.css\"/>
   <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/css/custom.css\"/>
   <link rel=\"stylesheet\" type=\"text/css\" href=\"https://fonts.googleapis.com/css?family=Libre+Baskerville|Bree+Serif|Ubuntu+Mono|Pacifico&subset=latin,greek\"/>")

(defun org-blog-postamble (_plist)
  "Post-amble for whole blog."
  "<footer class=\"footer\">
	<p> Built with
		<svg id=\"i-heart\">
    		<path d=\"M4 16 C1 12 2 6 7 4 12 2 15 6 16 8 17 6 21 2 26 4 31 6 31 12 28 16 25 20 16 28 16 28 16 28 7 20 4 16 Z\"/>
	    </svg> and <code>org-publish</code>
	</p>
  </footer>
  <script type=\"text/javascript\" src=\"assets/js/custom.js\"> </script> ")

(setq org-publish-project-alist
      `(("orgfiles"
         :base-directory "~/blog/src/"
         :base-extension "org"
         :publishing-directory "~/blog/"
         :recursive t
         :preparation-function org-blog-prepare
         :publishing-function org-html-publish-to-html

         :with-toc nil
         :with-title t
         :section-numbers nil
         :auto-sitemap t
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-default-style t
         :html-head-include-scripts nil
         :htmlized-source t
         :html-head-extra ,org-blog-head
         :html-postamble org-blog-postamble

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
