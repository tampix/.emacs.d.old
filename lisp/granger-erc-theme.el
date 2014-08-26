(deftheme granger-erc "Granger theme for ERC.")

(custom-theme-set-faces
 'granger-erc

 '(erc-default-face ((t (default))))
 '(erc-action-face ((t (:inherit default))))
 '(erc-input-face ((t (:inherit default))))
 '(erc-notice-face ((t (:inherit font-lock-comment-face))))
 '(erc-nick-default-face ((t (:inherit font-lock-function-name-face))))
 '(erc-nick-msg-face ((t (:inherit font-lock-function-name-face))))
 '(erc-my-nick-face ((t (:inherit font-lock-function-name-face :bold t))))
 '(erc-current-nick-face ((t (:inherit font-lock-function-name-face :bold t))))
 '(erc-timestamp-face ((t (:inherit font-lock-constant-face))))
 '(erc-prompt-face ((t (:inherit font-lock-constant-face))))
 '(erc-direct-msg-face ((t (:inherit font-lock-builtin-face))))
 '(erc-error-face ((t (:inherit font-lock-warning-face)))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'granger-erc)
