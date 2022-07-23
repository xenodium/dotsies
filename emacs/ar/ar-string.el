;;; ar-string.el --- String support.

;;; Commentary:
;; String helpers.


;;; Code:

(defun ar/string-numeric-p (string)
  "Return t if STRING is an unsigned integer.  nil otherwise."
  (string-match "\\`[[:digit:]]+\\'" string))

(defun ar/string-replace-regex-pairs (haystack &rest pairs)
  "Replace in HAYSTACK all PAIRS. For example: (ar/string-replace \"one two\" '(\"one\" . \"three\") '(\"two\" . \"four\"))"
  (mapc (lambda (pair)
          (assert (consp pair) nil "Each pair must be a cons cell")
          (setq haystack (replace-regexp-in-string (car pair)
                                                   (cdr pair)
                                                   haystack)))
        pairs)
  haystack)

(defun ar/string-decode-html-entities (html)
  (assert html nil "Must have HTML")
  (let ((result html)
        (entities [
                   ["&nbsp;" " "] ["&ensp;" " "] ["&emsp;" " "] ["&thinsp;" " "]
                   ["&rlm;" "‏"] ["&lrm;" "‎"] ["&zwj;" "‍"] ["&zwnj;" "‌"]
                   ["&iexcl;" "¡"] ["&cent;" "¢"] ["&pound;" "£"] ["&curren;" "¤"]
                   ["&yen;" "¥"] ["&brvbar;" "¦"] ["&sect;" "§"] ["&uml;" "¨"]
                   ["&copy;" "©"] ["&ordf;" "ª"] ["&laquo;" "«"] ["&not;" "¬"]
                   ["&shy;" "­"] ["&reg;" "®"] ["&macr;" "¯"] ["&deg;" "°"]
                   ["&plusmn;" "±"] ["&sup2;" "²"] ["&sup3;" "³"] ["&acute;" "´"]
                   ["&micro;" "µ"] ["&para;" "¶"] ["&middot;" "·"] ["&cedil;" "¸"]
                   ["&sup1;" "¹"] ["&ordm;" "º"] ["&raquo;" "»"] ["&frac14;" "¼"]
                   ["&frac12;" "½"] ["&frac34;" "¾"] ["&iquest;" "¿"]["&Agrave;" "À"]
                   ["&Aacute;" "Á"] ["&Acirc;" "Â"] ["&Atilde;" "Ã"] ["&Auml;" "Ä"]
                   ["&Aring;" "Å"] ["&AElig;" "Æ"] ["&Ccedil;" "Ç"] ["&Egrave;" "È"]
                   ["&Eacute;" "É"] ["&Ecirc;" "Ê"] ["&Euml;" "Ë"] ["&Igrave;" "Ì"]
                   ["&Iacute;" "Í"] ["&Icirc;" "Î"] ["&Iuml;" "Ï"] ["&ETH;" "Ð"]
                   ["&Ntilde;" "Ñ"] ["&Ograve;" "Ò"] ["&Oacute;" "Ó"] ["&Ocirc;" "Ô"]
                   ["&Otilde;" "Õ"] ["&Ouml;" "Ö"] ["&times;" "×"] ["&Oslash;" "Ø"]
                   ["&Ugrave;" "Ù"] ["&Uacute;" "Ú"] ["&Ucirc;" "Û"] ["&Uuml;" "Ü"]
                   ["&Yacute;" "Ý"] ["&THORN;" "Þ"] ["&szlig;" "ß"] ["&agrave;" "à"]
                   ["&aacute;" "á"] ["&acirc;" "â"] ["&atilde;" "ã"] ["&auml;" "ä"]
                   ["&aring;" "å"] ["&aelig;" "æ"] ["&ccedil;" "ç"] ["&egrave;" "è"]
                   ["&eacute;" "é"] ["&ecirc;" "ê"] ["&euml;" "ë"] ["&igrave;" "ì"]
                   ["&iacute;" "í"] ["&icirc;" "î"] ["&iuml;" "ï"] ["&eth;" "ð"]
                   ["&ntilde;" "ñ"] ["&ograve;" "ò"] ["&oacute;" "ó"] ["&ocirc;" "ô"]
                   ["&otilde;" "õ"] ["&ouml;" "ö"]["&divide;" "÷"] ["&oslash;" "ø"]
                   ["&ugrave;" "ù"] ["&uacute;" "ú"] ["&ucirc;" "û"] ["&uuml;" "ü"]
                   ["&yacute;" "ý"] ["&thorn;" "þ"] ["&yuml;" "ÿ"] ["&fnof;" "ƒ"]
                   ["&Alpha;" "Α"] ["&Beta;" "Β"] ["&Gamma;" "Γ"] ["&Delta;" "Δ"]
                   ["&Epsilon;" "Ε"] ["&Zeta;" "Ζ"] ["&Eta;" "Η"] ["&Theta;" "Θ"]
                   ["&Iota;" "Ι"] ["&Kappa;" "Κ"] ["&Lambda;" "Λ"] ["&Mu;" "Μ"]
                   ["&Nu;" "Ν"] ["&Xi;" "Ξ"] ["&Omicron;" "Ο"] ["&Pi;" "Π"]
                   ["&Rho;" "Ρ"] ["&Sigma;" "Σ"] ["&Tau;" "Τ"] ["&Upsilon;" "Υ"]
                   ["&Phi;" "Φ"] ["&Chi;" "Χ"] ["&Psi;" "Ψ"] ["&Omega;" "Ω"]
                   ["&alpha;" "α"] ["&beta;" "β"] ["&gamma;" "γ"] ["&delta;" "δ"]
                   ["&epsilon;" "ε"] ["&zeta;" "ζ"] ["&eta;" "η"] ["&theta;" "θ"]
                   ["&iota;" "ι"] ["&kappa;" "κ"] ["&lambda;" "λ"] ["&mu;" "μ"]
                   ["&nu;" "ν"] ["&xi;" "ξ"] ["&omicron;" "ο"] ["&pi;" "π"]
                   ["&rho;" "ρ"] ["&sigmaf;" "ς"] ["&sigma;" "σ"] ["&tau;" "τ"]
                   ["&upsilon;" "υ"] ["&phi;" "φ"] ["&chi;" "χ"] ["&psi;" "ψ"]
                   ["&omega;" "ω"] ["&thetasym;" "ϑ"] ["&upsih;" "ϒ"] ["&piv;" "ϖ"]
                   ["&bull;" "•"] ["&hellip;" "…"] ["&prime;" "′"] ["&Prime;" "″"]
                   ["&oline;" "‾"] ["&frasl;" "⁄"] ["&weierp;" "℘"] ["&image;" "ℑ"]
                   ["&real;" "ℜ"] ["&trade;" "™"] ["&alefsym;" "ℵ"] ["&larr;" "←"]
                   ["&uarr;" "↑"] ["&rarr;" "→"] ["&darr;" "↓"] ["&harr;" "↔"]
                   ["&crarr;" "↵"] ["&lArr;" "⇐"] ["&uArr;" "⇑"] ["&rArr;" "⇒"]
                   ["&dArr;" "⇓"] ["&hArr;" "⇔"] ["&forall;" "∀"] ["&part;" "∂"]
                   ["&exist;" "∃"] ["&empty;" "∅"] ["&nabla;" "∇"] ["&isin;" "∈"]
                   ["&notin;" "∉"] ["&ni;" "∋"] ["&prod;" "∏"] ["&sum;" "∑"]
                   ["&minus;" "−"] ["&lowast;" "∗"] ["&radic;" "√"] ["&prop;" "∝"]
                   ["&infin;" "∞"] ["&ang;" "∠"] ["&and;" "∧"] ["&or;" "∨"]
                   ["&cap;" "∩"] ["&cup;" "∪"] ["&int;" "∫"] ["&there4;" "∴"]
                   ["&sim;" "∼"] ["&cong;" "≅"] ["&asymp;" "≈"] ["&ne;" "≠"]
                   ["&equiv;" "≡"] ["&le;" "≤"] ["&ge;" "≥"] ["&sub;" "⊂"]
                   ["&sup;" "⊃"] ["&nsub;" "⊄"] ["&sube;" "⊆"] ["&supe;" "⊇"]
                   ["&oplus;" "⊕"] ["&otimes;" "⊗"] ["&perp;" "⊥"] ["&sdot;" "⋅"]
                   ["&lceil;" "⌈"] ["&rceil;" "⌉"] ["&lfloor;" "⌊"] ["&rfloor;" "⌋"]
                   ["&lang;" "〈"] ["&rang;" "〉"] ["&loz;" "◊"] ["&spades;" "♠"]
                   ["&clubs;" "♣"] ["&hearts;" "♥"] ["&diams;" "♦"] ["&quot;" "\""]
                   ["&OElig;" "Œ"] ["&oelig;" "œ"] ["&Scaron;" "Š"] ["&scaron;" "š"]
                   ["&Yuml;" "Ÿ"] ["&circ;" "ˆ"] ["&tilde;" "˜"] ["&ndash;" "–"]
                   ["&mdash;" "—"] ["&lsquo;" "‘"] ["&rsquo;" "’"] ["&sbquo;" "‚"]
                   ["&ldquo;" "“"] ["&rdquo;" "”"] ["&bdquo;" "„"] ["&dagger;" "†"]
                   ["&Dagger;" "‡"] ["&permil;" "‰"] ["&lsaquo;" "‹"] ["&rsaquo;" "›"]
                   ["&euro;" "€"] ["&amp;" "&"] ["&#39;" "'"] ["&#8211;" "–"] ["&#x27;" "'"]
                   ]))
    (mapc (lambda (entity)
            (setq result (s-replace (elt entity 0)
                                  (elt entity 1)
                                  result)))
          entities)
    result))

(provide 'ar-string)

;;; ar-string.el ends here
