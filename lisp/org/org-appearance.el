;;; org-appearance.el --- org-mode configuration -*- lexical-binding: t; -*-

;; Cool org mode
(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  :config
  (setq org-modern-todo nil
        org-modern-hide-stars nil
        org-modern-horizontal-rule nil
        org-modern-keyword "‣ "
        org-modern-star nil
        org-modern-block-fringe nil
        org-modern-table nil)
  )

;;Cool margin annotations
;; (use-package org-margin
;;   :ensure t
;;   :hook (org-mode . org-margin-mode)
;;   :vc (:url "https://github.com/rougier/org-margin")
;;   :requires svg-lib)

(use-package org-appear
  :ensure t
  :vc (:url "https://github.com/awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autolinks t)
  (setq org-appear-autokeywords t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-trigger 'always))


;; Modify org-pretty entity superscript
(defun my/org-raise-scripts-no-braces (_)
  (when (and (eq (char-after (match-beginning 3)) ?{)
	         (eq (char-before (match-end 3)) ?}))
    (remove-text-properties (match-beginning 3) (1+ (match-beginning 3))
		                    (list 'invisible nil))
    (remove-text-properties (1- (match-end 3)) (match-end 3)
		                    (list 'invisible nil))))

(advice-add 'org-raise-scripts :after #'my/org-raise-scripts-no-braces)

(defun vz/org-prettify--predicate (_start end _match)
  ;; There's no need the check the character before the entity match
  ;; since all of them start with \. The characters that are
  ;; acceptable after the match are mathematical operators and some
  ;; special characters.
  (seq-contains-p '(?\C-j ?} ?{ ?\\ ?_ ?- ?+ ?^ ?\( ?\) ?$ ?  ?/ ?| ?. ?, ?\;)
                  (char-after end)))

(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq-local prettify-symbols-alist
              (cl-copy-list my-org-prettify-symbols-alist))
  (setq-local prettify-symbols-compose-predicate #'vz/org-prettify--predicate)

  (prettify-symbols-mode))

(add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

(defvar my-org-prettify-symbols-alist
  '( ;; Lowercase Greek letters.
    ("\\\\" . ?↩)
    ("\\alpha" . ?α)
    ("\\beta" . ?β)
    ("\\gamma" . ?γ)
    ("\\delta" . ?δ)
    ("\\epsilon" . ?ϵ)
    ("\\zeta" . ?ζ)
    ("\\eta" . ?η)
    ("\\theta" . ?θ)
    ("\\iota" . ?ι)
    ("\\kappa" . ?κ)
    ("\\lambda" . ?λ)
    ("\\mu" . ?μ)
    ("\\nu" . ?ν)
    ("\\xi" . ?ξ)
    ;; There is no \omicron because it looks like a latin o.
    ("\\pi" . ?π)
    ("\\rho" . ?ρ)
    ("\\sigma" . ?σ)
    ("\\tau" . ?τ)
    ("\\upsilon" . ?υ)
    ("\\phi" . ?ϕ)
    ("\\chi" . ?χ)
    ("\\psi" . ?ψ)
    ("\\omega" . ?ω)
    ;; Uppercase Greek letters.
    ("\\Gamma" . ?Γ)
    ("\\Delta" . ?Δ)
    ("\\Lambda" . ?Λ)
    ("\\Phi" . ?Φ)
    ("\\Pi" . ?Π)
    ("\\Psi" . ?Ψ)
    ("\\Sigma" . ?Σ)
    ("\\Theta" . ?Θ)
    ("\\Upsilon" . ?Υ)
    ("\\Xi" . ?Ξ)
    ("\\Omega" . ?Ω)

    ;; Other math symbols (taken from leim/quail/latin-ltx.el).
    ("\\Box" . ?□)
    ("\\Bumpeq" . ?≎)
    ("\\Cap" . ?⋒)
    ("\\Cup" . ?⋓)
    ("\\Diamond" . ?◇)
    ("\\Downarrow" . ?⇓)
    ("\\H{o}" . ?ő)
    ("\\Im" . ?ℑ)
    ("\\Join" . ?⋈)
    ("\\Leftarrow" . ?⇐)
    ("\\Leftrightarrow" . ?⇔)
    ("\\Ll" . ?⋘)
    ("\\Lleftarrow" . ?⇚)
    ("\\Longleftarrow" . ?⇐)
    ("\\Longleftrightarrow" . ?⇔)
    ("\\Longrightarrow" . ?⇒)
    ("\\Lsh" . ?↰)
    ("\\Re" . ?ℜ)
    ("\\Rightarrow" . ?⇒)
    ("\\Rrightarrow" . ?⇛)
    ("\\Rsh" . ?↱)
    ("\\Subset" . ?⋐)
    ("\\Supset" . ?⋑)
    ("\\Uparrow" . ?⇑)
    ("\\Updownarrow" . ?⇕)
    ("\\Vdash" . ?⊩)
    ("\\Vert" . ?‖)
    ("\\Vvdash" . ?⊪)
    ("\\aleph" . ?ℵ)
    ("\\amalg" . ?∐)
    ("\\angle" . ?∠)
    ("\\approx" . ?≈)
    ("\\approxeq" . ?≊)
    ("\\ast" . ?∗)
    ("\\asymp" . ?≍)
    ("\\backcong" . ?≌)
    ("\\backepsilon" . ?∍)
    ("\\backprime" . ?‵)
    ("\\backsim" . ?∽)
    ("\\backsimeq" . ?⋍)
    ("\\backslash" . ?\\)
    ("\\barwedge" . ?⊼)
    ("\\because" . ?∵)
    ("\\beth" . ?ℶ)
    ("\\between" . ?≬)
    ("\\bigcap" . ?⋂)
    ("\\bigcirc" . ?◯)
    ("\\bigcup" . ?⋃)
    ("\\bigstar" . ?★)
    ("\\bigtriangledown" . ?▽)
    ("\\bigtriangleup" . ?△)
    ("\\bigvee" . ?⋁)
    ("\\bigwedge" . ?⋀)
    ("\\blacklozenge" . ?✦)
    ("\\blacksquare" . ?▪)
    ("\\blacktriangle" . ?▴)
    ("\\blacktriangledown" . ?▾)
    ("\\blacktriangleleft" . ?◂)
    ("\\blacktriangleright" . ?▸)
    ("\\bot" . ?⊥)
    ("\\bowtie" . ?⋈)
    ("\\boxminus" . ?⊟)
    ("\\boxplus" . ?⊞)
    ("\\boxtimes" . ?⊠)
    ("\\bullet" . ?•)
    ("\\bumpeq" . ?≏)
    ("\\cap" . ?∩)
    ("\\cdots" . ?⋯)
    ("\\centerdot" . ?·)
    ("\\checkmark" . ?✓)
    ("\\chi" . ?χ)
    ("\\cdot" . ?⋅)
    ("\\cdots" . ?⋯)
    ("\\circ" . ?∘)
    ("\\circeq" . ?≗)
    ("\\circlearrowleft" . ?↺)
    ("\\circlearrowright" . ?↻)
    ("\\circledR" . ?®)
    ("\\circledS" . ?Ⓢ)
    ("\\circledast" . ?⊛)
    ("\\circledcirc" . ?⊚)
    ("\\circleddash" . ?⊝)
    ("\\clubsuit" . ?♣)
    ("\\coloneq" . ?≔)
    ("\\complement" . ?∁)
    ("\\cong" . ?≅)
    ("\\coprod" . ?∐)
    ("\\cup" . ?∪)
    ("\\curlyeqprec" . ?⋞)
    ("\\curlyeqsucc" . ?⋟)
    ("\\curlypreceq" . ?≼)
    ("\\curlyvee" . ?⋎)
    ("\\curlywedge" . ?⋏)
    ("\\curvearrowleft" . ?↶)
    ("\\curvearrowright" . ?↷)
    ("\\dag" . ?†)
    ("\\dagger" . ?†)
    ("\\daleth" . ?ℸ)
    ("\\dashv" . ?⊣)
    ("\\ddag" . ?‡)
    ("\\ddagger" . ?‡)
    ("\\ddots" . ?⋱)
    ("\\diamond" . ?⋄)
    ("\\diamondsuit" . ?♢)
    ("\\divideontimes" . ?⋇)
    ("\\doteq" . ?≐)
    ("\\doteqdot" . ?≑)
    ("\\dotplus" . ?∔)
    ("\\dotsquare" . ?⊡)
    ("\\downarrow" . ?↓)
    ("\\downdownarrows" . ?⇊)
    ("\\downleftharpoon" . ?⇃)
    ("\\downrightharpoon" . ?⇂)
    ("\\ell" . ?ℓ)
    ("\\emptyset" . ?∅)
    ("\\eqcirc" . ?≖)
    ("\\eqcolon" . ?≕)
    ("\\eqslantgtr" . ?⋝)
    ("\\eqslantless" . ?⋜)
    ("\\equiv" . ?≡)
    ("\\exists" . ?∃)
    ("\\fallingdotseq" . ?≒)
    ("\\flat" . ?♭)
    ("\\forall" . ?∀)
    ("\\frown" . ?⌢)
    ("\\ge" . ?≥)
    ("\\geq" . ?≥)
    ("\\geqq" . ?≧)
    ("\\geqslant" . ?≥)
    ("\\gets" . ?←)
    ("\\gg" . ?≫)
    ("\\ggg" . ?⋙)
    ("\\gimel" . ?ℷ)
    ("\\gnapprox" . ?⋧)
    ("\\gneq" . ?≩)
    ("\\gneqq" . ?≩)
    ("\\gnsim" . ?⋧)
    ("\\gtrapprox" . ?≳)
    ("\\gtrdot" . ?⋗)
    ("\\gtreqless" . ?⋛)
    ("\\gtreqqless" . ?⋛)
    ("\\gtrless" . ?≷)
    ("\\gtrsim" . ?≳)
    ("\\gvertneqq" . ?≩)
    ("\\hbar" . ?ℏ)
    ("\\heartsuit" . ?♥)
    ("\\hookleftarrow" . ?↩)
    ("\\hookrightarrow" . ?↪)
    ("\\iff" . ?⇔)
    ("\\imath" . ?ı)
    ("\\in" . ?∈)
    ("\\infty" . ?∞)
    ("\\int" . ?∫)
    ("\\intercal" . ?⊺)
    ("\\langle" . 10216)          ; Literal ?⟨ breaks indentation.
    ("\\lbrace" . ?{)
    ("\\lbrack" . ?\[)
    ("\\lceil" . ?⌈)
    ("\\ldots" . ?…)
    ("\\le" . ?≤)
    ("\\leadsto" . ?↝)
    ("\\leftarrow" . ?←)
    ("\\leftarrowtail" . ?↢)
    ("\\leftharpoondown" . ?↽)
    ("\\leftharpoonup" . ?↼)
    ("\\leftleftarrows" . ?⇇)
    ;; ("\\leftparengtr" ?〈), see bug#12948.
    ("\\leftrightarrow" . ?↔)
    ("\\leftrightarrows" . ?⇆)
    ("\\leftrightharpoons" . ?⇋)
    ("\\leftrightsquigarrow" . ?↭)
    ("\\leftthreetimes" . ?⋋)
    ("\\leq" . ?≤)
    ("\\leqq" . ?≦)
    ("\\leqslant" . ?≤)
    ("\\lessapprox" . ?≲)
    ("\\lessdot" . ?⋖)
    ("\\lesseqgtr" . ?⋚)
    ("\\lesseqqgtr" . ?⋚)
    ("\\lessgtr" . ?≶)
    ("\\lesssim" . ?≲)
    ("\\lfloor" . ?⌊)
    ("\\lhd" . ?◁)
    ("\\rhd" . ?▷)
    ("\\ll" . ?≪)
    ("\\llcorner" . ?⌞)
    ("\\lnapprox" . ?⋦)
    ("\\lneq" . ?≨)
    ("\\lneqq" . ?≨)
    ("\\lnsim" . ?⋦)
    ("\\longleftarrow" . ?←)
    ("\\longleftrightarrow" . ?↔)
    ("\\longmapsto" . ?↦)
    ("\\longrightarrow" . ?→)
    ("\\looparrowleft" . ?↫)
    ("\\looparrowright" . ?↬)
    ("\\lozenge" . ?✧)
    ("\\lq" . ?‘)
    ("\\lrcorner" . ?⌟)
    ("\\ltimes" . ?⋉)
    ("\\lvertneqq" . ?≨)
    ("\\maltese" . ?✠)
    ("\\mapsto" . ?↦)
    ("\\measuredangle" . ?∡)
    ("\\mho" . ?℧)
    ("\\mid" . ?∣)
    ("\\models" . ?⊧)
    ("\\mp" . ?∓)
    ("\\multimap" . ?⊸)
    ("\\nLeftarrow" . ?⇍)
    ("\\nLeftrightarrow" . ?⇎)
    ("\\nRightarrow" . ?⇏)
    ("\\nVDash" . ?⊯)
    ("\\nVdash" . ?⊮)
    ("\\nabla" . ?∇)
    ("\\napprox" . ?≉)
    ("\\natural" . ?♮)
    ("\\ncong" . ?≇)
    ("\\ne" . ?≠)
    ("\\nearrow" . ?↗)
    ("\\neg" . ?¬)
    ("\\neq" . ?≠)
    ("\\nequiv" . ?≢)
    ("\\newline" . ? )
    ("\\nexists" . ?∄)
    ("\\ngeq" . ?≱)
    ("\\ngeqq" . ?≱)
    ("\\ngeqslant" . ?≱)
    ("\\ngtr" . ?≯)
    ("\\ni" . ?∋)
    ("\\nleftarrow" . ?↚)
    ("\\nleftrightarrow" . ?↮)
    ("\\nleq" . ?≰)
    ("\\nleqq" . ?≰)
    ("\\nleqslant" . ?≰)
    ("\\nless" . ?≮)
    ("\\nmid" . ?∤)
    ;; ("\\not" ?̸)              ;FIXME: conflict with "NOT SIGN" ¬.
    ("\\notin" . ?∉)
    ("\\nparallel" . ?∦)
    ("\\nprec" . ?⊀)
    ("\\npreceq" . ?⋠)
    ("\\nrightarrow" . ?↛)
    ("\\nshortmid" . ?∤)
    ("\\nshortparallel" . ?∦)
    ("\\nsim" . ?≁)
    ("\\nsimeq" . ?≄)
    ("\\nsubset" . ?⊄)
    ("\\nsubseteq" . ?⊈)
    ("\\nsubseteqq" . ?⊈)
    ("\\nsucc" . ?⊁)
    ("\\nsucceq" . ?⋡)
    ("\\nsupset" . ?⊅)
    ("\\nsupseteq" . ?⊉)
    ("\\nsupseteqq" . ?⊉)
    ("\\ntriangleleft" . ?⋪)
    ("\\ntrianglelefteq" . ?⋬)
    ("\\ntriangleright" . ?⋫)
    ("\\ntrianglerighteq" . ?⋭)
    ("\\nvDash" . ?⊭)
    ("\\nvdash" . ?⊬)
    ("\\nwarrow" . ?↖)
    ("\\odot" . ?⊙)
    ("\\oint" . ?∮)
    ("\\ominus" . ?⊖)
    ("\\oplus" . ?⊕)
    ("\\oslash" . ?⊘)
    ("\\otimes" . ?⊗)
    ("\\parallel" . ?∥)
    ("\\partial" . ?∂)
    ("\\perp" . ?⊥)
    ("\\pitchfork" . ?⋔)
    ("\\prec" . ?≺)
    ("\\precapprox" . ?≾)
    ("\\preceq" . ?≼)
    ("\\precnapprox" . ?⋨)
    ("\\precnsim" . ?⋨)
    ("\\precsim" . ?≾)
    ("\\prime" . ?′)
    ("\\prod" . ?∏)
    ("\\propto" . ?∝)
    ("\\qed" . ?∎)
    ("\\qquad" . ?⧢)
    ("\\quad" . ?␣)
    ("\\rangle" . 10217)            ; Literal ?⟩ breaks indentation.
    ("\\rbrace" . ?})
    ("\\rbrack" . ?\])
    ("\\rceil" . ?⌉)
    ("\\rfloor" . ?⌋)
    ("\\rightarrow" . ?→)
    ("\\rightarrowtail" . ?↣)
    ("\\rightharpoondown" . ?⇁)
    ("\\rightharpoonup" . ?⇀)
    ("\\rightleftarrows" . ?⇄)
    ("\\rightleftharpoons" . ?⇌)
    ;; ("\\rightparengtr" ?⦔) ;; Was ?〉, see bug#12948.
    ("\\rightrightarrows" . ?⇉)
    ("\\rightthreetimes" . ?⋌)
    ("\\risingdotseq" . ?≓)
    ("\\rtimes" . ?⋊)
    ("\\times" . ?×)
    ("\\sbs" . ?﹨)
    ("\\searrow" . ?↘)
    ("\\setminus" . ?∖)
    ("\\sharp" . ?♯)
    ("\\shortmid" . ?∣)
    ("\\shortparallel" . ?∥)
    ("\\sim" . ?∼)
    ("\\simeq" . ?≃)
    ("\\smallamalg" . ?∐)
    ("\\smallsetminus" . ?∖)
    ("\\smallsmile" . ?⌣)
    ("\\smile" . ?⌣)
    ("\\spadesuit" . ?♠)
    ("\\sphericalangle" . ?∢)
    ("\\sqcap" . ?⊓)
    ("\\sqcup" . ?⊔)
    ("\\sqsubset" . ?⊏)
    ("\\sqsubseteq" . ?⊑)
    ("\\sqsupset" . ?⊐)
    ("\\sqsupseteq" . ?⊒)
    ("\\square" . ?□)
    ("\\squigarrowright" . ?⇝)
    ("\\star" . ?⋆)
    ("\\straightphi" . ?φ)
    ("\\subset" . ?⊂)
    ("\\subseteq" . ?⊆)
    ("\\subseteqq" . ?⊆)
    ("\\subsetneq" . ?⊊)
    ("\\subsetneqq" . ?⊊)
    ("\\succ" . ?≻)
    ("\\succapprox" . ?≿)
    ("\\succcurlyeq" . ?≽)
    ("\\succeq" . ?≽)
    ("\\succnapprox" . ?⋩)
    ("\\succnsim" . ?⋩)
    ("\\succsim" . ?≿)
    ("\\sum" . ?∑)
    ("\\supset" . ?⊃)
    ("\\supseteq" . ?⊇)
    ("\\supseteqq" . ?⊇)
    ("\\supsetneq" . ?⊋)
    ("\\supsetneqq" . ?⊋)
    ("\\surd" . ?√)
    ("\\swarrow" . ?↙)
    ("\\therefore" . ?∴)
    ("\\thickapprox" . ?≈)
    ("\\thicksim" . ?∼)
    ("\\to" . ?→)
    ("\\top" . ?⊤)
    ("\\triangle" . ?▵)
    ("\\triangledown" . ?▿)
    ("\\triangleleft" . ?◃)
    ("\\trianglelefteq" . ?⊴)
    ("\\triangleq" . ?≜)
    ("\\triangleright" . ?▹)
    ("\\trianglerighteq" . ?⊵)
    ("\\twoheadleftarrow" . ?↞)
    ("\\twoheadrightarrow" . ?↠)
    ("\\ulcorner" . ?⌜)
    ("\\uparrow" . ?↑)
    ("\\updownarrow" . ?↕)
    ("\\upleftharpoon" . ?↿)
    ("\\uplus" . ?⊎)
    ("\\uprightharpoon" . ?↾)
    ("\\upuparrows" . ?⇈)
    ("\\urcorner" . ?⌝)
    ("\\u{i}" . ?ĭ)
    ("\\vDash" . ?⊨)
    ("\\varepsilon" . ?ε)
    ("\\varphi" . ?φ)
    ("\\varprime" . ?′)
    ("\\varpropto" . ?∝)
    ("\\varrho" . ?ϱ)
    ("\\varsigma" . ?ς)
    ("\\vartriangleleft" . ?⊲)
    ("\\vartriangleright" . ?⊳)
    ("\\vdash" . ?⊢)
    ("\\vdots" . ?⋮)
    ("\\vee" . ?∨)
    ("\\veebar" . ?⊻)
    ("\\vert" . ?|)
    ("\\wedge" . ?∧)
    ("\\wp" . ?℘)
    ("\\wr" . ?≀)
    ("\\Bbb{N}" . ?ℕ)			; AMS commands for blackboard bold
    ("\\Bbb{P}" . ?ℙ)			; Also sometimes \mathbb.
    ("\\Bbb{Q}" . ?ℚ)
    ("\\Bbb{R}" . ?ℝ)
    ("\\Bbb{T}" . ?𝕋)
    ("\\Bbb{Z}" . ?ℤ)
    ("\\mathbb{N}" . ?ℕ)			; AMS commands for blackboard bold
    ("\\mathbb{P}" . ?ℙ)			; Also sometimes \mathbb.
    ("\\mathbb{Q}" . ?ℚ)
    ("\\mathbb{R}" . ?ℝ)
    ("\\mathbb{T}" . ?𝕋)
    ("\\mathbb{Z}" . ?ℤ)
    ("\\pm" . ?±)
    ("\\|" . ?‖)
    ("\\varkappa" . ?ϰ)
    ;; caligraphic
    ("\\mathcal{A}" . ?𝒜)
    ("\\mathcal{B}" . ?ℬ)
    ("\\mathcal{C}" . ?𝒞)
    ("\\mathcal{D}" . ?𝒟)
    ("\\mathcal{E}" . ?ℰ)
    ("\\mathcal{F}" . ?ℱ)
    ("\\mathcal{G}" . ?𝒢)
    ("\\mathcal{H}" . ?ℋ)
    ("\\mathcal{I}" . ?ℐ)
    ("\\mathcal{J}" . ?𝒥)
    ("\\mathcal{K}" . ?𝒦)
    ("\\mathcal{L}" . ?ℒ)
    ("\\mathcal{M}" . ?ℳ)
    ("\\mathcal{N}" . ?𝒩)
    ("\\mathcal{O}" . ?𝒪)
    ("\\mathcal{P}" . ?𝒫)
    ("\\mathcal{Q}" . ?𝒬)
    ("\\mathcal{R}" . ?ℛ)
    ("\\mathcal{S}" . ?𝒮)
    ("\\mathcal{T}" . ?𝒯)
    ("\\mathcal{U}" . ?𝒰)
    ("\\mathcal{V}" . ?𝒱)
    ("\\mathcal{W}" . ?𝒲)
    ("\\mathcal{X}" . ?𝒳)
    ("\\mathcal{Y}" . ?𝒴)
    ("\\mathcal{Z}" . ?𝒵)
    ;; fractur
    ("\\mathfrak{A}" . ?𝔄)
    ("\\mathfrak{B}" . ?𝔅)
    ("\\mathfrak{C}" . ?ℭ)
    ("\\mathfrak{D}" . ?𝔇)
    ("\\mathfrak{E}" . ?𝔈)
    ("\\mathfrak{F}" . ?𝔉)
    ("\\mathfrak{G}" . ?𝔊)
    ("\\mathfrak{H}" . ?ℌ)
    ("\\mathfrak{I}" . ?ℑ)
    ("\\mathfrak{J}" . ?𝔍)
    ("\\mathfrak{K}" . ?𝔎)
    ("\\mathfrak{L}" . ?𝔏)
    ("\\mathfrak{M}" . ?𝔐)
    ("\\mathfrak{N}" . ?𝔑)
    ("\\mathfrak{O}" . ?𝔒)
    ("\\mathfrak{P}" . ?𝔓)
    ("\\mathfrak{Q}" . ?𝔔)
    ("\\mathfrak{R}" . ?ℜ)
    ("\\mathfrak{S}" . ?𝔖)
    ("\\mathfrak{T}" . ?𝔗)
    ("\\mathfrak{U}" . ?𝔘)
    ("\\mathfrak{V}" . ?𝔙)
    ("\\mathfrak{W}" . ?𝔚)
    ("\\mathfrak{X}" . ?𝔛)
    ("\\mathfrak{Y}" . ?𝔜)
    ("\\mathfrak{Z}" . ?ℨ)
    ("\\mathfrak{a}" . ?𝔞)
    ("\\mathfrak{b}" . ?𝔟)
    ("\\mathfrak{c}" . ?𝔠)
    ("\\mathfrak{d}" . ?𝔡)
    ("\\mathfrak{e}" . ?𝔢)
    ("\\mathfrak{f}" . ?𝔣)
    ("\\mathfrak{g}" . ?𝔤)
    ("\\mathfrak{h}" . ?𝔥)
    ("\\mathfrak{i}" . ?𝔦)
    ("\\mathfrak{j}" . ?𝔧)
    ("\\mathfrak{k}" . ?𝔨)
    ("\\mathfrak{l}" . ?𝔩)
    ("\\mathfrak{m}" . ?𝔪)
    ("\\mathfrak{n}" . ?𝔫)
    ("\\mathfrak{o}" . ?𝔬)
    ("\\mathfrak{p}" . ?𝔭)
    ("\\mathfrak{q}" . ?𝔮)
    ("\\mathfrak{r}" . ?𝔯)
    ("\\mathfrak{s}" . ?𝔰)
    ("\\mathfrak{t}" . ?𝔱)
    ("\\mathfrak{u}" . ?𝔲)
    ("\\mathfrak{v}" . ?𝔳)
    ("\\mathfrak{w}" . ?𝔴)
    ("\\mathfrak{x}" . ?𝔵)
    ("\\mathfrak{y}" . ?𝔶)
    ("\\mathfrak{z}" . ?𝔷)
    ("--" . ?–)
    ("---" . ?—)
    ("\\ordfeminine" . ?ª)
    ("\\ordmasculine" . ?º)
    ("\\lambdabar" . ?ƛ)
    ("\\celsius" . ?℃)
    ;; Text symbols formerly part of textcomp package:
    ("\\textdollar" . ?$)
    ("\\textborn" . ?*)
    ("\\textless" . ?<)
    ("\\textgreater" . ?>)
    ("\\textbackslash" . ?\\)
    ("\\textasciicircum" . ?^)
    ("\\textunderscore" . ?_)
    ("\\textbraceleft" . ?\{)
    ("\\textbar" . ?|)
    ("\\textbraceright" . ?\})
    ("\\textasciitilde" . ?~)
    ("\\textexclamdown" . ?¡)
    ("\\textcent" . ?¢)
    ("\\textsterling" . ?£)
    ("\\textcurrency" . ?¤)
    ("\\textyen" . ?¥)
    ("\\textbrokenbar" . ?¦)
    ("\\textsection" . ?§)
    ("\\textasciidieresis" . ?¨)
    ("\\textcopyright" . ?©)
    ("\\textordfeminine" . ?ª)
    ("\\guillemetleft" . ?«)
    ("\\guillemotleft" . ?«)
    ("\\textlnot" . ?¬)
    ("\\textregistered" . ?®)
    ("\\textasciimacron" . ?¯)
    ("\\textdegree" . ?°)
    ("\\textpm" . ?±)
    ("\\texttwosuperior" . ?²)
    ("\\textthreesuperior" . ?³)
    ("\\textasciiacute" . ?´)
    ("\\textmu" . ?µ)
    ("\\textparagraph" . ?¶)
    ("\\textpilcrow" . ?¶)
    ("\\textperiodcentered" . ?·)
    ("\\textonesuperior" . ?¹)
    ("\\textordmasculine" . ?º)
    ("\\guillemetright" . ?»)
    ("\\guillemotright" . ?»)
    ("\\textonequarter" . ?¼)
    ("\\textonehalf" . ?½)
    ("\\textthreequarters" . ?¾)
    ("\\textquestiondown" . ?¿)
    ("\\texttimes" . ?×)
    ("\\textdiv" . ?÷)
    ("\\textflorin" . ?ƒ)
    ("\\textasciicaron" . ?ˇ)
    ("\\textasciibreve" . ?˘)
    ("\\textacutedbl" . ?˝)
    ("\\textgravedbl" . 757)
    ("\\texttildelow" . 759)
    ("\\textbaht" . ?฿)
    ("\\textendash" . ?–)
    ("\\textemdash" . ?—)
    ("\\textbardbl" . ?‖)
    ("\\textquoteleft" . 8216)
    ("\\textquoteright" . 8217)
    ("\\quotesinglbase" . 8218)
    ("\\textquotedblleft" . 8220)
    ("\\textquotedblright" . 8221)
    ("\\quotedblbase" . 8222)
    ;; \textdagger and \textdied are replaced with DAGGER (#x2020) and
    ;; not with LATIN CROSS (#x271d)
    ("\\textdagger" . ?†)
    ("\\textdied" . ?†)
    ("\\textdaggerdbl" . ?‡)
    ("\\textbullet" . ?•)
    ("\\textellipsis" . ?…)
    ("\\textperthousand" . ?‰)
    ("\\textpertenthousand" . ?‱)
    ("\\guilsinglleft" . ?‹)
    ("\\guilsinglright" . ?›)
    ("\\textreferencemark" . ?※)
    ("\\textinterrobang" . ?‽)
    ("\\textfractionsolidus" . ?⁄)
    ("\\textlquill" . 8261) ; Literal ?⁅ breaks indentation
    ("\\textrquill" . 8262) ; Literal ?⁆ breaks indentation
    ("\\textdiscount" . ?⁒)
    ("\\textcolonmonetary" . ?₡)
    ("\\textlira" . ?₤)
    ("\\textnaira" . ?₦)
    ("\\textwon" . ?₩)
    ("\\textdong" . ?₫)
    ("\\texteuro" . ?€)
    ("\\textpeso" . ?₱)
    ("\\textguarani" . ?₲)
    ("\\textcelsius" . ?℃)
    ("\\textnumero" . ?№)
    ("\\textcircledP" . ?℗)
    ("\\textrecipe" . ?℞)
    ("\\textservicemark" . ?℠)
    ("\\texttrademark" . ?™)
    ("\\textohm" . ?Ω)
    ("\\textmho" . ?℧)
    ("\\textestimated" . ?℮)
    ("\\textleftarrow" . ?←)
    ("\\textuparrow" . ?↑)
    ("\\textrightarrow" . ?→)
    ("\\textdownarrow" . ?↓)
    ("\\textminus" . ?−)
    ("\\textsurd" . ?√)
    ("\\textlangle" . 9001) ; Literal ?〈 breaks indentation
    ("\\textrangle" . 9002) ; Literal ?〉 breaks indentation
    ("\\textblank" . ?␢)
    ("\\textvisiblespace" . ?␣)
    ("\\textopenbullet" . ?◦)
    ;; \textbigcircle is replaced with LARGE CIRCLE (#x25ef) and not
    ;; with COMBINING ENCLOSING CIRCLE (#x20dd)
    ("\\textbigcircle" . ?◯)
    ("\\textmusicalnote" . ?♪)
    ("\\textmarried" . ?⚭)
    ("\\textdivorced" . ?⚮)
    ("\\textlbrackdbl" . 10214) ; Literal ?⟦ breaks indentation
    ("\\textrbrackdbl" . 10215) ; Literal ?⟧ breaks indentation
    ("\\textinterrobangdown" . ?⸘)))

(provide 'org-appearance)

;;; org-appearance ends here
