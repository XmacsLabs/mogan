<TeXmacs|1.99.13>

<style|<tuple|tmdoc|automate|french>>

<\body>
  <block-assign|<implied-scm|t>|<implied-scm|(focus-tree)>>

  <block-assign|<implied-scm|tag>|<implied-scm|(tree-label t)>>

  <block-assign|<implied-scm|n>|<implied-scm|(tree-arity t)>>

  <block-assign|<implied-scm|nl>|<implied-scm|(focus-doc-arg-names t 0 '())>>

  <block-assign|<implied-scm|tag-doc>|<implied-scm|(tmdoc-search-tag tag)>>

  <tmdoc-title|Aide contextuelle sur la balise \S
  <output-string|<implied-scm|tag>> \T>

  <\block-if-else|<implied-scm|tag-doc>>
    <\unfolded-documentation|Description>
      <block-output|<implied-scm|tag-doc>>
    </unfolded-documentation>
  <|block-if-else>
    <\unfolded-documentation|Usage>
      <\explain|<inline-output|<implied-scm|`(explain-macro
      ,(symbol-\<gtr\>string tag) ,@nl)>>>
        <\block-texmacs-tag|description-aligned>
          <\block-for|<implied-scm|i>|<implied-scm|(.. 0 n)>>
            <inline-texmacs-tag|item*|<src-arg|<output-string|<implied-scm|(list-ref
            nl i)>>>><inline-if-else|<implied-scm|(== (list-ref nl i)
            "body")>|Le corps principal de la macro.|Un argument de type
            \P<output-string|<implied-scm|(tree-child-type t i)>>\Q.>
          </block-for>
        </block-texmacs-tag>
      </explain>
    </unfolded-documentation>
  </block-if-else>

  <\block-if|<implied-scm|(tree-label-extension? tag)>>
    <\unfolded-documentation|D�finition actuelle>
      <\surround||<vspace|2fn>>
        <\tm-fragment>
          <inline-texmacs-tag|inactive*|<inline-texmacs-tag|assign|<output-string|<implied-scm|tag>>|<inline-output|<implied-scm|(get-env-tree
          (symbol-\<gtr\>string tag))>>>>
        </tm-fragment>
      </surround>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(or (focus-has-variants? t) (focus-has-toggles?
  t))>>
    <\unfolded-documentation|Variantes structur�es>
      <\block-if|<implied-scm|(focus-has-variants? t)>>
        La balise <markup|<output-string|<implied-scm|tag>>> admet plusieurs
        <help-link|variantes structur�es|main/editing/man-structured-variants>
        : <inline-intersperse|, |<inline-for|<implied-scm|v>|<implied-scm|(variants-of
        tag)>|<markup|<output-string|<implied-scm|v>>>>>. Vous pouvez faire
        tourner ces variantes via les raccourcis clavier
        <shortcut|(variant-circulate (focus-tree) #t)> et
        <shortcut|(variant-circulate (focus-tree) #f)>. Vous pouvez aussi
        s�lectionner une variante sp�cifique dans le menu
        <menu|Focus|<output-string|<implied-scm|(focus-tag-name tag)>>> ou
        depuis le menu <menu|<output-string|<implied-scm|(focus-tag-name
        tag)>>> sur la barre de focus.
      </block-if>

      <\block-if|<implied-scm|(numbered-context? t)>>
        Pour la plupart de styles, l'environnement
        <markup|<output-string|<implied-scm|tag>>>
        <inline-if-else|<implied-scm|(symbol-numbered? tag)>|est
        num�rot�|n'est pas num�rot�>. L'environment admet une variante
        <inline-if-else|<implied-scm|(symbol-numbered? tag)>|non
        num�rot�e|num�rot�e> <markup|<output-string|<implied-scm|(symbol-toggle-number
        tag)>>>. Vous pouvez basculer la num�rotation avec le raccourci
        clavier <shortcut|(numbered-toggle (focus-tree))>, l'entr�e de menu
        <menu|Focus|Numbered>, o� en cliquant sur l'ic�ne
        <icon|tm_numbered.xpm> sur la barre de focus.
      </block-if>

      <\block-if|<implied-scm|(alternate-context? t)>>
        <\block-if|<implied-scm|(alternate-first? t)>>
          L'environnement <markup|<output-string|<implied-scm|tag>>> est \S
          pli� \T et <markup|<output-string|<implied-scm|tag>>> admet la
          variante d�pli�e <markup|<output-string|<implied-scm|(symbol-toggle-alternate
          tag)>>>. Vous pouvez d�plier l'environnement utilisant le raccourci
          <shortcut|(alternate-toggle (focus-tree))>, l'entr�e de menu
          <menu|Focus|Folded>, ou en cliquant sur l'ic�ne
          <icon|tm_alternate_first.xpm> sur la barre de focus.
        </block-if>

        <\block-if|<implied-scm|(alternate-second? t)>>
          L'environnement <markup|<output-string|<implied-scm|tag>>> est \S
          d�pli� \T et <markup|<output-string|<implied-scm|tag>>> admet la
          variante repli�e <markup|<output-string|<implied-scm|(symbol-toggle-alternate
          tag)>>>. Vous pouvez plier l'environnement utilisant le raccourci
          <shortcut|(alternate-toggle (focus-tree))>, l'entr�e de menu
          <menu|Focus|<output-string|<implied-scm|(alternate-second-name
          t)>>>, ou en cliquant sur l'ic�ne
          <inline-texmacs-tag|icon|<output-string|<implied-scm|(alternate-second-icon
          t)>>> sur la barre de focus.
        </block-if>

        En traversant un document en <help-link|mode de
        pr�sentation|main/beamer/man-beamer>, des environnements pliables
        sont pli�s et d�pli�s automatiquement.
      </block-if>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-has-preferences? t)>>
    <\unfolded-documentation|Pr�f�rences de style>
      <block-assign|<implied-scm|opts>|<implied-scm|(search-tag-options t)>>

      <block-assign|<implied-scm|pars>|<\implied-scm>
        (list-filter (search-tag-parameters t) parameter-show-in-menu?)
      </implied-scm>>

      <block-assign|<implied-scm|thms>|<\implied-scm>
        (search-tag-themes t)
      </implied-scm>>

      La rendue de la balise <markup|<output-string|<implied-scm|tag>>> peut
      �tre personnalis�e en �ditant la macro qui la d�finit. Pour ceci, il
      suffit de cliquer sur l'entr�e <menu|Edit macro> du menu
      <menu|Focus|Preferences> menu (ou du menu sous l'ic�ne
      <icon|tm_focus_prefs.xpm> de la barre de focus). Vous pouvez aussi
      �diter directement la macro dans le fichier de style o� elle est
      d�finie, utilisant <menu|Edit source>.

      <\block-if|<implied-scm|(nnull? (append opts pars thms))>>
        Toujours dans le menu <menu|Focus|Preferences> menu, il est possible
        de sp�cifier <inline-if|<implied-scm|(and (nnull? opts) (null?
        pars))>|des options de style><inline-if|<implied-scm|(and (null?
        opts) (nnull? pars))>|des param�tres de
        style><inline-if|<implied-scm|(and (nnull? opts) (nnull? pars))>|des
        options et des param�tres de style> qui s'appliquent � la balise
        <markup|<output-string|<implied-scm|tag>>>. Ces r�glages sont
        globaux, donc il s'appliquent � toutes les balises
        <markup|<output-string|<implied-scm|tag>>> dans votre document, et
        m�me � toutes les balises similaires.
      </block-if>

      <\block-if|<implied-scm|(nnull? thms)>>
        La balise <markup|<output-string|<implied-scm|tag>>> utilise des
        th�mes pour sa rendue. Ces th�mes viennent avec leurs propres
        param�tres de style qui peuvent �tre personnalis�s via
        <menu|Focus|Preferences|Theme parameters>.
      </block-if>

      <\block-if|<implied-scm|(nnull? opts)>>
        <\folded|<strong|Options de style>>
          <\block-for|<implied-scm|opt>|<implied-scm|opts>>
            <block-assign|<implied-scm|opt-doc>|<\implied-scm>
              (tmdoc-search-style opt)
            </implied-scm>>

            <block-assign|<implied-scm|sty-doc>|<\implied-scm>
              (style-get-documentation opt)
            </implied-scm>>

            <\block-if-else|<implied-scm|opt-doc>>
              <block-output|<implied-scm|opt-doc>>
            <|block-if-else>
              <\explain|<tmpackage|<output-string|<implied-scm|opt>>><explain-synopsis|<output-string|<implied-scm|(style-get-menu-name
              opt)>>>>
                <\block-if-else|<implied-scm|sty-doc>>
                  <inline-output|<implied-scm|sty-doc>>.
                <|block-if-else>
                  Paquetage de style non document�.
                </block-if-else>
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-if>

      <\block-if|<implied-scm|(nnull? pars)>>
        <\folded|<strong|Param�tres de style>>
          <\block-for|<implied-scm|par>|<implied-scm|pars>>
            <block-assign|<implied-scm|par-doc>|<\implied-scm>
              (tmdoc-search-parameter par)
            </implied-scm>>

            <\block-if-else|<implied-scm|par-doc>>
              <block-output|<implied-scm|par-doc>>
            <|block-if-else>
              <\explain|<src-var|<output-string|<implied-scm|par>>>>
                Un param�tre de type <output-string|<implied-scm|(tree-label-type
                (string-\<gtr\>symbol par))>>.
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-if>

      <\block-for|<implied-scm|thm>|<implied-scm|thms>>
        <\folded|<strong|Param�tres de style pour le th�me
        <output-string|<implied-scm|thm>>>>
          <\block-for|<implied-scm|par>|<implied-scm|(theme-\<gtr\>members
          thm)>>
            <block-assign|<implied-scm|par-doc>|<\implied-scm>
              (tmdoc-search-parameter par)
            </implied-scm>>

            <\block-if-else|<implied-scm|par-doc>>
              <block-output|<implied-scm|par-doc>>
            <|block-if-else>
              <\explain|<src-var|<output-string|<implied-scm|par>>>>
                Un param�tre de type <output-string|<implied-scm|(tree-label-type
                (string-\<gtr\>symbol par))>>.
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-for>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-can-move? t)>>
    <\unfolded-documentation|Navigation structur�e>
      Il est parfois utile de traverser rapidement toutes les balises
      <markup|<output-string|<implied-scm|tag>>> et ses variantes qui se
      trouvent dans un document. Ceci peut se faire efficacement utilisant
      des raccourcis claviers, des entr�es de menu, ou des ic�nes sur la
      barre de focus :

      <\description-long>
        <item*|<shortcut|(kbd-select-if-active traverse-first)>,
        <menu|Focus|First similar>, <icon|tm_similar_first.xpm>>Aller vers la
        premi�re balise similaire � <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-previous)>,
        <menu|Focus|Previous similar>, <icon|tm_similar_previous.xpm>>Aller
        vers la balise pr�c�dente et similaire �
        <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-next)>,
        <menu|Focus|Next similar>, <icon|tm_similar_next.xpm>>Aller vers la
        balise suivante et similaire � <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-last)>,
        <menu|Focus|Last similar>, <icon|tm_similar_last.xpm>>Aller vers la
        derni�re balise similaire � <markup|<output-string|<implied-scm|tag>>>.
      </description-long>

      Pour plus de renseignements et d'autres raccourcis clavier utils, nous
      vous renvoyons vers la section sur les <help-link|mouvements de curseur
      structur�s|main/editing/man-structured-move>.
    </unfolded-documentation>
  </block-if>

  <\block-if-else|<implied-scm|(and (defined? 'any-table-tag?)
  (any-table-tag? tag))>>
    <\unfolded-documentation|Insertions et suppressions structur�es>
      La balise <markup|<output-string|<implied-scm|tag>>> est un
      environnement tabulaire. De nouvelles lignes et colonnes peuvent �tre
      cr�es de mani�re suivante :

      <\description-long>
        <item*|<shortcut|(structured-insert-left)>, <menu|Focus|Insert left>,
        <icon|tm_insert_left.xpm>>Ins�rer une nouvelle colonne � gauche du
        curseur.

        <item*|<shortcut|(structured-insert-right)>, <menu|Focus|Insert
        right>, <icon|tm_insert_right.xpm>>Ins�rer une nouvelle colonne �
        droite du curseur.

        <item*|<shortcut|(structured-insert-up)>, <menu|Focus|Insert above>,
        <icon|tm_insert_up.xpm>>Ins�rer une nouvelle ligne au dessus du
        curseur.

        <item*|<shortcut|(structured-insert-down)>, <menu|Focus|Insert
        below>, <icon|tm_insert_down.xpm>>Ins�rer une nouvelle ligne en
        dessous du curseur.
      </description-long>

      Des lignes et des colonnes existantes peuvent �tre supprim�es comme
      suit :

      <\description-long>
        <item*|<shortcut|(structured-remove-left)>, <menu|Focus|Remove
        leftwards>, <icon|tm_delete_left.xpm>>Supprimer la colonne � gauche
        du curseur.

        <item*|<shortcut|(structured-remove-right)>, <menu|Focus|Remove
        rightwards>, <icon|tm_delete_right.xpm>>Supprimer la colonne avec le
        curseur et aller vers la colonne d'apr�s.

        <item*|<menu|Focus|Remove upwards>, <icon|tm_delete_up.xpm>>Supprimer
        la ligne au dessus du curseur.

        <item*|<menu|Focus|Remove downwards>,
        <icon|tm_delete_down.xpm>>Supprimer la ligne avec le curseur et aller
        vers la ligne en dessous.
      </description-long>
    </unfolded-documentation>
  <|block-if-else>
    <\block-if-else|<implied-scm|(and (defined? 'field-tags) (tm-in? t
    field-tags))>>
      <\unfolded-documentation|Insertions et suppressions structur�es>
        La balise <markup|<output-string|<implied-scm|tag>>> est utilis�e �
        l'int�rieur de sessions interactives. De nouveaux champs d'entr�e
        peuvent �tre ins�r�s de mani�re suivante :

        <\description-long>
          <item*|<shortcut|(structured-insert-up)>, <menu|Focus|Insert field
          above>, <icon|tm_insert_up.xpm>>Ins�rer un nouveau champ d'entr�e
          au dessus du curseur.

          <item*|<shortcut|(structured-insert-down)>, <menu|Focus|Insert
          field below>, <icon|tm_insert_down.xpm>>Ins�rer un nouveau champ
          d'entr�e en dessous du curseur.
        </description-long>

        Des champs d'entr�e et de sortie existants peuvent �tre supprim�s
        comme suit :

        <\description-long>
          <item*|<menu|Focus|Remove field above>,
          <icon|tm_delete_up.xpm>>Supprimer le champ au dessus du curseur.

          <item*|<menu|Focus|Remove field below>,
          <icon|tm_delete_down.xpm>>Supprimer le champ courant et aller au
          champ suivant.

          <item*|<menu|Focus|Remove banner>>Supprimer la banni�re de
          d�marrage de la session.

          <item*|<menu|Focus|Remove last field>>Supprimer le dernier champ de
          la session.
        </description-long>
      </unfolded-documentation>
    <|block-if-else>
      <\block-if|<implied-scm|(or (structured-horizontal? t)
      (structured-vertical? t))>>
        <\unfolded-documentation|Insertions et suppressions structur�es>
          La balise <markup|<output-string|<implied-scm|tag>>> admet un
          nombre variable d'arguments. De nouveaux arguments peuvent �tre
          ins�r�s de mani�re suivante :

          <\block-texmacs-tag|description-long>
            <\block-if|<implied-scm|(structured-horizontal? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-insert-left)>,
              <menu|Focus|Insert left>, <icon|tm_insert_left.xpm>>Ins�rer un
              nouvel argument � gauche du curseur.

              <inline-texmacs-tag|item*|<shortcut|(structured-insert-right)>,
              <menu|Focus|Insert right>, <icon|tm_insert_right.xpm>>Ins�rer
              un nouvel argument � droite du curseur.
            </block-if>

            <\block-if|<implied-scm|(structured-vertical? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-insert-up)>,
              <menu|Focus|Insert above>, <icon|tm_insert_up.xpm>>Ins�rer un
              nouvel argument au dessus du curseur.

              <inline-texmacs-tag|item*|<shortcut|(structured-insert-down)>,
              <menu|Focus|Insert below>, <icon|tm_insert_down.xpm>>Ins�rer un
              nouvel argument en dessous du curseur.
            </block-if>
          </block-texmacs-tag>

          Des arguments existants peuvent �tre supprim�s comme suit :

          <\block-texmacs-tag|description-long>
            <\block-if|<implied-scm|(structured-horizontal? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-remove-left)>,
              <menu|Focus|Remove leftwards>,
              <icon|tm_delete_left.xpm>>Supprimer l'argument � gauche du
              curseur.

              <inline-texmacs-tag|item*|<shortcut|(structured-remove-right)>,
              <menu|Focus|Remove rightwards>,
              <icon|tm_delete_right.xpm>>Supprimer l'argument courant et
              aller vers l'argument suivant.
            </block-if>

            <\block-if|<implied-scm|(structured-vertical? t)>>
              <inline-texmacs-tag|item*|<menu|Focus|Remove upwards>,
              <icon|tm_delete_up.xpm>>Supprimer l'argument au dessus du
              curseur.

              <inline-texmacs-tag|item*|<menu|Focus|Remove downwards>,
              <icon|tm_delete_down.xpm>>Supprimer l'argument courant et aller
              vers la l'argument en dessous.
            </block-if>
          </block-texmacs-tag>
        </unfolded-documentation>
      </block-if>
    </block-if-else>
  </block-if-else>

  <\block-if|<implied-scm|(\<less\> (length (tree-accessible-children t))
  n)>>
    <\unfolded-documentation|Les arguments cach�s>
      <block-assign|<implied-scm|il>|<implied-scm|(list-filter (.. 0 (length
      nl)) (lambda (i) (not (tree-accessible-child? t i))))>>

      <block-assign|<implied-scm|sl>|<implied-scm|(map (lambda (i) (list-ref
      nl i)) il)>>

      Quand la balise <markup|<output-string|<implied-scm|tag>>> est
      <help-link|activ�e|main/text/keyboard/man-dynamic>, les arguments
      suivants sont cach�s : <inline-intersperse|,
      |<inline-for|<implied-scm|x>|<implied-scm|sl>|<src-arg|<output-string|<implied-scm|x>>>>>.
      Pour �diter ces arguments, vous pouvez utiliser <menu|Focus|Show
      hidden> ou cliquer sur l'ic�ne<nbsp><icon|tm_show_hidden.xpm> de la
      barre de focus. Des balises d�sactiv�es peuvent �tre r�activ�es en
      appuyant sur<nbsp><shortcut|(kbd-return)>.

      Certains arguments cach�s de type cha�ne de caract�re peuvent aussi
      �tre �dit�s directement via les champs texte sur la barre de focus ;
      vous n'avez pas besoin de d�sactiver la balise dans ce cas.
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-has-geometry? t)>>
    <\unfolded-documentation|G�om�trie structur�e>
      Non document� pour l'instant.
    </unfolded-documentation>
  </block-if>
</body>

<initial|<\collection>
</collection>>