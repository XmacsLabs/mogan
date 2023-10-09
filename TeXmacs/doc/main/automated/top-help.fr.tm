<TeXmacs|1.99.13>

<style|<tuple|tmdoc|automate|french>>

<\body>
  <block-assign|<implied-scm|styles>|<implied-scm|(get-style-list)>>

  <block-assign|<implied-scm|style>|<implied-scm|(if (null? styles) "none"
  (car styles))>>

  <block-assign|<implied-scm|style-doc>|<implied-scm|(tmdoc-search-style
  style)>>

  <tmdoc-title|Aide contextuelle � la racine>

  <\unfolded-documentation|�dition � la racine du document>
    Le <help-link|focus actuel|main/text/man-structure> est sur le document
    en entier. Dans ce cas pr�cis, la barre de focus (<abbr|c.�.d.> la
    troisi�me barre d'outils) affiche les caract�ristiques principales de
    votre document, comme son style (<tmstyle|<output-string|<implied-scm|style>>>),
    la taille du papier (<output-string|<implied-scm|(get-init
    "page-type")>>), et la taille de la police
    (<output-string|<implied-scm|(get-init "font-base-size")>>pt). La barre
    de focus indique aussi la section dans laquelle se trouve votre curseur.

    Les propri�t�s et la section courante de ci-dessus peuvent �galement �tre
    modifi�es utilisant les ic�nes et entr�es sur la barre de focus, ou
    encore depuis le menu <menu|Focus>. Par exemple, en cliquant sur le style
    du document, un menu d�roulant s'ouvre depuis lequel vous pouvez changer
    de style. En cliquant sur l'ic�ne <icon|tm_add.xpm> apr�s le style du
    document, vous pouvez �galement s�lectuionner des paquetages de style
    suppl�mentaires.

    De m�me, en cliquant sur la section courante, un menu d�roulant s'ouvre
    avec toutes les sections du document, ce qui vous permet de rapidement
    sauter vers la section de votre choix. Si le curseur se trouve au d�but
    du document et vous ne lui avez pas encore donn� de titre, alors un
    bouton <menu|Title> � cet effet appara�tra sur la barre de focus. De
    mani�re similaire, si votre curseur est juste apr�s le titre, un bouton
    <menu|Abstract> peut appara�tre pour saisir un r�sum�.
  </unfolded-documentation>

  <\unfolded-documentation|Style du document>
    <\block-if-else|<implied-scm|style-doc>>
      <block-output|<implied-scm|style-doc>>
    <|block-if-else>
      <\explain|<tmstyle|<output-string|<implied-scm|style>>>>
        Documentation indisponible.
      </explain>
    </block-if-else>
  </unfolded-documentation>

  <\block-if|<implied-scm|(and (nnull? styles) (nnull? (cdr styles)))>>
    <\unfolded-documentation|Autres personnalisations de style actives>
      <\block-for|<implied-scm|pack>|<implied-scm|(cdr styles)>>
        <block-assign|<implied-scm|pack-doc>|<\implied-scm>
          (tmdoc-search-style pack)
        </implied-scm>>

        <\block-if-else|<implied-scm|pack-doc>>
          <block-output|<implied-scm|pack-doc>>
        <|block-if-else>
          <\explain|<tmpackage|<output-string|<implied-scm|pack>>>>
            Documentation indisponible.
          </explain>
        </block-if-else>
      </block-for>
    </unfolded-documentation>
  </block-if>
</body>

<initial|<\collection>
</collection>>