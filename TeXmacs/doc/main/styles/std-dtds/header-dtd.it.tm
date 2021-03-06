<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Intestazioni standard >

  Il <abbr|d.t.d.> <tmdtd|header> fornisce i tag per personalizzare le
  intestazioni e le note a pi? di pagina. La personalizzazione ? basata
  sull'idea che possiamo specificare una <em|testo di pagina> per ogni
  pagina. Questo testo di pagina pu? essere, per esempio, un titolo corrente
  o il nome della sezione attuale. Il testo di pagina pu? dipendere dalla
  parit? di una pagina e apparire in modo differente per pagine speciali come
  la pagina iniziale di un nuovo capitolo. I tag seguenti controllano
  l'impaginazione fisica dei tipi diversi di pagina:

  <\explain|<markup|start-page>>
    Questo tag, con testo di pagina come suo unico argomento, specifica
    l'impaginazione della prima pagina di un nuovo capitolo o di una sezione.
  </explain>

  <\explain|<markup|odd-page-text>>
    Analogo a <markup|start-page>, ma per l'impaginazione delle pagine
    dispari ordinarie.
  </explain>

  <\explain|<markup|even-page-text>>
    Analogo a <markup|start-page>, ma per l'impaginazione delle pagine pari
    ordinarie.
  </explain>

  I tag seguenti controllano le azioni logiche relative alle intestazioni da
  eseguire quando si specfica un titolo, un autore, o quando si inizia una
  nuova sezione.

  <\explain|<markup|header-title>>
    Tag con un ``argomento titolo'' che viene utilizzato per specificare il
    titolo del documento.
  </explain>

  <\explain|<markup|header-author>>
    Tag con un ``argomento autore'' che viene utilizzato per specificare
    l'autore del documento.
  </explain>

  <\explain|<markup|header-primary>>
    Tag con un ``argomento nome della sezione'' che viene utilizzato
    all'inizio di ciascuna sezione di base (cio? <markup|chapter> per lo
    stile libro, o <markup|section> per lo stile articolo).
  </explain>

  <\explain|<markup|header-secondary>>
    Tag con un ``argomento nome della sezione'' che ? utilizzato all'inizio
    sezione secondaria (cio? <markup|section> per lo stile libro, o
    <markup|subsection> per lo stile articolo).
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia Gecchelin|Andrea
  Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|italian>
  </collection>
</initial>