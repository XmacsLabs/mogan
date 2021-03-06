<TeXmacs|1.0.1.18>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\expand|tmdoc-title>
    Utilizzare sessioni di Qcl in <TeXmacs>
  </expand>

  <name|Qcl> ? un linguaggio di programmazione di alto livello e indipendente
  dall'architettura per i computer quantistici, con una sintassi derivata dai
  linguaggi procedurali classici come il <name|C> o il <name|Pascal>. Questo
  permette la completa implementazione e la simulazione di algoritmi
  quantistici (includendo componenti classiche) in un formalismo consistente.
  L'interfaccia di <TeXmacs> ? utile soprattutto per visualizzare in modo
  leggibile gli stati quantici. Per maggiori informazioni si veda

  <\verbatim>
    \ \ \ \ http://tph.tuwien.ac.at/~oemer/qcl.html
  </verbatim>

  Come consiglio, gli utilizzatori farebbero meglio ad installare la versione
  binaria pi? recente, ora vi ? la 0.5, poich? ? difficile compilare dai
  codici sorgenti. Inoltre, se si installa <name|Qcl> dalla versione binaria,
  ci si assicuri che tutta la directory di libreria, <verbatim|lib>, sia
  sotto la directory in cui sono posti i file binari <verbatim|qcl>.

  <\session|qcl|default>
    <\output>
      QCL Quantum Computation Language (32 qubits, seed 1051277574)

      [0/32] <with|mode|math|1<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|0>\<rangle\>>>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      qureg a[1];
    </input>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      Rot(pi/4,a);
    </input>

    <\output>
      [1/32] <with|mode|math|0.92388<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|0>\<rangle\>>-0.38268<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|1>\<rangle\>>>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      Mix(a);
    </input>

    <\output>
      [1/32] <with|mode|math|0.38268<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|0>\<rangle\>>+0.92388<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|1>\<rangle\>>>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      dump;
    </input>

    <\output>
      <\with|color|blue>
        <\verbatim>
          STATE: 1 / 32 qubits allocated, 31 / 32 qubits free
        </verbatim>
      </with>

      <\with|mode|math>
        0.38268<hspace|0.25spc><with|color|magenta|\|<with|math font
        family|rm|0>\<rangle\>>+0.92388<hspace|0.25spc><with|color|magenta|\|<with|math
        font family|rm|1>\<rangle\>>
      </with>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      include "shor.qcl";
    </input>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      operator dft(qureg q) { const n=#q; int i; int j;for i=1 to n { for j=1
      to i-1 { if q[n-1] and q[n-j] {Phase(pi/2^(i-j));}} H(q[n-1]);}
      flip(q); }
    </input>

    <\output>
      <\with|color|red>
        <\verbatim>
          at "operator dft(qureg q) { c ...":
        </verbatim>
      </with>

      <\with|color|red>
        <\verbatim>
          illegal scope: Global symbol dft already defined
        </verbatim>
      </with>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      dft(a);
    </input>

    <\output>
      [1/32] <with|mode|math|0.70711<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|0>\<rangle\>>+0.70711<hspace|0.25spc><with|color|magenta|\|<with|math
      font family|rm|1>\<rangle\>>>
    </output>

    <\input|<\with|color|red>
      <\verbatim>
        qcl\<gtr\>\ 
      </verbatim>
    </with>>
      \;
    </input>
  </session>

  <apply|tmdoc-copyright|2003|Chu-Ching Huang, Lucia Gecchelin>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-10|<tuple|8.2|?>>
    <associate|toc-11|<tuple|8.3|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|toc-12|<tuple|8.4|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|toc-13|<tuple|8.5|?>>
    <associate|idx-3|<tuple|3|?>>
    <associate|gly-4|<tuple|4|?>>
    <associate|toc-14|<tuple|8.6|?>>
    <associate|idx-4|<tuple|7|?>>
    <associate|gly-5|<tuple|5|?>>
    <associate|toc-15|<tuple|8.7|?>>
    <associate|idx-5|<tuple|8|?>>
    <associate|gly-6|<tuple|6|?>>
    <associate|toc-16|<tuple|8.8|?>>
    <associate|gly-7|<tuple|7|?>>
    <associate|gly-8|<tuple|8|?>>
    <associate|gly-9|<tuple|9|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|7|?>>
    <associate|toc-8|<tuple|8|?>>
    <associate|toc-9|<tuple|8.1|?>>
  </collection>
</references>
