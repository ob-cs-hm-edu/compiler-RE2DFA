# re2dfa

Compilieren (und installieren) Sie, wie gewohnt mit `stack`.

Erzeugt aus einem regulären Ausdruck mit Thompson's Construction einen
nichtdeterministischen endlichen Automaten (NEA, *engl.* NFA).

Aus dem NEA wird anschließend mit der Subset Construction ein deterministischer
endlicher Automat (DEA, *engl* DFA) erzeugt.

Mit Hopcroft's Algorithmus wird der DEA minimiert.

    Usage: re2dfa COMMAND REGEX
      Umwandlung eines Regex in einen minimalen DFA

    Available options:
      REGEX                    regulärer Ausdruck
      -h,--help                Show this help text

    Available commands:
      regex                    regex ausgeben
      nfa                      NFA ausgeben
      dfa                      DFA ausgeben
      mindfa                   Minimalen DFA ausgeben

Wenn Sie `re2dfa` mit `stack exec` ausführen, müssen Sie vor die
Kommandozeilenargumente die für `re2dfa` sind ein `--` anfügen, z.B.

    stack exec re2dfa -- mindfa "a*(bb*|cb*)"

Wenn Sie `re2dfa` mit `stack install` installiert haben, reicht natürlich

    re2dfa mindfa "a*(bb*|cb*)"

Über die verschiedenen Kommandos (`regex`, `nfa`, `dfa`, `mindfa`) können
Sie die Zwischenstände ausgeben lassen.

Die Ausgaben der endlichen Automaten erfolgen in der
[dot](http://www.graphviz.org/)-Syntax. Wenn Sie
[graphviz](http://www.graphviz.org/) installiert haben, können Sie daraus ein
Zustandsübergangsdiagramm erzeugen lassen. Alternativ kopieren Sie den
Code und geben Sie ihn unter <http://www.webgraphviz.com/> ein.

Wenn Sie graphviz unter einem Unixoiden Betriebssystem
(Linux, macOS, FreeBSD, ...) nutzen, können Sie ohne Umwege ein PNG erzeugen,
z.B.

    re2dfa mindfa "a*(bb*|cb*)" | dot -Tpng -o fa.png

oder mit `stack exec`:

    stack exec re2dfa -- mindfa "a*(bb*|cb*)" | dot -Tpng -o fa.png

Sollten Sie einen Bug finden oder einen Verbesserungsvorschlag haben,
freue ich mich über ein
[Issue](https://github.com/ob-cs-hm-edu/compiler-RE2DFA/issues) oder einen
[Pull-Request](https://github.com/ob-cs-hm-edu/compiler-RE2DFA/pulls).
