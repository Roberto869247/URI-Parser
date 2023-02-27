Pasta Roberto 869247
Il programma Prolog è stato realizzato con l'utilizzo di un automa a stati finiti non deterministico, con 33 stati presenti.
Come in Lisp, è presente una regola iniziale "uri-parse" che trasforma la stringa in input, in una lista di caratteri per poi 
analizzarla e scomporla nelle varie parti carattere per carattere tramite chiamate ricorsive.
Il programma concatena il carattere all'inizio della lista con la pila dei caratteri precedentemente già letti e concatenati 
(o la pila vuota), poi vengono chiamate le stesse regole ricorsivamente e come argomenti vengono passati la lista meno il primo
carattere letto, la pila aggiornata con l'ultimo carattere inserito, la struttura (lista) con le parti che sono già state riconosciute
e lo stato in cui si trova l'automa.
La modifica degli stati avviene tramite regole "delta" che prendono in input il carattere all'inizio della lista e in base allo
stato in cui si trovano ed al carattere letto ritornano lo stato successivo nel quale si troverà l'automa.
Quando viene riconosciuto un carattere speciale di separazione tra le diverse parti della URI, questo viene scartato e la pila dei
caratteri fino ad ora letti viene capovolta e salvata come atomo nella parte corrispondente della stuttura, nella chiamata alla
regola successiva viene poi passata al posto della pila una lista vuota per ricominciare a concatenare i caratteri riconosciuti e
lo stato aggiornato dalla "delta".
E' presente una funzione "uri-display" che stampa a schermo la struttura nel caso in cui non gli venga passato alcuno stream, 
viceversa stamperà la URI sullo stream di output indicato.
