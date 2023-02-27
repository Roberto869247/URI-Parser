Pasta Roberto 869247
Il programma Lisp è stato realizzato con una serie di funzioni chiamate ricorsivamente a partire da quella iniziale "uri-parse",
che presa una stringa la trasforma in lista di caratteri, per poi analizzarla e scomporla nelle varie parti carattere per carattere.
Il programma concatena il carattere all'inizio della lista con la pila dei caratteri precedentemente già letti e concatenati 
(o la pila vuota), poi vengono chiamate le stesse funzioni ricorsivamente e come argomenti vengono passati la lista meno il primo
carattere letto, la pila aggiornata con l'ultimo carattere inserito e la struttura (lista) con le parti che sono già state riconosciute.
Quando viene riconosciuto un carattere speciale di separazione tra le diverse parti della URI, questo viene scartato e la pila dei
caratteri fino ad ora letti viene capovolta e salvata come stringa nella parte corrispondente della stuttura, nella chiamata alla
funzione successiva viene poi passata al posto della pila una lista vuota per ricominciare a concatenare i caratteri riconosciuti.
La struttura scelta è una lista formata da sette elementi a cui si può accedere con le rispettive funzioni di chiamata (uri-scheme,
uri-userinfo, uri-host ...) ed è presente una funzione "uri-display" che stampa a schermo la struttura nel caso in cui non gli venga 
passato alcuno stream, viceversa stamperà la URI sullo stream di output indicato.
Per quanto riguarda la porta di default 80, ho considerato di inserirla solo nel caso in cui nella URI riconosciuta fosse presente
l'authority (ovvero almeno l'host) senza la specifica della porta, mentre invece nel caso in cui l'authority non è presente, non
inserisco alcuna porta o, ad esempio, quando viene riconosciuto uno scheme speciale "mailto" che non presenta la porta, questa non 
viene aggiunta dal programma.