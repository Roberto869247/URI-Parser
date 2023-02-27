Lo scopo di questo progetto è di realizzare due librerie che costruiscono delle strutture che 
rappresentino internamente delle URI a partire dalla loro rappresentazione come stringhe.
Costruire un parser per le URI semplificate richiede la costruzione di un automa a stati finiti.
Sia in Lisp che in Prolog la costruzione di automi a stati finiti è facilitata dal 
linguaggio. E' perciò richiesta una sequenzialità nella analisi e scomposizione della stringa in 
input, che va analizzata carattere per carattere per comporre una struttura adeguata a memorizzarne 
le componenti: ad esempio, l’eventuale authority (e la sua composizione interna in userinfo, host
e port) va determinata dopo l’individuazione dello scheme, ed il meccanismo di ricerca non deve 
ripartire dalla stringa iniziale ma bensì dal risultato della ricerca dello scheme stesso.
A partire dalla grammatica data, un’URI può essere scomposta quindi nelle seguenti componenti:
1. Scheme
2. Userinfo
3. Host
4. Port
5. Path
6. Query
7. Fragment
Da considerare poi alcune sintassi speciali da prendere in considerazione:
1. con Schema = "mailto"
2. con Schema = "news"
3. con Schema = "tel" o "fax"
4. con Schema = "zos"
