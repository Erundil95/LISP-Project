
 Cristian Ferrari 810932

 PROGETTO "DA REGEX A NFA". 


 ------ PARTE LISP -----------

 Il progetto consiste in 3 funzioni principali:

- (is-regexp re)
	
	Funzione che riceve una regex come argomento e ritorna T qualora 're' sia un'espressione
	regolare, la funzione controlla l'eventuale presenza di 4 operatori riservati (seq, or, star
	e plus), gli operatori Star/Plus sono strettamente unari mentre è stato scelto di gestire il
	caso degenere di Seq/Or, sono quindi accettati anche se dovessero presentare un solo
	argomento
	Sexps sono considerate simboli e sono quindi accettate da is-regexp

- (nfa-regexp-comp re)
	
	Funzione responsabile per la creazione dell'NFA come una lista di liste in formato:
	
	             ((START 0) (DELTA n <simbolo> m) ... (FINAL 1))
	
	Questa scelta è stata fatta principalmente per la semplicità d'implementazione e lettura.
	Gli stati intermedi sono generati utilizzando la variabile *gensym-counter* associata alla
	funzione Gensym e incrementata tramite (gensympp).
	Per prima cosa la funzione controlla che 're' sia una regex accettata, crea poi stato 
	iniziale e finale chiamando la funzione (nfa-comp re start final).
	
	NFA-COMP 
	
	scansiona 're', gestisce il caso base in cui 're' è semplicemente un atomo, una 
	lista o vuota, spartisce altrimenti il compito di creare l'automa in base all'operatore
	in testa alla lista della regex, ogni handler function gestisce il proprio caso secondo 
	l'agoritmo di Thompson e richiama nfa-comp in caso incontri un'altra lista innestata
	con un altro operatore come testa, operatori non riservati sono considerati semplicemente 
	come simboli
	Gli stati intermedi sono generati usando la parte numerica di gensym prendendo la variabile
	*gensym-counter*, quest'ultima è incrementata dalla funzione (gensympp) che richiama
	gensym e ritorna il valore di *gensym-counter*, questi valori intermedi sono poi passati
	appropriatamente come stati iniziali/finali per chiamate ricorsive

- (nfa-test fa input)
	
	Funzione che testa se 'input' è accettato o no dall'automa 'fa', la prima funzione è 
	semplicemente un involucro che gestisce gli errori nel caso in cui 'fa' non sia un
	automa, passa poi 'fa' rimuovendo stato iniziale e finale e passando una lista di
	stati correnti come argomento (nel primo caso conterrà solo lo stato iniziale 0
	ovviamente)

	NFA-TEST-STATE
	
	Funzione che controlla effettivamente se 'input' è accettato da 'fa' nel caso in cui
	l'input sia consumato e la lista 'states' contenga lo stato finale (1)
	richiama funzioni per la transizione per un simbolo di input e per la generazione
	della e-closure degli stati correnti

	NFA-DELTA
	
	Funzione involucro per ciclare la transizione per un simbolo in input su tutti gli stati
	correnti contenuti nella lista 'states'

	NFA-TRANSITION

	Funzione che esegue l'effettiva transizione dato un simbolo e un singolo stato corrente,
	cicla la lista dell'nfa cercando se esista una DELTA-transizione compatibile, altrimenti
	ritorna nil

	E-CLOSURE
	
	Funzione responsabile per la generazione dell'e-closure degli stati correnti, viene
	chiamata dopo ogni transizione per gestire le possibili e-transizioni contenute nell'NFA
	Ritorna la lista di stati correnti dopo aver eseguito la e-closure

Per finire si possono trovare poi delle semplici funzioni ausiliarie per incrementare gensym e per
Rendere più leggibile il codice controllando gli operatori in funzioni separate, controlli per 
operatori unari e multipli sono separati per una questione di controllo di errori







