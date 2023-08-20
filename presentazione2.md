<a name="br1"></a> 

Social analysis of Covid-

19

on YouTube



<a name="br2"></a> 

Nel periodo tra il 1 dicembre 2019 e il 14 febbraio 2020, sono

stati raccolti da YouTube circa 84mila video e 2milioni di

commenti inerenti al corona virus. In seguito sono state fatte

alcune analisi per cercare dei trend tra i canali che pubblicano o

dei comportamenti simili tra gli utenti che commentano.



<a name="br3"></a> 

Distribuzione delle attività

Come prima analisi si è andato a osservare come

variano le interazioni degli utenti. Si può notare

che la distribuzione di commenti, like e dislike è

l’esatta rappresentazione di una distribuzione

power law. Invece è possibile notare che la

distribuzione delle view è molto più simile ad una

curva gaussiana. Però se si va a concentrarsi al

punto di 1000 interazioni, c’è una leggera

discrepanza nelle attività riguardanti le view.



<a name="br4"></a> 

Il grafico in alto prende le interazioni da 1 a 1000 e come

scritto prima l’andamento di commenti, like e dislike è quello di

una distribuzione power law, mentre per le view è quello di una

distribuzione gaussiana. Però il taglio a 1000 interazioni ci

mostra che da quel punto in poi (figura in basso) la

progressione delle attività seppur con numeri diversi è

praticamente identica.

Questo ci può far pensare che per poche interazioni ci sia

qualcosa che va a diversificare il loro andamento. Mentre con

l’aumentare delle interazioni le varie attività inizino ad

assomigliarsi.



<a name="br5"></a> 

View, like, dislike e commenti a confronto

Successivamente si è analizzato il variare

delle interazioni degli utenti in funzione delle

tempo.

Ancora una volta si confrontano commenti, like,

dislike e view che gli utenti effettuano sui vari

video presi in esame.

Dividendo per 800 la progressione delle views,

di 20 quella dei like, e di 4 quella dei commenti

si ottiene che il loro andamento è pressochè

indentico. Ovvero da circa il giorno in cui è

stato trovato il primo paziente positivo al covid-

19 in europa la frequenza delle interazioni è

drasticamente aumentata. Raggiungendo il

picco massimo circa tra 2020-01-28/29/30. Per

poi diminuire abbastanza regolarmente nei

giorni a seguire. Questo può far ipotizzare che

in quel periodo ci sia stato un aumento dei

video pubblicati da parte di vari canali.



<a name="br6"></a> 

Quante persone nuove nel tempo

commentano?

Nella figura è rappresentato l’andamento delle

nuove persone che commentano e i nuovi video

pubblicati giorno per giorno.

Dividendo il numero di utenti per 40, è possibile

notare che la distribuzione di nuovi utenti e video

è praticamente identica.

Inizialmente regolare con un numero abbastanza

ridotto di commenti e video. Poi circa con il primo

caso in Europa (24-01-20 dal corriere della sera)

c’è un impennata di commenti e video per i primi

giorni che poi tende a ridursi nei giorni successivi.

Questo va a verificare l’ipotesi precedente. Infatti

con l’aumento di video pubblicati, il numero di

persone è aumentato e di conseguenza anche le

loro attività.



<a name="br7"></a> 

Quali sono le parole più digitate dagli

utenti su Youtube?

È stata fatta un analisi sulla frequenza delle parole utilizzate dagli utenti di youtube nei commenti dei video

relativi al coronavirus. Il risultato è che ‘china’ è la parola più digitata, subito a seguire ci sono ‘virus’ e

‘people’. È subito possibile notare dalla figura di destra che le persone tendono a utilizzare più

frequentemente parole che non sono coronavirus o infected o health ma si incentrino su china e virus.

Infatti nella tabella a destra che riporta le 10 parole più utilizzate ’coronavirus’ non appare nemmeno.

Questo potrebbe far pensare che la gente cerca di non utilizzare parole



<a name="br8"></a> 

R0 totale per intervalli

Per capire come nel periodo dal 1-12-2019 al 14-

02-2020 si sono diffuse le informazioni riguardanti

il COVID-19 sulla piattaforma di Youtube, è stata

fatta un analisi basandosi sul modello di diffusione

epidemico. Solitamente questi modelli si basano

sul valore di R0, ovvero il tasso netto di

riproduzione, il quale indica il numero di nuovi

infetti giornalieri che un singolo individuo può

causare durante il suo periodo di infezione.

Per questo calcolo è stata utilizzata una funzione

che analizza la progressione delle interazioni delle

persone giorno per giorno. Il risultato è che i valori

di R0 che variano tra il 3.23 e il 17.07, da questo si

può dedurre che la diffusione è estremamente

rapida in alcuni momenti, per poi rallentare ma non

tenderà a svanire.

Infatti un valore di R0 > 1 nel modello epidemico

indica che l’infezione tenderà a diffondersi nella

popolazione.



<a name="br9"></a> 

Proiezione sui canali YouTube

In seguito ci si è concentrati sull’osservare il

comportamento di alcuni canali YouTube. Nel grafo in

figura i nodi rappresentano i diversi canali presi in

esame, mentre gli archi sono i collegamenti tra essi.

Un collegamento esiste quando lo stesso utente

scrive un commento su due diversi video di due

diversi canali.

Lo spessore di ogni arco dipende da quanti utenti

hanno commentato su dei video dei canali alle

estremità di esso. La dimensione dei nodi dipende da

quanti collegamenti differenti ricevono.

È facile notare che la maggioranza dei nodi sono

molto piccoli, però emergono comunque alcuni nodi

con dimensioni rilevanti. Questo può indicare che

alcuni utenti che sono attivi su canali minori, si

assembrano su canali più grandi.

In più si riscontra tendono a raggrupparsi in alcuni

nodi differenti, ciò può portare alla formazione di

comunità.



<a name="br10"></a> 

Community detection

Tutti i canali in esame sono stati suddivisi in delle comunità in

base alle loro peculiarità.

Partendo dalla proiezione sui canali, sono stati applicati vari

algoritmi per la ricerca delle comunità: fastgreedy (fg) ,

walktrap (wt), multilevel (ml) e label propagation (lp).

Successivamente sono stati confrontate le diverse suddivisioni

in comunità ottenendo i seguenti risultati:

\- fg – wt : 0.902

\- fg – ml : 0.944

\- fg – lp : 0.786

\- wt – ml : 0.913

\- wt - lp : 0.818

\- ml – lp : 0.795

Dai confronti tra i diversi algoritmi si vede che le comunità che

si vengono a formare sono abbastanza simili nonostante

l’utilizzo di metodi che guardano caratteristiche differenti. Infine

è stata fatta l’intersezione dei diversi risultati degli algoritmi

applicati e quest’ultimo risultato è stato rappresentato qui

affianco.

Apparentemente ci sono solo 7 grandi comunità, il che fa

pensare che le persone hanno opinioni non troppo diverse tra



<a name="br11"></a> 

Analizzando solo le 4 comunità più

numerose

Dopo aver suddiviso i vari canali i comunità,

sono stati prese in esame solo le 4 comunità

più grandi.

Nella figura a lato sono stati analizzati i

commenti sotto i video delle relative comunità e

si è contata la frequenza con la quale le parole

sono state digitate dagli utenti.

È possibile notare che nel primo e nel quarto

cluster le parole digitate dagli utenti in essi

siano le stesse digitate nella maggior parte dei

commenti analizzati

Mentre nel secondo e terzo le parole come

people e virus passano in secondo piano, e ci

sono poche parole, rispetto agli altri due cluster,

che prevalgono sulle altre. Inoltre sembra che

anche la lingua sia cambiata, nel secondo si

può notare una prevalenza di parole di lingue

orientali. Nel terzo una prevalenza di parole di

lingue di origine ispanica.

Questa suddivisione delle comunità in lingue



<a name="br12"></a> 

Cumulativa dei 4 cluster più grandi

È possibile notare che principalmente nei primi

tre cluster avviene un rapido incremento al

incirca in concomitanza del primo caso in

Europa (20 gennaio 2020. Mentre nel quarto

avviene con un lieve ritardo.

In tutti e quattro i casi la diminuzione dei nuovi

utenti non è lineare ma probabilmente la

pubblicazione di nuovi post, provoca nuovi picchi.



<a name="br13"></a> 

R0

1\. 1.002603

2\. 0.9996495

3\. 0.9998838

4\. 0.9998975

Per ognuno delle 4 comunità più numerose, si è

cercato di calcolare il tasso di riproduzione netto.

In tutti i cluster, il valore di R0 è praticamente lo

stesso.

Infatti nonostante il numero di persone coinvolte sia

differente, l’andamento è il medesimo. La diffusione

delle informazioni diventa estremamente rapida e

‘’contagiosa’’ a partire dal giorno in cui viene

annunciato il primo caso di COVID-19 in Europa. Poi

col passare del tempo tende a diminuire fino ad

azzerarsi.

Se si fosse trattato di un virus avremmo potuto dire

che la sua diffusione è piuttosto lenta fino ad un certo

evento che ne causa una diffusione epidemica

rapidissima. Poi con il raggiungimento del picco

massimo di infetti, l’epidemia inizia lentamente a

guarire.



<a name="br14"></a> 

R0 parziale 1^ cluster

Come calcolato precedentemente si è

nuovamente ricercato un valore di R0 ma

prendendo in esame un cluster alla volta.

Prendendo in esame solamente il primo

cluster, è stato calcolato R0 per ogni

intervallo di 24 ore. Rispetto al modello in

cui R0 viene calcolato complessivamente,

qui dipende dal valore precedente. Anche

qui i valori variano leggermente tra il 0.73 e

il 1.34 quindi circa 1. Questo risultato

potrebbe far pensare che quindi la

diffusione di informazioni all’interno di

questo cluster tenda scemare. Invece se si

osservano i valori di R0 calcolati si può

notare che la maggioranza è superiore a 1.

Quindi se si trattasse di un infezione

significherebbe che l’epidemia avanzerebbe

mentre nel nostro caso implica che la

propagazione delle informazioni all’interno

di questa comunità continuerà.

R0 calcolati:



<a name="br15"></a> 

R0 parziale 2^ cluster

Anche qui come per il primo cluster, si

sono calcolati i valori di R0 in funzione del

tempo e si è disegnata la curva epidemica.

Ad una prima osservazione si potrebbe

dire che tutti i valori di R0 sono all’ incirca

1 ma prestando attenzione si vede che

tutti sono maggiori di zero. Ciò significa

che la diffusione di informazioni si

espanderà.

La curva secondo il modello epidemico

presente in figura, rappresenta proprio ciò

che è appena stato detto.



<a name="br16"></a> 

R0 parziale 3^ cluster

Come per i precedenti casi, si è calcolato

R0 e la curva epidemica. Questa volta

però la maggior parte dei valori di R0 è

inferiore a 1 perciò come si può vedere dal

grafico la diffusione delle informazioni non

si dilagherà molto. Ad eccezione di un

lieve incremento in concomitanza col

primo caso in Europa di Coronavirus, la

propagazione si fermerà rapidamente

subito dopo.



<a name="br17"></a> 

R0 parziale 4^ cluster

Al contrario dei cluster precedenti, si nota

un rapido aumento nella divulgazioni delle

informazioni fin dai primi giorni esaminati, al

quale corrisponde un valore di R0 di 2.13.

Successivamente il propagarsi delle

informazioni continua ad aumentare molto

rapidamente, infatti gli R0 calcolati , ad

esclusione di 3, sono tutti maggiori di 1.

Questo nel modello epidemico implica che

l’epidemia si diffonderà facilmente, ed è

proprio ciò che accade nel grafico.

Per concludere si può notare che

nonostante le curve epidemiche siano

diverse, il valore di R0 in tutti e 4 i cluster

può essere approssimato ad 1 ma nella

maggioranza dei casi gli è superiore.

Il che implica che il numero di persone

coinvolte aumenterà sempre e dal punto di

vista epidemico, l’epidemia persiterebbe.



<a name="br18"></a> 

Probability density in fuzione del lifetime

sui video

Mettendo sempre a confronto i 4 cluster più

numerosi, si è osservata la densità di probabilità

in funzione del lifetime. Il lifetime è stato

calcolato come la differenza di tempo tra l’ultimo

e il primo commento postato sotto un video, è

espresso in giorni.

L’andamento è pressoché identico per tutti e 4 i

grafici, seguendo il modello a coda lunga.

Inoltre si può vedere che prevale un lifetime

basso. Quindi nella maggior parte dei video,

l’attività degli utenti è concentrata nel giorno di

uscita del video, per andare a diminuire col

passare del tempo.



<a name="br19"></a> 

Probability density in fuzione del lifetime

sugli utenti

Per ogni utente, che ha commentato un video nelle

rispettive comunità di canali, è stato calcolato il

lifetime come la differenza tra la data del primo e

l’ultimo commento postato tra video della stessa

comunità, è espresso in giorni.

L’andamento delle 4 curve è molto simile, anche qui

ricorda una funzione power law. Infatti la maggior

parte degli utenti tende ad avere un lifetime molto

basso e pochi utenti commentano



<a name="br20"></a> 

Lifetime per views, likes e commenti

Il lifetime è calcolato come la differenza tra la

data del primo e ultimo commento postato sotto

un video, è espresso in giorni.

Il numero di view è stato ridotto di 400 volte, il

numero di likes di 10 volte per poterli

confrontare con il numero di commenti.

È facile osservare all’interno di ogni comunità la

tendenza di views, likes, e commenti è la stessa.

Inoltre la grande maggioranza dell’attività degli

utenti si concentra nei primi 10 giorni dalla

pubblicazione del video. Successivamente,

diminuisce drasticamente, con numerosi alti e

bassi.



<a name="br21"></a> 

Polarizzazione degli utenti

Il grafico mostra la distribuzione di probabilità

dell’attività degli utenti nei vari cluster. È

possibile notare che gli utenti non tendono a

concentrarsi su una sola comunità ma anzi si

distribuiscono.

In particolare nella prima, nella seconda e nella

quarta pochi sono attivi solo all’interno di

un’unica comunità ma molti sono ben distribuiti.

Invece nella terza la densità degli utenti che

sono attivi solamente in quella comunità e che

hanno poca/nulla attività in essa è molto simile.

Ma di nuovo la maggioranza tende a non

focalizzarsi in solo questa comunità.

Perciò possiamo dire che tra i cluster presi in

esame, non c’è una forte polarizzazione.

