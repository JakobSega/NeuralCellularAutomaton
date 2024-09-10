# Nevronski Celični Avtomat

## Opis projekta

V tem projektu sem implementiral **nevronski celični avtomat** v programskem jeziku OCaml. Projekt je implementiran s spletnim vmesnikom, v katerem lahko narišeš poljubno sliko velikost 20 x 20 celic in nato natreniraš nevronsko mrežo, ki se v
uporabi v pravilu za prehod, da iz začetne točke ustvari to sliko.

## Avtomat
Avtomat je teoretični model, ki se uporablja v računalništvu in matematiki predstavlja stroj, ki spreminja svoje notranje stanje na podlagi vhodov in prejšnjega stanja. Nabor stanj je običajno omejen in diskreten.

## Celični avtomati
Celični avtomati so posebna vrsta avtomatov, ki delujejo na prostorski mreži. Njihova stanja se hkrati posodabljajo z enotno uporabo funkcije prehoda stanja, ki se nanaša na stanja njihovih sosedov, ta način posodabljanja
imenujemo sinhrono posodabljanje. Celičnih avtomati so se prvič pojavili v 40. in 50. letih dvajsetega stoletja v delu Johna von Neumanna in njegovega sodelavca Stanisława Ulama.

**Primer: Conwayjeva igra življenja**

Tukaj je primer dvo-dimenzionalnega celičnega avtomata, znanega kot Conwayjeva igra življenja. V tej igri celice na mreži prehajajo med dvema stanjem: živim ali mrtvim. Pravila za prehod stanja so naslednja:

- Celica bo preživela in ostala živa, če ima točno dva ali tri žive sosede.
- Celica bo umrla zaradi osamitve, če ima manj kot dva žive sosede.
- Celica bo umrla zaradi prenatrpanosti, če ima več kot tri žive sosede.
- Celica se bo rodila, če ima točno tri žive sosede.

### Nevronski celični avtomat

Nevronski celični avtomat nadgradi klasični celični avtomat z uporabo nevronskih mrež, namesto fiksnih pravil prehoda klasičnih celičnih avtomatov, se pravila navronskih celičnih avtomatov določajo z nevronsko mrežo, ki je sposobna učenja in prilagajanja.

Matematična definicija nevronskega celičnega avtomata je:

$$ S^{t+1}(i,j) = \text{NN}(S^t(i,j), N(S^t(i,j))) $$

kjer S<sup>t(i,j)</sup> predstavlja stanje celice na poziciji (i,j)v času t, N(S<sup>t(i,j)</sup>) pa predstavlja stanja sosednjih celic. NN je nevronska mreža, ki določi novo stanje celice na osnovi trenutnega stanja in stanj sosednjih celic.

Nevronski celični avtomati se lahko uporabljajo za simulacijo kompleksnih pojavov, kot so rast tkiv ali umetni vzorci, saj omogočajo dinamično prilagajanje in učenje iz podatkov.


### Primeri uporabe celičnih avtomatov

**Biologija**

- **Kozica Conus textile** prikazuje vzorec celičnega avtomata na svoji lupini. Pigmentne celice na robu lupine izločajo barvila v skladu z aktivnostjo svojih sosedov, kar ustvari značilne vzorce.
- **Stoma rastlin** regulirajo vnos in izgubo plinov preko mehanizma celičnega avtomata, kjer vsaka celica (stoma) deluje kot posamezen celica v mreži.
- **Koža glavonožcev** prikazuje premikajoče se valovne vzorce, ki jih lahko simuliramo z dvostanjskimi, dvodimenzionalnimi celičnimi avtomati.

**Kemija**

- **Belousov–Zhabotinsky reakcija** je kemijski oscilator, ki lahko proizvaja geometrijske vzorce, kot so koncentrni krogi in spirale. Te vzorce je mogoče simulirati z celičnimi avtomati.

**Fizika**

- **Probabilistični celični avtomati** se uporabljajo za raziskovanje fluidne dinamike in faznih prehodov. Isingov model je primer celičnega avtomata, ki modelira magnetne lastnosti.

**Generiranje labirintov**

- Nekateri celični avtomati se lahko uporabijo za generiranje labirintov. Na primer, avtomati z pravili B3/S12345 in B3/S1234 ustvarjajo labirinte z dobro določenimi zidovi in hodniki.

## Struktura repozitorija
Znotraj mape **/NCA** se nahaja: 
- **/src**:
  - `definicije` vsebuje module za implementacijo celičnih avtomatov.
  - `ncaSpletniVmesnik` vsebuje module za implementacijo spletnega vmesnika.
  - `ai` vsebuje modul `network.ml`, ki implementira nevronske mreže, omogoča dostop do le ene funkcije, ki se uporablja za treniranje nevronske mreže, ki se uporabi v funkciji prehodnega stanja.
  
- **/html**: Vsebuje `ncaSpletniVmesnik.html` in `ncaSpletniVmesnik.bc.js`

## Navodila za uporabo

### Zahteve

Za zagon projekta potrebujemo OCaml in knjižnici js_of_ocam in vdom.

### Zagon

Premaknemo ve v mapo **/NCA**.
Projekt zaženemo z uporabo orodja `dune`:

   ```bash
   dune build
   ```

Ob zagonu bo program napisal `ncaSpletniVmesnik.bc.js` in ga shranil v mapo **/html**. Da zaženemo aplikacijo odpremo ncaSpletniVmesnik.htm.

### Začetna stran

Na začetni strani kliknemo na gumb **Paint**, ki nas preusmeri na stran za risanje.

### Stran za risanje

Tu nastavimo barvo z uporabo RGB in alpha, in s klikom na celico tabele to celico obarvamo z nastavljeno barvo.
S klikom na gumb `Begin Training` se začne proces treniranja nevronske mreže, ko se treniranje konča, se odpre stran z interaktivno tabelo, ki predstavlja naš nevronski celični avtomat.
S klikom na gumb `Begin Demonstration` se odpre stran se odpre stran z interaktivno tabelo, ki vsebuje našo sliko in predstavlja naš nevronski celični avtomat.

### Stran z interaktivno tabelo

Interaktivna tabela predstavlja celični avtomat, ki smo ga natrenirali na naši sliki. S pritiskom na tipko na tipkovnici, bo avtomat naredil korak, če je tipka število n, bo naredil 2 ** n korakov, 
če pritisnemo na tipko s puščice, se bo funkcija prehodnega stanja nastavila na funkcijo, ki bo vse aktivne točke premikala v smeri puščice. Če kliknemo na celico v tabeli se bo del tabele izbrisal, če kliknemo dvakrat, se bo celica obarvala črno.

## Primeri

Povezave do nakaj primerov nevronskih celičnih avtomatov:

-[Nevronski celični avtomat, ki je bil inspiracija za ta projekt.](https://distill.pub/2020/growing-ca/)
-[Nevronski celični avtomat, ki rekonstruira video.](https://aman-bhargava.com/ai/neuro/neuromorphic/2024/03/25/nca-do-active-inference.html)

