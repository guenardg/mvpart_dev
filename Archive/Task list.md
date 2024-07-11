1. Transférer les fichiers de jeux de données:

| Nom               | État  |
| :----             | :---: |
| car.test.frame.R  | OK    |
| kyphosis          | OK    |
| solder            | OK    |
| spider            | OK    |

2. Transférer les variables globales: OK

3. Transférer les fonctions génériques:

- Mettre les liens à jour lorsque les méthodes auront été transférées.

4. Transférer rpart.R:

4.1 Transférer les dépendances internes:
  - transférer na.rpart      -> strictement interne,
  - transférer rpart.matrix  -> strictement interne,
  - transférer rpart.control -> exporté avec documentation; sans exemples,
  - transférer rpartcallback -> strictement interne,
  - transférer rpart-methods -> strictement interne,
  - transférer formatg       -> strictement interne.

4.2 Transférer les méthodes afin de pouvoir exécuter les exemples.

4.3 






