# Informations

Version de Java : Oracle OpenJDK 20.0.2
Version de Scala : 3.3.4

Projet en un seul fichier : Exécuter l'objet 'Scalix' dans le package 'scalix'
Projet en différents objets fonctionnels : Exécuter l'objet 'MainApp' dans le package 'scalixObject'

Pas d'autre dépendence que json4s.

IA (Copilot) utilisée pour refactoriser le code et résoudre les erreurs de compilation.

# Commentaires de la section 5 :

Avantages de l'architecture objet: 
- Les classes Actor et Movie encapsulent les données et les comportements
- Meilleure lisibilité et gestion des responsabilités de chaque classe (meilleure maintenabilité)
- Tests et débogages simplifiés (on pourrait imaginer des tests unitaires sur chaque classe)

Inconvénients de l'architecture objet:
- Plus grande complexité (beaucoup de fichiers à gérer)
- Plus grosse consommation de mémoire pour chaque instance d'une classe
