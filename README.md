# Identification des tendances de sécheresse hydrologique sur les stations hydrométriques

Dans cette étude, nous utilisons les données de débit des stations hydrométriques des départementsv Français. Les informations concernant les stations et leurs chroniques sont bancarisées dans l’hydroportail. 
Elles sont récupérées à partir de l’API Hydrométrie de Hub’eau. Les stations hydrométriques sont des points de mesure permettant de surveiller en temps réel ou en différé les débits et niveaux des cours d’eau en France. 

Ces stations sont équipées de capteurs qui mesurent plusieurs paramètres hydrologiques :

1	Le niveau de l’eau (hauteur hydrométrique)

2	Le débit (volume d’eau écoulé par seconde)


Afin de déterminer si les indicateurs de sécheresse suivent une tendance monotone significative (une tendance monotone fait référence à une séquence ou une fonction qui ne change pas de direction). 
Une tendance monotone significative peut donc indiquer un changement dans les ressources en eau, influencé par des facteurs tels que le changement climatique, les pratiques de gestion de l’eau ou les modifications de
l’utilisation des terres. Dans cette étude, les tendances sont étudiées grâce à un test de Mann-Kendal (tendance monotone significative ou non) et un test de Sen-Theil
(pente de la droite de tendance : positive, négative ou nulle, qui détermine s’il y a une dégradation ou une amélioration).

