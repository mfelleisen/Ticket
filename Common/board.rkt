#lang racket

;; The Map guarantees each connection is specified at most once, using string<? to order the
;; connecting cities. Player strategies need access to all possible connections from one city to its
;; neighbors so a list is better than an enforced invariant. 

;; Once the map is fixed, the constraints are guaranteed.
;; BUT, every connection some player has taken is no longer available, so it should be removed.
;; fast lookup:
;; -- use a list first
;; -- and when it gets large use a hash of (connection ..) struct ?

;; ---------------------------------------------------------------------------------------------------
;; a data structure that enforces the logical invariant 
#; {type Graph = (U False [Hashof String [Hashof String Natural]])}
;; maps city to all existing connections; False also means no connection 
#; {type Conn = (connection City City String Natural)}
;; -- from city to city, its connection color, and the number of segments 
#; {type City = (node String Posn)}

;; A city can connect to many other cities. Each bundle of connections can
;; use any or all of the four colors. 
;; -- Mapping from City String to City String to Color String to Segment#
;; -- this would enforce that between two cities each color is used at most once 

;; ASSMMTIONSS:
;; -- cities are consistently named (keep separate city to posn map for drawing)
;; -- connection are two-ways (undirected)
;; -- serialize via string< for (from, to)
;; -- deserialize after checking this assumption

;; ---------------------------------------------------------------------------------------------------
;; representation of the referee's knowledge
;; 
;; State is [Player, ..., Player]
#; {type Player = (player Desitination Destination Rails [Hash Color Natural] [Conn, ..., Conn])}
;; Destination = [List City City] || path exists
;; Rails = Natural, the number of rails left
;; Hash :: how many cards of this color are in a player's hand
;; Conn :: which connections does the player own so that we can 
;; -- compute longest path for player
;; -- determine whether a player connected the destination cities



