#lang racket

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

