#lang scribble/manual

@(require "shared.rkt")
@(require "overview.rkt")
@[define ticket-to-ride-url "https://www.daysofwonder.com/tickettoride/en/usa/"]

@; -----------------------------------------------------------------------------

@title[#:tag "Trains Project"]{The Trains Competition}

@author{Matthias Felleisen}

@link[ticket-to-ride-url]{Ticket to Ride} is a board game for players eight
and older. 

This repository is a framework for programming @emph{Trains} competitions,
specifically software players, that compete in single-game, knock-out
tournaments. The game is a variant of ``Ticket to Ride;'' use the human-player
graphical interface to get to know it.

Participants design automated players that run on their desktops and connect to
a (remote) @emph{Trains} server. This server will run "knock out" tournaments of
games consisting of two to four players each. Any misbehavior of a
player---non-responsiveness due to bugs or cheating---results in immediate
termination. So the competition is not just about writing great strategies but
also delivering robust code.

@; ---------------------------------
@centerline[(overview 0.9)]
@; ---------------------------------

