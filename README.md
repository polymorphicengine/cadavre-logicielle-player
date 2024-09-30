# Cadavre Logicielle (client)

Cadavre Logicielle is a system for collaborative, distributed live coding in a local network. It is inspired by the surrealist parlour game *cadavre exquis* and is an adaptation of that game to the medium of code. In *cadavre exquis*, or Exquisite Corpse, players take turns to write a sentence on a piece of paper and hide part of the sentence by folding the paper before passing it on to the next player. After a couple of rounds the story is unfolded and read out loud.

There is a server and client application. The server application can be found [here](https://github.com/polymorphicengine/cadavre-logicielle).

In Cadavre Logicielle every player edits their own document but the state of the interpreter is shared. When players define an expression, the name and type of the expression is shared with all players and displayed in a small window next to their editors. Other players can then use these expressions via their name in their own code, without knowing them explicitly. Since the underlying programming language is strongly typed, knowing the type of an expression is enough for players to write interpretable code.

-----------------------------------

**Example.**

  Player A defines an expression executing the following code, defining a simple sequence of notes:

~~~~ {.haskell}
notes :: Pattern Note
notes = "[c6 f af [~ af f]]"
~~~~

This will broadcast the message "*Player A folded the document and revealed notes :: Pattern Note*" to all connected players. Another player could then make use of the defined expression *notes*, only knowing that it must be some sequence of notes. Someone could then run the following code to modify and play the sequence:

~~~~ {.haskell}
d1 $ every 2 rev $ n notes # s "superpiano"
~~~~

The system will then inform Player A that someone is using their definition, but Player A will not know how. This results in a situation where everyone has imperfect knowledge of the current state of the system.

-----------------------------------


Above example makes the connection to Exquisite Corpse apparent. While a fold in the original game is used to hide parts of an unfinished sentence, here the fold hides an expression behind its name and type. Since these names can be used multiple times by different players and anywhere in their code, this results in a distributed and non-linear version of the original game. Since the expressions represent musical patterns, the exquisite corpse is in a constant state of unfolding through sound.

Definitions themselves can also be live coded, as long as the type of the defined expression remains constant. Such changes will have immediate effect in all places that make use of the expression, opening  interesting possibilities for interwoven interactions.

Another part of the players interface is a chat window, where messages from the server are displayed. Inspired by early multi-user dungeon games, players also have the ability to use special commands like *say*  to broadcast messages to other players, or *look* to list the names of all connected players. The server itself has a similar interface that displays such messages, the state of the interpreter and a list of the connected players and the last code they sent to the server. The interface of the server can be projected for a potential audience to follow along and enjoy the conversational aspects of such a performance.
