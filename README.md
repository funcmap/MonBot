# MonBot

This program is written in the functional programming language Haskell (http://haskell.org).
Upon start with the correct parameters it connects to the ejabber server and listens for requests. 
Only a compiled-in list of users may talk to this bot; this has been chosen for security reasons.
If one belongs to the known users, the commands that are understood by the bot are limited.
For simplicity and security I have chosen to define the allowed commands as data types.
All commands (and parameters) start with an uppercase letter. If more than two words are needed, add parentheses.

Since this is all in early stage and my knowledge in Haskell is still limited, I would really like to hear your comments and follow your advices.


# configuration

in the file app/MonBot.hs 
* fill in the list of allowed users that may talk to this bot
* under Cmd: named executables and parameters to run
* under Sh: shell commands

# commands

* Df	- df; shows filesystem status
* Free	- free; shows memory allocation
* Uptime	- uptime; shows machine time since boot
* Date 	- date; shows machine clock

* Monit Summary 		- summary
* Monit (Status Proc01)	- status of process "proc01"
* Monit (Start Proc01)	- start process "proc01"
* Monit (Stop Proc01)	- stop process "proc01"
* Monit (Restart Proc01)	- restart process "proc01"
