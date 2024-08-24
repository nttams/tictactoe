# tictactoe

This is a dead simple version of tic tac toe game  
- It supports playing over HTTP only, the server must be started before any client can connect and start playing  
- The HTTP server is stateless, meaning it does not store any game status, it completely depends on data in client request, then provides move/win/lose state accordingly. Because of this, it can accept multiple client at the same time.  
- The HTTP client only has CLI interface