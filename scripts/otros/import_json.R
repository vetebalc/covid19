pacman::p_load(RJSONIO)
json_file = getURL("https://raw.githubusercontent.com/isrini/SI_IS607/master/books.json")
json_file2 <-  RJSONIO::fromJSON("0covid.params.json")
json_file2 
