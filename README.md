# baza-rolnikow
Application written in R for store, search and modify data in data base stored as R object (.fst or .qs file). File .qs should be used on Windows machines and .fst on Linux. The latter is faster, but because of problems with characters encoding on Windows, cannot be used with Polish language. By default, in the main folder, app.R with .qs files is used, version for Linux is in the "Linux" folder. Because SQL is not used, this is for relatively small data base (ca. 10 mln rows). Interface in Polish.  
Application was written as the place where user can store information about respondents who participated in the CATI surveys. Name "Baza rolników" is because the intention was to store only information about farmers who participated in the CATI surveys, but it could be used for any respondents.  
Application is quite complex (1600 lines of code) and is no finished, because needs more tests.  
The best place to start is to read the Instruction - file "Instrukcja_baza_rolnikow.pdf" in the "Instrukcja" folder. Instruction is in Polish as well.  
Application can be seen using this link: XXXXXXXXXXXXXXXXXXXXXXX. Application is hosting on the shinyapps.io using free plan, so it will be much slower than in reality, because only 1GB of RAM and one thread of processor is available.
