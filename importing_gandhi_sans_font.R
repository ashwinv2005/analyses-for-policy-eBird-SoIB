library(extrafont)

# step 1 - load otf or ttf Gandhi Sans fonts onto the Windows Fonts folder, it automatically installs

font_import(paths = "C:/Windows/Fonts/", prompt = F)

a = fonttable()

db_path <- paste0(system.file(package = "extrafontdb"),"/fontmap/fonttable.csv")
file.show(db_path)

loadfonts()
