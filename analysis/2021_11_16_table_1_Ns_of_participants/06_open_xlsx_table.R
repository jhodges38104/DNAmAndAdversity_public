
table1 <- openxlsx::read.xlsx(
  xlsxFile = 
    stringr::str_replace_all(
      table_shell_filepath,
      " ",
      "\\ "),
  colNames = F)

