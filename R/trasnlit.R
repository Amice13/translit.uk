# The rules of transliteration were introduced by the Decree #55 (27.01.2010) by The Cabinet of the Minister of Ukraine
# For the reference: https://zakon.rada.gov.ua/laws/show/55-2010-%D0%BF

# Cyryllic letters pattern
cyryllic_letters_pattern <- '[\u0430-\u044f\u0457\u0491\u0454\u0457\u0451]'
cyryllic_big_letters_pattern <- '[\u0410-\u042F\u0404\u0406\u0407\u0490]'
two_big_pattern <- paste0(cyryllic_big_letters_pattern, '[\u0410-\u042F\u0404\u0406\u0407\u0490A-Z]')

# The list of letters which must be handled differently if they are in the beginning of the word
first_letters_pattern <- '^([\u0454\u0457\u0439\u044e\u044f\u0407])'
first_letters_pattern2 <- '([^a-z\u0430-\u044f\u0454\u0456\u0457\u0491\u0451])([\u0454\u0457\u0439\u044e\u044f])'

# The pattern "зг" must treated in a different way.
# The direct transliteration "zh" means "ж". Therefore, the proper transliteration must be "zgh"
zg_letters_pattern <- '\u0437\u0433'

# All apostrophes must be removed
# Note that the apostrophes must be unified in advance
replace_apostrophe_pattern <- '([\u0430-\u044f\u0454\u0456\u0457\u0491])[\'\u2019]([\u0430-\u044f\u0454\u0456\u0457\u0491])'

# Transliteration of the first letters
first_letters <- data.frame(c(
  'Ye', 'Yi', 'Y', 'Yu', 'Ya', 'ye', 'yi', 'y', 'yu', 'ya'
))
rownames(first_letters) <- c(
  '\u0404', '\u0407', '\u0419', '\u042e', '\u042f',
  '\u0454', '\u0457', '\u0439', '\u044e', '\u044f')

# Transliteration of Ukrainian letters on other positions
other_letters <- data.frame(c(
  'A', 'B', 'V', 'H', 'G', 'D', 'E', 'Ie', 'Zh', 'Z', 'Y', 'I',
  'I', 'I', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U',
  'F', 'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '', '', 'Y', 'E', 'Iu',
  'Ia', 'a', 'b', 'v', 'h', 'g', 'd', 'e', 'ie', 'zh', 'z',
  'y', 'i', 'i', 'i', 'k', 'l', 'm', 'n', 'o', 'p', 'r', 's',
  't', 'u', 'f', 'kh', 'ts', 'ch', 'sh', 'shch', '', '', 'y',
  'E', 'iu', 'ia'
))

rownames(other_letters) <- c(
  '\u0410', '\u0411', '\u0412', '\u0413', '\u0490', '\u0414', '\u0415',
  '\u0404', '\u0416', '\u0417', '\u0418', '\u0406', '\u0407', '\u0419',
  '\u041a', '\u041b', '\u041c', '\u041d', '\u041e', '\u041f', '\u0420',
  '\u0421', '\u0422', '\u0423', '\u0424', '\u0425', '\u0426', '\u0427',
  '\u0428', '\u0429', '\u042c', '\u042a', '\u042b', '\u042d', '\u042e',
  '\u042f', '\u0430', '\u0431', '\u0432', '\u0433', '\u0491', '\u0434',
  '\u0435', '\u0454', '\u0436', '\u0437', '\u0438', '\u0456', '\u0457',
  '\u0439', '\u043a', '\u043b', '\u043c', '\u043d', '\u043e', '\u043f',
  '\u0440', '\u0441', '\u0442', '\u0443', '\u0444', '\u0445', '\u0446',
  '\u0447', '\u0448', '\u0449', '\u044c', '\u044a', '\u044b', '\u044d',
  '\u044e', '\u044f'
)

# Transliteration of "зг"
zg_letters <- data.frame(c('Zgh', 'zgh', 'zGh', 'ZGH'))
rownames(zg_letters) <- c('\u0417\u0433', '\u0437\u0433', '\u0437\u0413', '\u0417\u0413')

# Function to replace the string in position
replace_by_position <- function(text, start, end, replacement) {
  paste0(substr(text, 1, start - 1), replacement, substr(text, end + 1, nchar(text)))
}

#' Transliterate string in Ukrainian
#'
#' This function converts a given string from Ukrainian Cyrillic to
#' Latin characters using a specific set of transliteration rules.
#'
#' @param string A string in Ukrainian
#' @return A string in Latin
#' @export

translit <- function (string) {
  s <- string
  if (!is.character(s)) stop('This variable is not a character type')
  if (regexpr(cyryllic_letters_pattern, s, ignore.case = T, perl = T) == -1) {
    return(s)
  }
  # Replace apostrophes
  s <- gsub(replace_apostrophe_pattern, '\\1\\2', s, ignore.case = T, perl = T)
  
  # Replace all letters 'зг'
  repeat {
    position <- regexpr(zg_letters_pattern, s, ignore.case = T, perl = T)[1]
    if (position < 1) break
    str <- substr(s, position, position + 1)
    letter_to_check <- substring(s, position + 2, position + 2)
    capitalize <- regexpr(
      cyryllic_big_letters_pattern,
      substring(s, position + 2, position + 2),
      perl = T
    )[1] > 0
    replacement <- zg_letters[str, ]
    if (capitalize) replacement <- paste0(substr(replacement, 1, 1), 'GH')
    s <- replace_by_position(s, position, position + 1, replacement)
  }
  
  # Replace the first letter
  repeat {
    position <- regexpr(first_letters_pattern, s, ignore.case = T, perl = T)[1]
    if (position < 1) break
    capitalize <- regexpr(
      cyryllic_big_letters_pattern,
      substring(s, 2, 2),
      perl = T
    )[1] > 0
    replacement <- ifelse(
      capitalize,
      toupper(first_letters[substr(s, 1, 1),]),
      first_letters[substr(s, 1, 1),]
    )
    s <- replace_by_position(s, 1, 1, replacement)
  }
  repeat {
    position <- regexpr(first_letters_pattern2, s, ignore.case = T, perl = T)[1]
    if (position < 1) break
    str <- substr(s, position + 1, position + 1)
    letter_to_check <- substr(s, position + 2, position + 2)
    if (letter_to_check == "") letter_to_check <- substr(s, position, position)
    capitalize <- regexpr(
      cyryllic_big_letters_pattern,
      letter_to_check,
      perl = T
    )[1] > 0
    replacement <- ifelse(
      capitalize,
      toupper(first_letters[str, ]),
      first_letters[str, ]
    )
    s <- replace_by_position(s, position + 1, position + 1, replacement)
  }
  
  # Replace other symbols with mixed pattern
  repeat {
    position <- regexpr(two_big_pattern, s, ignore.case = F, perl = T)[1]
    if (position < 1) break
    str <- substr(s, position, position + 1)
    replacement <- paste0(toupper(other_letters[substr(str, 1, 1),]), substr(str, 2, 2))
    s <- replace_by_position(s, position, position + 1, replacement)
  }

  # Replace other symbols with mixed pattern
  for (i in 1:nrow(other_letters)) {
    s <- gsub(rownames(other_letters)[i], other_letters[i,], s)
  }
  return(s)
}
