# The rules of transliteration were introduced by the Decree #55 (27.01.2010) by The Cabinet of the Minister of Ukraine
# For the reference: https://zakon.rada.gov.ua/laws/show/55-2010-%D0%BF

# The list of letters which must be handled differently if they are in the beginning of the word
first_letters_pattern <- '^([єїйюяЇ])'
first_letters_pattern2 <- '([^а-яєіїґё\']\'?)([єїйюя])'

# The pattern "зг" must treated in a different way.
# The direct transliteration "zh" means "ж". Therefore, the proper transliteration must be "zgh"
zg_letters_pattern <- 'зг'

# All apostrophes must be removed
# Note that the apostrophes must be unified in advance
replace_apostrophe_pattern <- '([а-яєіїґ])[\'’]([а-яєіїґ])'

# Transliteration of the first letters
first_letters <- data.frame(c(
  'Ye', 'Yi', 'Y', 'Yu', 'Ya', 'ye', 'yi', 'y', 'yu', 'ya'
))
rownames(first_letters) <- c('Є', 'Ї', 'Й', 'Ю', 'Я', 'є', 'ї', 'й', 'ю', 'я')

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
  'А', 'Б', 'В', 'Г', 'Ґ', 'Д', 'Е', 'Є', 'Ж', 'З', 'И', 'І', 'Ї',
  'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х',
  'Ц', 'Ч', 'Ш', 'Щ', 'Ь', 'Ъ', 'Ы', 'Э', 'Ю', 'Я', 'а', 'б', 'в',
  'г', 'ґ', 'д', 'е', 'є', 'ж', 'з', 'и', 'і', 'ї', 'й', 'к', 'л',
  'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш',
  'щ', 'ь', 'ъ', 'ы', 'э', 'ю', 'я'
)

# Transliteration of "зг"
zg_letters <- data.frame(c('Zgh', 'zgh', 'ZGH'))
rownames(zg_letters) <- c('Зг', 'зг', 'ЗГ')

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

  # Replace apostrophes
  s = gsub(replace_apostrophe_pattern, '\\1\\2', s)

  # Replace all letters 'зг'
  while (regexpr(zg_letters_pattern, s, ignore.case = T, perl = T)[1] > 0) {
    match = regexpr(zg_letters_pattern, s, ignore.case = T, perl = T)
    str = substr(s, match[1], match[1] + 1)
    s = sub(substr(s, match[1], match[1] + 1), zg_letters[str,], s)
  }

  # Replace first letter
  
  match = regexpr(first_letters_pattern, s, ignore.case = T, perl = T)
  if (match[1] > 0) {
    s = sub(substr(s, 1, 1), first_letters[substr(s, 1, 1),], s)
  }
  while (regexpr(first_letters_pattern2, s, ignore.case = T, perl = T)[1] > 0) {
    match = regexpr(first_letters_pattern2, s, ignore.case = T, perl = T)
    str = substr(s, match[1] + 1, match[1] + 1)
    s = sub(str, first_letters[str, ], s)
  }
  for (i in 1:nrow(other_letters)) {
    s = gsub(rownames(other_letters)[i], other_letters[i,], s)
  }
  return(s)
}
