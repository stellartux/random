//@ts-check

const pairs = [
  ['SHCH', 'Щ'],
  ['SH', 'Ш'],
  ['CH', 'Ч'],
  ['ZH', 'Ж'],
  ['Z', 'З'],
  ['X', 'Х'],
  ['TZ', 'Ц'],
  ['T', 'Т'],
  ['YO', 'Ё'],
  ['YA', 'Я'],
  ['YU', 'Ю'],
  ['J', 'Й'],
  ['I', 'И'],
  ['Y', 'Ы'],
  ['P', 'П'],
  ['O', 'О'],
  ['M', 'М'],
  ['L', 'Л'],
  ['D', 'Д'],
  ['EH', 'Э'],
  ['A', 'А'],
  ['B', 'Б'],
  ['H', 'X'],
  ['R', 'Р'],
  ['G', 'Г'],
  ['F', 'Ф'],
  ['V', 'В'],
  ['U', 'У'],
  ['K', 'К'],
  ['E', 'Е'],
  ['N', 'Н'],
  ['S', 'С'],
  ['shch', 'щ'],
  ['sh', 'ш'],
  ['ch', 'ч'],
  ['zh', 'ж'],
  ['z', 'з'],
  ['x', 'х'],
  ['tz', 'ц'],
  ['t', 'т'],
  ['yo', 'ё'],
  ['ya', 'я'],
  ['yu', 'ю'],
  ['j', 'й'],
  ['i', 'и'],
  ['y', 'ы'],
  ['p', 'п'],
  ['o', 'о'],
  ['m', 'м'],
  ['l', 'л'],
  ['d', 'д'],
  ['eh', 'э'],
  ['a', 'а'],
  ['b', 'б'],
  ['r', 'р'],
  ['g', 'г'],
  ['f', 'ф'],
  ["'", 'ь'],
  ['v', 'в'],
  ['u', 'у'],
  ['k', 'к'],
  ['e', 'е'],
  ['n', 'н'],
  ['s', 'с'],
]

/** @param {string} text */
export function cyrillate(text) {
  let result = ''
  let previousLength = 0

  while (text.length) {
    previousLength = text.length
    for (const [lat, cyr] of pairs) {
      if (text.startsWith(lat)) {
        result += cyr
        text = text.substring(lat.length)
        break
      }
    }
    if (text.length === previousLength) {
      result += text[0]
      text = text.substring(1)
    }
  }

  return result
}
