document.getElementById('text-in').oninput = ev => {
  let text = document.getElementById('text-in').value
  const re = [
    [/('.*?)"(.*?)"(.*?')/g, '$1\\"$2\\"$3'],
    // escape any "" in string literals
    [/Test\.expect\((.+?)\s?,\s?["'](.+)["']\);?}?/g, '    @fact $1 --> true "$2"'],
    // Test.expect(x, y) to @fact x --> y
    [/Test\.assertEquals\((.+?),\s?(.+?),\s?["'](.+)['"]\);?/g, '    @fact $1 --> $2 "$3"'],
    // Test.expect(x, y) to @fact x --> y
    [/===/, '=='],
    // threequals to twoquals
    [/'(.+?)[^\\]?'/g, '"$1"'],
    // '' string literals to "" string literals
    [/\{(.+)\}/ms, 'do$1\nend']
    // {} on functions to do end
  ]
  for (const r of re)
    text = text.replace(r[0], r[1])
  document.getElementById('text-out').value = `using FactCheck\nusing Solution\n\nfacts("Test") do\n  context("") do\n${text}\n  end\nend`
}
