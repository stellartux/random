function convert (text) {
  const regex = [
    [/('.*?)"(.*?)"(.*?')/g, '$1\\"$2\\"$3'],
    // escape any " in string literals
    [/([a-zA-Z]*)\.length/, 'length($1)'],
    // length from property to function
    [/for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\)\s?([^{]+$)/gm, 'for $1 in $2:$3\n$4end\n'],
    [/for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\s?\)\s?{?/gm, 'for $1 in $2:$3'],
    [/for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+=\s*(\d+)\s*\)\s*{?/gm, 'for $1 in $2:$4:$3'],
    // for loops
    [/(?:Test\.)?describe\(["'`](.+?)["'`],.*{/g, 'facts("$1") do'],
    // Test.describe("description") to facts("description") do
    [/(?:Test\.)?it\(["'`](.+?)["'`](.+?),.*{/g, '  context("$1") do'],
    // Test.it("description") to context("description") do
    [/Test\.expect\((.+?)\s?,\s?["'`](.+)["'`]\);?}?/g,
      (match, p1, p2) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase()) + ' --> true "' + p2 + '"'
    }],
    // Test.expect(x, y) to @fact x --> y
    [/Test\.assertEquals\((.+?\(.+?\))\s*,\s*(.+?),\s?["'](.+?)["']\)+;?/g,
      (match, p1, p2, p3) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase() + '(') + ' --> ' + p2 + ' "' + p3 + '"'
    }],
    // Test.assertEquals(x, y, z) to @fact x --> y "z"
    [/Test\.assertEquals\((.+?\(.+?\))\s*,\s*(.+?)\)+;?/g,
      (match, p1, p2) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase() + '(') + ' --> ' + p2
    }],
    // Test.assertEquals(x, y) to @fact x --> y
    [/===/, '=='],
    // threequals to twoquals
    [/}\);?/g, 'end\n'],
    // }); to end
    [/'(.+?[^\\]?)'/g, '"$1"'],
    // '' string literals to "" string literals
    [/^(\s*)};?$/gm, '$1end'],
    // } to end
    [/{\s*$/gm, ' do'],
    // { to do
    [/function (.+?)\s?{\s?(?:return )([^}]+?)\s?;?\s?}/g, (m, p1, p2) => p1.replace(/\s+\(/, '(') + ' = ' + p2],
    // one liner function declarations to short form
    [/(var|let)\s?/g, ''],
    // no more vars or lets
    [/Math.random/g, 'rand'],
    [/Math.floor/g, 'floor'],
    // fix Math functions
    [/\s?=>\s?/g, ' -> '], // skinny arrow functions
    [/;?\s*$/gm, ''],  // strip line end semicolons and spaces
    [/\${(.+?)}/g, '\$($1)'], // template literals interpolation fix
    [/\n\n/g, '\n'], // no double new lines
    [/else if/g, 'elseif']
  ]
  for (const r of regex)
    text = text.replace(r[0], r[1])
  return 'using FactCheck\nusing Solution\n\n' + text
}
