function convert (text) {
  const regex = [
    // escape any " in string literals
    [/('.*?)"(.*?)"(.*?')/g, '$1\\"$2\\"$3'],
    // length from property to function
    [/([a-zA-Z]*)\.length/g, 'length($1)'],
    // x.indexOf(y) to indexin(x, y)
    [/(\w+)\.indexOf\(([^)]+\))/g, 'indexin($1, $2)'],
    // for loops
    [/for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\)\s*([^{]+$)/gm, 'for $1 in $2:$3\n$4end\n'],
    [/for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\s?\)\s*{?/gm, 'for $1 in $2:$3'],
    [/for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+=\s*(\d+)\s*\)\s*{?/gm, 'for $1 in $2:$4:$3'],
    // Test.describe("description") to facts("description") do
    [/^\s*(?:Test\.)?describe\(["'`](.+?)["'`],.*{/gmi, 'facts("$1") do'],
    // Test.it("description") to context("description") do
    [/^\s*(?:Test\.)?it\s*\(["'`](.+?)["'`].*?{/gmi, '  context("$1") do'],
    // Test.expect(x, y) to @fact x --> y
    [/^\s*Test\.expect\((.+?)\s*,\s*["'`](.+)["'`]\);?}?/gm,
      (match, p1, p2) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase().replace('_', '')) + ' --> true "' + p2 + '"'
    }],
    // Test.assertEquals(x, y, z) to @fact x --> y
    [/^\s*(?:Test\.)?assert(?:(?:[dD]eep)?Equals?)?\s*\((.+?\(.*?\))\s*,\s*(.+?),\s*["'](.+?)["']\)+;?/gmi,
      (match, p1, p2, p3) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase().replace('_', '') + '(') + ' --> ' + p2
    }],
    // Test.assertEquals(x, y) to @fact x --> y
    [/^\s*(?:Test\.)?assert\.?(?:[dD]eep)?Equals?\((.+?\(.*?\))\s*,\s*(.+?)\);?/gmi,
      (match, p1, p2) => {
      return '    @fact ' + p1.replace(/(.+)\(/g, (m, q1) => q1.toLowerCase().replace('_', '') + '(') + ' --> ' + p2
    }],
    // Test.assertSimilar(x, y) to @fact x --> roughly(y)
    [/^\s*Test\.assertSimilar\((.+?(?:\(.+?\))?)\s*,\s*(.+?)\);?/gmi, '    @fact $1 --> roughly($2)'],
    // assert.throw(x, y) to @fact_throws y x
    [/^\s*(?:Test\.)?assert\.?[tT]hrows?\s*\( *(.+), *(.+) *\)/gm, '    @fact_throws $2 $1'],
    // assert.throw(x) to @fact_throws x
    [/^\s*(?:Test\.)?assert\.?[tT]hrows?\s*\( *(.+) *\)/gm, '    @fact_throws $1'],
    // threequals to twoquals
    [/===/g, '=='],
    // }); to end
    [/([ |\t]*)}\);?/g, '$1end\n'],
    // '' string literals to "" string literals
    [/'(.+?[^\\]?)?'/g, '"$1"'],
    // one liner function declarations to short form
    [/function (.+?)\s?{\s?(?:return )([^}]+?)\s?;?\s?}/g, (m, p1, p2) => p1.replace(/\s+\(/, '(') + ' = ' + p2],
    // } to end
    [/^(\s*)};?$/gm, '$1end'],
    // { to do
    [/{\s*$/gm, ' do'],
    // no more vars consts or lets
    [/\s(var|let|const\s)/g, ''],
    // comments
    [/\/\//g, '#'],
    [/\/\*+/g, '#='],
    [/\*+\//g, '=#'],
    // fix Math functions
    [/Math.random/g, 'rand'],
    [/Math.floor/g, 'floor'],
    [/Math.pow\(/g, '^('],
    [/\*\*/g, '^'],
    // skinny arrow functions
    [/\s?=>\s?/g, ' -> '],
    // strip line end semicolons and spaces
    [/;?\s*$/gm, ''],
    // template literals interpolation fix
    [/\${(.+?)}/g, '\$($1)'],
    // no double new lines
    [/\n\n/g, '\n'],
    [/else if/g, 'elseif'],
    // remove brackets on if statements
    [/if\s*\((.+)\) *{? */gm, 'if $1 '],
    [/null/g, 'nothing']
  ]
  for (const r of regex)
    text = text.replace(r[0], r[1])
  return 'using FactCheck\n\n' + text
}
