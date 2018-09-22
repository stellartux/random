function convert (text) {
  const regex = [
    [/('.*?)"(.*?)"(.*?')/g, '$1\\"$2\\"$3'],
    // escape any " in string literals
    [/for\s?\(\s?(?:var|let)\s(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\)\s?([^{]+$)/gm, 'for $1 in $2:$3\n$4end\n'],
    [/for\s?\(\s?(?:var|let)\s(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\s?\)\s?{?/gm, 'for $1 in $2:$3'],
    // for loops
    [/(?:Test\.)?describe\(["'`](.+?)["'`],.*{/g, 'facts("$1") do'],
    // Test.describe("description") to facts("description") do
    [/(?:Test\.)?it\(["'`](.+?)["'`],.*{/g, '  context("$1") do'],
    // Test.it("description") to context("description") do
    [/Test\.expect\((.+?)\s?,\s?["'`](.+)["'`]\);?}?/g, '    @fact $1 --> true "$2"'],
    // Test.expect(x, y) to @fact x --> y
    [],
    [/Test\.assertEquals\((.+?\(.+?\))\s*?,\s*?(.+?),\s?["'](.+?)["']\)+;?/g, '    @fact $1 --> $2 "$3"'],
    // Test.assertEquals(x, y, z) to @fact x --> y "z"
    [/Test\.assertEquals\((.+?\(.+?\))\s*?,\s*?(.+?)\)+;?/g, '    @fact $1 --> $2'],
    // Test.assertEquals(x, y) to @fact x --> y
    [/===/, '=='],
    // threequals to twoquals
    [/}\);?/g, 'end\n'],
    // }); to end
    [/'(.+?)[^\\]?'/g, '"$1"'],
    // '' string literals to "" string literals
    [/^(\s*)};?$/gm, '$1end'],
    // } to end
    [/{\s*$/gm, ' do'],
    // { to do
    [/(var|let)\s?/g, ''],
    // no more vars or lets
    [/Math.random\(/g, 'rand('],
    // random numbers
    [/\s?=>\s?/g, ' -> '],
    // skinny arrow functions
    [/;\s?$/gm, ''],
    // strip line end
    [/\${(.+?)}/g, '\$($1)'],
    [/\n\n/g, '\n']
  // no double new lines
  ]
  for (const r of regex)
    text = text.replace(r[0], r[1])
  return 'using FactCheck\nusing Solution\n\n' + text
}
