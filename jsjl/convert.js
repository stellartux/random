function convert(text) {
  const regex = [
    // escape any " in string literals
    [/('.*?)"(.*?)"(.*?')/g, '$1\\"$2\\"$3'],
    // length from property to function
    [/([a-zA-Z]*)\.length/g, 'length($1)'],
    // x.indexOf(y) to indexin(x, y)
    [/(\w+)\.indexOf\(([^)]+\))/g, 'indexin($1, $2)'],
    // for loops
    [
      /for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\)\s*([^{]+$)/gm,
      'for $1 in $2:$3\n$4end\n',
    ],
    [
      /for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+\+\s?\)\s*{?/gm,
      'for $1 in $2:$3',
    ],
    [
      /for\s*\(\s*(?:var|let)\s*(.+?)=(.+?);(?:.+?)<(.+?);.+?\+=\s*(\d+)\s*\)\s*{?/gm,
      'for $1 in $2:$4:$3',
    ],
    // Test.describe("description") to facts("description") do
    [
      /^\s*(?:[tT]est\.)?describe\s*\(\s*["'`](.+?)["'`],.*\n?\s*{/gim,
      'facts("$1") do',
    ],
    // Test.it("description") to context("description") do
    [
      /^\s*(?:[tT]est\.)?it\s*\(\s*["'`](.+?)["'`].*?\n?\s*{/gim,
      '    context("$1") do',
    ],
    // Test.expect(x, y) to @fact x --> y
    [
      /^\s*[tT]est\.expect\((.+?)\s*,\s*["'`](.+)["'`]\);?}?/gm,
      (match, p1, p2) => {
        return (
          '        @fact ' +
          p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase().replace('_', '')) +
          ' --> true "' +
          p2 +
          '"'
        )
      },
    ],
    // Test.assertEquals(x, y, z) to @fact x --> y
    [
      /^\s*(?:[tT]est\.)?assert\.?_?(?:[dD]eep|[sS]trict)?_?[eE]quals?\s*\((.+?\(.*?\))\s*,\s*(.+?),\s*["'](.+?)["']\)+;?/gim,
      (match, p1, p2, p3) => {
        return (
          '        @fact ' +
          p1.replace(
            /(.+)\(/,
            (m, q1) => q1.toLowerCase().replace('_', '') + '('
          ) +
          ' --> ' +
          p2
        )
      },
    ],
    // Test.assertEquals(x, y) to @fact x --> y
    [
      /^\s*(?:[tT]est\.)?assert\.?(?:[dD]eep|[sS]trict)?_?[eE]quals?\((.+?\(.*?\))\s*,\s*(.+?)\);?/gim,
      (match, p1, p2) => {
        return (
          '        @fact ' +
          p1.replace(
            /(.+)\(/g,
            (m, q1) => q1.toLowerCase().replace('_', '') + '('
          ) +
          ' --> ' +
          p2
        )
      },
    ],
    // Test.assertSimilar(x, y) to @fact x --> roughly(y)
    [
      /^\s*[tT]est\.assert_?[sS]imilar\((.+?(?:\(.+?\))?)\s*,\s*(.+?)\);?/gim,
      '        @fact $1 --> $2',
    ],
    // assert.throw(x, y) to @fact_throws y x
    [
      /^\s*(?:[tT]est\.)?assert\.?_?[tT]hrows?\s*\( *(.+), *(.+) *\)/gm,
      '        @fact_throws $2 $1',
    ],
    // assert.throw(x) to @fact_throws x
    [
      /^\s*(?:[tT]est\.)?assert\.?[tT]hrows?\s*\( *(.+) *\)/gm,
      '        @fact_throws $1',
    ],
    // threequals to twoquals
    [/===/g, '=='],
    // }); to end
    [/([ |\t]*)}\);?/g, '$1end\n'],
    // '' string literals to "" string literals
    [/'(.+?[^\\]?)??'/g, '"$1"'],
    // one liner function declarations to short form
    [
      /function (.+?)\s?\n?\s*{\n?\s*(?:return )([^}]+?)\s?;?\s?}/g,
      (m, p1, p2) => p1.replace(/\s+\(/, '(') + ' = ' + p2,
    ],
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
    [/Math.PI/g, 'π'],
    [/\*\*/g, '^'],
    // skinny arrow functions
    [/\s?=>\s?/g, ' -> '],
    // strip line end semicolons and spaces
    [/;?\s*$/gm, ''],
    // template literals interpolation fix
    [/\${(.+?)}/g, '$($1)'],
    // no double new lines
    [/\n\n/g, '\n'],
    [/}? *else if/g, ' elseif'],
    // remove brackets on if statements
    [/if\s*\((.+)\) *\n? *{? */gm, 'if $1 '],
    [/null/g, 'nothing'],
  ]
  for (const r of regex) {
    text = text.replace(r[0], r[1])
  }
  return (document.querySelector('input[name="factcheck"]').checked ? 'using FactCheck\n\n' : '') + text
}

window.onload = () => {
  document.getElementById('text-in').oninput = ev => {
    document.getElementById('text-out').value = convert(
      document.getElementById('text-in').value
    )
  }
}

function copyText() {
  navigator.clipboard.writeText(document.getElementById('text-out').value)
}

function pasteCopy() {
  navigator.clipboard
    .readText()
    .then(text => {
      const code = convert(text)
      document.getElementById('text-in').value = text
      document.getElementById('text-out').value = code
      navigator.clipboard.writeText(code)
    })
}
