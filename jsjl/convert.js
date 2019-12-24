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
    [/^\s*(?:Test\.)?describe\s*\(\s*["'`](.+?)["'`],.*\n?\s*{/gmi, 'facts("$1") do'],
    // Test.it("description") to context("description") do
    [/^\s*(?:Test\.)?it\s*\(\s*["'`](.+?)["'`].*?\n?\s*{/gmi, '  context("$1") do'],
    // Test.expect(x, y) to @fact x --> y
    [/^\s*Test\.expect\((.+?)\s*,\s*["'`](.+)["'`]\);?}?/gm,
      (match, p1, p2) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase().replace('_', '')) + ' --> true "' + p2 + '"'
    }],
    // Test.assertEquals(x, y, z) to @fact x --> y
    [/^\s*(?:Test\.)?assert\.?(?:[dD]eep|[sS]trict)?[eE]quals?\s*\((.+?\(.*?\))\s*,\s*(.+?),\s*["'](.+?)["']\)+;?/gmi,
      (match, p1, p2, p3) => {
      return '    @fact ' + p1.replace(/(.+)\(/, (m, q1) => q1.toLowerCase().replace('_', '') + '(') + ' --> ' + p2
    }],
    // Test.assertEquals(x, y) to @fact x --> y
    [/^\s*(?:Test\.)?assert\.?(?:[dD]eep|[sS]trict)?[eE]quals?\((.+?\(.*?\))\s*,\s*(.+?)\);?/gmi,
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
    [/'(.+?[^\\]?)??'/g, '"$1"'],
    // one liner function declarations to short form
    [/function (.+?)\s?\n?\s*{\n?\s*(?:return )([^}]+?)\s?;?\s?}/g, (m, p1, p2) => p1.replace(/\s+\(/, '(') + ' = ' + p2],
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
    [/\${(.+?)}/g, '\$($1)'],
    // no double new lines
    [/\n\n/g, '\n'],
    [/else if/g, 'elseif'],
    // remove brackets on if statements
    [/if\s*\((.+)\) *\n? *{? */gm, 'if $1 '],
    [/null/g, 'nothing']
  ]
  for (const r of regex)
    text = text.replace(r[0], r[1])
  return 'using FactCheck\n\n' + text
}

const polys = {
  'randstring': {
    backfill: "randstring(range, len) = join(rand([range...], len))",
    frontfill: 'using Random'
  },
  '-- default randstring': {
    backfill: "randstring(len::Int) = randstring(vcat(collect('a':'z'), collect('A':'Z'), collect('0':'9')), len)",
  },
  'replace': {
    backfill: 'import Base.replace\n  replace(a, p::Pair; count=typemax(Int)) = replace(a, p[1], p[2], count)'
  },
  'occursin': {
    backfill: 'occursin = ismatch'
  },
  'isdefined': {
    description: '@isdefined',
    backfill: 'macro isdefined(s) isdefined(Symbol(s)) end'
  },
  'joincharstring': {
    description: '*(::Char, ::String)',
    backfill: 'import Base.*\n  *(s::String, c::Char) = string(c) * s\n  *(c::Char, s::String) = s * string(c)'
  },
  'shufflestring': {
    description: 'shuffle(::String)',
    endfill: 'shuffle(s::String) = join(shuffle!(split(s, "")), "")'
  }
}

window.onload = () => {
  document.getElementById('text-in').oninput = ev => {
    document.getElementById('text-out').value = convert(document.getElementById('text-in').value)
  }
  const container = document.getElementById('checkboxes')
  for (let p of Object.keys(polys)) {
    const label = document.createElement('label')
    const labelText = document.createElement('code')
    labelText.innerText = 'description' in polys[p] ? polys[p].description : p + '()'
    const input = document.createElement('input')
    input.setAttribute('name', p)
    input.setAttribute('type', 'checkbox')
    input.addEventListener('change', polyfills)
    label.appendChild(labelText)
    label.appendChild(input)
    container.appendChild(label)
  }
}

function polyfills () {
  let backs = new Set(), fronts = new Set(), ends = new Set()
  for (let box of document.querySelectorAll('#checkboxes [type="checkbox"]')) {
    if (box.checked) {
      const pf = polys[box.name]
      if ('backfill' in pf) {
        backs.add(pf.backfill)
      }
      if ('frontfill' in pf) {
        fronts.add(pf.frontfill)
      }
      if ('endfill' in pf) {
        ends.add(pf.endfill)
      }
    }
  }
  let output = ''
  if (backs.size > 0) {
    output += ['if VERSION < v"1"', ...backs.values()].join('\n  ')
  }
  if (fronts.size > 0) {
    if (output === '') {
      output += 'if VERSION >= v"1"'
    } else {
      output += '\nelse'
    }
    output += '\n  ' + [...fronts.values()].join('\n  ')
  }
  if (output !== '') output += '\nend'
  if (ends.size > 0) {
    output += ['\n', ...ends.values()].join('\n')
  }
  document.getElementById('polyfilloutput').value = output
}
