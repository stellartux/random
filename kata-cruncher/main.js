// import { parse } from 'https://cdn.skypack.dev/espree@9.5.2'
import { parse } from 'https://cdn.skypack.dev/pin/espree@v9.5.2-QB1pleC5s3x1EAqjiTTI/mode=imports,min/optimized/espree.js'

const identity = (x) => x

function abandon(ast, reason = 'Something went wrong') {
  console.dir(ast)
  throw new Error(reason)
}

function isIteratorInitializer({ computed, key, type }) {
  return type === 'MethodDefinition'
    && computed
    && key.type === 'MemberExpression'
    && key.object.name === 'Symbol'
    && key.property.name === 'Iterator'
}

function isNumericalForStatement({ init, test, type, update }) {
  return type === 'ForStatement' &&
    init.type === 'VariableDeclaration' &&
    init.kind === 'let' &&
    init.declarations.length === 1 &&
    init.declarations[0].type === 'VariableDeclarator' &&
    test.type === 'BinaryExpression' &&
    (test.operator[0] === '<' || test.operator[0] === '>') &&
    test.left.type === 'Identifier' &&
    test.left.name === init.declarations[0].id.name &&
    (
      update.type === 'UpdateExpression' && update.argument.name === init.declarations[0].id.name
      || update.type === 'AssignmentExpression' && update.left.name === init.declarations[0].id.name
    )
}

/** Given an expression, returns an expression representing its logical inverse. */
function not(ast) {
  const { argument, operator, type, value } = ast
  if (type === 'UnaryExpression' && argument.type === 'UnaryExpression' && argument.operator === '!') {
    return argument
  } else if (type === 'BinaryExpression' && '!==='.includes(operator)) {
    return {
      ...ast,
      operator: (operator[0] === '!' ? '=' : '!') + operator.slice(1)
    }
  } else if (type === 'Literal' && typeof value === 'boolean') {
    return {
      type: 'Literal',
      raw: value ? 'false' : 'true',
      value: !value
    }
  } else {
    return {
      type: 'UnaryExpression',
      operator: '!',
      argument: ast
    }
  }
}

// see https://github.com/estree/estree/tree/master

const generics = {
  ArrayExpression({ elements }) {
    return `[${this.list(elements)}]`
  },
  ArrayPattern(ast) {
    return this.ArrayExpression(ast)
  },
  ArrowFunctionExpression(ast, i) {
    return this.FunctionExpression(ast, i)
  },
  AssignmentExpression({ id, left, operator, right }, i) {
    const rhs = this.toCode(right ?? id, i)
    return this.toCode(left ?? id, i) + (rhs ? ' ' + operator + ' ' + rhs : '')
  },
  BinaryExpression({ operator, left, right }) {
    const result = [this.toCode(left), this.toOperator(operator), this.toCode(right)]
    function pred({ operator, type }, parentOperator) {
      return type === 'BinaryExpression' &&
        '+*&&||'.includes(parentOperator) &&
        operator !== parentOperator
    }
    if (pred(left, operator)) {
      result[0] = `(${result[0]})`
    }
    if (pred(right, operator)) {
      result[2] = `(${result[2]})`
    }
    return result.join(' ')
  },
  body(body, i = 0, ...xs) {
    return body.map((stmt) => this.toCode(stmt, i + 1, ...xs)).filter(Boolean).join('\n')
  },
  BlockStatement({ body }, i, opener = this.blockOpener, closer = this.blockClose(i)) {
    return opener + '\n' + this.body(body, i) + closer
  },
  blockOpener: '{',
  blockCloser: '}',
  blockClose(i) {
    return `\n${this.indent(i)}${this.blockCloser}`
  },
  boolTrue: 'true',
  boolFalse: 'false',
  CallExpression(ast, i) {
    return `${this.toCode(ast.callee)}(${ast.arguments.map((x) => this.toCode(x, i)).join(', ')})`
  },
  ConditionalExpression({ alternate, consequent, test }, i) {
    return `${this.toCode(test, i)} ${this.conditionThen} ${this.toCode(consequent, i)} ${this.conditionElse} ${this.toCode(alternate, i)}`
  },
  conditionThen: '?',
  conditionElse: ':',
  DebuggerStatement() {
    console.warn('Skipping debugger statement')
  },
  ExpressionStatement({ expression }, i) {
    return this.indent(i) + this.toCode(expression, i)
  },
  FunctionDeclaration(ast, i, ...xs) {
    return `${this.indent(i)}${this.FunctionExpression(ast, i, ...xs)}\n`
  },
  elseKeyword: ' else',
  functionKeyword: 'function',
  FunctionExpression({ body, id, params }, i, keyword = id ? this.functionKeyword : this.lambdaKeyword) {
    return `${keyword} ${id ? this.toCode(id) : ''}(${this.list(params)})${this.BlockStatement(body, i, this.halfBlockOpener)}`
  },
  indent(depth = 0) {
    return ' '.repeat(depth * this.tabWidth)
  },
  isLiteral(ast) {
    return ast.type === 'Literal' ||
      (ast.type === 'ObjectExpression' && this.isQuoteable(ast))
  },
  Identifier: ({ name }) => name,
  IfStatement({ alternate, consequent, test }, i) {
    return this.indent(i) + this.ifKeyword + this.test(test, i) + this.thenKeyword +
      this.toCode(consequent, i, '', alternate ? '\n' : this.blockClose(i)) +
      (alternate ? this.elseKeyword + this.toCode(alternate, i) : '')
  },
  ifKeyword: 'if ',
  lambdaKeyword: 'function',
  thenKeyword: '',
  isQuoteable(ast) {
    if (ast.type === 'Literal' || ast.type === 'Identifier') {
      return true
    } else if (ast.type === 'Property') {
      return this.isQuoteable(ast.key) && this.isQuoteable(ast.value)
    } else if (ast.type === 'ObjectExpression') {
      return ast.properties.every(this.isQuoteable.bind(this))
    } else if (ast.type === 'ArrayExpression') {
      return ast.elements.every(this.isQuoteable.bind(this))
    }
    return false
  },
  list(list, i = 0, separator = ', ') {
    return list ? list.map(x => this.toCode(x, i)).join(separator) : ''
  },
  Literal({ raw, value, regex }) {
    if (regex) {
      return this.RegExpLiteral(regex)
    } else if (typeof value === 'boolean') {
      return value ? this.boolTrue : this.boolFalse
    } else if (typeof value === 'string') {
      return `"${raw.slice(1, -1)}"`
    } else {
      return raw
    }
  },
  LogicalExpression(ast, i) {
    return this.BinaryExpression(ast, i)
  },
  MemberExpression({ computed, object, property }, i) {
    if (object.name === 'Test') {
      return this.toCode(property, i)
    } else if (computed) {
      return `${this.toCode(object, i)}[${this.toCode(property, i)}]`
    } else {
      return `${this.toCode(object, i)}.${this.toCode(property, i)}`
    }
  },
  ObjectExpression({ properties }, i) {
    const props = properties.map((prop) => this.Property(prop, i))
    if (props.length < 4) {
      return `{ ${props.join(', ')} }`
    } else {
      const ind = this.indent(i + 1)
      return `{\n${ind}${props.join(',\n' + ind)}${'\n' + this.indent(i)}}`
    }
  },
  Property({ key, value }, i) {
    return this.toCode(key, i) + ': ' + this.toCode(value, i)
  },
  Program({ body }) {
    return body.map(x => this.toCode(x, 0)).join('\n')
  },
  ReturnStatement({ argument }, i) {
    return this.indent(i) + 'return ' + this.toCode(argument, i)
  },
  tabWidth: 2,
  test(ast, i) {
    return this.toCode(ast, i)
  },
  ThrowStatement(ast, i) {
    if (this.throwKeyword) {
      return `${this.indent(i)}${this.throwKeyword} ${this.toCode(ast.argument, i)}`
    }
    abandon(ast, 'Unimplemented: ThrowStatement')
  },
  toCode(ast, ...xs) {
    if (!ast) {
      return this.undefined
    } else if (this[ast.type]) {
      return this[ast.type](ast, ...xs)
    } else {
      abandon(ast, `Unimplemented: ${ast.type}`)
    }
  },
  toOperator: identity,
  UnaryExpression({ argument, operator, prefix }, i) {
    if (prefix) {
      return operator + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + operator
    }
  },
  UpdateExpression({ argument, operator, prefix }, i) {
    if (prefix) {
      return this.indent(i) + this.toOperator(operator) + this.toCode(argument)
    } else {
      return this.toCode(argument, i) + this.toOperator(operator)
    }
  },
  VariableDeclarator({ id, init }, i) {
    return this.toCode(id, i) + (init ? ' = ' + this.toCode(init) : '')
  },
  VariableDeclaration({ declarations }, i) {
    const result = [this.indent(i)]
    const ids = []
    const inits = []
    for (const { id, init } of declarations) {
      ids.push(id)
      inits.push(init)
    }
    result.push(this.list(ids))
    while (inits.at(-1) === null) {
      inits.pop()
    }
    if (inits.some(x => x !== undefined)) {
      result.push(' = ')
      result.push(this.list(inits))
    }
    return result.join('')
  },
  WhileStatement({ body, test }, i, opener = this.whileOpener) {
    return this.indent(i) + 'while ' + this.test(test, i, opener) + this.toCode(body, i, this.whileOpener)
  }
}

/** Shared implementations for s-expression based languages */
const sexpr = {
  ArrayExpression({ elements }, i) {
    return `(list ${this.list(elements, i)})`
  },
  AssignmentExpression({ id, left, operator, right }, i) {
    const lhs = this.toCode(left ?? id, i)
    const rhs = this.toCode(right ?? id, i)
    if (operator === '=') {
      return `(${this.assignmentKeyword} ${lhs} ${rhs})`
    } else {
      return `(${this.assignmentKeyword} ${lhs} (${operator[0]} ${lhs} ${rhs}))`
    }
  },
  BinaryExpression({ operator, left, right }, i) {
    if (operator === '==' || operator === '===') {
      return `(${right.type === 'Literal' && typeof right.value === 'number' ? '=' : 'equal?'} ${this.toCode(left, i)} ${this.toCode(right, i)})`
    }
    return `(${this.toOperator(operator)} ${this.toCode(left, i)} ${this.toCode(right, i)})`
  },
  BlockStatement(ast, i, ...xs) {
    if (ast.body.length === 1) {
      return `\n${this.toCode(ast.body[0], i + 1, ...xs)}`
    } else {
      return generics.BlockStatement.call(this, ast, i, ...xs)
    }
  },
  blockOpener: '(begin',
  blockClose() {
    return this.blockCloser
  },
  blockCloser: ')',
  CallExpression(ast, i) {
    return `(${this.list([ast.callee, ...ast.arguments], i)})`
  },
  ConditionalExpression({ alternate, consequent, test }, i) {
    if (consequent.type === 'BlockStatement' && consequent.body.length === 1) {
      consequent = consequent.body[0]
    }
    if (alternate && alternate.type === 'BlockStatement' && alternate.body.length === 1) {
      alternate = alternate.body[0]
    }
    if (alternate) {
      return `(if ${this.test(test, i)}
${this.indent(i + 4)}${this.toCode(consequent, i + 4)}
${this.indent(i + 4)}${this.toCode(alternate, i + 4)})`
    } else {
      return `(when ${this.test(test, i)}\n${this.toCode(consequent, i + 1)})`
    }
  },
  halfBlockOpener: '',
  Identifier: ({ name }) => toKebabCase(name),
  IfStatement(ast, i) {
    return this.indent(i) + this.ConditionalExpression(ast, i + 1)
  },
  LabeledStatement(ast) {
    abandon(ast, 'No goto in s-expr based languages.')
  },
  lambdaKeyword: '(lambda',
  list(list, i) {
    return generics.list.call(this, list, i, ' ')
  },
  MemberExpression({ object, property }, i) {
    if (object.name === 'Test') {
      return this.toCode(property, i)
    } else if (property.name === 'length') {
      return `(length ${this.toCode(object, i)})`
    } else if (object.name === 'console') {
      return 'displayln'
    } else {
      return `(assoc ${this.toCode(object, i)} '${this.toCode(property, i)})`
    }
  },
  NewExpression(ast, i) {
    const type = this.toCode(ast.callee)
    if (type === 'map') {
      return ast.arguments.length ? `(make-hash '(${ast.arguments[0].elements.map((pair) => `(${pair.elements.map(this.toCode.bind(this)).join(' . ')})`)}))` : '(make-hash)'
    } else {
      return `(make-${type}${ast.arguments.length ? ' ' + this.list(ast.arguments, i) : ''})`
    }
  },
  RestElement({ argument }) {
    return `. ${this.toCode(argument)}`
  },
  ReturnStatement({ argument }, i) {
    return this.indent(i) + this.toCode(argument, i)
  },
  SequenceExpression({ expressions }, i) {
    return `(begin ${expressions.map((expr) => this.toCode(expr, i)).join(' ')})`
  },
  tabWidth: 1,
  UpdateExpression({ argument, operator }, i) {
    const id = this.toCode(argument)
    return `(${this.assignmentKeyword} ${id} (${operator === '++' ? this.incrementFunction : this.decrementFunction} ${id}))`
  },
  VariableDeclaration({ declarations }, i) {
    return this.indent(i) + `(${this.variableDeclarationKeyword} ${declarations
      .map((decl) => `${this.toCode(decl.id, i)} ${this.toCode(decl.init, i)}`)
      .join(' ')
      })`
  },
}
Object.setPrototypeOf(sexpr, generics)

const CommonLisp = {
  assignmentKeyword: 'setf',
  boolTrue: 't',
  boolFalse: 'nil',
  CallExpression(ast, i) {
    if (ast.callee.type === 'MemberExpression' && ast.callee.object.name === 'assert') {
      const lhs = this.toCode(ast.arguments[0])
      const rhs = this.toCode(ast.arguments[1])
      if (ast.callee.property.name === 'throws') {
        return `(ok (signals ${lhs}${ast.arguments[1] ? ' ' + rhs : ''}))`
      } else {
        const cmp = typeof (ast.arguments[1].value) === 'number' ? '=' : 'equalp'
        if (this.isLiteral(ast.arguments[1])) {
          return `(ok (${cmp} ${lhs} ${rhs}))`
        } else {
          return `(let ((expected ${rhs})) \n${this.indent(i + 1)} (ok (${cmp} ${lhs} expected)))`
        }
      }
    } else if (ast.callee.type === 'MemberExpression') {
      if (ast.callee.object.name === 'console') {
        return '(displayln ' + this.list(ast.arguments, i) + ')'
      }
      return '(' + [
        this.toCode(ast.callee.property),
        this.toCode(ast.callee.object),
        ...ast.arguments.map((x) => this.toCode(x, i))
      ].join(' ') + ')'
    } else if (ast.callee.name === 'describe') {
      return `(deftest ${toKebabCase(ast.arguments[0].raw.slice(1, -1))}${this.toCode(ast.arguments[1].body, i)})`
    } else if (ast.callee.name === 'it') {
      return `($ ${this.toCode(ast.arguments[0])}${this.toCode(ast.arguments[1].body, i, '')}`
    } else {
      const args = [...ast.arguments]
      if (ast.callee) {
        args.unshift(ast.callee)
      }
      return '(' + this.list(args) + ')'
    }
  },
  ForStatement(ast, i) {
    if (isNumericalForStatement(ast)) {
      const { body, init, test } = ast
      return [
        '(loop for ', this.toCode(init.declarations[0].id),
        ' from ', this.toCode(init.declarations[0].init),
        ' to ', , this.toCode(test.right, i),
        ' do ', this.toCode(body, i + 6), ')'
      ].join('')
    }
  },
  functionKeyword: '(defun',
  ObjectExpression({ properties }, i) {
    return (properties.every(this.isQuoteable.bind(this)) ? "'(" : '(list ') + this.list(properties, i) + ')'
  },
  Property({ key, value }, i) {
    return `(${this.toCode(key, i)} . ${this.toCode(value, i)})`
  },
  RestElement({ argument }) {
    return `&rest ${this.toCode(argument)}`
  },
  toOperator(op, i) {
    switch (op) {
      case '==': return 'equal'
      case '===': return 'eql'
    }
    return generics.toOperator.call(this, op, i)
  },
  undefined: 'nil',
  UpdateExpression({ argument, operator, prefix }, i) {
    if (operator === '++' || operator === '--') {
      return `(${operator == '++' ? 'in' : 'de'}cf ${this.toCode(argument, i)})`
    } else if (prefix) {
      return this.toOperator(operator) + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + this.toOperator(operator)
    }
  },
  variableDeclarationKeyword: 'defparameter',
  WhileStatement({ body, test }, i) {
    return `${this.indent(i)}(loop while ${this.test(test, i)}\n${this.indent(i + 3)}do ${this.toCode(body, i + 6).trimStart()})`
  }
}
Object.setPrototypeOf(CommonLisp, sexpr)

const JavaScript = {
  ArrowFunctionExpression({ body, params }, i) {
    return `(${this.list(params)}) => ${this.toCode(body, i)}`
  },
  BreakStatement({ label }, i) {
    return this.indent(i) + 'break' + (label ? ' ' + label.name : '')
  },
  ClassBody({ body }, i) {
    return `{
${body.map((x) => this.toCode(x, i + 1)).join('\n')}
${this.indent(i)}}
`
  },
  ClassDeclaration({ body, id, superClass }, i) {
    return `class ${this.toCode(id)}${superClass ? ` extends ${this.toCode(superClass)}` : ''} ${this.toCode(body, i)}`
  },
  ContinueStatement({ label }, i) {
    return this.indent(i) + 'continue' + (label ? ' ' + label.name : '')
  },
  DebuggerStatement(_, i) {
    return `${this.indent(i)}debugger`
  },
  DoWhileStatement({ body, test }, i) {
    return `${this.indent(i)}do ${this.toCode(body, i)} while ${this.test(test)}`
  },
  ExportNamedDeclaration({ declaration }, i) {
    return `export ${this.toCode(declaration, i)}`
  },
  ForStatement({ body, init, test, update }, i) {
    return this.indent(i) + `for (${this.toCode(init)}; ${this.toCode(test, i)}; ${this.toCode(update, i)}) ${this.toCode(body, i)}`
  },
  ForInStatement({ body, left, right }, i) {
    return this.indent(i) + `for (${this.toCode(left)} in ${this.toCode(right, i)}) ${this.toCode(body, i)}`
  },
  ForOfStatement({ body, left, right }, i) {
    return this.indent(i) + `for (${this.toCode(left)} of ${this.toCode(right, i)}) ${this.toCode(body, i)}`
  },
  halfBlockOpener: ' {',
  Identifier: ({ name }) => name[0] + toCamelCase(name.slice(1)),
  LabeledStatement({ body, label }, i) {
    return `${this.indent(i)}${label.name}:\n${this.toCode(body, i)}`
  },
  Literal(literal) {
    if (typeof literal.value == 'string' && !/'/.test(literal.value)) {
      return `'${literal.raw.slice(1, -1).replace(/\\"/g, '"')}'`
    }
    return generics.Literal.call(this, literal)
  },
  MethodDefinition(ast, i) {
    const { computed, key, kind, value } = ast
    let result = this.indent(i)
    if (ast.static) {
      result += 'static '
    }
    if (kind === 'get') {
      result += 'get '
    } else if (kind === 'set') {
      result += 'set '
    }
    if (computed) {
      result += '['
    }
    result += this.toCode(key)
    if (computed) {
      result += ']'
    }
    result += `(${value.params.map(this.toCode.bind(this)).join(', ')}) ${this.toCode(value.body, i)}`
    return result
  },
  NewExpression(ast, i) {
    return `new ${toTitleCase(this.toCode(ast.callee, i))}(${this.list(ast.arguments, i)})`
  },
  PropertyDefinition(ast, i) {
    const { key, value } = ast
    let result = this.indent(i)
    if (ast.static) {
      result = 'static '
    }
    result += this.toCode(key)
    if (value) {
      result += ' = ' + this.toCode(value)
    }
    return result
  },
  RegExpLiteral({ pattern, flags }) {
    return `/${pattern}/${flags}`
  },
  RestElement({ argument }) {
    return `...${this.toCode(argument)}`
  },
  SequenceExpression({ expressions }, i) {
    return this.list(expressions, i)
  },
  SpreadElement({ argument }) {
    return `...${this.toCode(argument)}`
  },
  Super() {
    return 'super'
  },
  test(ast, i) {
    return `(${this.toCode(ast, i)})`
  },
  thenKeyword: ' {',
  throwKeyword: 'throw',
  ThisExpression: () => 'this',
  TryStatement({ block, handler, finalizer }, i) {
    let result = `try ${this.toCode(block, i)}`
    if (handler) {
      result += ' catch '
      if (handler.param) {
        result += `(${this.toCode(handler.param)}) `
      }
      result += this.toCode(handler.body, i)
    }
    if (finalizer) {
      result += ` finally ${this.toCode(finalizer, i)}`
    }
    return result
  },
  UnaryExpression({ argument, operator, prefix }, i) {
    if (prefix) {
      return operator + (/\w$/.test(operator) ? ' ' : '') + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + operator
    }
  },
  undefined: 'undefined',
  VariableDeclaration(ast, i) {
    return this.indent(i) + ast.kind + ' ' + ast.declarations.map((d) => this.toCode(d, i)).join(', ')
  },
  whileOpener: ' {',
}
Object.setPrototypeOf(JavaScript, generics)

const Julia = {
  ArrowFunctionExpression({ body, params = [] }, i = 0) {
    return `(${params.map(x => this.toCode(x))}) -> ${this.toCode(body, i)} `
  },
  blockOpener: 'begin',
  blockCloser: 'end',
  BreakStatement({ label }, i) {
    return this.indent(i) + (label ? `@goto ${label.name}` : 'break')
  },
  CallExpression(ast, i) {
    if (ast.callee.type === 'MemberExpression' && ast.callee.object.name === 'assert' ||
      ast.callee.type === 'Identifier' && /assert_?(deep|strict)_?equals?/i.test(ast.callee.name)) {
      const lhs = this.toCode(ast.arguments[0])
      const rhs = this.toCode(ast.arguments[1])
      if (ast.callee.property.name === 'throws') {
        return `@fact_throws ${lhs} ${ast.arguments[1] ? rhs : ''} `
      } else if (ast.arguments[1].type !== 'CallExpression') {
        return `@fact ${lhs} --> ${rhs}`
      } else {
        return `expected = ${rhs} \n${this.indent(i)} @fact ${lhs} --> expected "@fact ${lhs} --> $(repr(expected))"`
      }
    } else if (ast.callee.type === 'MemberExpression') {
      if (ast.callee.object.name === 'console') {
        const name = ast.callee.property.name
        const args = this.list(ast.arguments)
        if (name === 'warn') {
          return `@warn ${args}`
        } else if (name === 'debug') {
          return `@debug ${args}`
        } else if (name === 'error') {
          return `@error ${args}`
        } else if (name === 'assert') {
          return '@assert ${args}'
        } else {
          return `println(${args})`
        }
      } else if (ast.callee.object.name === 'Math') {
        return ({
          random: 'rand',
        }[ast.callee.property.name] || ast.callee.property.name) +
          '(' + this.list(ast.arguments) + ')'
      }
      ast.arguments.unshift(ast.callee.object)
      return `${this.toCode(ast.callee.property, i)}(${this.list(ast.arguments, i)})`
    } else if (ast.callee.name === 'describe' || ast.callee.name === 'it') {
      return `${ast.callee.name === 'describe' ? 'facts' : 'context'}(${this.toCode(ast.arguments[0], i)}) ${this.toCode(ast.arguments[1].body, i, 'do')}`
    } else {
      return `${this.toCode(ast.callee, i)}(${this.list(ast.arguments, i)})`
    }
  },
  ClassDeclaration({ body, id, superClass }, i) {
    const name = this.toCode(id)
    const methods = body.body.filter(({ type }) => type === 'MethodDefinition')
    const properties = body.body.filter(({ type }) => type === 'PropertyDefinition')
    let result = `${this.indent(i)}struct ${name}`
    if (superClass) {
      result += ` <: ${this.toCode(superClass)}`
    }
    result += '\n' + properties.map((prop) => this.toCode(prop, i + 1)).join('\n')
    const constructor = methods.find((method) => method.kind === 'constructor')
    if (constructor) {
      result += `\n${this.indent(i + 1)}function ${name}(${this.list(constructor.value.params)})
${this.indent(i + 2)}self = new()
${constructor.value.body.body.map((stmt) => this.toCode(stmt, i + 2)).join('\n')}
${this.indent(i + 2)}self
${this.indent(i + 1)}end`
    }
    result += `\n${this.indent(i)}end\n\n`
    result += methods
      .map((method) => this.toCode(method, i, name))
      .filter(Boolean)
      .join('\n')
    const getters = methods.filter(({ kind }) => kind === 'get')
    const setters = methods.filter(({ kind }) => kind === 'set')
    if (getters.length > 0) {
      result += `\n${this.indent(i)}function Base.getproperty(self::${name}, key::Symbol)
${this.indent(i + 1)}${getters.map(({ key, value }) => `if key == ${this.IdentifierToSymbol(key)}
${value.body.body
          .map((stmt) => {
            if (stmt.type === 'ReturnStatement') {
              return this.indent(i + 2) + this.toCode(stmt.argument)
            } else {
              return this.toCode(stmt, i + 2)
            }
          })}`)
          .join(`\n${this.indent(i + 1)}else`)}
${this.indent(i + 1)}else
${this.indent(i + 2)}getfield(self, key)
${this.indent(i + 1)}end
${this.indent(i)}end\n`
    }
    if (setters.length > 0) {
      result += `
${this.indent(i)}function Base.setproperty!(self::${name}, key::Symbol, value)
${this.indent(i + 1)}${setters.map(({ key, value }) => {
        let result = `if key == ${this.IdentifierToSymbol(key)}\n`
        if (value.params[0].name !== 'value') {
          result += `${this.indent(i + 2)}${value.params[0].name} = value\n`
        }
        result += value.body.body.map((stmt) => this.toCode(stmt, i + 2)).join('\n')
        return result
      }).join(`\n${this.indent(i + 1)}else`)}
${this.indent(i + 1)}else
${this.indent(i + 2)}setfield!(self, key, value)
${this.indent(i + 1)}end
${this.indent(i)}end\n`
    }
    return result
  },
  ContinueStatement({ label }, i) {
    if (label) {
      throw new Error('Labeled continue not supported in Julia')
    }
    return this.indent(i) + 'continue'
  },
  ForStatement(ast, i) {
    if (isNumericalForStatement(ast)) {
      const { body, init, test, update } = ast
      return [
        'for ',
        this.toCode(init.declarations[0]),
        ':',
        update.operator === '++'
          ? ''
          : update.operator === '--'
            ? '-1:'
            : (update.operator[0] === '-' ? '-' : '') + this.toCode(update.right) + ':',
        test.operator === '--'
          ? '1'
          : test.right.value + (test.operator.at(-1) === '=' ? 0 : -1),
        this.toCode(body, i, '')
      ].join('')
    }
    abandon(ast, 'Unimplemented: ForStatement')
  },
  ForInStatement({ body, left, right }, i) {
    return `for ${this.toCode(left, i)} in eachindex(${this.toCode(right, i)}) ${this.toCode(body, i, '')}`
  },
  ForOfStatement({ body, left, right }, i) {
    return `for ${this.toCode(left, i)} in ${this.toCode(right, i)} ${this.toCode(body, i, '')}`
  },
  FunctionDeclaration(ast, i) {
    const body = ast.body.body
    const lastIndex = body.length - 1
    if (body[lastIndex].type === 'ReturnStatement') {
      body[lastIndex].type = 'ExpressionStatement'
      body[lastIndex].expression = body[lastIndex].argument
    }
    return generics.FunctionDeclaration.call(this, ast, i)
  },
  halfBlockOpener: '',
  Identifier: ({ name }) => {
    switch (name) {
      case 'shift': return 'popfirst!'
      case 'unshift': return 'pushfirst!'
      case 'pop': case 'push':
        return name + '!'
      default:
        return name[0] === name[0].toLowerCase() ? name.toLowerCase() : toTitleCase(name)
    }
  },
  IdentifierToSymbol({ name, type, raw }) {
    if (type === 'Literal') {
      return `Symbol(${raw})`
    } else {
      return `:${name}`
    }
  },
  LabeledStatement({ label, body }, i) {
    return `${this.toCode(body, i)}\n${this.indent(i)}@label ${label.name}`
  },
  MemberExpression(ast, i) {
    if (ast.property.name === 'length') {
      return 'length(' + this.toCode(ast.object) + ')'
    } else {
      return generics.MemberExpression.call(this, ast, i)
    }
  },
  MethodDefinition(ast, i, type = 'Any') {
    if (ast.kind === 'method') {
      const { key, value } = ast
      let name = this.toCode(key)
      let result = `${this.indent(i)}function `
      if (name === 'tostring') {
        result += `Base.print(io::IO, self::${type})\n`
        for (const stmt of value.body.body) {
          if (stmt.type === 'ReturnStatement') {
            result += `${this.indent(i + 1)}print(io, ${this.toCode(stmt.argument)})\n`
          } else {
            result += this.toCode(stmt, i + 1)
          }
        }
        result += `${this.indent(i)}end\n`
      } else {
        const params = [...value.params.map(this.toCode.bind(this))]
        if (!ast.static) {
          params.unshift(`self::${type}`)
        }
        result += `${name}(${params.join(', ')})${this.toCode(value.body, i, "")}\n`
      }
      return result
    }
  },
  NewExpression(ast, i) {
    return `${toTitleCase(this.toCode(ast.callee, i))}(${this.list(ast.arguments, i)})`
  },
  ObjectExpression({ properties }, i) {
    const props = properties.map((prop) => this.Property(prop, i))
    if (props.length < 4) {
      return `(${props.join(', ')})`
    } else {
      const ind = this.indent(i + 1)
      return '(\n' + ind + props.join(',\n' + ind) + ')\n' + this.indent(i)
    }
  },
  Property({ key, value }, i) {
    return this.toCode(key, i) + ' = ' + this.toCode(value, i)
  },
  PropertyDefinition({ key }, i) {
    return this.indent(i) + this.toCode(key, i)
  },
  RegExpLiteral({ pattern, flags }) {
    return `r"${pattern.replace(/"/g, '\\"').replace(/\\\//g, '/')}"${flags}`
  },
  RestElement({ argument }) {
    return `${this.toCode(argument)}...`
  },
  SequenceExpression({ expressions }, i) {
    return `begin ${expressions.map(this.toCode.bind(this)).join('; ')} end`
  },
  SpreadElement({ argument }) {
    return `${this.toCode(argument)}...`
  },
  tabWidth: 4,
  ThisExpression() {
    return 'self'
  },
  ThrowStatement({ argument }, i) {
    return `${this.indent(i)}throw(${this.toCode(argument, i)})`
  },
  toOperator(op) {
    switch (op) {
      case '**': return '^'
      case '^': return '⊻'
      case '===': return '=='
      default: return op
    }
  },
  TryStatement({ block, handler, finalizer }, i) {
    let result = `try ${this.toCode(block, i, '', '')}`
    if (handler) {
      result += '\ncatch'
      if (handler.param) {
        result += ` ${this.toCode(handler.param)}`
      }
      result += this.toCode(handler.body, i, '', '')
    }
    if (finalizer) {
      result += `\nfinally ${this.toCode(finalizer, i, '', '')}`
    }
    result += this.blockClose(i)
    return result
  },
  undefined: 'nothing',
  UpdateExpression({ argument, operator, prefix }, i) {
    if (operator === '++' || operator === '--') {
      return this.toCode(argument, i) + ' ' + operator[0] + '= 1'
    } else if (prefix) {
      return this.toOperator(operator) + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + this.toOperator(operator)
    }
  },
  whileOpener: '',
}
Object.setPrototypeOf(Julia, generics)

const Lua = {
  ArrayExpression({ elements }, i) {
    return `{ ${this.list(elements, i)} }`
  },
  AssignmentExpression({ id, left, operator, right }, i) {
    const lhs = this.toCode(left ?? id, i)
    const rhs = this.toCode(right ?? id, i)
    if (operator === '=') {
      return lhs + (right ? ' = ' + rhs : '')
    } else {
      return `${lhs} = ${lhs} ${operator[0]} ${rhs}`
    }
  },
  BlockStatement({ body }, i, blockOpen = this.blockOpen, blockClose = this.blockClose) {
    return `\n${this.body(body, i, blockOpen, blockClose)}\n${this.indent(i)}end`
  },
  BreakStatement({ label }, i) {
    if (label) {
      return `${this.indent(i)}goto ${this.toCode(label)}`
    } else {
      return `${this.indent(i)}break`
    }
  },
  CallExpression(ast, i) {
    if (ast.callee.type === 'MemberExpression') {
      const { object, property } = ast.callee
      if (object.name === 'assert') {
        const args = ast.arguments.map((arg) => this.toCode(arg, i))
        if (/^(expect(_?similar)?|(strict_?)?equals?)$/i.test(property.name)) {
          [args[0], args[1]] = [args[1], args[0]]
          return `assert.equal(${args.join(', ')})`
        } else if (/deep_?equals?/i.test(property.name)) {
          [args[0], args[1]] = [args[1], args[0]]
          return `assert.same(${args.join(', ')})`
        } else if (/not_?equals?/i.test(property.name)) {
          [args[0], args[1]] = [args[1], args[0]]
          return `assert.are_not.equal(${args.join(', ')})`
        } else if (/throws?|errors?/i.test(property.name)) {
          return `assert.has_error(${args.join(', ')})`
        } else {
          console.warn(`assert.${property.name}`)
        }
      } else if (property.name === 'toString') {
        return `tostring(${object.name})`
      }
    }
    return generics.CallExpression.call(this, ast, i)
  },
  ClassDeclaration({ body, id, superClass }, i) {
    const name = this.toCode(id)
    const methods = body.body.filter((expr) => expr.type === 'MethodDefinition')
    const properties = body.body.filter((expr) => expr.type === 'PropertyDefinition')
    const constructor = methods.find((method) => method.kind === 'constructor')
    let result = `${this.indent(i)}local ${name} = {\n`
    if (!constructor) {
      for (const prop of properties) {
        if (prop.value) {
          result += `${this.indent(i + 1)}${this.toCode(prop.key)} = ${this.toCode(prop.value)},\n`
        }
      }
    }
    result += `${this.indent(i + 1)}__name = "${name}"\n${this.indent(i)}}\n`
    if (superClass) {
      result += `setmetatable(${name}, ${this.toCode(superClass)})\n`
    }
    result += `\n${this.indent(i)}function ${name}:new(`
    if (constructor) {
      result += `${this.list(constructor.value.params)})
${this.indent(i + 1)}local self, super = {}, self
${constructor.value.body.body.map((stmt) => this.toCode(stmt, i + 1)).join('\n')}
${this.indent(i + 1)}return setmetatable(self, super)\n`
    } else {
      result += `)
${this.indent(i + 1)}return setmetatable({}, self)\n`
    }
    result += this.indent(i) + 'end\n\n'
    result += methods
      .map((method) => this.toCode(method, i, name))
      .filter(Boolean)
      .join('\n')
    if (methods.some(({ kind }) => kind === 'get' || kind === 'set')) {
      const getters = methods.filter(({ kind }) => kind === 'get')
      const setters = methods.filter(({ kind }) => kind === 'set')
      if (getters.length > 0) {
        result += `
${this.indent(i)}function ${name}:__index(key)
${this.indent(i + 1)}${getters.map(({ computed, key, value }) => `if key == ${computed ? this.toCode(key) : `"${key.name}"`} then
${value.body.body.map((stmt) => this.toCode(stmt, i + 2))}`).join(`\n${this.indent(i + 1)}else`)}
${this.indent(i + 1)}else
${this.indent(i + 2)}return ${name}[key]
${this.indent(i + 1)}end
${this.indent(i)}end
`
      }
      if (setters.length > 0) {
        result += `
${this.indent(i)}function ${name}:__newindex(key, value)
${this.indent(i + 1)}${setters.map((setter) => {
          let result = `if key == "${setter.key.name}" then\n`
          if (setter.value.params[0].name !== 'value') {
            result += `${this.indent(i + 2)}local ${setter.value.params[0].name} = value\n`
          }
          result += setter.value.body.body.map((stmt) => this.toCode(stmt, i + 2)).join('\n')
          return result
        }).join(`\n${this.indent(i + 1)}else`)}
${this.indent(i + 1)}else
${this.indent(i + 2)}rawset(self,key, value)
${this.indent(i + 1)}end
${this.indent(i)}end
`
      }
    } else {
      result += `\n${this.indent(i)}${name}.__index = ${name}\n`
    }
    return result
  },
  conditionThen: 'and',
  conditionElse: 'or',
  DebuggerStatement(_, i) {
    return `${this.indent(i)}debug.debug()`
  },
  DoWhileStatement({ body, test }, i) {
    return `${this.indent(i)}repeat
${this.body(body.body, i)}
${this.indent(i)}until ${this.toCode(not(test), i)}`
  },
  ForStatement(ast, i) {
    const { body, init, test, type, update } = ast
    if (isNumericalForStatement(ast)) {
      if (test.operator[0] === '<' || test.operator[0] === '>') {
        return [
          'for ',
          this.toCode(init.declarations[0]),
          ', ',
          test.operator[1] === '='
            ? this.toCode(test.right)
            : test.right.type === 'Literal'
              ? (test.right.value + (test.operator[0] === '>' ? 1 : -1)).toString()
              : this.toCode(test.right) + ' ' + update.operator[0] + ' 1',
          update.operator[0] === '+'
            ? update.operator[1] === '+'
              ? ''
              : ', ' + this.toCode(update.right)
            : update.operator[1] === '-'
              ? ', -1'
              : ', -' + this.toCode(update.right),
          ' do',
          this.toCode(body, i)
        ].join('')
      }
    }
    abandon(ast, `ForStatement: ${type}`)
  },
  ForOfStatement({ body, left, right }, i) {
    const id = left.declarations[0].id
    let args = '_, '
    if (id.type === 'Identifier') {
      args += this.toCode(id)
    } else {
      args = this.list(id.elements)
    }
    return `for ${args} in pairs(${this.toCode(right, i)}) do ${this.toCode(body, i)}`
  },
  ForInStatement({ body, left, right }, i) {
    return `for ${this.toCode(left.declarations[0].id)} in pairs(${this.toCode(right, i)}) do ${this.toCode(body, i)}`
  },
  Identifier({ name }) {
    const keys = {
      Math: 'math',
      toString: 'tostring'
    }
    if (Object.hasOwn(keys, name)) {
      return keys[name]
    }
    return name[0] === name[0].toLowerCase() ? toCamelCase(name) : toTitleCase(name)
  },
  LabeledStatement({ body, label }, i) {
    return `${this.toCode(body, i)}\n${this.indent(i)}::${label.name}::`
  },
  MemberExpression(ast, i) {
    if (ast.object.name === 'console') {
      return 'print'
    } else if (ast.property.name === 'length') {
      return '#' + this.toCode(ast.object)
    } else {
      return generics.MemberExpression.call(this, ast, i)
    }
  },
  MethodDefinition(ast, i, name) {
    const { key, kind, value } = ast
    if (kind === 'method') {
      let methodName = this.toCode(key)
      if (methodName === 'tostring') {
        methodName = '__tostring'
      }
      return `${this.indent(i)}function ${name}${ast.static ? '.' : ':'}${methodName}(${this.list(value.params)})${this.toCode(value.body, i)}\n`
    }
  },
  NewExpression(ast, i) {
    if (ast.callee.name === 'Error') {
      return this.list(ast.arguments, i)
    }
    return `${this.toCode(ast.callee, i)}:new(${this.list(ast.arguments, i)})`
  },
  Property({ key, value }, i) {
    let keyStr = this.toCode(key, i)
    if (!Number.isNaN(+keyStr)) {
      keyStr = `[${keyStr}]`
    } else if (!/[a-zA-Z_][\w_]*/.test(keyStr)) {
      keyStr = `["${keyStr}"]`
    }
    return keyStr + ' = ' + this.toCode(value, i)
  },
  RegExpLiteral({ pattern, flags }) {
    if (flags) {
      throw new Error('Unimplemented: regexp flags')
    }
    console.info(`Converting /${pattern}/ to Lua string match pattern`)
    return `"${pattern.replace(/\\/g, '%').replace(/"/g, '\\"').replace(/\\\//g, '/')}"`
  },
  RestElement() {
    return '...'
  },
  SpreadElement({ argument }) {
    return `table.unpack(${this.toCode(argument)})`
  },
  Super() {
    return `getmetatable(self)`
  },
  tabWidth: 4,
  test(ast, i, keyword = ' then') {
    return `${this.toCode(ast, i)}${keyword}`
  },
  ThisExpression: () => 'self',
  ThrowStatement({ argument }, i) {
    return `${this.indent(i)}error(${this.toCode(argument)})`
  },
  toOperator(op) {
    switch (op) {
      case '^': return '~'
      case '&&': return 'and'
      case '||': case '??': return 'or'
      case '===': return '=='
      case '!=': case '!==': return '~='
      default: console.warn('Unknown operator ' + op)
      case '+': case '-': case '*': case '/': case '==': case '<<': case '>>':
      case '<': case '>': case '<=': case '>=':
        return op
    }
  },
  TryStatement({ block, handler, finalizer }, i) {
    const offset = handler?.param ? 1 : 0
    const pcall = `pcall(function ()${this.toCode(block, i + offset)})`
    if (!finalizer && handler.body.body.length === 0) {
      return pcall
    }
    let result = ''
    if (handler) {
      if (handler.param) {
        const success = `_try_catch_${i + offset}`
        result += `do\n${this.indent(i + 1)}local ${success}, ${this.toCode(handler.param)} = ${pcall}
${this.indent(i + offset)}if not ${success}`
      } else {
        result += `if not ${pcall}`
      }
      result += ` then${this.toCode(handler.body, i + offset)}\n`
    }
    if (finalizer) {
      result += `${this.body(finalizer.body, i)}\n`
    }
    if (offset) {
      result += `${this.indent(i)}end`
    }
    return result
  },
  undefined: 'nil',
  UnaryExpression(ast) {
    if (ast.operator === '!') {
      return `not ${ast.argument.type === 'BinaryExpression' ? '(' : ''}${this.toCode(ast.argument)}${ast.argument.type === 'BinaryExpression' ? ')' : ''}`
    } else {
      return generics.UnaryExpression.call(this, ast)
    }
  },
  UpdateExpression({ argument, operator, prefix }, i) {
    if (operator === '++' || operator === '--') {
      const arg = this.toCode(argument, i)
      return `${arg} = ${arg} ${operator[0]} 1`
    } else if (prefix) {
      return this.toOperator(operator) + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + this.toOperator(operator)
    }
  },
  VariableDeclaration(ast, i) {
    return this.indent(i) + 'local ' + generics.VariableDeclaration.call(this, ast, i).trimStart()
  },
  whileOpener: ' do'
}
Object.setPrototypeOf(Lua, generics)

const Python = {
  ArrowFunctionExpression({ body, params }) {
    if (body.type === 'BlockStatement') {
      if (body.body.length === 1 && body.body[0].type === 'ReturnStatement') {
        body = body.body[0].argument
      } else {
        abandon(body, "Can't represent multiline arrow functions in Python.")
      }
    }
    return `lambda ${this.list(params)}: ${this.toCode(body)}`
  },
  blockOpener: ':',
  blockCloser: '',
  blockClose() {
    return ''
  },
  boolTrue: 'True',
  boolFalse: 'False',
  ClassDeclaration(ast, i) {
    const { body, id, superClass } = ast
    return `${this.indent(i)}class ${this.toCode(id)}${superClass ? `(${this.toCode(superClass)})` : ''}:
${this.body(body.body, i, ast)}\n\n`
  },
  ConditionalExpression({ alternate, consequent, test }) {
    let result = `${this.toCode(consequent)} if ${this.toCode(test)}`
    if (alternate) {
      result += ` else ${this.toCode(alternate)}`
    }
    return result
  },
  ForStatement(ast, i) {
    if (isNumericalForStatement(ast)) {
      const { body, init, test, update } = ast
      let params = this.toCode(init.declarations[0].id)
      let range = [this.toCode(init.declarations[0].init), this.toCode(test.right)]
      if (update.type === 'UpdateExpression' && update.operator !== '++') {
        range.push('-1')
      } else if (update.type === 'AssignmentExpression') {
        if (update.operator === '+=') {
          if (update.right.value !== 1) {
            range.push(this.toCode(update.right))
          }
        } else if (update.operator === '-=') {
          range.push(`-${this.toCode(update.right)}`)
        } else {
          abandon(ast, 'Unhandled arithmetic progression in numerical for loop')
        }
      }
      return `${this.indent(i)}for ${params} in range(${range.join(', ')}):\n${this.body(body.body, i)}`
    }
    abandon(ast, 'Unhandled for statement variation')
  },
  ForInStatement({ body, left, right }, i) {
    return `${this.indent(i)}for ${this.toCode(left)}, _ in enumerate(${this.toCode(right)}):
${this.body(body.body, i)}`
  },
  ForOfStatement({ body, left, right }, i) {
    return `${this.indent(i)}for ${this.toCode(left)} in ${this.toCode(right)}:
${this.body(body.body, i)}`
  },
  functionKeyword: 'def',
  MemberExpression(ast, i) {
    if (ast.object.name === 'console') {
      return 'print'
    } else if (ast.property.name === 'length') {
      return `len(${this.toCode(ast.object)})`
    } else if (ast.property.name === 'toString') {
      return `str(${this.toCode(ast.object)})`
    } else {
      return generics.MemberExpression.call(this, ast, i)
    }
  },
  MethodDefinition(ast, i, { body }) {
    const { computed, key, kind, value } = ast
    let result = `\n${this.indent(i)}`
    const params = [...value.params]
    let name = this.toCode(key)
    if (name === 'constructor') {
      name = '__init__'
    } else if (name === 'length') {
      name = '__len__'
    } else if (name === 'toString') {
      name = '__str__'
    } else if (name === 'next' && body.body.some(isIteratorInitializer)) {
      result += `def __next__(self, *xs):
${this.indent(i + 1)}result = self.next(*xs)
${this.indent(i + 1)}if result != None:
${this.indent(i + 2)}return result.value
${this.indent(i + 1)}else:
${this.indent(i + 2)}raise StopIteration

${this.indent(i)}`
    } else if (computed) {
      if (isIteratorInitializer(ast)) {
        name = '__iter__'
      } else {
        name = toSnakeCase(name).slice(1, -1)
      }
    }
    if (ast.static) {
      result += `@staticmethod\n${this.indent(i)}`
    } else {
      params.unshift({ type: 'ThisExpression' })
    }
    if (kind === 'get') {
      result += `@property\n${this.indent(i)}`
    } else if (kind === 'set') {
      result += `@${name}.setter\n${this.indent(i)}`
    }
    result += `def ${name}(${this.list(params)})${this.toCode(value.body, i)}`
    return result
  },
  NewExpression(ast, i) {
    if (ast.callee?.name === 'Error') {
      return `Exception(${this.list(ast.arguments, i)})`
    } else {
      return this.CallExpression(ast, i)
    }
  },
  Property({ key, value }, i) {
    let keyStr = this.toCode(key, i)
    if (!Number.isNaN(+keyStr)) {
      keyStr = `[${keyStr}]`
    } else if (!/[a-zA-Z_][\w_]*/.test(keyStr)) {
      keyStr = `["${keyStr}"]`
    }
    if (value) {
      return keyStr + ' = ' + this.toCode(value, i)
    } else {
      return keyStr
    }
  },
  PropertyDefinition(ast, i) {
    return this.indent(i) + this.Property(ast, i)
  },
  RegExpLiteral({ pattern, flags }) {
    if (flags) {
      throw new Error('Unimplemented: regexp flags')
    }
    return `r"${pattern.replace(/"/g, '\\"').replace(/\\\//g, '/')}"`
  },
  RestElement() {
    return `*${this.toCode(argument)}`
  },
  SequenceExpression(ast, i) {
    return JavaScript.SequenceExpression.call(this, ast, i)
  },
  SpreadElement({ argument }) {
    return `*${this.toCode(argument)}`
  },
  Super() {
    return 'super()'
  },
  ThisExpression() {
    return 'self'
  },
  tabWidth: 4,
  throwKeyword: 'raise',
  toOperator(op) {
    switch (op) {
      case 'delete': return 'del'
      case '!': return 'not '
      case '&&': return 'and'
      case '||': return 'or'
      case '===': return '=='
      case '!==': return '!='
      case '>>>':
        console.warn('Converting unsigned right bitshift to signed')
        return '>>'
      default: return op
    }
  },
  TryStatement({ block, handler, finalizer }, i) {
    let result = `try${this.toCode(block, i)}`
    if (handler) {
      result += '\nexcept'
      if (handler.param) {
        result += ` ${this.toCode(handler.param)}`
      }
      result += this.toCode(handler.body, i)
    }
    if (finalizer) {
      result += `\nfinally${this.toCode(finalizer, i)}`
    }
    result += this.blockClose(i)
    return result
  },
  undefined: 'nil',
  UpdateExpression: Julia.UpdateExpression
}
Object.setPrototypeOf(Python, generics)

const Racket = {
  assignmentKeyword: 'set!',
  boolTrue: '#t',
  boolFalse: '#f',
  decrementFunction: 'sub1',
  ForStatement(ast, i) {
    if (isNumericalForStatement(ast)) {
      const decl = ast.init.declarations[0]
      const id = this.toCode(decl.id)
      let update = ''
      if (ast.update?.type === 'UpdateExpression') {
        update = ` (${ast.update.operator === '++' ? 'add' : 'sub'}1 ${id})`
      } else if (ast.update) {
        update = ` (${this.toOperator(ast.update.operator.slice(0, -1))} ${id} ${this.toCode(ast.update.right)})`
      }
      return `${this.indent(i)}(do ((${id} ${this.toCode(decl.init)}${update}))
${this.indent(i + 4)}(${this.test(ast.test)})${this.toCode(ast.body, i + 1)})`
    } else {
      abandon(ast, 'Unimplemented: non-numerical for statement')
    }
  },
  ForOfStatement(ast, i) {
    const { left, right, body } = ast
    if (left.type === 'VariableDeclaration') {
      const decl = left.declarations[0].id
      return `${this.indent(i)}(for ((${decl.type === 'Identifier' ?
        this.toCode(decl) :
        `(${this.list(decl.elements)})`} ${this.toCode(right)}))${this.toCode(body, i + 1, '')}`
    }
    abandon(ast, 'Unhandled `for..of` statement')
  },
  functionKeyword: '(define',
  incrementFunction: 'add1',
  RegExpLiteral({ pattern, flags }) {
    if (flags) {
      throw new Error('Unimplemented: regexp flags')
    }
    return `#px"${pattern}"`
  },
  tabWidth: 1,
  ThrowStatement({ argument }, i) {
    return `${this.indent(i)}(error ${this.toCode(argument, i)})`
  },
  toOperator(op) {
    switch (op) {
      case '==': return 'equal?'
      case '===': return 'eq?'
    }
    return generics.toOperator.call(this, op)
  },
  TryStatement({ block, handler, finalizer }, i) {
    let result = `(with-handlers ([exn:fail?`
    if (handler && handler.body.body.length) {
      result += `\n${this.indent(i + 1)}(lambda (${handler.param ? this.toCode(handler.param) : ''}) ${handler.body.body.length > 0 ? this.toCode(handler.body, i + 1) : '#<void>'})])`
    } else {
      result += ' void])'
    }
    result += this.toCode(block, i) + ')\n'
    if (finalizer) {
      result += this.body(finalizer.body, i - 1, '')
    }
    return result
  },
  undefined: '#f',
  variableDeclarationKeyword: 'define',
  WhileStatement({ body, test }, i) {
    return `${this.indent(i)}(do () (${this.test(test, i)})
${this.body(body.body, i)})`
  }
}
Object.setPrototypeOf(Racket, sexpr)

/** @param {string} str */
function toCamelCase(str) {
  return (str[0] || '').toLowerCase() + str.slice(1).replace(/\W+./g, (x) => x.slice(-1).toUpperCase())
}

/** @param {string} str */
function toKebabCase(str) {
  return (str[0].toLowerCase() + str.slice(1))
    .trim(/[_ ]+/)
    .replace(/[A-Z]/g, c => '-' + c.toLowerCase())
    .replace(/\W/g, '-') || '_'
}

/** @param {string} str */
function toSnakeCase(str) {
  return str.replace(/[A-Z]/g, (x) => '_' + x.toLowerCase()).replace(/[^\w_]/g, '_')
}

/** @param {string} str */
function toTitleCase(str) {
  return str[0].toUpperCase() + str.slice(1).replace(/\W./g, (x) => x[1].toUpperCase())
}

export const languages = { 'Common Lisp': CommonLisp, Julia, JavaScript, Lua, Python, Racket }

/** @type {Record<string,{(code: string) => { type: 'Program', body: {}[] }}>}*/
export const from = {
  JavaScript(code) {
    return parse(code, { ecmaVersion: 'latest', sourceType: 'module' })
  },
}

export const to = new Proxy({
  camelCase: toCamelCase,
  snakeCase: toSnakeCase,
  titleCase: toTitleCase,
  kebabCase: toKebabCase,
}, {
  get(target, prop, __) {
    if (prop in target) {
      return target[prop]
    } else if (prop in languages) {
      return languages[prop].toCode.bind(languages[prop])
    } else {
      console.log(prop)
    }
  }
})
