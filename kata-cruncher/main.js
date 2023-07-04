// import { parse } from 'https://cdn.skypack.dev/espree@9.5.2'
import { parse } from 'https://cdn.skypack.dev/pin/espree@v9.5.2-QB1pleC5s3x1EAqjiTTI/mode=imports,min/optimized/espree.js'
const identity = (x) => x

function abandon(ast, reason = 'Something went wrong') {
  console.dir(ast)
  throw new Error(reason)
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
  body(body, i = 0) {
    return body.map((stmt) => this.toCode(stmt, i + 1)).join('\n')
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
  FunctionDeclaration({ body, id, params }, i, keyword = id ? this.functionKeyword : this.lambdaKeyword) {
    return `${keyword} ${id ? this.CallExpression({ id: id || { type: 'Identifier', name: '', raw: '' }, arguments: params }) : `(${this.list(params)})`}${this.BlockStatement(body, i, this.halfBlockOpener)}${id ? '\n' : ''}`
  },
  elseKeyword: ' else',
  functionKeyword: 'function',
  FunctionExpression(ast, i) {
    return this.FunctionDeclaration(ast, i)
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
    return list.map(x => this.toCode(x, i)).join(separator)
  },
  Literal({ raw, value }) {
    if (typeof value === 'boolean') {
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
  ReturnStatement({ argument }, i) {
    return this.indent(i) + this.toCode(argument, i)
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
  DebuggerStatement(_, i) {
    return `${this.indent(i)}debugger`
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
  FunctionExpression({ body, params }, i) {
    return `function (${params.map((x) => this.toCode(x)).join(', ')}) ${this.toCode(body, i)}`
  },
  Identifier: ({ name }) => name[0] + toCamelCase(name.slice(1)),
  Literal(literal) {
    if (typeof literal.value == 'string' && !/'/.test(literal.value)) {
      return `'${literal.raw.slice(1, -1).replace(/\\"/g, '"')}'`
    }
    return generics.Literal.call(this, literal)
  },
  NewExpression(ast, i) {
    return `new ${toTitleCase(this.toCode(ast.callee, i))}(${this.list(ast.arguments, i)})`
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
    if (operator === 'typeof') {
      return `typeof ${this.toCode(argument, i)}`
    } else if (prefix) {
      return operator + this.toCode(argument, i)
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
  MemberExpression(ast, i) {
    if (ast.property.name === 'length') {
      return 'length(' + this.toCode(ast.object) + ')'
    } else {
      return generics.MemberExpression.call(this, ast, i)
    }
  },
  NewExpression(ast, i) {
    return `${toTitleCase(this.toCode(ast.callee, i))}(${this.list(ast.arguments, i)})`
  },
  ObjectExpression({ properties }, i) {
    const props = properties.map((prop) => this.property(prop, i))
    if (props.length < 4) {
      return `(${props.join(' ')})`
    } else {
      const ind = this.indent(i + 1)
      return '(\n' + ind + props.join(',\n' + ind) + ')\n' + this.indent(i)
    }
  },
  property({ key, value }, i) {
    return this.toCode(key, i) + ' = ' + this.toCode(value, i) + ','
  },
  tabWidth: 4,
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
  ArrayExpression(ast, i) {
    return `{ ${this.list(ast.elements, i)} }`
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
  CallExpression(ast, i) {
    if (ast.callee.type === 'MemberExpression' && ast.callee.object.name === 'assert') {
      const args = ast.arguments.map((arg) => this.toCode(arg, i))
      if (/^(expect(_?similar)?|(strict_?)?equals?)$/i.test(ast.callee.property.name)) {
        [args[0], args[1]] = [args[1], args[0]]
        return `assert.equal(${args.join(', ')})`
      } else if (/deep_?equals?/i.test(ast.callee.property.name)) {
        [args[0], args[1]] = [args[1], args[0]]
        return `assert.same(${args.join(', ')})`
      } else if (/not_?equals?/i.test(ast.callee.property.name)) {
        [args[0], args[1]] = [args[1], args[0]]
        return `assert.are_not.equal(${args.join(', ')})`
      } else if (/throws?|errors?/i.test(ast.callee.property.name)) {
        return `assert.has_error(${args.join(', ')})`
      } else {
        console.warn(`assert.${ast.callee.property.name}`)
      }
    }
    return generics.CallExpression.call(this, ast, i)
  },
  conditionThen: 'and',
  conditionElse: 'or',
  DebuggerStatement(_, i) {
    return `${this.indent(i)}debug.debug()`
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
    const keys = { Math: 'math' }
    if (name in keys) return keys[name]
    return name[0] === name[0].toLowerCase() ? toCamelCase(name) : toTitleCase(name)
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
  undefined: 'nil',
  UnaryExpression(ast) {
    if (operator === '!') {
      return 'not ' + this.toCode(ast.argument)
    } else {
      return generics.UnaryExpression(ast)
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
    } else {
      return generics.MemberExpression.call(this, ast, i)
    }
  },
  NewExpression(ast, i) {
    if (ast.callee?.name === 'Error') {
      return `Exception(${this.list(ast.arguments, i)})`
    } else {
      return this.CallExpression(ast, i)
    }
  },
  tabWidth: 4,
  throwKeyword: 'raise',
  toOperator(op) {
    switch (op) {
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
  tabWidth: 1,
  toOperator(op) {
    switch (op) {
      case '==': return 'equal?'
      case '===': return 'eq?'
    }
    return generics.toOperator.call(this, op)
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
  return (str[0] || '').toLowerCase() + str.slice(1).replace(/[-_ ]./g, (x) => x[1].toUpperCase())
}

/** @param {string} str */
function toKebabCase(str) {
  return (str[0].toLowerCase() + str.slice(1))
    .trim(/[_ ]+/)
    .replace(/[A-Z]/g, c => '-' + c.toLowerCase())
    .replace(/[ _-]+/g, '-') || '_'
}

/** @param {string} str */
function toSnakeCase(str) {
  return str.replace(/[A-Z]/g, (x) => '_' + x.toLowerCase()).replace(/[- _]+/g, '_')
}

/** @param {string} str */
function toTitleCase(str) {
  return str[0].toUpperCase() + str.slice(1).replace(/[-_ ]./g, (x) => x[1].toUpperCase())
}

export const languages = { 'Common Lisp': CommonLisp, Julia, JavaScript, Lua, Python, Racket }

/** @type {Record<string,{(code: string) => { type: 'Program', body: {}[] }}>}*/
export const from = {
  JavaScript(code) {
    return parse(code, { ecmaVersion: 13 })
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
