// import { parse } from 'https://cdn.skypack.dev/espree@9'
import { parse } from 'https://cdn.skypack.dev/pin/espree@v9.6.1-Wxp29AG7hjpUOCb3BWsw/mode=imports/optimized/espree.js'

const identity = (x) => x

Array.prototype.toSorted ??= function (...xs) { return this.slice().sort(...xs) }

export class AbandonError extends Error { }

function abandon(ast, reason = 'Something went wrong') {
  console.dir(ast)
  throw new AbandonError(reason)
}

function implicitReturn(ast) {
  while (ast.body?.length === 1 && ast.body?.body?.type === 'BlockStatement') {
    ast.body = ast.body.body
  }
  const body = ast.body?.body
  const lastIndex = body?.length - 1
  if (body?.[lastIndex].type === 'ReturnStatement') {
    body[lastIndex].type = 'ExpressionStatement'
    body[lastIndex].expression = body[lastIndex].argument
    delete body[lastIndex].argument
  }
  return ast
}

function isFloorDivide({ operator, left, right, type }) {
  return type === 'BinaryExpression' && operator === '|'
    && right.type === 'Literal' && right.value === 0
    && left.type === 'BinaryExpression' && left.operator === '/'
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
    init?.type === 'VariableDeclaration' &&
    init.kind === 'let' &&
    init.declarations.length === 1 &&
    init.declarations[0].type === 'VariableDeclarator' &&
    test?.type === 'BinaryExpression' &&
    (test.operator[0] === '<' || test.operator[0] === '>') &&
    test.left.type === 'Identifier' &&
    test.left.name === init.declarations[0].id.name &&
    (
      update?.type === 'UpdateExpression' && update.argument.name === init.declarations[0].id.name
      || update?.type === 'AssignmentExpression' && update.left.name === init.declarations[0].id.name
    )
}

function isStringConcatenation({ operator, left, right, type }) {
  return type === 'BinaryExpression' && operator === '+' && (
    left.type === 'Literal' && typeof left.value === 'string' ||
    right.type === 'Literal' && typeof right.value === 'string')
}

function loopsForever(ast) {
  if (ast?.type === 'ForStatement') {
    return !ast.test || (ast.test.type?.endsWith('Literal') && !!ast.test.value)
  } else if (ast?.type === 'WhileStatement') {
    return ast.test.type?.endsWith('Literal') && !!ast.test.value
  }
}

/** Given an expression, returns an expression representing its logical inverse. */
function not(ast) {
  const { argument, operator, type, value } = ast
  const opposites = {
    '==': '!=', '!=': '==', '===': '!==', '!==': '===',
    '<': '>=', '>': '<=', '<=': '>', '>=': '<'
  }
  if (type === 'UnaryExpression' && argument.type === 'UnaryExpression' && argument.operator === '!') {
    return argument
  } else if (type === 'BinaryExpression' && opposites[operator]) {
    return { ...ast, operator: opposites[operator] }
  } else if (type === 'Literal') {
    return {
      type: 'Literal',
      raw: !value ? 'true' : 'false',
      value: !value
    }
  }
  return {
    type: 'UnaryExpression',
    operator: '!',
    argument: ast
  }
}

/**
 * Converts a SwitchStatement to the equivalent IfStatement, if possible.
 * @return {object|null}
 **/
function switchStatementToIfStatement({ cases, discriminant }) {
  if (/(^MemberExpression|Identifier|Literal)$/.test(discriminant.type)
    && cases?.slice(0, -1).every(({ consequent }) => {
      const finalStatement = consequent.at(-1)
      return finalStatement === undefined
        || finalStatement.type === 'ReturnStatement'
        || finalStatement.type === 'BreakStatement'
    })) {
    const result = { alternate: {} }
    let stmt = result
    for (let i = 0; i < cases.length; ++i) {
      if (cases[i].test) {
        stmt = stmt.alternate
        stmt.type = 'IfStatement'
        stmt.test = {
          type: 'BinaryExpression',
          left: discriminant,
          operator: '===',
          right: cases[i].test,
        }
        stmt.consequent = {
          type: 'BlockStatement',
          body: cases[i].consequent,
        }
        stmt.alternate = {}
      } else if (i === cases.length - 1) {
        stmt.alternate = {
          type: 'BlockStatement',
          body: cases[i].consequent,
        }
        stmt = {}
      } else {
        return null
      }
    }
    stmt.alternate = null
    return result.alternate
  } else {
    return null
  }
}

function updateExpressionToAssignmentExpression({ argument, operator, prefix }) {
  const right = { type: 'Literal', raw: '1', value: 1 }
  const result = {
    type: 'AssignmentExpression',
    left: argument,
    operator: operator[0] + '=',
    right,
  }
  if (prefix) {
    return result
  } else {
    return {
      type: 'BinaryExpression',
      left: result,
      operator: operator === '++' ? '-' : '+',
      right
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
  AssignmentPattern(ast, i) {
    return this.AssignmentExpression({ ...ast, operator: '=' }, i)
  },
  ArrowFunctionExpression(ast, i) {
    const { body } = ast
    if (!body.type.startsWith('Block')) {
      ast = {
        ...ast,
        body: {
          type: 'BlockStatement',
          body: [{
            type: 'ReturnStatement',
            argument: body
          }],
        }
      }
    }
    return this.FunctionExpression(ast, i)
  },
  AssignmentExpression({ id, left, operator, right }, i) {
    const rhs = this.toCode(right ?? id, i)
    return this.toCode(left ?? id, i) + (rhs ? ' ' + this.toOperator(operator) + ' ' + rhs : '')
  },
  BinaryExpression(ast) {
    const { operator, left, right } = ast
    const result = [this.toCode(left), this.toOperator(operator, ast), this.toCode(right)]
    if (this.binaryExpressionNeedsParens(operator, left)) {
      result[0] = `(${result[0]})`
    }
    if (this.binaryExpressionNeedsParens(operator, right)) {
      result[2] = `(${result[2]})`
    }
    return result.join(' ')
  },
  binaryExpressionNeedsParens(parentOperator, ast) {
    return ast && this.binaryOperatorPrecedence[parentOperator] > this.binaryOperatorPrecedence[ast.operator]
  },
  binaryOperatorPrecedence: {
    ",": 0,
    "=": 1,
    "+=": 1,
    "-=": 1,
    "**=": 1,
    "*=": 1,
    "/=": 1,
    "%=": 1,
    "<<=": 1,
    ">>=": 1,
    ">>>=": 1,
    "&=": 1,
    "^=": 1,
    "|=": 1,
    "&&=": 1,
    "||=": 1,
    "??=": 1,
    "||": 2,
    "??": 2,
    "&&": 3,
    "|": 4,
    "^": 5,
    "&": 6,
    "==": 7,
    "!=": 7,
    "===": 7,
    "!==": 7,
    "<": 8,
    "<=": 8,
    ">": 8,
    ">=": 8,
    "in": 8,
    "instanceof": 8,
    "<<": 9,
    ">>": 9,
    ">>>": 9,
    "+": 10,
    "-": 10,
    "*": 11,
    "/": 11,
    "%": 11,
    "**": 12,
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
  consequent(ast, i) {
    if (ast?.type.startsWith('Block')) {
      return '\n' + this.body(ast.body, i)
    } else {
      return '\n' + this.indent(i + 1) + this.toCode(ast, i)
    }
  },
  DebuggerStatement() {
    console.warn('Skipping debugger statement')
  },
  EmptyStatement() {
    return ';'
  },
  ExpressionStatement({ expression }, i) {
    return this.indent(i) + this.toCode(expression, i)
  },
  FunctionDeclaration(ast, i, ...xs) {
    return `${this.indent(i)}${this.FunctionExpression(ast, i, ...xs)}\n`
  },
  elseIfKeyword: 'elseif',
  elseKeyword: 'else',
  functionKeyword: 'function',
  FunctionExpression(ast, i, keyword = ast.id ? this.functionKeyword : this.lambdaKeyword) {
    const { async, body, generator } = ast
    if (generator) {
      if (this.generatorKeyword) {
        keyword = this.generatorKeyword
      } else {
        abandon(ast, 'Generator functions not supported.')
      }
    }
    if (async) {
      if (this.asyncKeyword) {
        keyword = this.asyncKeyword + keyword
      } else {
        console.warn('Stripping async')
      }
    }
    return `${keyword} ${this.functionExpressionCallExpression(ast, i)}${this.halfBlockOpener}
${this.body(body.body, i, ast)}${this.blockClose(i)}`
  },
  functionExpressionCallExpression({ id, params }, i, ...xs) {
    if (id === null) {
      return this.params(params, i, ...xs)
    } else if (this.params) {
      return `${this.toCode(id)}(${this.params(params, i, ...xs)})`
    } else {
      return this.CallExpression({ callee: id, arguments: params }, i, ...xs)
    }
  },
  halfBlockOpener: '',
  indent(depth = 0) {
    return ' '.repeat(depth * this.tabWidth)
  },
  isLiteral(ast) {
    return ast.type === 'Literal' ||
      (ast.type === 'ObjectExpression' && this.isQuoteable(ast))
  },
  Identifier({ name }) {
    if (name === 'undefined') {
      return this.undefined
    }
    return name
  },
  IfExpression({ alternate, consequent, test }, i) {
    const result = [
      this.ifKeyword,
      this.test(test, i),
      this.thenKeyword,
      this.consequent(consequent, i)
    ]
    for (; alternate?.type.startsWith('If'); alternate = alternate.alternate) {
      result.push('\n', this.indent(i), this.elseIfKeyword, ' ', this.test(alternate.test, i), this.thenKeyword, this.consequent(alternate.consequent, i))
    }
    if (alternate) {
      result.push('\n', this.indent(i), this.elseKeyword, this.consequent(alternate, i))
    }
    result.push(this.blockClose(i))
    return result.join('')
  },
  ifKeyword: 'if ',
  IfStatement(ast, i, parent) {
    return `${this.indent(i)}${this.IfExpression(ast, i, parent)}`
  },
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
  Literal(ast) {
    const { raw, value, regex } = ast
    if (regex) {
      if (this.RegExpLiteral) {
        return this.RegExpLiteral(regex)
      } else {
        abandon(ast, 'RegExp not supported.')
      }
    } else if (typeof value === 'boolean') {
      return value ? this.boolTrue : this.boolFalse
    } else if (typeof value === 'string') {
      return `"${raw.slice(1, -1)}"`
    } else if (value === null) {
      return this.null
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
  null: 'null',
  ObjectExpression({ properties }, i) {
    const props = properties.map((prop) => this.Property(prop, i))
    if (props.length < 4) {
      return `{ ${props.join(', ')} }`
    } else {
      return `{\n${ind}${props.join(',\n' + this.indent(i + 1))}${'\n' + this.indent(i)}}`
    }
  },
  params(params, i) {
    return this.list(params, i)
  },
  PrivateIdentifier(ast) {
    return this.Identifier(ast)
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
  SpreadElement(ast) {
    return this.RestElement(ast)
  },
  SwitchStatement(ast, i) {
    let ifStmt = switchStatementToIfStatement(ast)
    if (ifStmt) {
      return this.toCode(ifStmt, i)
    } else {
      abandon(ast, 'Could not convert SwitchStatement to IfStatement')
    }
  },
  tabWidth: 2,
  test(ast, i) {
    return this.toCode(ast, i) + this.thenKeyword
  },
  thenKeyword: '',
  ThrowStatement(ast, i) {
    if (this.throwKeyword) {
      return `${this.indent(i)}${this.throwKeyword} ${this.toCode(ast.argument, i)}`
    }
    abandon(ast, 'Unimplemented: ThrowStatement')
  },
  toCode(ast, ...xs) {
    if (!ast) {
      return ''
    } else if (this[ast.type]) {
      return this[ast.type](ast, ...xs)
    } else {
      abandon(ast, `Unimplemented: ${ast.type}`)
    }
  },
  toOperator: identity,
  UnaryExpression({ argument, operator, prefix }, i) {
    if (prefix) {
      return this.toOperator(operator) + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + this.toOperator(operator)
    }
  },
  undefined: 'undefined',
  UpdateExpression({ argument, operator, prefix }, i) {
    if (prefix) {
      return this.toOperator(operator) + this.toCode(argument, i)
    } else {
      return this.toCode(argument, i) + this.toOperator(operator)
    }
  },
  VariableDeclarator({ id, init }, i) {
    return this.toCode(id, i) + ' = ' + (init ? this.toCode(init) : this.undefined)
  },
  VariableDeclaration({ declarations }, i) {
    if (declarations.length === 1) {
      return `${this.indent(i)}${this.toCode(declarations[0], i)}`
    }
    const ids = []
    const inits = []
    const nothing = { type: 'Literal', value: null }
    for (const { id, init } of declarations) {
      ids.push(id)
      inits.push(init ?? nothing)
    }
    return `${this.indent(i)}${this.list(ids)} = ${this.list(inits, i)}`
  },
  WhileStatement({ body, test }, i, opener = this.whileOpener) {
    return this.indent(i) + 'while ' + this.test(test, i, opener) + this.toCode(body, i, this.whileOpener)
  },
  YieldExpression(ast, i) {
    const { argument, delegate } = ast
    if (!this.yieldKeyword) {
      abandon(ast, 'Yield expressions not supported.')
    } else if (delegate && !this.yieldDelegateKeyword) {
      abandon(ast, 'Delegated yield not supported.')
    }
    return `${delegate ? this.yieldDelegateKeyword : this.yieldKeyword} ${this.toCode(argument, i)}`
  },
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
      return `(${this.assignmentKeyword} ${lhs} (${this.toOperator(operator.slice(0, -1))} ${lhs} ${rhs}))`
    }
  },
  BinaryExpression(ast, i) {
    const { left, operator, right } = ast
    if (operator === '>>' || operator === '>>>') {
      if (operator === '>>>') {
        console.warn('Treating unsigned bitshift as signed')
      }
      return `(${this.shiftOperator} ${this.toCode(left, i)} ${right.type === 'Literal' && typeof right.value === 'number' ? -right.value : `(- ${this.toCode(right, i)})`})`
    }
    return `(${this.toOperator(operator, ast)} ${this.toCode(left, i)} ${this.toCode(right, i)})`
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
  ConditionalExpression(ast) {
    const { alternate, consequent } = ast
    if (consequent.type === 'BlockStatement' && consequent.body.length === 1) {
      consequent = consequent.body[0]
    }
    if (alternate && alternate.type === 'BlockStatement' && alternate.body.length === 1) {
      alternate = alternate.body[0]
    }
    return this.IfExpression(ast)
  },
  EmptyStatement() {
    return '(begin)'
  },
  FunctionExpression(ast, ...xs) {
    return generics.FunctionExpression.call(this, implicitReturn(ast), ...xs)
  },
  halfBlockOpener: '',
  Identifier({ name }) {
    if (name === 'undefined') {
      return this.undefined
    }
    return toKebabCase(name)
  },
  IfExpression({ alternate, consequent, test }, i) {
    if (alternate?.type.startsWith('If')) {
      const result = [
        '(cond \n',
        this.indent(i), '(', this.toCode(test, i),
        this.toCode(consequent, i), ')'
      ]
      for (; alternate?.type.startsWith('If'); alternate = alternate.alternate) {
        result.push(`\n${this.indent(i)}(${this.toCode(alternate.test, i)}${this.toCode(alternate.consequent, i)})`)
      }
      result.push(`\n${this.indent(i)}(${this.elseKeyword}${this.toCode(alternate, i)})`)
      return result.join('') + ')'
    } else if (alternate) {
      return `(if ${this.test(test, i)}${this.toCode(consequent, i + 2)}${this.toCode(alternate, i + 2)})`
    } else {
      return `(when ${this.test(test, i)}\n${this.toCode(consequent, i + 5)})`
    }
  },
  IfStatement(ast, i) {
    return this.indent(i) + this.IfExpression(ast, i + 1)
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
  null: 'nil',
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
  ThrowStatement({ argument }, i) {
    return `${this.indent(i)}(error ${this.toCode(argument, i)})`
  },
  toOperator(operator) {
    switch (operator) {
      case '&&': return 'and'
      case '||': case '??': return 'or'
      case '&': return 'logand'
      case '|': return 'logior'
      case '^': return 'logxor'
      case '~': return 'lognot'
      case '!': return 'not'
      case '<<': return this.shiftOperator
      default: return operator
    }
  },
  UnaryExpression(ast, i) {
    const { argument, operator } = ast
    const arg = this.toCode(argument, i)
    if ((operator === '+' || operator === '-') && typeof argument.value === 'number') {
      return operator + arg
    } else if (operator === '!' && argument.type === 'BinaryExpression') {
      if (argument.operator === '&&') {
        return `(nand ${this.toCode(argument.left, ast)} ${this.toCode(argument.right, ast)})`
      } else if (argument.operator === '||') {
        return `(nor ${this.toCode(argument.left, ast)} ${this.toCode(argument.right, ast)})`
      }
    } else if (operator === 'delete' || operator === 'typeof') {
      abandon(ast, `No ${operator} in s-expr based languages`)
    }
    return `(${this.toOperator(operator)} ${arg})`
  },
  undefined: 'nil',
  UpdateExpression({ argument, operator, prefix }, i) {
    const id = this.toCode(argument, i)
    const result = `(${this.assignmentKeyword} ${id} (${operator === '++' ? this.incrementFunction : this.decrementFunction} ${id}))`
    if (prefix) {
      return result
    } else {
      return `(${operator === '++' ? this.decrementFunction : this.incrementFunction} ${result})`
    }
  },
  VariableDeclarator({ id, init }, i) {
    return this.toCode(id, i) + (init ? ' ' + this.toCode(init) : '')
  },
  VariableDeclaration({ declarations }, i) {
    return this.indent(i) + `(${this.variableDeclarationKeyword} ${this.list(declarations, i)})`
  },
  YieldExpression({ argument }, i) {
    return `(yield ${this.toCode(argument, i)})`
  }
}
Object.setPrototypeOf(sexpr, generics)

const CommonLisp = {
  assignmentKeyword: 'setf',
  boolTrue: 't',
  boolFalse: 'nil',
  BinaryExpression(ast, i) {
    const { left, operator, right } = ast
    if (operator === '==' || operator === '===') {
      if (right.type === 'Literal' && right.value === 0) {
        return `(zerop ${this.toCode(left, i)})`
      }
    } else if (isFloorDivide(ast)) {
      return this.CallExpression({
        type: 'CallExpression',
        callee: { type: 'Identifier', name: 'floor' },
        arguments: [left.left, left.right]
      })
    } else if (operator === '!=' || operator === '!==') {
      return `(not ${this.toCode(not(ast), i)})`
    }
    return sexpr.BinaryExpression.call(this, ast, i)
  },
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
  elseKeyword: 't',
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
  functionExpressionCallExpression({ id, params }, i, ...xs) {
    if (id === null) {
      return `(${this.params(params, i, ...xs)})`
    }
    return `${this.toCode(id)} (${this.params(params, i, ...xs)})`
  },
  ObjectExpression({ properties }, i) {
    return (properties.every(this.isQuoteable.bind(this)) ? "'(" : '(list ') + this.list(properties, i) + ')'
  },
  optionalParameter({ left, right }, i) {
    return `&optional (${this.toCode(left, i)} ${this.toCode(right, i)})`
  },
  Property({ key, value }, i) {
    return `(${this.toCode(key, i)} . ${this.toCode(value, i)})`
  },
  RestElement({ argument }) {
    return `&rest ${this.toCode(argument)}`
  },
  ReturnStatement(ast, i) {
    console.dir(ast)
  },
  shiftOperator: 'ash',
  toOperator(operator, i) {
    if (operator === '===' && ((parent?.left?.type === 'Literal' && typeof parent.left.value === 'number') ||
      (parent?.right?.type === 'Literal' && typeof parent.right.value === 'number'))) {
      return '='
    } else if (operator === '==') {
      return 'equal'
    } else if (operator === '===') {
      return 'eql'
    }
    return sexpr.toOperator.call(this, operator, i)
  },
  undefined: 'nil',
  UpdateExpression({ argument, operator, prefix }, i) {
    const result = `(${operator == '++' ? 'in' : 'de'}cf ${this.toCode(argument, i)})`
    if (prefix) {
      return result
    } else {
      return `(1${operator === '++' ? '-' : '+'} ${result})`
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
  asyncKeyword: 'async ',
  binaryExpressionNeedsParens(parentOperator, ast) {
    if (ast) {
      const { operator } = ast
      const parentPrecedence = this.binaryOperatorPrecedence[parentOperator]
      const operatorPrecedence = this.binaryOperatorPrecedence[operator]
      if (parentPrecedence === operatorPrecedence) {
        return operator !== parentOperator
      } else {
        return parentPrecedence > operatorPrecedence
      }
    }
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
  elseKeyword: '} else {',
  elseIfKeyword: '} else if',
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
  generatorKeyword: 'function*',
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
  PrivateIdentifier({ name }) {
    return '#' + name
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
  SwitchCase({ test, consequent }, i) {
    return `${this.indent(i)}${test ? 'case ' + this.toCode(test, i) : 'default'}:
${this.list(consequent, i + 1, '\n')}`
  },
  SwitchStatement({ cases, discriminant, }, i) {
    return `${this.indent(i)}switch (${this.toCode(discriminant)}) {\n${this.list(cases, i + 1, '\n')}\n${this.indent(i)}}`
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
  VariableDeclarator({ id, init }, i) {
    return this.toCode(id, i) + (init ? ' = ' + this.toCode(init) : '')
  },
  VariableDeclaration({ kind, declarations }, i) {
    return `${this.indent(i)}${kind} ${this.list(declarations)}`
  },
  whileOpener: ' {',
  yieldDelegateKeyword: 'yield*',
  yieldKeyword: 'yield',
}
Object.setPrototypeOf(JavaScript, generics)

const Julia = {
  ArrowFunctionExpression({ body, params }, i) {
    return `(${this.params(params)}) -> ${implicitReturn(this.toCode(body, i))} `
  },
  AssignmentExpression(ast, i) {
    if (ast.operator === '??=') {
      const left = this.toCode(ast.left)
      return `${left} = something(${left}, ${this.toCode(ast.right, i)})`
    }
    return generics.AssignmentExpression.call(this, ast, i)
  },
  AssignmentPattern({ left, right }) {
    return this.toCode(left) + '=' + this.toCode(right)
  },
  blockOpener: 'begin',
  blockCloser: 'end',
  BinaryExpression(ast, ...xs) {
    if (ast.operator === '|'
      && ast.right.type === 'Literal' && ast.right.value === 0
      && ast.left.type === 'BinaryExpression' && ast.left.operator === '/') {
      ast = { ...ast.left, operator: '÷' }
    } else if (isStringConcatenation(ast)) {
      ast = { ...ast, operator: '*' }
    }
    return generics.BinaryExpression.call(this, ast, ...xs)
  },
  binaryOperatorPrecedence: Object.setPrototypeOf({
    "==": 4,
    "===": 4,
    "!=": 4,
    "!==": 4,
    "<": 4,
    ">": 4,
    "<=": 4,
    ">=": 4,
    "+": 5,
    "-": 5,
    "^": 5,
    "|": 5,
    "*": 6,
    "/": 6,
    "÷": 6,
    "%": 6,
    "&": 6,
    "<<": 7,
    ">>": 7,
    ">>>": 7,
  }, generics.binaryOperatorPrecedence),
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
        return `expected = ${rhs}
${this.indent(i)}@fact ${lhs} --> expected "${this.toCode(ast.arguments[0].callee)}(${ast.arguments[0].arguments.length ? '$(repr(' + ast.arguments[0].arguments.map((arg) => this.toCode(arg)).join(')), $(repr(') + ')' : ''})) --> $(repr(expected))"`
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
  ContinueStatement(ast, i) {
    if (ast.label) {
      abandon(ast, 'Labeled continue not supported in Julia')
    }
    return this.indent(i) + 'continue'
  },
  elseIfKeyword: 'elseif',
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
    return `for ${this.list(left.declarations.map(({ id }) => id), i)} in eachindex(${this.toCode(right, i)}) ${this.toCode(body, i, '')}`
  },
  ForOfStatement({ body, left, right }, i) {
    return `for ${this.list(left.declarations.map(({ id }) => id), i)} in ${this.toCode(right, i)} ${this.toCode(body, i, '')}`
  },
  FunctionDeclaration(ast, i) {
    return generics.FunctionDeclaration.call(this, implicitReturn(ast), i)
  },
  halfBlockOpener: '',
  Identifier({ name }) {
    switch (name) {
      case 'shift': return 'popfirst!'
      case 'unshift': return 'pushfirst!'
      case 'append': case 'pop': case 'push':
        return name + '!'
      case 'undefined':
        return 'nothing'
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
  Literal(ast, i) {
    if (typeof ast.value === 'bigint') {
      return `big"${ast.raw.slice(0, -1)}"`
    } else {
      return generics.Literal.call(this, ast, i)
    }
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
  null: 'nothing',
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
  toOperator(operator) {
    switch (operator) {
      case '**': return '^'
      case '^': return '⊻'
      case '===': return '=='
      case '!==': return '!='
      case 'instanceof': return 'isa'
      default: return operator
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
  UpdateExpression(ast, i) {
    return this.toCode(updateExpressionToAssignmentExpression(ast), i)
  },
  VariableDeclaration(ast, i) {
    if (ast?.declarations.every(({ init }) => init === null)) {
      return `local ${this.list(ast.declarations.map(({ id }) => id))}`
    }
    return generics.VariableDeclaration.call(this, ast, i)
  },
  whileOpener: '',
}
Object.setPrototypeOf(Julia, generics)

const Lua = {
  ArrayExpression({ elements }, i) {
    return `{ ${this.list(elements, i)} }`
  },
  AssignmentExpression(ast, i) {
    if (ast.operator === '=') {
      return this.BinaryExpression(ast, i)
    } else {
      const { left, operator, right } = ast
      return this.BinaryExpression({
        type: 'BinaryExpression',
        left,
        operator: '=',
        right: {
          type: 'BinaryExpression',
          left,
          operator: operator.slice(0, -1),
          right,
        },
      }, i)
    }
  },
  AssignmentPattern(ast, i) {
    return this.toCode(ast.left, i)
  },
  BinaryExpression(ast, ...xs) {
    if (ast.operator === 'in') {
      return `${this.toCode(ast.right, ...xs)}[${this.toCode(ast.left, ...xs)}] ~= nil`
    } else if (isFloorDivide(ast)) {
      ast = { ...ast.left, operator: '//' }
    } else if (isStringConcatenation(ast)) {
      ast = { ...ast, operator: '..' }
    }
    return generics.BinaryExpression.call(this, ast, ...xs)
  },
  binaryOperatorPrecedence: Object.setPrototypeOf({
    // https://www.lua.org/manual/5.4/manual.html#3.4.8
    "<": 3.5,
    ">": 3.5,
    "<=": 3.5,
    ">=": 3.5,
    "!=": 3.5,
    "==": 3.5,
    "!==": 3.5,
    "===": 3.5,
    "in": 3.5,
    "..": 9.5,
    "//": 11,
  }, generics.binaryOperatorPrecedence),
  blockCloser: 'end',
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
  FunctionExpression({ async, body, generator, id, params }, i) {
    const result = []
    if (async) {
      console.warn('Stripping async')
    }
    result.push('function ', this.toCode(id), '(', this.list(params, i), ')')
    for (const param of params) {
      if (param.type === 'AssignmentPattern') {
        result.push('\n' + this.indent(i + 1) + this.toCode({
          ...param,
          operator: '??=',
          type: 'AssignmentExpression',
        }))
      }
    }
    if (generator) {
      i += 1
      result.push('\n', this.indent(i), 'return coroutine.wrap(function ()')
    }
    result.push(this.toCode(body, i))
    if (generator) {
      i -= 1
      result.push(')\n', this.indent(i), 'end')
    }
    return result.join('')
  },
  Identifier({ name }) {
    const keys = {
      Math: 'math',
      toString: 'tostring',
      undefined: 'nil',
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
    } else if (ast.object.type === 'Literal') {
      return `(${this.toCode(ast.object, i)}):${this.toCode(ast.property, i)}`
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
  null: 'nil',
  Property({ key, value }, i) {
    let keyStr = this.toCode(key, i)
    if (!Number.isNaN(+keyStr)) {
      keyStr = `[${keyStr}]`
    } else if (!/[a-zA-Z_][\w_]*/.test(keyStr)) {
      keyStr = `["${keyStr}"]`
    }
    return keyStr + ' = ' + this.toCode(value, i)
  },
  RegExpLiteral(ast) {
    const { flags, pattern } = ast
    if (flags) {
      abandon(ast, 'Unimplemented: regexp flags')
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
      case '>>': console.warn('Treating signed bitshift as unsigned')
      case '>>>': return '>>'
      case 'void': return ''
      default: console.warn('Unknown operator ' + op)
      case '+': case '-': case '*': case '/': case '==': case '<<':
      case '<': case '>': case '<=': case '>=': case '%': case '=':
      case '|': case '&': case '//': case '..': case '~':
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
  UpdateExpression(ast, i) {
    if (!ast.prefix) {
      console.warn('Treating postfix UpdateExpression as prefix')
      ast = { ...ast, prefix: true }
    }
    return this.toCode(updateExpressionToAssignmentExpression(ast), i)
  },
  VariableDeclarator({ id, init }, i) {
    return this.toCode(id, i) + (init ? ' = ' + this.toCode(init) : '')
  },
  VariableDeclaration(ast, i) {
    return this.indent(i) + 'local ' + generics.VariableDeclaration.call(this, ast, i).trimStart()
  },
  whileOpener: ' do',
  YieldExpression({ argument, delegate }, i) {
    if (delegate) {
      return `for value in ${this.toCode(argument, i)} do coroutine.yield(value) end`
    } else {
      return `coroutine.yield(${this.toCode(argument, i)})`
    }
  }
}
Object.setPrototypeOf(Lua, generics)

const Nim = Object.setPrototypeOf({
  binaryOperatorPrecedence: Object.setPrototypeOf({
    '|': 3,
    '^': 3,
    '&': 4,
    '==': 5,
    '!=': 5,
    '===': 5,
    '!==': 5,
    '<': 5,
    '>': 5,
    '<=': 5,
    '>=': 5,
    'in': 5,
    'instanceof': 5,
    '!': 5,
    '+': 8,
    '-': 8,
    '*': 9,
    '/': 9,
    'div': 9,
    'mod': 9,
    '%': 9,
  }, generics.binaryOperatorPrecedence),
  BinaryExpression(ast, ...xs) {
    if (ast.operator === '**') {
      abandon(ast, `Nim: operator ${ast.operator} not supported.`)
    }
    return generics.BinaryExpression.call(this, ast, ...xs)
  },
  blockClose() {
    return ''
  },
  blockCloser: '',
  blockOpener: ':',
  BreakStatement(ast, i) {
    if (ast.argument) {
      abandon(ast, "No labels in Nim.")
    }
    return `${this.indent(i)}break`
  },
  ClassBody({ body }, i, name) {
    return body.toSorted((a, b) => a > b).map((stmt) => this.toCode(stmt, i, name)).join('\n')
  },
  ClassDeclaration({ body, id, superClass }, i) {
    const name = this.toCode(id)
    return `${this.indent(i)}type
${this.indent(i + 1)}${name} = ${superClass ? `ref object of ${this.toCode(superClass)}` : 'object'}${this.toCode(body, i, name)}
`
  },
  ContinueStatement(ast, i) {
    if (ast.argument) {
      abandon(ast, "No labels in Nim.")
    }
    return `${this.indent(i)}continue`
  },
  elseKeyword: 'else:',
  elseIfKeyword: 'elif',
  ForOfStatement({ body, left, right }, i) {
    return `for ${this.list(left.declarations.map(({ id }) => id))} in ${this.toCode(right, i)}${this.toCode(body, i)}`
  },
  functionExpressionCallExpression({ id, params }) {
    if (params.length === 0) {
      return `${this.toCode(id)}(): auto =`
    } else {
      return `${this.toCode(id)}(${this.params(params)}): auto =`
    }
  },
  functionKeyword: 'proc',
  generatorKeyword: 'iterator',
  halfBlockOpener: '',
  Identifier({ name }) {
    return {
      'Error': 'newException',
      'length': 'len',
    }[name] ?? name
  },
  inferType(literal) {
    if (!literal) return null
    switch (typeof literal.value) {
      case 'boolean':
        return 'bool'
      case 'function':
        return 'proc'
      case 'number':
        return Number.isInteger(literal.value) ? 'int' : 'float'
      case 'string':
        return 'string'
      default:
        return 'auto'
    }
  },
  lambdaKeyword: 'proc',
  MemberExpression(ast, ...xs) {
    if (ast.object.name === 'console') {
      if (ast.property.name === 'log') {
        return 'echo'
      }
    }
    return generics.MemberExpression.call(this, ast, ...xs)
  },
  MethodDefinition(ast, i, inferredType) {
    const { computed, key, kind, value } = ast
    if (ast.static) {
      abandon(ast, 'No static methods in Nim.')
    } else if (computed) {
      abandon(ast, 'No computed methods in Nim.')
    }
    const result = {
      ...value,
      id: key,
    }
    result.params.unshift({ type: 'ThisExpression', inferredType })
    if (kind === 'method') {
      return `${this.indent(i)}${this.FunctionDeclaration(result, i)}`
    }
  },
  NewExpression(...xs) {
    return this.CallExpression(...xs)
  },
  params(params) {
    return params.map((param) => {
      if (param.type.endsWith('Identifier')) {
        return param.name + ': auto'
      } else if (param.type.startsWith('Assignment')) {
        return `${param.left.name}: ${this.inferType(param.right)} = ${this.toCode(param.right)}`
      } else if (param.type === 'RestElement') {
        return this.RestElement(param)
      } else if (param.type === 'ThisExpression') {
        return `self: ${param.inferredType ?? 'auto'}`
      } else {
        abandon(param, `Unimplemented parameter type: ${param.type}`)
      }
    }).join(', ')
  },
  PropertyDefinition(ast, i) {
    const { computed, key, value } = ast
    if (ast.static) {
      abandon(ast, 'No static properties in Nim.')
    } else if (computed) {
      abandon(ast, 'No computed properties in Nim.')
    }
    if (value) {
      return `${this.indent(i + 2)}${this.toCode(key)}*: ${this.inferType(value)}`
    }
  },
  RegExpLiteral({ flags, pattern }) {
    if (flags) {
      return `re("${pattern}", "${flags}")`
    } else {
      return `re"${pattern}"`
    }
  },
  RestElement({ argument }) {
    return `${this.toCode(argument)}: varargs`
  },
  SpreadElement(ast) {
    abandon(ast, 'Spread operator not supported in Nim.')
  },
  SwitchCase({ consequent, test }, i) {
    const result = [this.indent(i)]
    if (test) {
      result.push(`of ${this.test(test, i)}\n`)
    } else {
      result.push('else:\n')
    }
    if (consequent.at(-1)?.type === 'BreakStatement') {
      consequent = consequent.slice(0, -1)
    }
    result.push(this.list(consequent, i + 1, '\n'))
    return result.join('')
  },
  SwitchStatement({ cases, discriminant }, i) {
    // todo: validate no fallthrough in cases
    return `case ${this.toCode(discriminant, i)}\n${this.list(cases, i, '\n')}`
  },
  test(ast, i) {
    return this.toCode(ast, i) + ':'
  },
  ThisExpression() {
    return 'self'
  },
  throwKeyword: 'raise',
  toOperator(operator) {
    switch (operator) {
      case 'void': return 'discard '
      case '%': return 'mod'
      case '||': case '??': return 'or'
      case '&&': return 'and'
      case '!': return 'not '
      case 'instanceof': return 'is'
      case '<<': return 'shl'
      case '>>>': console.warn('Treating unsigned shift as signed')
      case '>>': return 'shr'
      case '===': return '=='
      case '!==': return '!='
    }
    return operator
  },
  TryStatement({ block, handler, finalizer }, i) {
    let result = `${this.indent(i)}try${this.toCode(block, i, ':', '')}`
    if (handler) {
      result += `\n${this.indent(i)}except`
      if (handler.param) {
        result += ` Exception as ${this.toCode(handler.param)}`
      }
      result += this.toCode(handler.body, i, ':', '')
    }
    if (finalizer) {
      result += `\nfinally${this.toCode(finalizer, i, ':', '')}`
    }
    return result
  },
  UnaryExpression(ast, i) {
    const { operator, argument } = ast
    if (operator === 'typeof') {
      return `typeof(${this.toCode(argument, i)})`
    } else if (operator === 'delete') {
      abandon(ast, 'No delete operator in Nim.')
    }
    return generics.UnaryExpression.call(this, ast, i)
  },
  UpdateExpression(...xs) {
    return Lua.UpdateExpression.call(this, ...xs)
  },
  VariableDeclarator({ id, init }, i) {
    return this.toCode(id, i) + (init ? ' = ' + this.toCode(init) : '')
  },
  VariableDeclaration({ kind, declarations }, i) {
    if (kind === 'let') {
      kind = 'var'
    } else if (kind === 'const') {
      kind = 'let'
    }
    return `${this.indent(i)}${kind}${declarations.length > 1 ? `\n${this.indent(i + 1)}` : ' '}${this.list(declarations, i + 1, `\n${this.indent(i + 1)}`)}`
  },
  whileOpener: '',
  yieldKeyword: 'yield',
}, generics)

const Python = Object.setPrototypeOf({
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
  AssignmentExpression(ast, i) {
    switch (ast.operator) {
      case '??=': case '&&=': case '||=':
        const { left, operator, right } = ast
        return this.BinaryExpression({
          type: 'BinaryExpression',
          left,
          operator: '=',
          right: {
            type: 'BinaryExpression',
            left,
            operator: operator.slice(0, -1),
            right,
          },
        }, i)
      default:
        return this.BinaryExpression(ast, i)
    }
  },
  BinaryExpression(ast, ...xs) {
    if (isFloorDivide(ast)) {
      ast = { ...ast.left, operator: '//' }
    }
    return generics.BinaryExpression.call(this, ast, ...xs)
  },
  binaryOperatorPrecedence: Object.setPrototypeOf({
    // https://docs.python.org/3/reference/expressions.html#operator-precedence
    "in": 3.5,
    "instanceof": 3.5,
    "<": 3.5,
    ">": 3.5,
    "<=": 3.5,
    ">=": 3.5,
    "==": 3.5,
    "!=": 3.5,
    "===": 3.5,
    "!==": 3.5,
    "//": 11,
  }, generics.binaryOperatorPrecedence),
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
  elseKeyword: 'else:',
  elseIfKeyword: 'elif',
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
  generatorKeyword: 'def',
  halfBlockOpener: ':',
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
  null: 'None',
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
  RegExpLiteral(ast) {
    const { flags, pattern } = ast
    if (flags) {
      abandon(ast, 'Unimplemented: regexp flags')
    }
    return `r"${pattern.replace(/"/g, '\\"').replace(/\\\//g, '/')}"`
  },
  RestElement({ argument }) {
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
  test(ast, i) {
    return generics.test.call(this, ast, i) + ':'
  },
  ThisExpression() {
    return 'self'
  },
  tabWidth: 4,
  throwKeyword: 'raise',
  toOperator(operator) {
    switch (operator) {
      case 'delete': return 'del'
      case '!': return 'not '
      case '&&': return 'and'
      case '??': case '||': return 'or'
      case '===': return '=='
      case '!==': return '!='
      case '>>>':
        console.warn('Treating unsigned bitshift as signed')
        return '>>'
      default: return operator
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
  undefined: 'None',
  UpdateExpression(...xs) {
    return Lua.UpdateExpression.call(this, ...xs)
  },
  whileOpener: '',
  yieldKeyword: 'yield',
  yieldDelegateKeyword: 'yield from',
}, generics)

const Racket = {
  assignmentKeyword: 'set!',
  boolTrue: '#t',
  boolFalse: '#f',
  BinaryExpression(ast, i) {
    const { left, operator, right } = ast
    if (operator === '==' || operator === '===') {
      if (right.type === 'Literal' && right.value === 0) {
        return `(zero? ${this.toCode(left, i)})`
      }
    } else if (isFloorDivide(ast)) {
      return this.CallExpression({
        type: 'CallExpression',
        callee: { type: 'Identifier', name: 'quotient' },
        arguments: [left.left, left.right]
      })
    }
    return sexpr.BinaryExpression.call(this, ast, i)
  },
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
${this.indent(i + 4)}(${this.test(not(ast.test))})${this.toCode(ast.body, i + 1)})`
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
  FunctionExpression(ast, i) {
    if (ast.generator) {
      let { body } = ast
      if (body.type === 'BlockStatement' && body.body.length === 1 && loopsForever(body.body[0])) {
        body = body.body[0]
      }
    }
    return sexpr.FunctionExpression.call(this, ast, i)
  },
  functionExpressionCallExpression({ id, params }, i, ...xs) {
    if (id === null) {
      return `(${this.params(params, i, ...xs)})`
    }
    return `(${this.toCode(id)} ${this.params(params, i, ...xs)})`
  },
  generatorKeyword: '(generator',
  incrementFunction: 'add1',
  optionalParameter({ left, right }, i) {
    return `(${this.toCode(left, i)} ${this.toCode(right, i)})`
  },
  RegExpLiteral(ast) {
    const { pattern, flags } = ast
    if (flags) {
      abandon(ast, 'Unimplemented: regexp flags')
    }
    return `#px"${pattern}"`
  },
  shiftOperator: 'arithmetic-shift',
  tabWidth: 1,
  toOperator(operator, parent) {
    if (operator === '===' && ((parent?.left?.type === 'Literal' && typeof parent.left.value === 'number') ||
      (parent?.right?.type === 'Literal' && typeof parent.right.value === 'number'))) {
      return '='
    }
    switch (operator) {
      case '==': return 'equal?'
      case '===': return 'eq?'
      case '!=': return '(negate equal?)'
      case '!==': return '(negate eq?)'
    }
    return sexpr.toOperator.call(this, operator)
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
    return `${this.indent(i)}(do () (${this.test(not(test), i)})
${this.body(body.body, i)})`
  }
}
Object.setPrototypeOf(Racket, sexpr)

const Ruby = Object.setPrototypeOf({
  ArrowFunctionExpression({ body, expression, params }, i) {
    const paramList = params.length === 0 ? '' : ` |${this.list(params, i)}|`
    if (expression) {
      return `{${paramList} ${this.toCode(body, i)} }`
    } else {
      return `do${paramList}${this.toCode(body, i)}`
    }
  },
  blockOpener: '',
  blockCloser: 'end',
  binaryOperatorPrecedence: Object.setPrototypeOf({
    // https://ruby-doc.org/3.2.2/syntax/precedence_rdoc.html
    "==": 3.5,
    "!=": 3.5,
    "===": 3.5,
    "!==": 3.5,
    "=~": 3.5,
    "!~": 3.5,
    "<": 4,
    "<=": 4,
    ">": 4,
    ">=": 4,
    "|": 5,
    "in": 13,
    "instanceof": 13,
  }, generics.binaryOperatorPrecedence),
  CallExpression(ast, i) {
    if (ast.callee.type === 'MemberExpression' && ast.arguments.length === 0) {
      return this.toCode(ast.callee, i)
    }
    return generics.CallExpression.call(this, ast, i)
  },
  ClassBody({ body }, i) {
    return '\n' + this.body(body, i)
  },
  ClassDeclaration({ body, id, superClass }, i) {
    const result = ['class']
    if (id) {
      result.push(' ', this.toCode(id))
    }
    if (superClass) {
      result.push(' < ', this.toCode(superClass))
    }
    result.push(this.toCode(body, i))
    result.push(this.blockClose(i))
    return result.join('')
  },
  elseIfKeyword: 'elsif',
  functionKeyword: 'def',
  FunctionDeclaration(...xs) {
    return Julia.FunctionDeclaration.call(this, ...xs)
  },
  lambdaKeyword: 'do',
  MemberExpression(ast, i) {
    if (ast.object?.name === 'console') {
      return 'puts'
    }
    return generics.MemberExpression.call(this, ast, i)
  },
  MethodDefinition(ast, i) {
    const { computed, key, kind, value } = ast
    const result = [this.indent(i), 'def ']
    if (ast.static) {
      result.push('self.')
    }
    result.push(this.toCode(key), '(', this.list(value.params, i), ')', this.toCode(value.body, i))
    return result.join('')
  },
  NewExpression(ast, i) {
    return `${this.toCode(ast.callee)}.new(${this.list(ast.arguments)})`
  },
  null: 'nil',
  PropertyDefinition(ast, i) {
    const { computed, key, value } = ast
    if (computed) {
      abandon(ast, 'Computed properties not supported')
    }
    // todo: accessors, private identifiers
    const result = [this.indent(i), '@', this.toCode(key, i)]
    if (value) {
      result.push(' = ', this.toCode(value, i))
    }
    return result.join('')
  },
  RegExpLiteral(ast) {
    return JavaScript.RegExpLiteral.call(this, ast)
  },
  RestElement({ argument }) {
    return `*${this.toCode(argument)}`
  },
  ThrowStatement({ argument }, i) {
    if (!argument) {
      return `${this.indent(i)}raise`
    } else if (/(Call|New)Expression/.test(argument.type)) {
      return `${this.indent(i)}raise ${this.toCode(argument.callee)}, ${this.list(argument.arguments)}`
    } else {
      return `${this.indent(i)}raise ${this.toCode(argument)}`
    }
  },
  toOperator(operator) {
    switch (operator) {
      case '??=': return '||='
      case '??': return '||'
      case '===': return '=='
      case '!==': return '!='
      case '>>>':
        console.warn('Treating unsigned bitshift as signed')
        return '>>'
      default: return operator
    }
  },
  undefined: 'nil',
  UpdateExpression(...xs) {
    return Lua.UpdateExpression.call(this, ...xs)
  },
}, generics)

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

export const languages = { 'Common Lisp': CommonLisp, Julia, JavaScript, Lua, Nim, Python, Racket, Ruby }

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

async function cli() {
  const extToLang = {
    js: "JavaScript",
    jl: "Julia",
    lisp: "Common Lisp",
    lua: "Lua",
    nim: "Nim",
    py: "Python",
    rb: "Ruby",
    rkt: "Racket",
    scm: "Racket",
    ts: "JavaScript",
  }

  const args = {
    _: globalThis.scriptArgs ?? globalThis.Deno?.args
  }
  while (args._.length && args._[0].startsWith('-')) {
    const arg = args._.shift()
    if (arg === '--') {
      break
    } else if (arg.startsWith('--')) {
      const match = arg.match(/--([^=]+)(?:=(.*))/)
      args[match[1]] = match[2] ?? true
    } else {
      args[arg[1]] = true
    }
  }
  args._ = scriptArgs

  if (args.help || args.h) {
    console.log(`

        Kata Cruncher - the test case conversion tool for CodeWars
        usage: kata-cruncher [options] [filenames...]

    Options:
    -h, --help
        Show this help message.
    --to='${[...Object.values(extToLang)].join("','")}'
        Choose the output language. Defaults to 'Julia'.
        Short versions of the language name may also be used.
    `)
    return
  }

  const decoder = new TextDecoder("utf-8")

  if (!args.to) {
    args.to = "Julia"
  } else if (args.to.toLowerCase?.() in extToLang) {
    args.to = extToLang[args.to.toLowerCase()]
  }

  if (args._.length === 0) {
    console.log(
      to[args.to](from.JavaScript(decoder.decode(await readAll(Deno.stdin)))),
    )
  } else {
    for (const filename of args._) {
      console.log(
        to[args.to](decoder.decode(Deno.readFileSync(filename))),
      )
    }
  }
}

if (import.meta.main) {
  await cli()
}
