// import { parse } from 'https://cdn.skypack.dev/espree@9.3.2'
import { parse } from 'https://cdn.skypack.dev/pin/espree@v9.3.2-xGpgE2rj5eSDJMT99glR/mode=imports,min/optimized/espree.js'

const identity = (x) => x

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
  CallExpression(ast, i) {
    return `${this.toCode(ast.callee)}(${ast.arguments.map((x) => this.toCode(x, i)).join(', ')})`
  },
  ExpressionStatement({ expression }, i) {
    return this.indent(i) + this.toCode(expression, i)
  },
  FunctionDeclaration({ body, id, params }, i) {
    return `function ${id ? this.toCode(id) : ''}(${this.list(params)}) ${this.toCode(body, i)}`
  },
  FunctionExpression(ast, i) {
    return this.FunctionDeclaration(ast, i)
  },
  indent(depth = 0) {
    return ' '.repeat(depth * this.tabWidth)
  },
  list(list, i = 0, separator = ', ') {
    return list.map(x => this.toCode(x, i)).join(separator)
  },
  Literal: ({ raw }) => raw,
  Identifier: ({ name }) => name,
  IfStatement({ alternate, consequent, test }, i) {
    return this.indent(i) +
      'if ' + this.test(test, i) + ' ' +
      this.toCode(consequent, i) +
      (alternate ? ' else ' + this.toCode(alternate, i) : '')

  },
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
  Program({ body }, i) {
    const indent = this.indent(i)
    return indent + body.map(x => this.toCode(x, i)).join('\n' + indent)
  },
  ReturnStatement({ argument }, i) {
    return this.indent(i) + 'return ' + this.toCode(argument, i)
  },
  tabWidth: 2,
  test(ast, i) {
    return this.toCode(ast, i)
  },
  toCode(ast, ind = 0) {
    if (!ast) {
      return this.undefined
    } else if (this[ast.type]) {
      return this[ast.type](ast, ind)
    } else {
      console.dir(ast)
      throw new Error('Unimplemented: ' + ast.type)
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
      return this.toOperator(operator) + this.toCode(argument, i)
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
  WhileStatement({ body, test }, i) {
    return this.indent(i) + 'while ' + this.test(test, i) + ' ' + this.toCode(body, i)
  }
}

const CommonLisp = {
  ArrayExpression({ elements }, i) {
    return `(list ${this.list(elements, i)})`
  },
  AssignmentExpression({ id, left, operator, right }, i) {
    const lhs = this.toCode(left ?? id, i)
    const rhs = this.toCode(right ?? id, i)
    if (operator === '=') {
      return `(setf ${lhs} ${rhs})`
    } else {
      return `(setf ${lhs} (${operator[0]} ${lhs} ${rhs}))`
    }
  },
  BinaryExpression({ operator, left, right }, i) {
    if ((operator === '==' || operator === '===') && right.type === 'Literal' && typeof right.value === 'number') {
      return `(= ${this.toCode(left, i)} ${this.toCode(right, i)})`
    }
    return `(${this.toOperator(operator)} ${this.toCode(left, i)} ${this.toCode(right, i)})`
  },
  BlockStatement({ body }, i) {
    return body.map((x) => this.toCode(x, i)).join('\n')
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
        return this.indent(i) + '(print ' + this.list(ast.arguments, i) + ')'
      }
      return this.indent(i) + '(' + [
        this.toCode(ast.callee.property),
        this.toCode(ast.callee.object),
        ...ast.arguments.map((x) => this.toCode(x, i))
      ].join(' ') + ')'
    } else if (ast.callee.name === 'describe') {
      return `(deftest ${toKebabCase(ast.arguments[0].raw.slice(1, -1))}\n${this.toCode(ast.arguments[1].body, i + 1)})`
    } else if (ast.callee.name === 'it') {
      return `(testing ${this.toCode(ast.arguments[0])}\n${this.toCode(ast.arguments[1].body, i + 1)}`
    } else {
      return this.indent(i) + '(' + [this.toCode(ast.callee, i), ...ast.arguments.map((x) => this.toCode(x, i))].join(' ') + ')'
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
  FunctionDeclaration({ body, id, params }, i) {
    const result = ['defun']
    if (id) result.push(this.toCode(id))
    result.push(`(${this.list(params)})`)
    if (body.type === 'BlockStatement' && body.body.length === 1) {
      const stmt = body.body[0]
      result.push(this.toCode(stmt.type === 'ReturnStatement' ? stmt.argument : stmt, i))
    } else {
      result.push('\n' + this.toCode(body, i + 2))
    }
    return this.indent(i) + `(${result.join(' ')})`
  },
  Identifier: ({ name }) => toKebabCase(name),
  IfStatement({ alternate, consequent, test }, i) {
    if (alternate) {
      return `(if ${this.test(test, i)}\n${this.toCode(consequent, i + 4)}\n${this.toCode(alternate, i + 4)})`
    } else if (consequent.type === 'ExpressionStatement') {
      return `(when ${this.test(test, i)} ${this.toCode(consequent, i)})`
    } else if (consequent.type === 'BlockStatement' && consequent.body.length === 1) {
      return `(when ${this.test(test, i)} ${this.toCode(consequent.body[0], i)})`
    }
    return `(when ${this.test(test, i)} \n${this.toCode(consequent, i)})`
  },
  isLiteral(ast) {
    return ast.type === 'Literal' ||
      (ast.type === 'ObjectExpression' && this.isQuoteable(ast))
  },
  list(list, i) {
    return generics.list.call(this, list, i, ' ')
  },
  Literal({ raw, value }) {
    if (typeof value === 'string') {
      return `"${value}"`
    } else {
      return raw
    }
  },
  MemberExpression({ object, property }, i) {
    if (object.name === 'Test') {
      return this.toCode(property, i)
    } else if (property.name === 'length') {
      return `(length ${this.toCode(object, i)})`
    } else {
      return `(assoc '${this.toCode(object, i)} '${this.toCode(property, i)})`
    }
  },
  ObjectExpression({ properties }, i) {
    return (properties.every(this.isQuoteable.bind(this)) ? "'(" : '(list ') + this.list(properties, i) + ')'
  },
  Property({ key, value }, i) {
    return `(${this.toCode(key, i)} . ${this.toCode(value, i)})`
  },
  ReturnStatement({ argument }, i) {
    return this.indent(i) + this.toCode(argument, i)
  },
  tabWidth: 1,
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
  VariableDeclaration({ declarations }, i) {
    return this.indent(i) + `(setf ${declarations
      .map((decl) => `${this.toCode(decl.id, i)} ${this.toCode(decl.init, i)}`)
      .join(' ')
      })`
  },
  WhileStatement({ body, test }, i) {
    return `${this.indent(i)}(loop while ${this.test(test, i)}\n${this.indent(i)}   do ${this.toCode(body, i + 6).trimStart()})`
  }
}
Object.setPrototypeOf(CommonLisp, generics)

const JavaScript = {
  ArrowFunctionExpression({ body, params }, i) {
    return `(${this.list(params)}) => ${this.toCode(body, i)}`
  },
  BlockStatement({ body }, i) {
    return `{\n${body.map((x) => this.toCode(x, i + 1)).join('\n')}\n${this.indent(i)}}`
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
  FunctionExpression({ body, params }, i) {
    return `function (${params.map((x) => this.toCode(x)).join(', ')}) ${this.toCode(body, i)}`
  },
  Identifier: ({ name }) => name[0] + toCamelCase(name.slice(1)),
  Literal({ value, raw }) {
    if (typeof value == 'string' && !/'/.test(raw)) {
      return `'${value}'`
    }
    return raw
  },
  NewExpression(ast, i) {
    return `new ${toTitleCase(this.toCode(ast.callee, i))}(${this.list(ast.arguments, i)})`
  },
  test(ast, i) {
    return `(${this.toCode(ast, i)})`
  },
  ThisExpression: () => 'this',
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
  }
}
Object.setPrototypeOf(JavaScript, generics)

const Julia = {
  ArrowFunctionExpression({ body, params = [] }, i = 0) {
    return `(${params.map(x => this.toCode(x))}) -> ${this.toCode(body, i)} `
  },
  BlockStatement({ body }, i) {
    return `\n${body.map((expr) => this.toCode(expr, i + 1))
      .join('\n')}\n${this.indent(i)}end`
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
        return `print(${this.list(ast.arguments)})`
      } else if (ast.callee.object.name === 'Math') {
        return ({
          random: 'rand',
        }[ast.callee.property.name] || ast.callee.property.name) +
          '(' + this.list(ast.arguments) + ')'
      }
      ast.arguments.unshift(ast.callee.object)
      return `${this.toCode(ast.callee.property, i)}(${this.list(ast.arguments, i)})`
    } else if (ast.callee.name === 'describe' || ast.callee.name === 'it') {
      return `${ast.callee.name === 'describe' ? 'facts' : 'context'}(${this.toCode(ast.arguments[0], i)}) do${this.toCode(ast.arguments[1].body, i)}`
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
        this.toCode(body, i)
      ].join('')
    }
    console.dir(ast)
    throw new Error('Unimplemented: ForStatement')
  },
  ForInStatement({ body, left, right }, i) {
    return `for ${this.toCode(left, i)} in eachindex(${this.toCode(right, i)}) ${this.toCode(body, i)}`
  },
  ForOfStatement({ body, left, right }, i) {
    return `for ${this.toCode(left, i)} in ${this.toCode(right, i)} ${this.toCode(body, i)}`
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
  Literal({ raw, value }) {
    if (typeof value == 'string') {
      return '"' + value.replace(/"/g, '\\"').replace(/\\'/, "'") + '"'
    }
    return raw
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
  toOperator(op) {
    switch (op) {
      case '**': return '^'
      case '^': return '⊻'
      case '===': return '=='
      default: return op
    }
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
}
Object.setPrototypeOf(Julia, generics)

const Lua = {
  ArrayExpression(ast, i) {
    return `{ ${this.list(ast.elements, i)} } `
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
  BlockStatement({ body }, i) {
    return `\n${body.map((expr) => this.toCode(expr, i + 1))
      .join('\n' + this.indent(i))}\n${this.indent(i)}end`
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
        console.dir(`assert.${ast.callee.property.name}`)
      }
    }
    return generics.CallExpression.call(this, ast, i)
  },
  ForStatement(ast, i) {
    const { body, init, test, type, update } = ast
    if (isNumericalForStatement(ast)) {
      if (test.operator[0] === '<' || test.operator[0] === '>') {
        console.dir(update)
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
              : ', ' + update.right.raw
            : update.operator[1] === '-'
              ? ', -1'
              : ', -' + update.right.raw,
          ' do',
          this.toCode(body, i)
        ].join('')
      }
    }
    console.dir(ast)
    console.warn('ForStatement')
  },
  ForOfStatement({ body, left, right }, i) {
    // TODO: pick pairs/ipairs depending on typeof right
    return `for _, ${left.declarations.map((x) => this.toCode(x))} in ipairs(${this.toCode(right, i)}) do ${this.toCode(body, i)}`
  },
  ForInStatement({ body, left, right }, i) {
    return `for ${left.declarations.map((x) => this.toCode(x))} in pairs(${this.toCode(right, i)}) do ${this.toCode(body, i)}`
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
  test(ast, i) {
    return `${this.toCode(ast, i)} then`
  },
  ThisExpression: () => 'self',
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
  }
}
Object.setPrototypeOf(Lua, generics)

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

const preamble = {
  'Common Lisp': '(in-package #:cl-user)\n(defpackage #:challenge/tests/solution\n(:use #:cl #:rove #:challenge/solution))\n(in-package #:challenge/tests/solution)\n\n',
  JavaScript: "const assert = require('chai').assert\n\n",
  Julia: 'using FactCheck\n\n',
  Lua: 'local SOLUTION = require "solution"\n\n'
}

export const languages = { 'Common Lisp': CommonLisp, Julia, JavaScript, Lua }

function normalize(ast) {
  if (!ast) return
  switch (ast.type) {
    case 'ArrowFunctionExpression':
    case 'FunctionExpression':
      normalize(ast.id)
      ast.params.forEach(normalize)
      normalize(ast.body)
      break

    case 'AssignmentExpression':
    case 'BinaryExpression':
      normalize(ast.left)
      normalize(ast.right)
      break

    case 'BlockStatement':
    case 'Program':
      ast.body.forEach(normalize)
      break

    case 'CallExpression':
      normalize(ast.callee)
      ast.arguments.forEach(normalize)
      break

    case 'ExpressionStatement':
      normalize(ast.expression)
      break

    case 'Identifier':
      if (/assert[-_ ]?Equals?/i.test(ast.name)) {
        console.dir(ast)
        ast.type = 'MemberExpression'
        ast.object = { name: 'assert', type: 'Identifier', start: ast.start, end: ast.end }
        ast.property = { name: 'deepEqual', type: 'Identifier', start: ast.start, end: ast.end }
        ast.optional = false
        ast.computed = false
        delete ast.name
      }
      break

    case 'IfStatement':
      normalize(ast.test)
      normalize(ast.consequent)
      normalize(ast.alternate)
      break

    case 'MemberExpression':
      if (ast.object.name === 'assert') {
        switch (ast.property.name) {
          case 'deepEqual':
          case 'deepEquals':
            ast.property.name = 'deepEqual'
            break

          case 'equal':
          case 'equals':
            ast.property.name = 'equal'
            break

          case 'strictEqual':
          case 'strictEquals':
            ast.property.name = 'strictEqual'
            break

          case 'throws':
            break

          default:
            console.log('normalize::assert')
            console.dir(ast)
        }
      } else if (ast.object.name === 'Test') {
        if (ast.property.name === 'expect') {
          ast.type = 'Identifier'
          ast.name = 'assert'
          ast.end = ast.property.end
          delete ast.computed
          delete ast.object
          delete ast.optional
          delete ast.property
        } else if (/assert_?(deep|strict)?_?equals?/i.test(ast.property.name)) {
          ast.object.name = 'assert'
          const name = toCamelCase(ast.property.name.slice(6))
          ast.property.name = /^equals?$/i.test(name) ? 'strictEqual' : name
          normalize(ast)
        }
      } else {
        normalize(ast.object)
        normalize(ast.property)
      }
      break

    case 'ObjectExpression':
      ast.properties.forEach(normalize)
      break

    case 'Property':
      normalize(ast.key)
      normalize(ast.value)
      break

    case 'VariableDeclaration':
      ast.declarations.forEach(normalize)
      break

    case 'VariableDeclarator':
      normalize(ast.id)
      normalize(ast.init)
      break

    default:
    // console.warn('normalize::Unimplemented:' + ast.type)
    // console.dir(ast)
    case 'Literal':
  }
  return ast
}

/**
 * 
 * @param {string} input 
 * @param {object|string} [options] or `options.outputLanguage`
 * @param {string} [options.outputLanguage='JavaScript'] 'JavaScript' by default
 * @param {string} [options.showPreamble=false] 'false' by default
 * @returns {string}
 */
export function convert(input, options) {
  const { outputLanguage = 'JavaScript', showPreamble = false } =
    typeof (options) === 'object' ? options : { outputLanguage: options }
  return (showPreamble ? preamble[outputLanguage] : '') +
    languages[outputLanguage].toCode(normalize(parse(input, { ecmaVersion: 'latest' })))
}
