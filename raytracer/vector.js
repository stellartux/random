export class Vector extends Array {
  /*
  constructor (...args) {
    super(...args)
    const indices = { x: 0, y: 1, z: 2, w: 3, r: 0, g: 1, b: 2, a: 3, s: 0, t: 1, p: 2, q: 3 }
    const isDynamicKey = k => typeof k === 'string' && /^([xyzw]{1,4}|[rgba]{1,4}|[stpq]{1,4})$/.test(k)

    return new Proxy(this, {
      get: function (obj, key) {
        if (isDynamicKey(key)) return new Vector(...[...key].map(c => obj[indices[c]]))
        else return obj[key]
      },
      set: function (obj, key, vals) {
        if (isDynamicKey(key)) {
          if (key.length === new Set([...key]).size) [...key].forEach((c, i) => { obj[indices[c]] = vals[i] })
        } else obj[key] = vals
        return true
      }
    })
  }
  */

  /** Applies op(v1, v2) provided that the lengths of v1 and v2 match or either argument is a number
   * @param {Function} op The operation to be applied
   * @param {Vector|number} v1
   * @param {Vector|number} v2
   * @returns {Vector|number} The result of applying op(v1, v2) elementwise
   **/
  static operation(op, v1, v2) {
    if (typeof v1 === 'number' && v2.length) {
      return new Vector(...v2.map(c => op(v1, c)))
    } else if (typeof v2 === 'number' && v1.length) {
      return new Vector(...v1.map(c => op(c, v2)))
    } else if (v1.length === v2.length) {
      return new Vector(...v1.map((c, i) => op(c, v2[i])))
    } else if (typeof v1 === 'number' && typeof v2 === 'number') {
      return op(v1, v2)
    } else throw new Error(`Could not apply ${op} to ${v1}, ${v2}`)
  }

  static add(v1, v2) {
    return Vector.operation((a, b) => a + b, v1, v2)
  }

  static sub(v1, v2) {
    return Vector.operation((a, b) => a - b, v1, v2)
  }

  static mul(v1, v2) {
    return Vector.operation((a, b) => a * b, v1, v2)
  }

  static div(v1, v2) {
    return Vector.operation((a, b) => a / b, v1, v2)
  }

  /**
   * @param {Vector} v the vector to be normalized
   * @returns {Vector} a new vecttor
   **/
  static normalize(v) {
    return Vector.div(v, v.magnitude)
  }

  static equal(v1, v2) {
    return Vector.operation((a, b) => a === b, v1, v2)
  }

  static dot(v1, v2) {
    return Vector.mul(v1, v2).reduce((a, c) => a + c, 0)
  }

  static cross(v1, v2) {
    if (v1.length !== 3 || v2.length !== 3) {
      throw new Error('Can only calculate cross product of 3D vectors')
    }
    return new Vector([
      v1[1] * v2[2] - v1[2] * v2[1],
      v1[0] * v2[2] - v1[2] * v1[0],
      v1[0] * v2[1] - v1[1] * v1[0],
    ])
  }

  /**
   * @param {Vector} vector the vector by rotated
   * @param {Number} angle angle in radians
   **/
  static rotate2D([x, y], angle) {
    const cos = Math.cos(angle)
    const sin = Math.sin(angle)
    return new Vector([x * cos - y * sin, x * sin + y * cos])
  }

  /** Copies the values of v into this
   * @param {Vector} v the source vector
   * @returns this
   **/
  copy(v) {
    v.forEach((c, i) => {
      this[i] = c
    })
    return this
  }

  add(v) {
    this.copy(Vector.add(this, v))
    return this
  }

  sub(v) {
    this.copy(Vector.sub(this, v))
    return this
  }

  mul(v) {
    this.copy(Vector.mul(this, v))
    return this
  }

  div(v) {
    this.copy(Vector.div(this, v))
    return this
  }

  get magnitude() {
    return Math.hypot(...this)
  }

  normalize() {
    const m = this.magnitude
    this.forEach((c, i) => {
      this[i] = c / m
    })
    return this
  }

  cross(v) {
    this.copy(Vector.cross(this, v))
    return this
  }

  /** Rotate a 2D vector by the given angle
   * @param {Number} angle angle in radians
   **/
  rotate2D(angle) {
    const [x, y] = this
    const cos = Math.cos(angle)
    const sin = Math.sin(angle)
    this[0] = x * cos - y * sin
    this[1] = x * sin + y * cos
    return this
  }
}
