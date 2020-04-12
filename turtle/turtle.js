import { Vector } from '../raytracer/vector.min.js'

export const toRadians = degrees => (degrees * Math.PI) / 180

export function turtlePath(code) {
  const bearing = new Vector(0, -1)
  let dir
  let path = 'M 0 0'
  let prefix = 'l'
  const instrs = code.trim().split(/\s+/)
  while (instrs.length) {
    dir = 1
    switch (instrs.shift().toLowerCase()) {
      case 'pu':
      case 'penup':
        prefix = 'm'
        break

      case 'pd':
      case 'pendown':
        prefix = 'l'
        break

      case 'lt':
      case 'left':
        dir = -1
      case 'rt':
      case 'right':
        bearing.rotate2D(toRadians(dir * instrs.shift()))
        break

      case 'back':
      case 'bk':
        dir = -1
      case 'forward':
      case 'fd':
        path += ` ${prefix} ${Vector.mul(bearing, dir * instrs.shift())
          .map(Math.round)
          .join(' ')}`
        break

      default:
        throw Error('Parsing Error')
    }
  }
  const re = /^(M|m) (-?\d+(?:\.\d+)?) (-?\d+(?:\.\d+)?) m (-?\d+(?:\.\d+)?) (-?\d+(?:\.\d+)?)/
  while (re.test(path)) {
    path = path.replace(re, (...m) => `${m[1]} ${Number(m[2]) + Number(m[4])} ${Number(m[3]) + Number(m[5])}`)
  }
  return path
}

export function isValid(code) {
  return /(?:(?:^|([\s\[\]]))+((f(?:orwar)?d|b(?:ac)?k|l(?:ef)?t|r(?:igh|epea)?t)\s+(\d+(?:\.\d*)?)|(p(?:en)?(?:up?|d(?:own)?))))+(\])*$/.test(
    code
  )
}
