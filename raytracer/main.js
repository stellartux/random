import { Vector } from './vector.min.js'

const canvas = document.getElementById('canvas')
const ctx = canvas.getContext('2d')
const imageData = ctx.createImageData(320, 180)

function setPixel (xy) {
  const p = (xy[1] * imageData.width + xy[0]) * 4
  rayMarch(xy).forEach((c, i) => { imageData.data[p + i] = c })
  imageData.data[p + 3] = 255
}
const sdSphere = (point, r) => Math.hypot(...point) - r

function rayMarch (xy) {
  xy.sub(new Vector(imageData.width / 2, imageData.height / 2)).mul(1 / imageData.width)
  const ray = {
    origin: new Vector(0, 0, 15),
    position: new Vector(...xy, 12)
  }
  ray.direction = Vector.sub(ray.position, ray.origin).normalize()
  let dist = 0
  let minDist = Infinity
  const threshold = 0.003
  for (let s = 0; s < 100; s++) {
    dist = Math.min(
      sdSphere(Vector.sub(ray.position, new Vector(0.9, 0.2, -0.1)), 0.8),
      sdSphere(Vector.sub(ray.position, new Vector(-0.9, 0.2, 0.1)), 0.7)
    )
    minDist = Math.min(dist, minDist)
    ray.position.add(Vector.mul(ray.direction, dist))
    if (dist < threshold) {
      return Vector.mul(ray.position, 255)
    } else if (dist > 200) break
  }
  const m = Math.max(0, 255 - minDist * 1000)
  return [m, m, m]
}

for (let y = 0; y < imageData.height; y++) {
  for (let x = 0; x < imageData.width; x++) {
    const xy = new Vector(x, y)
    setPixel(xy)
  }
}

// Draw image data to the canvas
ctx.putImageData(imageData, 0, 0)
