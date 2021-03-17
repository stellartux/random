const canvas = document.querySelector('canvas'),
  otherCanvas = document.querySelector('canvas#other-canvas'),
  ctx = canvas.getContext('2d'),
  otherCtx = otherCanvas.getContext('2d')

/*
def tocmy(c):
    r1 = c[0]/255
    g1 = c[1]/255
    b1 = c[2]/255

    k = .99-max(r1, g1, b1)

    cyan = (1-r1-k)/(1-k)
    mag = (1-g1-k)/(1-k)
    yellow=(1-b1-k)/(1-k)

    return (cyan, mag, yellow)
*/

/**
 * @param {number[]} color rgb
 */
function toCMY(color) {
  const k = 0.99 - Math.max(...color) / 255
  return color.map((col) => (1 - col / 255 - k) / (1 - k))
}

/*
def torgb(c):
    r = 255*(1-c[0])
    g = 255*(1-c[1])
    b = 255*(1-c[2])

    return (int(r),int(g),int(b))
*/

/**
 * @param {number[]} color rgb
 */
function toRGB(color) {
  return color.map((col) => (255 * (1 - col)) | 0)
}

/**
 * @param {number[]} color rgb
 * @returns {string}
 */
function toHex(color) {
  return '#' + color.map((col) => col.toString(16).padStart(2, '0')).join('')
}

/**
 * @param {string} url
 * Get an image from the internet somewhere
 */
function loadImage(url) {
  return new Promise((then) => {
    const img = new Image()
    img.setAttribute('crossorigin', 'anonymous')
    img.src = url
    img.onload = () => then(img)
  })
}

const form = document.querySelector('form#pick-url')

form.addEventListener('submit', async (ev) => {
  ev.preventDefault()
  changeImage(
    await loadImage(ev.target.querySelector('input[name="url-picker"]').value)
  )
})

/**
 * @param {number} x
 * @param {number} y
 * @param {number} width
 */
const getColorIndicesForCoord = (x, y, width) => {
  const red = y * (width * 4) + x * 4
  return [red, red + 1, red + 2]
}

async function changeImage(image) {
  /*
  for x in range(0, 800):
      for y in range(0, 800):
          out.putpixel((x,y),(255,255,255))
  */
  canvas.width = image.width
  canvas.height = image.height
  canvas.style.maxHeight = image.height
  canvas.style.minWidth = image.width
  ctx.drawImage(image, 0, 0)
  colorHexagon()
}

function colorHexagon() {
  otherCanvas.classList.add('waiting')
  const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height)
  const colours = []

  for (let x = 0; x < canvas.width; x++) {
    for (let y = 0; y < canvas.height; y++) {
      colours.push(
        getColorIndicesForCoord(x, y, canvas.width).map(
          (index) => imageData.data[index]
        )
      )
    }
  }

  otherCtx.clearRect(0, 0, otherCanvas.width, otherCanvas.height)
  otherCtx.stroke = 'none'

  const spotSize = parseInt(
    document.querySelector('input[name="spot-size"]').value
  )
  const alpha = parseInt(document.querySelector('input[name="alpha"]').value)

  colours.forEach((rgb) => {
    const cmy = toCMY(rgb)
    const [u, v] = calculateOffset(cmy)
    const x = (otherCanvas.width / 2.1) * u + otherCanvas.width / 2
    const y = (otherCanvas.height / 2.1) * v + otherCanvas.height / 2

    otherCtx.fillStyle = toHex(toRGB(cmy).concat(alpha))
    otherCtx.beginPath()
    otherCtx.arc(x, y, spotSize, 0, Math.PI * 2)
    otherCtx.fill()
  })
  otherCanvas.classList.remove('waiting')
}

/*
for c in colorscmy:
    u = t[0]*c[0] + bl[0]*c[1] + br[0]*c[2]
    v = t[1]*c[0] + bl[1]*c[1] + br[1]*c[2]
    x = 200 * u + 400
    y = 200 * v + 400
    out.putpixel((int(x), int(y)), (torgb(c)))
*/
/**
 * @param {number[]} c a CMY color array
 */
function calculateOffset(c) {
  const t = [-0.5, 0.87],
    bl = [1, 0],
    br = [-0.5, -0.86],
    u = t[0] * c[0] + bl[0] * c[1] + br[0] * c[2],
    v = t[1] * c[0] + bl[1] * c[1] + br[1] * c[2]

  return [u, v]
}

/** @type {number | null} */
let debounceRef = null
/**
 * Debounces a function so it can only be called once in the given time period
 * @param {Function} func the function to be called
 * @param {number} [time = 100] the debounce period, in milliseconds
 */
export function debounce(func, time = 100) {
  if (!debounceRef) {
    debounceRef = setTimeout(() => {
      func()
      debounceRef = null
    }, time)
  }
}

document
  .querySelector('input[name="spot-size"]')
  .addEventListener('change', () => debounce(colorHexagon, 200))

document
  .querySelector('input[name="alpha"]')
  .addEventListener('change', () => debounce(colorHexagon, 200))

changeImage(await loadImage('https://unsplash.it/600/600'))

/** @type {HTMLInputElement} */
const filePicker = document.querySelector('input[name="file-picker"]')
filePicker.addEventListener('change', async () => {
  const files = filePicker.files
  for (let i = 0; i < files.length; i++) {
    const file = files[i]

    if (!file.type.startsWith('image/')) {
      continue
    }

    changeImage(await createImageBitmap(file))
  }
})
