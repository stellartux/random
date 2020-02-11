/*
 And so we begin. Let's get infinitely curvy.
 We're going to use the L-system technique of generating
 our curves, then parse the L-string into an SVG path.
 The instructions are shorter, so it must be easier.
 We're trying to draw a picture, obviously we need regex.
*/

// The strings generated by our L-system
const lStrings = ['A'] // A is for axiom

// The production rules taken from Wikipedia
const productionRules = {
  A: '-BF+AFA+FB-',
  B: '+AF-BFB-FA+'
}
/*
  https://en.wikipedia.org/wiki/Hilbert_curve#Representation_as_Lindenmayer_system
  Here, "F" means "draw forward", "−" means "turn left 90°",
  "+" means "turn right 90°" and "A" and "B" are ignored during drawing.
*/

// Build our own jQuery from dirt and twigs
const $ = x => document.querySelector(x)

// a function for applying those production rules to our string
function iterateLSystem() {
  // get the last l-string we calculated
  const last = lStrings[lStrings.length - 1]
  // apply the production rules to it and push it into the array
  lStrings.push(last.replace(/(A|B)/g, m => productionRules[m]))
  // strip the unneeded As, Bs and cancelled rotations from the previous string
  lStrings[lStrings.length - 2] = last.replace(/A|B|-\+|\+-/g, '')
}
// generate all the strings we'll need and an extra in case we need a spare
while (lStrings.length <= +$('[name="order"]').max + 1) {
  iterateLSystem()
}

// Generator functions are handy for animation, yield on each frame
// and you're free to animate at your own pace.
function* lStringToPath(n = 1) {
  $('path').setAttribute('stroke-width', 16 - 2 * n)

  // We need to remember where we're going
  // Make some stuff to keep track of that for us
  const directions = ['v ', 'h ', 'v -', 'h -'] // up, right, down, left in SVG
  let currentDirection = 1
  const turnRight = () => { currentDirection = (currentDirection + 1) & 3 }
  const turnLeft = () => { currentDirection = (currentDirection - 1) & 3 }

  // work out how big out picture really is
  const margin = 10
  const totalLength = $('svg').clientWidth - 2 * margin
  // so we can calculate the length each segment needs to be to fill the space
  const len = (totalLength / Math.max(1, 2 ** n - 1)).toFixed(2)

  // now we make our SVG path!
  // start slightly offset
  let dPath = `M ${margin} ${margin} `

  // parse our L-strings the way the wiki told us
  for (const instruction of lStrings[n]) {
    switch (instruction) {
      case '-':
        turnLeft()
        break
      case '+':
        turnRight()
        break
      case 'F':
        dPath += directions[currentDirection] + len + ' '
        $('path').setAttribute('d', dPath)
        // yield each time a line segment is drawn
        // this lets us draw the lines as quickly as we like
        yield
        break
      default:
    }
  }
}

// Keep track of whether we're animating with a reference to the next frame
let nextFrame
// and a function to stop the animation if we're starting a new one
function cancelFrame() {
  if (nextFrame) {
    window.cancelAnimationFrame(nextFrame)
    nextFrame = undefined
  }
}

// calls our line drawing iterator frame by frame to
// animate the drawing of the Hilbert curve
function animator() {
  cancelFrame()
  let framesDrawn = 0
  let currentFrame = 1
  const iter = lStringToPath($('[name="order"]').value)

  ;(function drawFrame() {
    let result = { done: false }
    while (framesDrawn <= currentFrame && !result.done) {
      res = iter.next()
      // fetching this every time lets us speed up the animation while it plays
      framesDrawn += 5.1 - Math.sqrt($('[name="speed"]').value)
    }
    currentFrame += 1

    if (!result.done) {
      nextFrame = window.requestAnimationFrame(drawFrame)
    } else {
      nextFrame = undefined
    }
  })()
}

// or we can draw it all at once, less waiting around
function render() {
  cancelFrame()
  // call the iterator for side effects only
  for (const _ of lStringToPath($('[name="order"]').value));
}

// draw a curve when the page loads to make it prettier
render()

function updateColor(value) {
  $('path').setAttribute('stroke', value)
}
// just toggling attributes, all the colours and shiny are SVG filters in the HTML
function rainbow() {
  if ($('[name="rainbow"]').checked) {
    updateColor('white')
    $('path').setAttribute('stroke', 'url("#rainbowify")')
    $('path').setAttribute('filter', 'url("#shiny")')
  } else {
    updateColor($('[type="color"]').value)
    $('path').setAttribute('filter', 'none')
  }
}
